-module(map_dlg_server).
-author('Harald Welte <laforge@gnumonks.org>').

-behaviour(gen_server).

-include_lib("TCAP/include/tcap.hrl").

-export([init/1, handle_cast/2, code_change/3, handle_call/3, handle_info/2, terminate/2]).

-record(state, {
		supervisor,
		tcap,
		dialogue_id,
		app_ctx,
		app_ctx_version,
		app_mod,
		ssm_tbl
}).

-record(ssm_rec, {
		invoke_id,
		pid
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Supervisor, Tcap, AppCtx, DlgId, AppMod]) ->
	% AC Version is the last digit of the Application Context
	ACList = tuple_to_list(AppCtx),
	ACVersion = lists:nth(length(ACList), ACList),
	Tbl = ets:new(ssn_table, [ordered_set, {keypos, #ssm_rec.invoke_id}]),
	{ok, #state{tcap = Tcap, app_ctx = AppCtx, app_ctx_version = ACVersion,
		    dialogue_id = DlgId, ssm_tbl = Tbl, app_mod = AppMod,
	    	    supervisor = Supervisor}}.

% Primitives from the TCAP stack (TC-user API)
handle_cast({'TC','BEGIN',indication,
	     #'TC-BEGIN'{dialogueID = DlgId, appContextName = AppCtx,
		     	 userInfo = UserInfo}}, LoopDat) ->
	% this is basically an Assert
	AppCtx = LoopDat#state.app_ctx,
	DlgId = LoopDat#state.dialogue_id,
	{noreply, LoopDat};
handle_cast({'TC','INVOKE',indication,
	      I=#'TC-INVOKE'{linkedID=asn1_NOVALUE,operation={local, Op},
	      		     invokeID=InvokeId}}, LoopDat) ->
	% Invoke with no Linked ID
	#state{app_ctx = AppCtx, app_mod = Mod} = LoopDat,
	case Mod:get_ssm_mod(AppCtx, Op) of
		{ok, Module} ->
			Ssn = 2342, % FIXME
			Name = list_to_atom("map_ssn_" ++ integer_to_list(Ssn) ++ "_"
					    ++ integer_to_list(LoopDat#state.dialogue_id) + "_"
					    ++ integer_to_list(InvokeId)),
			Opts = [self()],
			Mfa = {gen_fsm, start_link, [Name, Module, Opts, [{debug, [trace]}]]},
			ChildSpec = {Name, Mfa, temporary, 1000, worker, [Module]},
			{ok, Pid} = supervisor:start_child(LoopDat#state.supervisor, ChildSpec),
			ets:insert(LoopDat#state.ssm_tbl, #ssm_rec{invoke_id = InvokeId, pid = Pid}),
			decode_to_pid(I, Pid);
		_ ->
			error_logger:error_report(["TC-INVOKE for unknown Operation",
						   {operation, Op}]),
			% FIXME
			ok
	end,
	{noreply, LoopDat};
handle_cast({'TC','INVOKE',indication,
	      I=#'TC-INVOKE'{linkedID = LinkedId}},  LoopDat) ->
	% INVOKE linked to exisiting other Invoke
	case get_ssm_by_id(LinkedId, LoopDat) of
		{ok, Pid} ->
			decode_to_pid(I, Pid);
		_ ->
			error_logger:error_report(["Linked TC-INVOKE for unknown Invoke",
						   {linked_id, LinkedId}]),
			% FIXME
			ok
	end,
	{noreply, LoopDat};
handle_cast({'TC','END',indication,P}, LoopDat) ->
	% TCAP has told us that the dialogue has ended. We simply terminate,
	% and as the SSMs are linked to us, they will all terminate, too.
	{stop, normal, LoopDat};
handle_cast({'TC',_What,indication,P}, LoopDat) ->
	InvokeId = tcap_user:get_invoke_id(P),
	case get_ssm_by_id(InvokeId, LoopDat) of
		{ok, Pid} ->
			decode_to_pid(P, Pid);
		_ ->
			error_logger:error_report(["TC Primitive for unknown Invoke",
						   {prim, P},  {invoke_id, InvokeId}]),
			% FIXME
			ok
	end,
	{noreply, LoopDat};

% Primitives from the SSMs
handle_cast({result_l, InvokeId, LocalOp, Rec}, LoopDat) ->
	case map_comp:encode_map_comp(LocalOp, result, Rec) of 
		{ok, MapEnc} ->
			R = #'TC-RESULT-L'{dialogueID = LoopDat#state.dialogue_id,
					   invokeID = InvokeId, parameters = MapEnc},
			tcap_user:send_prim(LoopDat#state.tcap, {'TC','RESULT-L', request, R}),
			ets:delete_object(LoopDat#state.ssm_tbl, InvokeId);
		_ ->
			error_logger:error_report(["SSM primitive couldn't be encoded",
						   {operation, LocalOp},
						   {ssm_record, Rec}]),
			ok
	end,
	{noreply, LoopDat};
handle_cast({result_nl, InvokeId, LocalOp, Rec}, LoopDat) when is_record(Rec, 'TC-RESULT-NL') ->
	case map_comp:encode_map_comp(LocalOp, result, Rec) of
		{ok, MapEnc} ->
			R = #'TC-RESULT-NL'{dialogueID = LoopDat#state.dialogue_id,
					    invokeID = InvokeId, parameters = MapEnc},
			tcap_user:send_prim(LoopDat#state.tcap, {'TC','RESULT-NL', request, R});
		_ ->
			error_logger:error_report(["SSM primitive couldn't be encoded",
						   {operation, LocalOp},
						   {ssm_record, Rec}]),
			ok
	end,
	{noreply, LoopDat};
handle_cast({u_error, InvokeId, Error, Rec}, LoopDat) ->
	case map_comp:encode_map_err(Error, Rec) of
		{ok, MapEnc} ->
			E = #'TC-U-ERROR'{dialogueID = LoopDat#state.dialogue_id,
					  invokeID = InvokeId, error = Error,
					  parameters = MapEnc},
			tcap_user:send_prim(LoopDat#state.tcap, {'TC','U-ERROR', request, E});
		_ ->
			error_logger:error_report(["SSM U-ERROR couldn't be encoded",
						   {error_nr, Error},
						   {ssm_record, Rec}]),
			ok
	end,
	{noreply, LoopDat};
handle_cast({'end'}, LoopDat) ->
	% User SSM has requested the dialogue to END, we issue a corresponding
	% TC-END.req to TCAP - and as the SSMs are linked to us, they will all
	% terminate, too.
	E = #'TC-END'{dialogueID = LoopDat#state.dialogue_id,
		      appContextName = LoopDat#state.app_ctx},
	tcap_user:send_prim(LoopDat#state.tcap, {'TC','END',request,E}),
	{stop, normal, LoopDat};
handle_cast({continue}, LoopDat) ->
	C = #'TC-CONTINUE'{dialogueID = LoopDat#state.dialogue_id,
			   appContextName = LoopDat#state.app_ctx},
	tcap_user:send_prim(LoopDat#state.tcap, {'TC','CONTINUE',request,C}),
	{next_state, LoopDat}.

handle_call(What, From, LoopDat) ->
	error_logger:error_report(["Unknown call", {from, From}, {data, What}]),
	{noreply, LoopDat}.

handle_info(What, LoopDat) ->
	error_logger:error_report(["Unknown info", {data, What}]),
	{noreply, LoopDat}.

code_change(_OldVsn, LoopDat, _Extra) ->
	{ok, LoopDat}.

terminate(_Reason, LoopDat) ->
	ets:delete(LoopDat#state.ssm_tbl),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intenral functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_to_pid(I, Pid) ->
	% FIXME: catch exceptions here...
	I2 = map_comp:decode_tcap_prim(I),
	I3 = map_helper:postproc(I2, post),
	to_pid(I3, Pid).

to_pid(#'TC-INVOKE'{operation = {local, LocalOp}, parameters = Params}, Pid) ->
	gen_fsm:send_event(Pid, {invoke, LocalOp, Params});
to_pid(#'TC-RESULT-L'{operation = {local, LocalOp}, parameters = Params}, Pid) ->
	gen_fsm:send_event(Pid, {result_l, LocalOp, Params});
to_pid(#'TC-RESULT-NL'{operation = {local, LocalOp}, parameters = Params}, Pid) ->
	gen_fsm:send_event(Pid, {result_nl, LocalOp, Params});
to_pid(#'TC-U-ERROR'{error = {local, Err}, parameters = Params}, Pid) ->
	gen_fsm:send_event(Pid, {u_error, Err, Params}).

get_ssm_mod_by_op(Op, #state{app_ctx = AppCtx, app_mod = Mod}) ->
	Mod:get_ssm_mod(AppCtx, Op).

get_ssm_by_id(Id, LoopDat) ->
	case ets:lookup(LoopDat#state.ssm_tbl, Id) of
		[#ssm_rec{pid = Pid}] ->
			{ok, Pid};
		_ ->
			{error, notfound}
	end.
