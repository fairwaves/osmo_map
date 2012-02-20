-module(map_ss_server).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("TCAP/include/tcap.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/3, bind_ac/3, unbind_ac/3, dump/0]).

-record(state, {
		supervisor,
		tcap_pid,
		as_tbl,
		dlg_tbl
}).

-record(ass_record, {
		ac,
		module
}).

-record(dlg_record, {
		dialogue_id,
		pid
}).


% client side

bind_ac(Srv, AcName, Module) when is_list(AcName) ->
	bind_ac(Srv, list_to_tuple(AcName), Module);
bind_ac(Srv, AcName, Module) when is_tuple(AcName) ->
	gen_server:call(Srv, {bind_ac, AcName, Module}).

unbind_ac(Srv, AcName, Module) when is_list(AcName) ->
	unbind_ac(Srv, list_to_tuple(AcName), Module);
unbind_ac(Srv, AcName, Module) when is_tuple(AcName) ->
	gen_server:call(Srv, {unbind_ac, AcName, Module}).

dump() ->
	fixme.


% gen_fsm callbacks

start_link(Supervisor, Ssn, TCO) when is_integer(Ssn) ->
	ProcName = list_to_atom(?MODULE ++ "_" ++ integer_to_list(Ssn)),
	gen_server:start_link({local, ProcName}, ?MODULE, [Supervisor, Ssn, TCO], []).

init([Supervisor, Ssn, TCO]) ->
	AssTbl = ets:new(list_to_atom(?MODULE ++ "_" ++ integer_to_list(Ssn)),
			 [ordered_set, named_table, {keypos, #ass_record.ac}]),
	DlgTbl = ets:new(list_to_atom(?MODULE ++ "_" ++ integer_to_list(Ssn) ++ "_dlg"),
			 [ordered_set, named_table, {keypos, #dlg_record.dialogue_id}]),
	{ok, #state{supervisor = Supervisor, tcap_pid = TCO, as_tbl = AssTbl, dlg_tbl = DlgTbl}}.


handle_call({bind_ac, Ac, Module}, _From, LoopDat) ->
	NewRec = #ass_record{ac = Ac, module = Module},
	case ets:insert_new(LoopDat#state.as_tbl, NewRec) of
		false ->
			{reply, {error, ets_insert}, LoopDat};
		_ ->
			{reply, ok, LoopDat}
	end;

handle_call({unbind_ac, Ac, Module}, _From, LoopDat) ->
	DelRec = #ass_record{ac = Ac, module = Module},
	ets:delete_object(LoopDat#state.as_tbl, DelRec),
	{reply, ok, LoopDat}.

handle_cast(W={'TC',_,_,_}, LoopDat) ->
	handle_tcap(W, LoopDat);
handle_cast(Info, LoopDat) ->
	error_logger:error_report(["unknown handle_cast",
				  {module, ?MODULE}, {info, Info},
				  {state, LoopDat}]),
	{noreply, LoopDat}.

handle_info(Info, LoopDat) ->
	error_logger:error_report(["unknown handle_info",
				  {module, ?MODULE}, {info, Info},
				  {state, LoopDat}]),
	{noreply, LoopDat}.

terminate(Reason, _LoopDat) ->
	io:format("terminating ~p with reason ~p~n", [self(), Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% server side
handle_tcap({'TC','BEGIN',indication,
	     I=#'TC-BEGIN'{appContextName = Ac, dialogueID = DlgId}}, LoopDat) ->
	case dlg_pid_for_id(DlgId, LoopDat) of
		{ok, _Pid} ->
			error_logger:error_report(["TC-BEGIN for existing Dialogue",
						   {dialogue_id, DlgId}]),
			Abrt = gen_abort(I, applicationContextNotSupported),
			tcap_user:send_prim(LoopDat#state.tcap_pid, {'TC','U-ABORT',request,Abrt});
		{error, _} ->
			case mod_for_ac(Ac, LoopDat) of
				{ok, Module} ->
					Args = [LoopDat#state.tcap_pid, Ac, DlgId, Module],
					ChildName = list_to_atom("dlg_" ++ integer_to_list(DlgId)),
					Mfa = {gen_server, start_link, [map_dlg_server, Args, []]},
					ChildSpec = {ChildName, Mfa, temporary, 1000, worker, [map_dlg_server, Module]},
					{ok, Pid} = supervisor:start_child(LoopDat#state.supervisor, ChildSpec),
					gen_fsm:send_event(Pid, I),
					% FIXME: how to safely remove the Pid in all cases of dialogue termination?
					ets:insert(LoopDat#state.dlg_tbl, #dlg_record{dialogue_id=DlgId, pid=Pid});
				{error, _Reason} ->
					error_logger:error_report(["TC-BEGIN for non-existing AC",
								  {application_context, Ac}]),
					% send a TC-U-ABORT
					Abrt = gen_abort(I, applicationContextNotSupported),
					tcap_user:send_prim(LoopDat#state.tcap_pid, {'TC','U-ABORT',request,Abrt})
			end
	end;
handle_tcap({'TC', What, indication, P}, LoopDat) when
			What == 'CONTINUE'; What == 'END';
			What == 'U-ABORT'; What == 'P-ABORT';
			What == 'NOTICE' ->
	DlgId = tcap_user:get_dialg_id(P),
	% look up the Pid for the specific Dialogue Handler
	case dlg_pid_for_id(DlgId, LoopDat) of
		{ok, Pid} ->
			gen_fsm:send_event(Pid, P);
		_ ->
			error_logger:error_report(["TC-Dialogue non-existing DialogueID",
						   {dialogue_id, DlgId}]),
			Abrt = gen_abort(P, dialogueRefused),
			tcap_user:send_prim(LoopDat#state.tcap_pid, {'TC','U-ABORT',request,Abrt})
	end;
handle_tcap({'TC', What, indication, P}, LoopDat) when
			What == 'INVOKE';
			What == 'RESULT-L'; What == 'RESULT-NL';
			What == 'U-ERROR'; What == 'L-CANCEL';
			What == 'L-REJECT'; What == 'U-REJECT';
			What == 'TIMER-RESET' ->
	DlgId = tcap_user:get_dialg_id(P),
	% look up the Pid for the specific Dialogue Handler
	case dlg_pid_for_id(DlgId, LoopDat) of
		{ok, Pid} ->
			gen_fsm:send_event(Pid, P);
		_ ->
			error_logger:error_report(["TC-Component non-existing DialogueID",
						   {dialogue_id, DlgId}]),
			Abrt = gen_abort(P, dialogueRefused),
			tcap_user:send_prim(LoopDat#state.tcap_pid, {'TC','U-ABORT',request,Abrt})
	end.

mod_for_ac(Ac, LoopDat) ->
	case ets:lookup(LoopDat#state.as_tbl, Ac) of
		[#ass_record{module = Module}] ->
			{ok, Module};
		_ ->
			{error, no_such_ac}
	end.

dlg_pid_for_id(DlgId, LoopDat) when is_record(LoopDat, state) ->
	case ets:lookup(LoopDat#state.dlg_tbl, DlgId) of
		[{DlgId, Pid}] ->
			{ok, Pid};
		_ ->
			{error, notfound}
	end.

gen_abort(#'TC-BEGIN'{qos = Qos, appContextName = AcName, dialogueID = DlgId},
	  Reason) ->
	#'TC-U-ABORT'{qos = Qos, appContextName = AcName, dialogueID = DlgId,
		      abortReason = Reason}.

gen_reject(#'TC-INVOKE'{dialogueID = DlgId, invokeID = InvId}, Code) ->
	#'TC-U-REJECT'{dialogueID = DlgId, invokeID = InvId, problemCode = Code};
gen_reject(#'TC-RESULT-L'{dialogueID = DlgId, invokeID = InvId}, Code) ->
	#'TC-U-REJECT'{dialogueID = DlgId, invokeID = InvId, problemCode = Code};
gen_reject(#'TC-RESULT-NL'{dialogueID = DlgId, invokeID = InvId}, Code) ->
	#'TC-U-REJECT'{dialogueID = DlgId, invokeID = InvId, problemCode = Code}.



