
-module(map_as_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([bind_ac/1, unbind_ac/1, dump/0]).

-record(ass_state, {sap_sup_pid, as_tbl}).

-record(ass_record, {ac, user_pid}).


% client side

bind_ac(Srv, AcName) when is_list(AcName) ->
	bind_ac(Srv, list_to_tuple(AcName);
bind_ac(Srv, AcName) when is_tuple(AcName) ->
	gen_server:call(Srv, {bind_ac, AcName}).

unbind_ac(Srv, AcName) when is_list(AcName) ->
	unbind_ac(Srv, list_to_tuple(AcName);
unbind_ac(Srv, AcName) when is_tuple(AcName) ->
	gen_server:call(Srv, {unbind_ac, AcName}).

dump() ->
	fixme.


% gen_fsm callbacks

start_link(Ssn, {M, A, O}) when is_integer(Ssn) ->
	ProcName = list_to_atom(?MODULE ++ "_" ++ integer_to_list(Ssn)),
	gen_server:start_link({local, ProcName}, ?MODULE, [Ssn, {M, A, O}], []).

init([Ssn, {M, A, O}]) ->
	{ok, SapSupPid} = tcap_sap_sup:start_link(M, A, O),
	AssTbl = ets:new(list_to_atom(?MODULE ++ "_" ++ integer_to_list(Ssn)),
			 [ordered_set, named_table, {keypos, #ass_record.ac}]),
	{ok, #ass_state{sap_sup_pid = SapSupPid, as_tbl = AssTbl}}.


handle_call({bind_ac, Ac}, {FromPid, _FromRef}, LoopDat) ->
	NewRec = #actbl_record{ac = Ac, user_pid = FromPid},
	case ets:insert_new(LoopDat#ass_state.as_tbl, NewRec) of
		false ->
			{reply, {error, ets_insert}, LoopDat};
		_ ->
			link(FromPid),
			{reply, ok, LoopDat}
	end;

handle_call({unbind_ac, Ac}, {FromPid, _FromRef}, LoopDat) ->
	DelRec = #actbl_record{ac = Ac, user_pid = FromPid},
	ets:delete_object(LoopDat#ass_state.as_tbl, DelRec),
	{reply, ok, LoopDat}.

handle_cast(Info, LoopDat) ->
	error_logger:error_report(["unknown handle_cast",
				  {module, ?MODULE}, {info, Info},
				  {state, LoopDat}]),
	{noreply, LoopDat}.

handle_info({'EXIT', Pid, Reason}, LoopDat) ->
	io:format("EXIT from process ~p (~p), cleaning up tables~n",
		  [Pid, Reason]),
	ets:match_delete(LoopDat#ass_state.as_tbl, #actbl_record{user_pid = Pid}),
	{noreply, LoopDat};
handle_info(Info, LoopDat) ->
	error:logger:error_report(["unknown handle_info",
				  {module, ?MODULE}, {info, Info},
				  {state, LoopDat}]),
	{noreply, LoopDat}.

terminate(Reason, _LoopDat) ->
	io:format("terminating ~p with reason ~p~n", [self(), Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% server side
as_for_ac(Ac, LoopDat) ->
	case ets:lookup(LoopDat#ass_state.as_tbl, Ac) of
		[#ass_record{user_pid = UserPid}] ->
			{ok, UserPid};
		_ ->
			{error, no_such_ac}
	end.

handle_tcap({'TC','BEGIN',indication,
	     I='TC-BEGIN'{appContextName = Ac, dialogueID = DlgId}}, LoopDat) ->
	case as_for_ac(Ac, LoopDat) of
		{ok, UserPid} ->
			gen_fsm:send_event(UserPid, I);
		{error, Reason} ->
			error_logger:error_report(["TC-BEGIN for non-existing AC",
						  {application_context, Ac}]),
			ok
	end;
handle_tcap({'TC', What, indication, P}) when
			What == 'CONTINUE'; What == 'END';
			What == 'U-ABORT'; What == 'P-ABORT';
			What == 'NOTICE' ->
	% look up the Pid for the specific Dialogue Handler
	case as_for_ac(Ac, LoopDat) of
		FIXME FIXME

	ok;
handle_tcap({'TC', 'INVOKE', indication, P}) when
	% look up AS for AC, start new invoke in dialogue

handle_tcap({'TC', What, indication, P}) when
			What == 'RESULT-L'; What == 'RESULT-NL';
			What == 'U-ERROR'; What == 'L-CANCEL';
			What == 'L-REJECT'; What == 'U-REJECT';
			What == 'TIMER-RESET' ->
	% look up the gen_fsm for the specific Invoke

	ok.

