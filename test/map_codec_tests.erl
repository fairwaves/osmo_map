-module(map_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("map.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_ss7/include/m2ua.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").

% modified version of assertEqual()
-define(assertEqualArgs(Expect, Expr, Args),
        ((fun (__X) ->
            case (Expr) of
                __X -> ok;
                __V ->  ?debugFmt("Expected: ~w~nValue: ~w~n", [__X, __V]),
			.erlang:error({assertEqual_failed,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??Expr)},
                                       {expected, __X},
                                       {value, __V}] ++ Args})
            end
          end)(Expect))).
-define(_assertEqualArgs(Expect, Expr, Args), ?_test(?assertEqual(Expect, Expr, Args))).

-define(TCAP_MSG_BIN, <<100,65,73,4,81,1,2,200,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,13,163,11,2,1,64,2,1,8,48,3,10,1,0>>).
-define(TCAP_MSG_DEC, {'end',{'MapSpecificPDUs_end',[81,1,2,200],{'EXTERNAL',{0,0,17,773,1,1,1},asn1_NOVALUE,asn1_NOVALUE, {'single-ASN1-type', [97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0]}},[{basicROS,{returnError,{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnError',{present,64},{local,8},{'RoamingNotAllowedParam',plmnRoamingNotAllowed,asn1_NOVALUE,asn1_NOVALUE}}}}]}}).

parse_test() ->
	?assertEqual(?TCAP_MSG_DEC, map_codec:parse_tcap_msg(?TCAP_MSG_BIN)).
encode_test() ->
	?assertEqual(?TCAP_MSG_BIN, map_codec:encode_tcap_msg(?TCAP_MSG_DEC)).

-define(ADDR_DEC, #party_number{nature_of_addr_ind = ?ISUP_ADDR_NAT_INTERNATIONAL,
				internal_net_num = undefined,
				number_incompl_ind = undefined,
				numbering_plan = 0,
				present_restrict = undefined,
				screening_ind = undefined,
				phone_number = [1,2,3,4,5,6,7,8,9,0]}).
-define(ADDR_LIST, [144,33,67,101,135,9]).

encode_addr_list_test() ->
	?assertEqual(?ADDR_LIST, map_codec:encode_addr_string(?ADDR_DEC)).
encode_addr_int_test() ->
	AddrDec = ?ADDR_DEC,
	?assertEqual(?ADDR_LIST, map_codec:encode_addr_string(AddrDec#party_number{phone_number=1234567890})).
decode_addr_list_test() ->
	?assertEqual(?ADDR_DEC, map_codec:parse_addr_string(?ADDR_LIST)).
decode_addr_bin_test() ->
	?assertEqual(?ADDR_DEC, map_codec:parse_addr_string(list_to_binary(?ADDR_LIST))).


pcap_parse_test_() ->
	{ timeout, 5*60, [ fun pcap_parse_t/0 ] }.

% parser test for real-world MAP/TCAP data
pcap_parse_t() ->
	Args = [{rewrite_fn, fun pcap_cb/5}],
	File = "../priv/map.pcap",
	case file:read_file_info(File) of
		{ok, _Info} ->
			{ok, NrPkts} = ?debugTime("PCAP", osmo_ss7_pcap:pcap_apply(File, "", Args)),
			?debugFmt("Parsed ~p PCAP packets~n", [NrPkts]);
		{error, _Reason} ->
			?debugFmt("Skipping PCAP based tests as no ~p could be found~n",
				  [File])
	end.

pcap_cb(sctp, _From, Path, 2, DataBin) ->
	{ok, M2ua} = m2ua_codec:parse_m2ua_msg(DataBin),
	M2uaReenc = m2ua_codec:encode_m2ua_msg(M2ua),
	?assertEqualArgs(DataBin, M2uaReenc, [{layer, m2ua}, {path, Path}]),
	handle_m2ua(M2ua, Path),
	DataBin.

handle_m2ua(M2ua = #m2ua_msg{msg_class = ?M2UA_MSGC_MAUP,
			     msg_type = ?M2UA_MAUP_MSGT_DATA,
			     parameters = Params}, Path) ->
	{_Len, M2uaPayload} = proplists:get_value(16#300, Params),
	Mtp3 = mtp3_codec:parse_mtp3_msg(M2uaPayload),
	Mtp3Reenc = mtp3_codec:encode_mtp3_msg(Mtp3),
	?assertEqualArgs(M2uaPayload, Mtp3Reenc, [{layer, mtp3}, {path, Path}]),
	handle_mtp3(Mtp3, Path ++ [M2ua]);
handle_m2ua(M2ua = #m2ua_msg{}, _Path) ->
	M2ua.

handle_mtp3(Mtp3 = #mtp3_msg{service_ind = ?MTP3_SERV_SCCP,
			     payload = Payload}, Path) ->
	{ok, SccpDec} = sccp_codec:parse_sccp_msg(Payload),
	SccpReenc = sccp_codec:encode_sccp_msg(SccpDec),
	% We cannot assume that the data is equal due to SCCP allowing
	% different encodings of the same data. instead we re-decode
	{ok, SccpReencDec} = sccp_codec:parse_sccp_msg(SccpReenc),
	?assertEqualArgs(SccpDec, SccpReencDec, [{layer, sccp}, {path, Path}]),
	handle_sccp(SccpDec, Path ++ [Mtp3]);
handle_mtp3(Mtp3 = #mtp3_msg{}, _Path) ->
	Mtp3.

handle_sccp(S = #sccp_msg{msg_type = ?SCCP_MSGT_UDT, parameters = Params}, Path) ->
	UserData = proplists:get_value(user_data, Params),
	PathOut = Path ++ [S],
	case map_codec:parse_tcap_msg(UserData) of
	{error, Error} ->
		ErrTuple = {Error, erlang:get_stacktrace(), []},
		?debugFmt("Path: ~p~nMAP Decode Error: ~w~n", [PathOut, ErrTuple]),
		erlang:error(ErrTuple);
	MapDec ->
		%?debugFmt("~w~n", [MapDec]),
		case map_codec:encode_tcap_msg(MapDec) of
		{error, Error} ->
			ErrTuple = {Error, erlang:get_stacktrace(), [{map_dec, MapDec}]},
			?debugFmt("Path: ~p~nMAP Encode Error: ~w~n", [PathOut, ErrTuple]),
			erlang:error(ErrTuple);
		MapReenc ->
			%?assertEqualArgs(UserData, MapReenc, [{layer, map}, {path, Path}]),
			MapReencDec = map_codec:parse_tcap_msg(MapReenc),
			?assertEqualArgs(MapDec, MapReencDec, [{layer, map}, {path, Path}])
		end
	end,
	S;
handle_sccp(S = #sccp_msg{}, _Path) ->
	S.
