-module(map_app_sup).
-author('Harald Welte <laforge@gnumonks.org>').

-behaviour(supervisor).

-export([init/1, start_link/3]).

init([{Mod, Args, Opts}, Ssn, AppMod]) ->
	TcapFunc = {tcap_sap_sup, start_link, [Mod, Args, Opts]},
	TcapSpec = {tcap, TcapFunc, permanent, 4000, worker, [tcap_sap_sup, Mod]},
	% FIXME: this should go...
	TcoName = list_to_atom("tcap_tco_ssn_" ++ integer_to_list(Ssn)),
	SssFunc = {gen_server, start_link, [map_ss_server, [Ssn, TcoName], []]},
	SssSpec = {sss, SssFunc, permanent, 4000, worker, [map_ss_server]},
	{ok, {{one_for_one, 1, 1}, [TcapSpec, SssSpec]}}.


start_link(MAO, Ssn, AppMod) ->
	supervisor:start_link(?MODULE, [MAO, Ssn, AppMod]).
