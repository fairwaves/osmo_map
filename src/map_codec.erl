% GSM MAP codec wrapper functions

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(map_codec).
-author('Harald Welte <laforge@gnumonks.org>').
%-include("map.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

-export([parse_tcap_msg/1, encode_tcap_msg/1]).
-export([parse_addr_string/1, encode_addr_string/1]).

-define(MAP_ADDR_NAT_UNKNOWN,		0).
-define(MAP_ADDR_NAT_INTERNATIONAL,	1).
-define(MAP_ADDR_NAT_NATIONAL,		2).
-define(MAP_ADDR_NAT_NETWORK_SPEC,	3).
-define(MAP_ADDR_NAT_SUBSCRIBER,	4).
-define(MAP_ADDR_NAT_RES,		5).
-define(MAP_ADDR_NAT_ABBREVIATED,	6).
-define(MAP_ADDR_NAT_RES_FOR_EXT,	7).

% convert from MAP -> ISUP 'nature of address'
nature_map2isup(NatureMap) ->
	case NatureMap of
		?MAP_ADDR_NAT_INTERNATIONAL -> ?ISUP_ADDR_NAT_INTERNATIONAL;
		?MAP_ADDR_NAT_NATIONAL -> ?ISUP_ADDR_NAT_NATIONAL;
		?MAP_ADDR_NAT_SUBSCRIBER -> ?ISUP_ADDR_NAT_SUBSCRIBER;
		_ -> NatureMap
	end.

% convert from ISUP -> MAP 'nature of address'
nature_isup2map(NatureIsup) ->
	case NatureIsup of
		?ISUP_ADDR_NAT_INTERNATIONAL -> ?MAP_ADDR_NAT_INTERNATIONAL;
		?ISUP_ADDR_NAT_NATIONAL -> ?MAP_ADDR_NAT_NATIONAL;
		?ISUP_ADDR_NAT_SUBSCRIBER -> ?MAP_ADDR_NAT_SUBSCRIBER;
		_ -> NatureIsup
	end.


parse_addr_string(AddrList) when is_list(AddrList) ->
	parse_addr_string(list_to_binary(AddrList));
parse_addr_string(AddrBin) when is_binary(AddrBin) ->
	<<1:1, NatureMap:3, Numplan:4, Remain/binary>> = AddrBin,
	PhoneNum = isup_codec:parse_isup_party(Remain, 0),
	NatureIsup = nature_map2isup(NatureMap),
	#party_number{nature_of_addr_ind = NatureIsup,
		      numbering_plan = Numplan,
		      phone_number = PhoneNum}.

encode_addr_string(#party_number{nature_of_addr_ind = NatureIsup,
				 numbering_plan = Numplan,
				 phone_number = PhoneNum}) ->
	NatureMap = nature_isup2map(NatureIsup),
	{PhoneBin, _OddEven} = isup_codec:encode_isup_party(PhoneNum),
	Bin = <<1:1, NatureMap:3, Numplan:4, PhoneBin/binary>>,
	binary_to_list(Bin).

parse_tcap_msg(MsgBin) when is_binary(MsgBin) ->
	Msg = binary_to_list(MsgBin),
	parse_tcap_msg(Msg);
parse_tcap_msg(Msg) when is_list(Msg) ->
	case asn1rt:decode('map', 'MapSpecificPDUs', Msg) of
		{ok, {Type, TcapMsgDec}} ->
			{Type, TcapMsgDec};
		Error ->
			Error
	end.

encode_tcap_msg({Type, TcapMsgDec}) ->
	case asn1rt:encode('map', 'MapSpecificPDUs', {Type, TcapMsgDec}) of
		{ok, List} ->
			list_to_binary(List);
		Error ->
			Error
	end.
