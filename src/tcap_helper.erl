% TCAP codec helper functions for test scripts and the like

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
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(tcap_helper).
-author('Harald Welte <laforge@gnumonks.org>').

-include("tcap_asn.hrl").

-export([build_inv_comp/3, build_inv_comp/4,
	 build_retres_comp/2, build_retreslast_comp/2, build_retreslast_comp/1,
	 enc_component/1,
	 build_begin/4, build_begin/3, build_end/3,
	 build_inv_begin/5, build_retresl_end/4
 	]).

format_id(undefined) ->
	asn1_NOVALUE;
format_id(Int) when is_integer(Int) ->
	{present, Int}.

process_uint32(Int) when is_integer(Int) ->
	<<Int:32/big>>;
process_uint32(Int) when is_list(Int); is_binary(Int) ->
	Int.

process_undefined(undefined) ->
	asn1_NOVALUE;
process_undefined(Other) ->
	Other.


% build a decoded Invoke component
build_inv_comp(InvIdIn, Opcode, Argument, LinkedIdIn) ->
	InvId = format_id(InvIdIn),
	LinkedId = format_id(LinkedIdIn),
	{invoke, #'Invoke'{invokeId = InvId, linkedId = LinkedId,
				opcode = {local, Opcode},
				argument = Argument}}.
build_inv_comp(InvIdIn, Opcode, Argument) ->
	build_inv_comp(InvIdIn, Opcode, Argument, undefined).

% build a BER-encoded ReturnResult component
build_retres_comp(InvIdIn, Data) ->
	InvId = format_id(InvIdIn),
	{resurnResult, #'ReturnResult'{invokeId = InvId, result = Data}}.

% build a BER-encoded ReturnResultLast component
build_retreslast_comp(InvIdIn, Data) ->
	InvId = format_id(InvIdIn),
	{returnResultLast, #'ReturnResult'{invokeId = InvId, result = Data}}.
build_retreslast_comp(InvIdIn) ->
	build_retreslast_comp(InvIdIn, asn1_NOVALUE).

% helper function for BER encoding a Component
enc_component(Comp) ->
	{ok, Ret} = tcap_asn:encode('Component', Comp),
	Ret.

% helper function for BER-encoding the DialoguePDU and wrapping it in 'EXTERNAL' type
enc_dialg_ext(Dpdu) ->
	{ok, DpduEnc} = tcap_asn:encode('DialoguePDU', Dpdu),
	ExtPdu = #'EXTERNAL'{'direct-reference' = {0,0,17,773,1,1,1},
			     'encoding' = {'single-ASN1-type', DpduEnc}},
	ExtPdu.

% Build a binary-encoded 'Begin' DialoguePortion message with specified components
build_begin(OtidIn, ACname, ComponentsIn) ->
	build_begin(OtidIn, ACname, asn1_NOVALUE, ComponentsIn).
build_begin(OtidIn, ACname, UserDlgInfoIn, ComponentsIn) ->
	Otid = process_uint32(OtidIn),
	UserDlgInfo = process_undefined(UserDlgInfoIn),
	Dpdu = {dialogueRequest, #'AARQ-apdu'{'protocol-version' = [version1],
					      'application-context-name' = ACname,
					      'user-information' = UserDlgInfo}},
	ExtPdu = enc_dialg_ext(Dpdu),
	{ok, EncComponents} = tcap_asn:encode('Components', ComponentsIn),
	Msg={'begin', #'Begin'{otid = Otid, dialoguePortion = ExtPdu, components = EncComponents}},
	enc_msg(Msg).

% Build a binary-encoded 'End' DialoguePortion message with specified components
build_end(DtidIn, ACname, ComponentsIn) ->
	Dtid = process_uint32(DtidIn),
	Dpdu = {dialogueResponse, #'AARE-apdu'{'protocol-version' = [version1],
						'application-context-name' = ACname,
						'result' = 0}},
	ExtPdu = enc_dialg_ext(Dpdu),
	{ok, EncComponents} = tcap_asn:encode('Components', ComponentsIn),
	Msg = {'end', #'End'{dtid = Dtid, dialoguePortion = ExtPdu, components = EncComponents}},
	enc_msg(Msg).


enc_msg(Dlg) ->
	{ok, Ret} = tcap_asn:encode('TCMessage', Dlg),
	Ret.


% build a BER-encoded Begin dialogue with Invoke component
build_inv_begin(DlgId, InvId, ACname, Opcode, Argument) ->
	C = build_inv_comp(InvId, Opcode, Argument),
	build_begin(DlgId, ACname, [C]).

% build a BER-encoded End dialogue with ReturnResultLast component
build_retresl_end(Dtid, InvId, ACname, Argument) ->
	C = build_retreslast_comp(InvId, Argument),
	build_end(Dtid, ACname, [C]).
