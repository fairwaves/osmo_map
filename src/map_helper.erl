% GSM MAP codec wrapper functions

% (C) 2011-2012 by Harald Welte <laforge@gnumonks.org>
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

-module(map_helper).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_map/include/map_operations.hrl").

-export([postproc/2, postproc_gt/2, postproc_imsi/2, postproc_msisdn/2,
	 encode_op/3, decode_op/3, timer4op/1, class4op/1]).

postproc(M=#'UpdateLocationArg'{imsi = Imsi, 'msc-Number' = Msc, 'vlr-Number' = Vlr,
				    'v-gmlc-Address' = Gmlc}, Mode) ->
	M#'UpdateLocationArg'{imsi = postproc_imsi(Imsi, Mode),
			      'msc-Number' = postproc_gt(Msc, Mode),
			      'vlr-Number' = postproc_gt(Vlr, Mode),
		      	      'v-gmlc-Address' = postproc_gt(Gmlc, Mode)};
postproc(M=#'UpdateLocationRes'{'hlr-Number' = Hlr}, Mode) ->
	M#'UpdateLocationRes'{'hlr-Number' = postproc_gt(Hlr, Mode)};
postproc(M=#'UpdateGprsLocationArg'{imsi = Imsi, 'sgsn-Address' = Sgsn}, Mode) ->
	M#'UpdateGprsLocationArg'{imsi = postproc_imsi(Imsi, Mode),
				  'sgsn-Address' = postproc_gt(Sgsn, Mode)};
postproc(M=#'UpdateGprsLocationRes'{'hlr-Number' = Hlr}, Mode) ->
	M#'UpdateGprsLocationRes'{'hlr-Number' = postproc_gt(Hlr, Mode)};
postproc(M=#'SendAuthenticationInfoArg'{imsi = Imsi}, Mode) ->
	M#'SendAuthenticationInfoArg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'ReadyForSM-Arg'{imsi = Imsi}, Mode) ->
	M#'ReadyForSM-Arg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'MO-ForwardSM-Arg'{imsi = Imsi}, Mode) ->
	M#'MO-ForwardSM-Arg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'RoutingInfoForSM-Res'{imsi = Imsi}, Mode) ->
	M#'RoutingInfoForSM-Res'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'DeactivateTraceModeArg'{imsi = Imsi}, Mode) ->
	M#'DeactivateTraceModeArg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'ActivateTraceModeArg'{imsi = Imsi}, Mode) ->
	M#'ActivateTraceModeArg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'InsertSubscriberDataArg'{imsi = Imsi, msisdn = Msisdn}, Mode) ->
	M#'InsertSubscriberDataArg'{imsi = postproc_imsi(Imsi, Mode),
				    msisdn = postproc_msisdn(Msisdn, Mode)};
postproc(M=#'AuthenticationFailureReportArg'{imsi = Imsi}, Mode) ->
	M#'AuthenticationFailureReportArg'{imsi = postproc_imsi(Imsi, Mode)};
postproc(M=#'AlertServiceCentreArg'{msisdn = Msisdn, serviceCentreAddress = Smsc}, Mode) ->
	M#'AlertServiceCentreArg'{msisdn = postproc_msisdn(Msisdn, Mode),
				  serviceCentreAddress = postproc_gt(Smsc, Mode)};
postproc(M=#'RoutingInfoForSM-Arg'{msisdn = Msisdn, serviceCentreAddress = Smsc}, Mode) ->
	M#'RoutingInfoForSM-Arg'{msisdn = postproc_msisdn(Msisdn, Mode),
				 serviceCentreAddress = postproc_gt(Smsc, Mode)};
postproc(M=#'ProvideRoamingNumberArg'{imsi = Imsi, msisdn = Msisdn,
					  'gmsc-Address' = Gmsc}, Mode) ->
	M#'ProvideRoamingNumberArg'{imsi = postproc_imsi(Imsi, Mode),
				    msisdn = postproc_msisdn(Msisdn, Mode),
				    'gmsc-Address' = postproc_gt(Gmsc, Mode)};
postproc(M=#'SendRoutingInfoRes'{msisdn = Msisdn}, Mode) ->
	M#'SendRoutingInfoRes'{msisdn = postproc_msisdn(Msisdn, Mode)};
postproc(M=#'SendRoutingInfoArg'{msisdn = Msisdn, 'gmsc-OrGsmSCF-Address' = Gmsc}, Mode) ->
	M#'SendRoutingInfoArg'{msisdn = postproc_msisdn(Msisdn, Mode),
			       'gmsc-OrGsmSCF-Address' = postproc_gt(Gmsc, Mode)};
postproc(M=#'USSD-Arg'{msisdn = Msisdn}, Mode) ->
	M#'USSD-Arg'{msisdn = postproc_msisdn(Msisdn, Mode)};
postproc(M=#'ReportSM-DeliveryStatusArg'{msisdn = Msisdn,
					     serviceCentreAddress = Smsc}, Mode) ->
	M#'ReportSM-DeliveryStatusArg'{msisdn = postproc_msisdn(Msisdn, Mode),
					serviceCentreAddress = postproc_gt(Smsc, Mode)};
postproc(M=#'AnyTimeInterrogationArg'{'gsmSCF-Address' = Scf}, Mode) ->
	M#'AnyTimeInterrogationArg'{'gsmSCF-Address' = postproc_gt(Scf, Mode)};
postproc(M, _Mode) ->
	M.


postproc_gt(In, post) when is_binary(In) ->
	postproc_gt(binary_to_list(In), post);
postproc_gt(asn1_NOVALUE, post) ->
	undefined;
postproc_gt(In, post) ->
	map_codec:parse_addr_string(In);
postproc_gt(undefined, pre) ->
	asn1_NOVALUE;
postproc_gt(In, pre) when is_record(In, party_number) ->
	map_codec:encode_addr_string(In);
postproc_gt(In, pre) ->
	In.

postproc_imsi(asn1_NOVALUE, post) ->
	undefined;
postproc_imsi(In, post) ->
	map_codec:parse_map_addr(In);
postproc_imsi(undefined, pre) ->
	asn1_NOVALUE;
postproc_imsi([], pre) ->
	asn1_NOVALUE;
postproc_imsi(In, pre) ->
	map_codec:encode_map_tbcd(In).

postproc_msisdn(asn1_NOVALUE, post) ->
	undefined;
postproc_msisdn(In, post) ->
	map_codec:parse_map_addr(In);
postproc_msisdn(undefined, pre) ->
	asn1_NOVALUE;
postproc_msisdn([], pre) ->
	asn1_NOVALUE;
postproc_msisdn(In, pre) ->
	map_codec:encode_map_tbcd(In).


rec4op({local, ?MAP_OP_SEND_IDENTIFICATION})	-> 'UpdateLocation';
rec4op({local, ?MAP_OP_CANCEL_LOCATION})	-> 'CancelLocation';
rec4op({local, ?MAP_OP_PURGE_MS})		-> 'PurgeMS-';
rec4op({local, ?MAP_OP_SEND_IDENTIFICATION})	-> 'SendIdentification';
rec4op({local, ?MAP_OP_UPDATE_GRPS_LOCATION})	-> 'UpdateGprsLocation';
rec4op({local, ?MAP_OP_PROVIDE_SUBSCR_INFO})	-> 'ProvideSubscriberInfo';
rec4op({local, ?MAP_OP_ANYTIME_INTERROGATION})	-> 'AnyTimeInterrogation';
rec4op({local, ?MAP_OP_ANYTIME_SUBSCR_INTERR})	-> 'AnyTimeSubscriptionInterrogation';
rec4op({local, ?MAP_OP_ANYTIME_MODIFICATION})	-> 'AnyTimeModification';
rec4op({local, ?MAP_OP_NOTE_SUBSCR_DAT_MODIF})	-> 'NoteSubscriberDataModified';
rec4op({local, ?MAP_OP_PREPARE_HANDOVER})	-> 'PrepareHO-';
rec4op({local, ?MAP_OP_SEND_END_SIGNAL})	-> 'SendEndSignal-';
rec4op({local, ?MAP_OP_PROC_ACC_SIGNALLING})	-> 'ProcessAccessSignalling-';
rec4op({local, ?MAP_OP_FWD_ACC_SIGNALLING})	-> 'ForwardAccessSignalling-';
rec4op({local, ?MAP_OP_PREPARE_SUBSEQ_HO})	-> 'PrepareSubsequentHO-';
rec4op({local, ?MAP_OP_SEND_AUTH_INFO})		-> 'SendAuthenticationInfo';
rec4op({local, ?MAP_OP_AUTH_FAIL_REPORT})	-> 'AuthenticationFailureReport';
rec4op({local, ?MAP_OP_CHECK_IMEI})		-> 'CheckIMEI-';
rec4op({local, ?MAP_OP_INSERT_SUBSCR_DATA})	-> 'InsertSubscriberData';
rec4op({local, ?MAP_OP_DELETE_SUBSCR_DATA})	-> 'DeleteSubscriberData';
rec4op({local, ?MAP_OP_RESET})			-> 'Reset';
rec4op({local, ?MAP_OP_FW_CHECK_SS})		-> {error, 'FIXME'};
rec4op({local, ?MAP_OP_RESTORE_DATA})		-> 'RestoreData';
rec4op({local, ?MAP_OP_SRI_FOR_GPRS})		-> 'SendRoutingInfoForGprs';
rec4op({local, ?MAP_OP_FAILURE_REPORT})		-> 'FailureReport';
rec4op({local, ?MAP_OP_NOTE_MS_PRESENT_GPRS})	-> 'NoteMsPresentForGprs';
rec4op({local, ?MAP_OP_NOTE_MM_EVENT})		-> 'NoteMM-Event';
rec4op({local, ?MAP_OP_SRI_FOR_SM})		-> 'SendRoutingInfo';
rec4op({local, ?MAP_OP_MO_FORWARD_SM})		-> 'MO-ForwardSM-';
rec4op({local, ?MAP_OP_MT_FORWARD_SM})		-> 'MT-ForwardSM-';
rec4op({local, ?MAP_OP_REPORT_SM_DEL_STATUS})	-> 'ReportSM-DeliveryStatus';
rec4op({local, ?MAP_OP_NOTE_SUBSCR_PRESENT})	-> {error, 'FIXME'};
rec4op({local, ?MAP_OP_ALERT_SC_NO_RESULT})	-> {error, 'FIXME'};
rec4op({local, ?MAP_OP_ALERT_SC})		-> 'AlertServiceCentre';
rec4op({local, ?MAP_OP_INFORM_SC})		-> 'InformServiceCentre';
rec4op({local, ?MAP_OP_READY_FOR_SM})		-> 'ReadyForSM-';
rec4op({local, ?MAP_OP_MT_FW_SMS_VGCS})		-> 'MT-ForwardSM-VGCS-'.
% FIXME: SS-Operations.asn, SupplementaryServiceOperations, MAP-CallHandlingOperations, MAP-LocationServiceOperations

argres(arg) -> "Arg";
argres(argument) -> "Arg";
argres(res) -> "Res";
argres(result) -> "Res".

decode_op(_, _, <<>>) ->
	undefined;
decode_op(_, _, []) ->
	undefined;
decode_op({local, Op}, ArgRes, Enc) when is_binary(Enc) or is_list(Enc) ->
	case rec4op({local, Op}) of
		{error, Err} ->
			{error, Err};
		TypePrefix ->
			RecType = list_to_atom(TypePrefix ++ argres(ArgRes)),
			map_only:decode(RecType, Enc)
	end.

encode_op({local, Op}, ArgRes, Dec) ->
	case rec4op({local, Op}) of
		{error, Err} ->
			{error, Err};
		TypePrefix ->
			RecType = list_to_atom(TypePrefix ++ argres(ArgRes)),
			map_only_encode(RecType, Dec)
	end.

timer4op({local, Op}) when is_integer(Op) ->
	timer4op(Op);

timer4op(?MAP_OP_SEND_IDENTIFICATION)	-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_SEND_END_SIGNAL)	-> ?MAP_INV_TIMER_L;
timer4op(?MAP_OP_PROC_ACC_SIGNALLING)	-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_FWD_ACC_SIGNALLING)	-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_FW_CHECK_SS)		-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_MO_FORWARD_SM)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_MT_FORWARD_SM)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_REPORT_SM_DEL_STATUS)	-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_ALERT_SC)		-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_INFORM_SC)		-> ?MAP_INV_TIMER_S;
timer4op(?MAP_OP_MT_FW_SMS_VGCS)	-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_PROC_USS_REQ)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_USS_REQ)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_USS_NOTIFY)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_REGISTER_PW)		-> ?MAP_INV_TIMER_ML;
timer4op(?MAP_OP_PROV_SUBSCR_LOC)	-> ?MAP_INV_TIMER_ML;
% FIXME: GroupCallOperations, OandMOperations, SecureTransportOperations
timer4op(Op) when is_integer(Op)	-> ?MAP_INV_TIMER_M.

class4op({local, Op}) when is_integer(Op) ->
	class4op(Op);
class4op(?MAP_OP_SEND_END_SIGNAL)	-> 3;
class4op(?MAP_OP_PROC_ACC_SIGNALLING)	-> 4;
class4op(?MAP_OP_FWD_ACC_SIGNALLING)	-> 4;
class4op(?MAP_OP_RESET)			-> 4;
class4op(?MAP_OP_FW_CHECK_SS)		-> 4;
class4op(?MAP_OP_NOTE_SUBSCR_PRESENT)	-> 4;
class4op(?MAP_OP_ALERT_SC_NO_RESULT)	-> 4;
class4op(?MAP_OP_INFORM_SC)		-> 4;
class4op(?MAP_OP_NOTIFY_SS)		-> 4;
class4op(?MAP_OP_FW_CHG_ADVICE)		-> 3;
class4op(?MAP_OP_FW_CUG_INFO)		-> 4;
class4op(?MAP_OP_GET_PW)		-> 3;
% FIXME: GroupCallOperations, OandMOperations, SecureTransportOperations
class4op(Op) when is_integer(Op)	-> 1.
