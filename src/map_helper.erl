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

-module(map_helper).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_map/include/map.hrl").

-export([postproc/2, postproc_gt/2, postproc_imsi/2, postproc_msisdn/2]).

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

