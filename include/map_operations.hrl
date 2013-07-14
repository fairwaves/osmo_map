% from  MAP-MobileServiceOperations.asn 3GPP TS 29.002 V9.3.0 (2010-09)
-define(MAP_OP_UPDATE_LOCATION,		2).
-define(MAP_OP_CANCEL_LOCATION,		3).
-define(MAP_OP_PURGE_MS,		67).
-define(MAP_OP_SEND_IDENTIFICATION,	55).
-define(MAP_OP_UPDATE_GRPS_LOCATION,	23).
-define(MAP_OP_PROVIDE_SUBSCR_INFO,	70).
-define(MAP_OP_ANYTIME_INTERROGATION,	71).
-define(MAP_OP_ANYTIME_SUBSCR_INTERR,	62).
-define(MAP_OP_ANYTIME_MODIFICATION,	65).
-define(MAP_OP_NOTE_SUBSCR_DAT_MODIF,	5).
-define(MAP_OP_PREPARE_HANDOVER,	68).
-define(MAP_OP_SEND_END_SIGNAL,		29).
-define(MAP_OP_PROC_ACC_SIGNALLING,	33).
-define(MAP_OP_FWD_ACC_SIGNALLING,	34).
-define(MAP_OP_PREPARE_SUBSEQ_HO,	69).
-define(MAP_OP_SEND_AUTH_INFO,		56).
-define(MAP_OP_AUTH_FAIL_REPORT,	15).
-define(MAP_OP_CHECK_IMEI,		43).
-define(MAP_OP_INSERT_SUBSCR_DATA,	7).
-define(MAP_OP_DELETE_SUBSCR_DATA,	8).
-define(MAP_OP_RESET,			37).
-define(MAP_OP_FW_CHECK_SS,		38).
-define(MAP_OP_RESTORE_DATA,		57).
-define(MAP_OP_SRI_FOR_GPRS,		24).
-define(MAP_OP_FAILURE_REPORT,		25).
-define(MAP_OP_NOTE_MS_PRESENT_GPRS,	26).
-define(MAP_OP_NOTE_MM_EVENT,		89).

% from ShortMessageOperations.asn
-define(MAP_OP_SRI_FOR_SM,		45).
-define(MAP_OP_MO_FORWARD_SM,		46).
-define(MAP_OP_MT_FORWARD_SM,		44).
-define(MAP_OP_REPORT_SM_DEL_STATUS,	47).
-define(MAP_OP_NOTE_SUBSCR_PRESENT,	48).
-define(MAP_OP_ALERT_SC_NO_RESULT,	49).
-define(MAP_OP_ALERT_SC,		64).
-define(MAP_OP_INFORM_SC,		63).
-define(MAP_OP_READY_FOR_SM,		66).
-define(MAP_OP_MT_FW_SMS_VGCS,		21).

% from SS-Operations.asn 3GPP TS 24.080 V9.1.0
-define(MAP_OP_NOTIFY_SS,		16).
-define(MAP_OP_PROC_USS_DATA,		19).
-define(MAP_OP_LCS_PER_LOC_CANCEL,	109).
-define(MAP_OP_LCS_LOC_UPD,		110).
-define(MAP_OP_LCS_PER_LOC_REQ,		111).
-define(MAP_OP_LCS_AREA_EVT_CANCEL,	112).
-define(MAP_OP_LCS_AREA_EVT_REP,	113).
-define(MAP_OP_LCS_AREA_EVT_REQ,	114).
-define(MAP_OP_LCS_MOLR,		115).
-define(MAP_OP_LCS_LOC_NOTIF,		116).
-define(MAP_OP_CALL_DEFLECTION,		117).
-define(MAP_OP_USER_USER_SVC,		118).
-define(MAP_OP_ACC_REG_CC_ENTRY,	119).
-define(MAP_OP_FW_CUG_INFO,		120).
-define(MAP_OP_SPLIT_MPTY,		121).
-define(MAP_OP_RETR_MPTY,		122).
-define(MAP_OP_HOLD_MPTY,		123).
-define(MAP_OP_BUILD_MPTY,		124).
-define(MAP_OP_FW_CHG_ADVICE,		125).
-define(MAP_OP_EXPLICIT_CT,		126).

% from SupplementaryServiceOperations.asn
-define(MAP_OP_REGISTER_SS,		10).
-define(MAP_OP_ERASE_SS,		11).
-define(MAP_OP_ACTIVATE_SS,		12).
-define(MAP_OP_DEACTIVATE_SS,		13).
-define(MAP_OP_INTERROGATE_SS,		14).
-define(MAP_OP_PROC_USS_REQ,		59).
-define(MAP_OP_USS_REQ,			60).
-define(MAP_OP_USS_NOTIFY,		61).
-define(MAP_OP_REGISTER_PW,		17).
-define(MAP_OP_GET_PW,			18).
-define(MAP_OP_SS_INVOC_NOTIF,		72).
-define(MAP_OP_REGISTER_CC_ENTRY,	76).
-define(MAP_OP_ERASE_CC_ENTRY,		77).

% from MAP-CallHandlingOperations.asn
-define(MAP_OP_SEND_ROUTING_INFO,	22).
-define(MAP_OP_PROVIDE_ROAMING_NR,	4).
-define(MAP_OP_RESUME_CALL_HANDLING,	6).
-define(MAP_OP_SET_REPORTING_STATE,	73).
-define(MAP_OP_STATUS_REPORT,		74).
-define(MAP_OP_REMOTE_USER_FREE,	75).
-define(MAP_OP_IST_ALERT,		87).
-define(MAP_OP_IST_COMMAND,		88).
-define(MAP_OP_RELEASE_RESOURCES,	20).

% from MAP-LocationServiceOperations.asn
-define(MAP_OP_SRI_FOR_LCS,		85).
-define(MAP_OP_PROV_SUBSCR_LOC,		83).
-define(MAP_OP_SUBSCR_LOC_REPORT,	86).

% FIXME: GroupCallOperations, OandMOperations, SecureTransportOperations

% maximum values as gvin in TS 09.02 / 17.1.2
-define(MAP_INV_TIMER_S,		10*1000).
-define(MAP_INV_TIMER_M,		30*1000).
-define(MAP_INV_TIMER_ML,		10*60*1000).
-define(MAP_INV_TIMER_L,		38*60*60*1000).
