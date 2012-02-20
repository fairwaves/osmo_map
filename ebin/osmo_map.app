{application, osmo_map,
	[{description, "Osmocom SS7 MAP"},
	 {vsn, "1"},
	 {modules, [map, map_codec, map_helper,
		    tcap_asn, tcap_helper,
		    map_only,
		    map_app_sup, map_ss_server, map_dlg_server]},
	 {registered, []},
	 {applications, []},
	 {env, [
	  ]}
]}.
