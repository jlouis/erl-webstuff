%% This is obviously wrong, but we will change it later on
{application,webstuff,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{odata_app,[]}},
              {env,[]},
              {modules,
	        [odata,odata_app,odata_sup,
		 atom,
		 inet_time_scanner, inet_time_parser_simple, inet_time,
	         uri,iri]}]}.
