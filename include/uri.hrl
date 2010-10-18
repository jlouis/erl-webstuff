%% URI structures
-record(hier,
	{authority :: string() | none,
	 path      :: string() | none}).

-record(uri,
	{scheme :: atom(),
	 hier :: #hier{},
	 q :: [{string(), string()}],
	 fragment :: string() | none}).

