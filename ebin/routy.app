{application, routy, [
	{description, "HTTP Request Routing"},
	{vsn, "1.3"},
	{mod, {routy, []}},
	{registered, [routy_cache]},
	{modules, [routy, routy_sup, routy_util]},
	{applications, [kernel, stdlib, elib]},
	{env, [
		{id, "routy"}, {server_name, "localhost"}, {logdir, "."}, {port, 8000},
		{route_mods, []}, {routes, []}, {cache, true}, {authkey, false},
		{secret, "routy"}, {authdirs, ["/"]}, {realm, []}
	]}
]}.
