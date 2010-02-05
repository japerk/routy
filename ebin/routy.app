{application, routy, [
	{description, "HTTP Request Routing"},
	{vsn, "1.6"},
	{mod, {routy, []}},
	{registered, []},
	{modules, [routy, routy_sup, routy_util, routy_handlers]},
	{applications, [kernel, stdlib, elib]},
	{env, [
		{id, "routy"}, {server_name, "localhost"}, {logdir, "."}, {port, 8000},
		{route_mods, []}, {routes, []}, {cache, true}, {authkey, false},
		{secret, "routy"}, {authdirs, ["/"]}, {realm, []}
	]}
]}.
