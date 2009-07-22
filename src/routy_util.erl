%% @doc Yaws utility functions.
%%
%% @author Jacob Perkins
%% @todo define prop as an edoc type
-module(routy_util).

-export([
	encode_props/1,
	encode_props/2,
	encode_prop/1,
	decode/1,
	decode_props/1,
	dejsonify/1,
	stringify/1,
	structify/1,
	render_tmpl/2,
	compile_templates/1
]).

%% @doc Encode url props.
%% @spec encode_props([Prop::term()]) -> string()
%% @see encode_prop/1
encode_props([]) ->
	[];
encode_props([Prop]) ->
	encode_prop(Prop);
encode_props([Prop | Props]) ->
	encode_prop(Prop) ++ "&" ++ encode_props(Props).

%% @doc Encodes props and appends to Url.
%% @spec encode_props(Url, Props) -> string()
%% @todo check if Url ends already has `?'
%% @see encode_props/1
encode_props(Url, []) -> Url;
encode_props(Url, Props) -> Url ++ "?" ++ encode_props(Props).

%% @doc Url encodes property.
%% @spec encode_prop(Prop::term()) -> string()
encode_prop({Key, Val}) ->
	stringify(Key) ++ "=" ++ yaws_api:url_encode(stringify(Val));
encode_prop(Key) ->
	stringify(Key) ++ "=".

%% @doc Decodes Url's props.
%% @spec decode(string()) -> [Prop::term()]
decode(URL) ->
	case estring:splitc(URL, $?) of
		{URL, []} -> [];
		{_, Query} -> decode_props(Query)
	end.

%% @doc Decode url props.
%% @spec decode_props(string()) -> [Prop::term()]
decode_props(Query) ->
	F = fun(Var) -> estring:splitc(Var, $=) end,
	lists:map(F, string:tokens(Query, "&")).

%% @doc Removes Yaws JSON terms and tuple enclosures.
%% @spec dejsonify(JsonTerm::term()) -> term()
dejsonify({struct, Props}) -> [dejsonify(P) || P <- Props];
dejsonify({array, List}) -> [dejsonify(I) || I <- List];
dejsonify({Key, Val}) when is_atom(Key) -> {Key, dejsonify(Val)};
dejsonify(Item) -> Item.

%% @doc Converts term to string. Uses `io_lib:format(~f)' instead of
%% `float_to_list' to get default precision of 6.
%% @spec stringify(term()) -> string()
stringify(Term) when is_atom(Term) -> atom_to_list(Term);
stringify(Term) when is_integer(Term) -> integer_to_list(Term);
stringify(Term) when is_float(Term) -> hd(io_lib:format("~f", [Term]));
stringify(Term) when is_binary(Term) -> binary_to_list(Term);
stringify({{_, _, _}, {_, _, _}}=Term) -> datetime:iso_string(Term);
stringify(Term) when is_tuple(Term) -> tuple_to_list(Term);
stringify(Term) when is_list(Term) -> Term.

%% @doc Creates Yaws JSON compatible terms by enclosing known patterns in JSON
%% tuples.
%% @spec structify(Term::term()) -> JSONTerm::term()
structify([]) ->
	{struct, []};
structify({Key, [{_, _} | _]=Vals}) ->
	{Key, {struct, [structify(V) || V <- Vals]}};
structify({Key, Val}) when is_atom(Val) ->
	{Key, atom_to_list(Val)};
structify({Key, Val}) ->
	{Key, Val};
structify([_ | _]=Vals) ->
	{struct, [structify(V) || V <- Vals]}.

render_tmpl(Tmpl, Vars) ->
	{ok, Html} = Tmpl:render(Vars),
	{html, Html}.

compile_templates(App) when is_atom(App) ->
	% TODO: this is slight different than emod:compile_templates, but we should
	% choose one over the other and delete the other
	application:load(App),
	Out = filename:join(code:lib_dir(App), "ebin"),
	Docroot = filename:join(code:priv_dir(App), "docroot"),
	Opts = [{out_dir, Out}, {custom_tags_dir, Docroot}],
	
	F = fun(Template) ->
			File = filename:join(Docroot, atom_to_list(Template)),
			io:format("compiling ~s to ~p~n", [File, Template]),
			ok = erlydtl_compiler:compile(File, Template, Opts)
		end,
	
	{ok, Templates} = application:get_env(App, templates),
	lists:foreach(F, Templates);
compile_templates(App) ->
	lists:foreach(fun compile_templates/1, App).
