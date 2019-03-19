%% ----------------------------------
%% Jsonnet for Erlang library
%% ----------------------------------

-module(jsonnet).

%% API exports
-export([evaluateFile/1, evaluateSnippet/1]).
-on_load(init/0).

-define(APPNAME, jsonnet).
-define(LIBNAME, "jsonnet_nif").


%%====================================================================
%% API functions
%%====================================================================

%% @doc Accepts a file name string
-spec evaluateFile(Filename :: string()) -> string() | {error, any()}.

evaluateFile(_) ->
    not_loaded(?LINE).

%% @doc Accepts a string to evaluate
-spec evaluateSnippet(Snippet :: string()) -> string() | {error, any()}.

evaluateSnippet(_) ->
    not_loaded(?LINE).


%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

