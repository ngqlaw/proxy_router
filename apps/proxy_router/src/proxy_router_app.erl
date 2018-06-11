%%%-------------------------------------------------------------------
%% @doc proxy_router public API
%% @end
%%%-------------------------------------------------------------------

-module(proxy_router_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() -> start(proxy_router).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

start(_StartType, _StartArgs) ->
    case proxy_router_sup:start_link() of
        {ok, _} = OK ->
            proxy_router:start(),
            OK;
        Res ->
            Res
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
