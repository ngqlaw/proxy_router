%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% proxy router
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_router).

%% API exports
-export([
    start/0, 
    open/2, 
    close/1
]).

-define(CHILD(Id, Mod, Opt), {Id, {Mod, start_link, [Opt]}, permanent, 5000, supervisor, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

%% 启动配置映射表
start() ->
    Maps = application:get_env(?MODULE, maps, []),
    do_start(Maps),
    ok.

%% 启动服务
open(Ref, Opt) ->
    Child = ?CHILD(Ref, router_sup, [{ref, Ref}|Opt]),
    proxy_router_sup:start_child(Child).

%% 停止服务
close(Ref) ->
    case cowboy:stop_listener(Ref) of
        ok ->
            proxy_router_sup:stop_child(Ref);
        Error ->
            Error  
    end.

%%====================================================================
%% Internal functions
%%====================================================================

do_start([{ws, {ToIP, ToPort, Config}, LocalPort}|T]) ->
    open({ToIP, ToPort, ws}, [
        {module, ws},
        {shutdown, 0},
        {host, proplists:get_value(host, Config, '_')},
        {port, LocalPort},
        {path, proplists:get_value(path, Config, "/")},
        {proxy, {ToIP, ToPort}}
    ]),
    do_start(T);
do_start([{http, {ToIP, ToPort, _Config}, LocalPort}|T]) ->
    open({ToIP, ToPort, http}, [
        {module, http},
        {shutdown, 0},
        {port, LocalPort},
        {proxy, {ToIP, ToPort}}
    ]),
    do_start(T);
do_start([]) ->
    ok.
