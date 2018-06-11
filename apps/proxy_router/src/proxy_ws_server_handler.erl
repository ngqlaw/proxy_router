%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% cowboy websocket proxy server handler
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_ws_server_handler).

-behaviour(cowboy_websocket).

%% API
-export([send/2]).
%% cowboy callback function
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    Shutdown = case proplists:get_value(shutdown, Opts, 0) of
        N when is_integer(N) -> N;
        _ -> 0
    end,
    State = #{
        init => Req,
        sup_pid => proplists:get_value(sup_pid, Opts),
        shutdown => Shutdown,
        proxy => proplists:get_value(proxy, Opts)
    },
    {cowboy_websocket, Req, State}.

websocket_init(#{sup_pid := SupPid, proxy := {ClientHandler, ServerHandler, IP, Port}, init := Req}) ->
    {ok, ClientPid} = ClientHandler:start_link(self(), ServerHandler, IP, Port, Req),
    Ref = erlang:monitor(process, ClientPid),
    %% 等待客户端代理连接完毕
    receive
        ok -> ok
    end,
    {ok, #{sup_pid => SupPid, ref => Ref, client_pid => ClientPid, client_handler => ClientHandler}}.

%% 客户端请求
websocket_handle(Msg, #{client_pid := Pid, client_handler := ClientHandler} = State) when is_pid(Pid) ->
    ClientHandler:send(Pid, Msg),
    {ok, State};
websocket_handle(_, State) ->
    {ok, State}.

%% 代理客户端返回
websocket_info({'DOWN', Ref, process, _Object, Reason}, #{ref := Ref} = State) ->
    lager:debug("proxy client process down:~p", [Reason]),
    {stop, State};
websocket_info(Msg, State) ->
    % lager:debug("proxy client reply:~p", [Msg]),
    {reply, Msg, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("proxy server stop:~p", [Reason]),
    ok.

send(Server, Msg) ->
    erlang:send(Server, Msg).
