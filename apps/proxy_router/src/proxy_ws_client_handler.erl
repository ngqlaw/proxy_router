%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game websocket proxy client handler
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_ws_client_handler).
-author("ninggq").

-behaviour(websocket_client).

%% API
-export([start_link/5, send/2]).

%% websocket_client callbacks
-export([
    init/1,
    onconnect/2,
    ondisconnect/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-record(state, {
    ref = undefined :: undefined | reference(),
    server_pid = undefined :: undefined | pid(),
    server_handler = undefined :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(pid(), atom(), string(), integer(), term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Server, ServerHandler, Ip, Port, Req) ->
    [Host, Path, Qs, Fragment] = cowboy_req:uri(Req),
    Scheme = case binary:split(list_to_binary(Host), [<<"//">>]) of
        [<<"http:">>, _] -> <<"ws:">>;
        [<<"https:">>, _] -> <<"wss:">>
    end,
    Url = list_to_binary([Scheme, <<"//">>, Ip, ":", integer_to_list(Port), Path, Qs, Fragment]),
    websocket_client:start_link(Url, ?MODULE, [Server, ServerHandler]).

%% @doc 发送信息
send(Pid, Msg) ->
    websocket_client:send(Pid, Msg).

%%%===================================================================
%%% websocket_client callbacks
%%%===================================================================

init([Server, ServerHandler]) ->
    Ref = erlang:monitor(process, Server),
    {once, #state{
        ref = Ref,
        server_pid = Server,
        server_handler = ServerHandler
    }}.

onconnect(_WSReq, #state{server_pid = Server} = State) ->
    lager:debug("proxy client connect!"),
    erlang:send(Server, ok),
    {ok, State}.

ondisconnect(Reason, State) ->
    lager:debug("proxy client disconnect:~p", [Reason]),
    {ok, State}.

%% 真正的服务器返回
websocket_handle(Msg, _ConnState, #state{
        server_pid = Pid,
        server_handler = ServerHandler
    } = State) when is_pid(Pid) ->
    ServerHandler:send(Pid, Msg),
    {ok, State};
websocket_handle(_, _ConnState, State) ->
    {ok, State}.

%% 代理服务器请求
websocket_info({'DOWN', Ref, process, _Object, Reason}, _ConnState, #state{ref = Ref} = State) ->
    lager:debug("proxy server process down:~p", [Reason]),
    {close, <<>>, State};
websocket_info(_Msg, _ConnState, State) ->
    {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
    lager:debug("Websocket closed in state ~p wih reason ~p~n", [State, Reason]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
