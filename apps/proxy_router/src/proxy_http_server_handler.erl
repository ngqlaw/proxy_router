%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game http proxy server handler
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_http_server_handler).
-author("ninggq").

%% cowboy http callback
-export([init/2]).
%% API
-export([send/2]).

init(Req, Opts) ->
    {ClientHandler, ServerHandler, IP, Port} = proplists:get_value(proxy, Opts),
    {ok, NewReq} = ClientHandler:init(ServerHandler, IP, Port, Req),
    {ok, NewReq, Opts}.

send(_, _) ->
    ok.
