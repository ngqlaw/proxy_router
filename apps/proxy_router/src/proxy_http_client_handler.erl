%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game http proxy client handler
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_http_client_handler).
-author("ninggq").

-export([init/4, send/2]).

init(_ServerHandler, IP, Port, Req) ->
    Method = method(cowboy_req:method(Req)),
    [Host, Path, Qs, Fragment] = cowboy_req:uri(Req),
    [Scheme, _] = binary:split(list_to_binary(Host), [<<"//">>]),
    Url = list_to_binary([Scheme, <<"//">>, IP, ":", integer_to_list(Port), Path, Qs, Fragment]),
    case catch httpc:request(Method, {Url, []}, [], []) of
        {ok, Res} ->
            %% TODO
            lager:info("reply info:~p", [Res]);
        Error ->
            lager:error("http url ~p fail:~p", [Url, Error])
    end,
    %% TODO
    NewReq = cowboy_req:reply(400, #{}, <<"Missing body.">>, Req),
    {ok, NewReq}.

send(_, _) ->
    ok.

method(<<"HEAD">>) -> head;
method(<<"GET">>) -> get;
method(<<"PUT">>) -> put;
method(<<"POST">>) -> post;
method(<<"TRACE">>) -> trace;
method(<<"OPTIONS">>) -> options;
method(<<"DELETE">>) -> delete;
method(<<"PATCH">>) -> patch.
