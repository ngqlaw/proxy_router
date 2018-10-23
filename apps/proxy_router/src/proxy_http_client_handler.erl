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
    case catch httpc:request(Method, {binary_to_list(Url), []}, [], []) of
        {ok, {{_, StatusCode, _}, HttpHeader, Body}} ->
            TempHeader = maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- HttpHeader]),
            CookieInfo = maps:get(<<"set-cookie">>, TempHeader, <<>>),
            Cookies = binary:split(CookieInfo, [<<";">>], [global]),
            Header = TempHeader#{<<"set-cookie">> => Cookies};
        {ok, {StatusCode, Body}} ->
            Header = #{};
        Error ->
            lager:error("http url ~p fail:~p", [Url, Error]),
            StatusCode = 400,
            Header = #{},
            Body = <<"Error!">>
    end,
    NewReq = cowboy_req:reply(StatusCode, Header, Body, Req),
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
