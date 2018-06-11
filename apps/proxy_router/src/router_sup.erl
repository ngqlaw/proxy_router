%%%-------------------------------------------------------------------
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% websocket for game
%%% @end
%%%-------------------------------------------------------------------
-module(router_sup).
-author("ninggq").

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, temporary, 5000, worker, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_child(State, ParentPid) ->
    {Srever, NewState} = maps:take(sup_pid, State),
    supervisor:start_child(Srever, [NewState, ParentPid]).

stop(Srever) ->
    Children = supervisor:which_children(Srever),
    N = lists:foldl(fun
        ({_, Pid, _, _}, Acc) when is_pid(Pid) ->
        case catch gen_server:call(Pid, {soft_stop_immediately, self()}) of
            ok -> Acc + 1;
            _ -> Acc
        end
    end, 0, Children),
    loop_stop(N). 

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Opt :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opt) ->
    supervisor:start_link(?MODULE, [Opt]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}
    | ignore).
init([Opt]) ->
    %% 启动网络进程
    Ref = proplists:get_value(ref, Opt, undefined),
    {ServerHandler, ClientHandler} = case proplists:get_value(module, Opt, undefined) of
        ws -> {proxy_ws_server_handler, proxy_ws_client_handler};
        _ -> {proxy_http_server_handler, proxy_http_client_handler}
    end,
    Timeout = proplists:get_value(timeout, Opt, 60000),
    ProxyPort = proplists:get_value(port, Opt, 8080),
    Host = proplists:get_value(host, Opt, '_'),
    Path = proplists:get_value(path, Opt, "/"),
    {IP, Port} = proplists:get_value(proxy, Opt, {"127.0.0.1", ProxyPort}),
    Proxy = {ClientHandler, ServerHandler, IP, Port},
    BaseOtp = normalize_options(Opt, [{sup_pid, self()}, {proxy, Proxy}]),
    
    Dispatch = cowboy_router:compile([
        {Host, [
            {Path, ServerHandler, BaseOtp}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Ref, [{port, ProxyPort}], #{
        env => #{dispatch => Dispatch},
        idle_timeout => Timeout
    }),
    {ok, {{simple_one_for_one, 3, 10}, [?CHILD(router_handler)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop_stop(N) when N > 0 ->
    receive
        ok -> loop_stop(N - 1)
        after 5000 ->
            {error, timeout}  
    end;
loop_stop(_) ->
    ok.

normalize_options([{shutdown, V}|T], Opt) ->
    normalize_options(T, [{shutdown, V}|Opt]);
normalize_options([{msg_type, V}|T], Opt) ->
    normalize_options(T, [{msg_type, V}|Opt]);
normalize_options([_|T], Opt) ->
    normalize_options(T, Opt);
normalize_options([], Opt) -> Opt.
