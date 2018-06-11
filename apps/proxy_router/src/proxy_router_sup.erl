%%%-------------------------------------------------------------------
%% @doc proxy_router top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(proxy_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Child) ->
    supervisor:start_child(?SERVER, Child).

stop_child(Ref) ->
    Children = supervisor:which_children(?SERVER),
    case lists:keyfind(Ref, 1, Children) of
        {_, Pid, _, _} ->
            case router_sup:stop(Pid) of
                ok ->
                    supervisor:terminate_child(?SERVER, Ref),
                    supervisor:delete_child(?SERVER, Ref);
                Error ->
                    Error
            end;      
        _ ->
            {error, not_found}  
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
