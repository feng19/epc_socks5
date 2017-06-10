-module(epc_socks5_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_child/5
]).

-export([init/1]).
-define(SERVER, ?MODULE).

%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(LPort, Server, Port, User, Pass) ->
    supervisor:start_child(?SERVER, [LPort, Server, Port, User, Pass]).

%%===================================================================
init([]) ->
    Child = {epc_socks5_child_sup, {epc_socks5_child_sup, start_link, []},
        temporary, brutal_kill, worker, [epc_socks5_child_sup]},

    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.