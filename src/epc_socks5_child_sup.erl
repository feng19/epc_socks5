-module(epc_socks5_child_sup).

-behaviour(supervisor).

-export([
    start_link/5,
    start_child/1
]).

-export([init/1]).
-define(SERVER, ?MODULE).
-define(LISTEN_OPTIONS,
    [binary, {ip, {127, 0, 0, 1}}, {reuseaddr, true},
        {active, false}, {backlog, 256}]).

%%===================================================================
start_link(LPort, Server, Port, User, Pass) ->
    supervisor:start_link(?MODULE, [LPort, Server, Port, User, Pass]).

start_child(Sup) ->
    supervisor:start_child(Sup, []).

%%===================================================================
init([LPort, Server, Port, User, Pass]) ->
    {ok, LSock} = gen_tcp:listen(LPort, ?LISTEN_OPTIONS),
    Child = {epc_socks5_child, {epc_socks5_child, start_link, [self(), LSock, Server, Port, User, Pass]},
        temporary, brutal_kill, worker, [epc_socks5_child]},

    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.

