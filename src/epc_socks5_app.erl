%%%-------------------------------------------------------------------
%% @doc epc_socks5 public API
%% @end
%%%-------------------------------------------------------------------

-module(epc_socks5_app).

-behaviour(application).

%% Application callbacks
-export([main/1, start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

main(_) ->
    start(),
    receive
        stop ->
            stop
    end.

start() ->
    application:ensure_all_started(epc_socks5).

start(_StartType, _StartArgs) ->
    {ok, List} = application:get_env(ports),
    case epc_socks5_sup:start_link() of
        {ok, _} = Res ->
            [start_do(Opts) || {_Name, Opts} <- List],
            Res;
        Err -> Err
    end.

start_do(Opts) ->
    LPort = proplists:get_value(port, Opts),
    Server = proplists:get_value(socks5_server, Opts),
    Port = proplists:get_value(socks5_port, Opts),
    User = proplists:get_value(socks5_user, Opts),
    Pass = proplists:get_value(socks5_pass, Opts),
    {ok, Sup} = epc_socks5_sup:start_child(LPort, Server, Port, User, Pass),
    [epc_socks5_child_sup:start_child(Sup) || _ <- lists:seq(1, worker_count())].

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
worker_count() ->
    min(8, erlang:system_info(schedulers_online) * 2).
