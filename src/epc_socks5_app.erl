%%%-------------------------------------------------------------------
%% @doc epc_socks5 public API
%% @end
%%%-------------------------------------------------------------------

-module(epc_socks5_app).

-behaviour(application).

%% Application callbacks
-export([main/1, start/0, start/2, stop/1]).

-define(LISTEN_OPTIONS,
    [binary, {ip, {0, 0, 0, 0}}, {reuseaddr, true},
        {active, false}, {backlog, 256}]).

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
    {ok, Port} = application:get_env(port),
    {ok, LSock} = gen_tcp:listen(Port, ?LISTEN_OPTIONS),
    case epc_socks5_sup:start_link(LSock) of
        {ok, _} = Res ->
            [epc_socks5_sup:start_child() || _ <- lists:seq(1, worker_count())],
            Res;
        Err -> Err
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
worker_count() ->
    min(8, erlang:system_info(schedulers_online) * 2).