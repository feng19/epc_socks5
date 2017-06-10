-module(epc_socks5_child).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/6]).

-define(DEBUG(Msg), io:format("[~p:~p] ~p~n", [?MODULE, ?LINE, Msg])).
-define(DEBUG(Format, Args), io:format("[~p:~p] " ++ Format ++ "~n", [?MODULE, ?LINE | Args])).

-record(state, {sup, lsock, socket, server, port, user, pass, remote_socket, status = 0}).

%%%===================================================================
start_link(Sup, LSock, Server, Port, User, Pass) ->
    gen_server:start_link(?MODULE, [Sup, LSock, Server, Port, User, Pass], []).

%%%===================================================================
init([Sup, LSock, Server, Port, User, Pass]) ->
    process_flag(trap_exit, true),
    {ok, #state{sup = Sup, lsock = LSock, server = Server, port = Port, user = User, pass = Pass}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{socket = Socket, remote_socket = RemoteSocket, status = 2} = State) ->
    gen_tcp:send(RemoteSocket, Request),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp, Socket, Request}, #state{status = 1} = State) ->
    case Request of
        <<5:8, _Nmethods:8, _Bin/binary>> ->
            gen_tcp:send(Socket, <<5:8, 0:8>>),
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{status = 2}};
        Data ->
            ?DEBUG("error data:~p", [Data]),
            {stop, normal, State}
    end;
%% recv from remote, and send back to client
handle_info({tcp, RemoteSocket, Response}, #state{remote_socket = RemoteSocket} = State) ->
    gen_tcp:send(State#state.socket, Response),
    {noreply, State};
handle_info(timeout, #state{lsock = LSock, socket = undefined, sup = Sup,
    server = Server, port = Port, user = User, pass = Pass} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_socks5_child_sup:start_child(Sup),
    ok = inet:setopts(Socket, [{active, once}]),
    case start_process(Server, Port, User, Pass, 2) of
        {ok, RemoteSocket} ->
            {noreply, State#state{socket = Socket, remote_socket = RemoteSocket, status = 1}};
        {error, Error} ->
            {stop, Error, State}
    end;
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    ?DEBUG("tcp_error stop:~p", [Reason]),
    {stop, Reason, State}.

terminate(_Reason, #state{socket = Socket, remote_socket = RemoteSocket}) ->
    case is_port(Socket) of
        true ->
            gen_tcp:close(Socket);
        false ->
            ok
    end,
    case is_port(RemoteSocket) of
        true ->
            gen_tcp:close(RemoteSocket);
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

start_process(Server, Port, User, Pass, N) ->
    case gen_tcp:connect(Server, Port, [binary]) of
        {ok, RemoteSocket} ->
            gen_tcp:send(RemoteSocket, <<5, 1, 2>>),
            receive
                {tcp, RemoteSocket, <<5:8, _Nmethods:8, _Bin/binary>>} ->
                    gen_tcp:send(RemoteSocket, <<1, (byte_size(User)), User/binary,
                        (byte_size(Pass)), Pass/binary>>),
                    receive
                        {tcp, RemoteSocket, <<1, 0>>} ->
                            {ok, RemoteSocket};
                        {tcp, RemoteSocket, _} ->
                            {error, err_user_or_pass}
                    end
            end;
        {error, timeout} when N =/= 0 ->
            start_process(Server, Port, User, Pass, N - 1);
        {error, Error} ->
            {error, Error}
    end.