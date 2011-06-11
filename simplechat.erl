-module(simplechat).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3, start_link/1, accept/1]).

-record(state, {clients, lsock}).

start_link(Port) ->
    gen_server:start_link({local, simplechat}, simplechat, [Port], []).

init([Port]) ->
    io:format("Chat started on port ~p~n", [Port]),
    case gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]) of
        {ok, LSock} ->
            spawn_link(?MODULE, accept, [LSock]),
            {ok, #state{lsock = LSock, clients = []}, 0};
        {error, Reason} ->
            {stop, Reason}
    end.

accept(LSock) ->
    io:format("Accepting connection~n", []),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Chatter} = chatter:start_link(Sock),
    gen_tcp:controlling_process(Sock, Chatter),
    gen_server:cast(simplechat, {new_connection, Chatter}),
    accept(LSock).

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast({new_connection, Pid}, #state{clients = Clients} = State) ->
    io:format("Now handling connection from: ~p~n", [Pid]),
    {noreply, State#state{clients = [Pid | Clients]}};

handle_cast({bcast, From, Msg}, #state{clients = Clients} = State) ->   
    io:format("Broadcasting: ~p from ~p~n", [Msg, From]),
    lists:foreach(fun(X) -> gen_server:cast(X, {bcast, From, Msg}) end, Clients),
    {noreply, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, _State) ->
    ok.
