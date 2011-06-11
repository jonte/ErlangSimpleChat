-module(chatter).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3, start_link/1]).

start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

init([Sock]) ->
    io:format("Started with socket ~p~n", [Sock]),
    {ok, Sock}.


handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast({bcast, From, Msg}, State) ->
    gen_tcp:send(State, pid_to_list(From) ++ ": " ++ Msg),
    {noreply, State};

handle_cast(Msg, S) ->
    io:format("Got cast: ~p~n", [Msg]),
    {noreply, S}.

handle_info({tcp, _Sock, Data}, State) ->
    gen_server:cast(simplechat, {bcast, self(), Data}),
    {noreply, State}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, _State) ->
    ok.
