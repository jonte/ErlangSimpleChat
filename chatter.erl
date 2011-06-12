-module(chatter).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3, start_link/1]).

-record(state, {socket, nick}).

start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

init([Sock]) ->
    io:format("Started with socket ~p~n", [Sock]),
    {ok, #state{socket = Sock, nick = pid_to_list(self())}}.


handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast({bcast, From, Msg}, #state{socket = Sock} = State) ->
    gen_tcp:send(Sock, From ++ ": " ++ Msg),
    {noreply, State};

handle_cast(Msg, S) ->
    io:format("Got cast: ~p~n", [Msg]),
    {noreply, S}.

handle_info({tcp, _Sock, Data}, #state{nick = Nick} = State) ->
    % Naïve way to split commands and parameters
    [Cmd | Params] = re:split(Data, " ", [{return, list}]),
    JointParams = string:join(Params, " "),
    case Cmd of
        "NICK" ->
            {noreply, State#state{nick=string:strip(JointParams)}};
        "CHAT" ->
            gen_server:cast(simplechat, {bcast, Nick, JointParams}),
            {noreply, State};
        _Other -> {noreply, State}
    end;

handle_info({tcp_closed, _Sock}, State) ->
    gen_server:cast(simplechat, {bcast, "SERVER", "User disconnected"}),
    {noreply, State}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, _State) ->
    ok.
