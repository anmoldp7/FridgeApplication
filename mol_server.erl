-module(mol_server).

-export([start / 2, start_link / 2, call / 2, cast / 2, reply / 2]).

start(Module, InitialState) -> spawn(fun() -> init(Module, InitialState) end).
start_link(Module, InitialState) -> spawn_link(fun() -> init(Module, InitialState) end).

init(Module, InitialState) -> loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
	{sync, From, Msg} ->
	    loop(Module, Module:handle_call(State, From, Msg));
	{async, Msg} ->
	    loop(Module, Module:handle_cast(State, Msg))
    end.

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, {self(), Ref}, Msg},
    receive
	{Ref, Reply} ->
	    erlang:demonitor(Ref, [flush]),
	    Reply;
	{'DOWN', Ref, process, Pid, Reason} ->
	    erlang:error(Reason)
    after 5000 ->
	erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Msg) ->
    Pid ! {Ref, Msg}.

