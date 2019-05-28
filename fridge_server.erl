-module(fridge_server).

-export([start / 0, start / 1, start_link / 0, start_link / 1, init / 1]).
-export([store_food / 2, delete_food / 2, list_food/ 1, shutdown_fridge / 1]).
-export([handle_call / 3, handle_cast / 2]).

init(X) -> X.

start(InitialState) -> mol_server:start(?MODULE, InitialState).
start_link(InitialState) -> mol_server:start_link(?MODULE, InitialState).

start() -> mol_server:start(?MODULE, []).
start_link() -> mol_server:start_link(?MODULE, []).

store_food(Pid, Food) -> mol_server:cast(Pid, {store, Food}).

delete_food(Pid, Food) -> mol_server:cast(Pid, {delete, Food}).

list_food(Pid) -> mol_server:call(Pid, {list_food}).

shutdown_fridge(Pid) -> mol_server:call(Pid, {terminate}).

handle_call(State, {Pid, Ref}, {list_food}) ->
    mol_server:reply({Pid, Ref}, State),
    State;

handle_call(_, {Pid, Ref}, {terminate}) ->
    mol_server:reply({Pid, Ref}, "Shutting down fridge server."),
    exit(normal).

handle_cast(State, {store, Food}) ->
    State ++ [Food];

handle_cast(State, {delete, Food}) ->
    case lists:member(Food, State) of
	true ->	State -- [Food];
	false -> State
    end.
