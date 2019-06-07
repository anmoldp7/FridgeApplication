-module(fridge_server).

-export([start / 0, start_link / 0]).
-export([store_food / 2, delete_food / 2, list_food/ 1, shutdown_fridge / 1]).
-export([init / 1, handle_call / 3, handle_cast / 2, handle_info / 2, terminate / 2, code_change / 3]).

-behavior(gen_server).

%% Public API
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_food(Pid, Food) -> gen_server:cast(Pid, {store, Food}).

delete_food(Pid, Food) -> gen_server:cast(Pid, {delete, Food}).

list_food(Pid) -> gen_server:call(Pid, list_food).

shutdown_fridge(Pid) -> gen_server:call(Pid, terminate).

%% Server functions
init([]) -> {ok, []}.

handle_call(list_food, _From, State) ->
	{reply, State, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({store, Food}, State) ->
    {noreply, State ++ [Food]};
handle_cast({delete, Food}, State) ->
    case lists:member(Food, State) of
		true  -> {noreply, State -- [Food]};
		false -> {noreply, State}
    end.

handle_info(Msg, State) ->
	io:format("Unknown message received: ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
