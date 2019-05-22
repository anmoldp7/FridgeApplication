-module(fridge).
-compile(export_all).

%% For starting and adding a monitor on fridge server.
start_monitor() ->
    spawn_monitor(?MODULE, fridge_server, [[]]).

%% Fridge server program. TCO, thus no overhead due to recursive function calls.
fridge_server(X) ->
    receive
	{Pid, Message, Item} ->
	    if Message =:= store ->
		Pid ! "Item stored in fridge: " ++ atom_to_list(Item),
		fridge_server(X ++ [Item]);
	    Message =:= take ->
		case lists:member(Item, X) of
		    true  -> Pid ! "Item removed from fridge: " ++ atom_to_list(Item);
		    false -> Pid ! "Item does not exist in fridge"
		end,
		fridge_server(X -- [Item]);
	    Message =/= take andalso Message =/= store ->
		io:format("Illegal request received.")
	    end;
	{Pid, display} ->
	    Pid ! X,
	    fridge_server(X);
	{Pid, shutdown} ->
	    Pid ! "Shutting down fridge server.";
	_ ->
	    io:format("Illegal request received.")
    end.

%% For sending request to server for storing food in fridge.
store_food(Pid, X) ->
    if is_atom(X) =:= true ->
	Pid ! {self(), store, X},
	receive
	    Msg -> Msg
	after 5000 ->
	    io:format("Server Unresponsive~n")
	end;
    is_atom(X) =:= false ->
	io:format("Incorrect food item. It should be in erlang's atom datatype format.~n")
    end.

%% For fetching food from fridge server.
take_food(Pid, X) ->
    if is_atom(X) =:= true ->
	Pid ! {self(), take, X},
	receive
	    Msg -> Msg
	after 5000 ->
	    io:format("Server Unresponsive~n")
	end;
    is_atom(X) =:= false ->
	io:format("Incorrect food item. It should be in erlang's atom datatype format.~n")
    end.

%% For receiving data regarding all food items presently stored in fridge server.
display_food(Pid) ->
    Pid ! {self(), display},
    receive
	Msg -> Msg
    after 5000 ->
	io:format("Server Unresponsive~n")
    end.

%% For shutting down fridge process server.
shutdown_fridge(Pid) ->
    Pid ! {self(), shutdown},
    receive
	Msg -> Msg
    after 5000 ->
	io:format("Server Unresponsive~n")
    end.

