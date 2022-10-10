-module(server).
-import(lists, [nth/2]).
-export([
    startFull/1,
    createFullList/2,
    spawnFull/3,
    initiateFull/2,
    spreadFullRumor/5,
    randomize/5,
    startLine/1,
    createLineList/2,
    spawnLine/3,
    initiateLine/2,
    spreadLineRumor/2,
    start2D/2,
    create2DList/3,
    spawn2D/4,
    initiate2D/3,
    spread2DRumor/3,
    start3D/2,
    create3DList/3,
    spawn3D/4,
    initiate3D/3,
    spread3DRumor/3,
    processKiller/1,
    checkIfEqual/2
]).

startFull(NumNodes) ->
    List = createFullList(1, NumNodes),
    RandList = [rand:uniform(NumNodes) || _ <- lists:seq(1, NumNodes div 10)],
    Curr = nth(1, RandList),
    randomize(RandList, 1, List, NumNodes, Curr).

randomize(RandList, Index, List, NumNodes, Curr) ->
    case Index =< length(RandList) of
        true ->
            nth(Curr, List) ! {receiveRumour, List, NumNodes},
            Curr2 = nth(Index + 1, RandList),
            randomize(RandList, Index + 1, List, NumNodes, Curr2);
        false ->
            ""
    end.

startLine(NumNodes) ->
    List = createLineList(1, NumNodes),
    nth(1, List) ! {receiveRumour, List}.

start2D(NumNodes, NumCols) ->
    List = create2DList(1, NumNodes, NumCols),
    nth(1, List) ! {receiveRumour, List}.

start3D(NumNodes, NumCols) ->
    List = create3DList(1, NumNodes, NumCols),
    nth(1, List) ! {receiveRumour, List}.

createFullList(S, E) ->
    spawnFull(S, E, []).

createLineList(S, E) ->
    spawnLine(S, E, []).

create2DList(S, E, NumCols) ->
    spawn2D(S, E, NumCols, []).

create3DList(S, E, NumCols) ->
    spawn3D(S, E, NumCols, []).

spawnFull(S, E, L) ->
    case S =< E of
        true ->
            spawnFull(
                S + 1, E, lists:append([L, [spawn(server, initiateFull, ["This is a rumor", 1])]])
            );
        false ->
            L
    end.

spawnLine(S, E, L) ->
    case S =< E of
        true ->
            spawnLine(
                S + 1, E, lists:append([L, [spawn(server, initiateLine, ["This is a rumor", 1])]])
            );
        false ->
            L
    end.

spawn2D(S, E, NumCols, L) ->
    case S =< E of
        true ->
            spawn2D(
                S + 1,
                E,
                NumCols,
                lists:append([L, [spawn(server, initiate2D, ["This is a rumor", 1, NumCols])]])
            );
        false ->
            L
    end.

spawn3D(S, E, NumCols, L) ->
    case S =< E of
        true ->
            spawn3D(
                S + 1,
                E,
                NumCols,
                lists:append([L, [spawn(server, initiate3D, ["This is a rumor", 1, NumCols])]])
            );
        false ->
            L
    end.

initiateFull(Rumor, Count) ->
    receive
        {receiveRumour, List, NumNodes} ->
            io:format("~p ~p ~n", [self(), Rumor]),
            io:format("~p Count: ~n", [Count]),
            if
                Count == 5 ->
                    processKiller(self());
                true ->
                    spreadFullRumor(length(List), NumNodes, List, Rumor, self()),
                    initiateFull(Rumor, Count + 1)
            end;
        stop ->
            io:format("~p Stopping~n", [self()])
    end.

initiateLine(Rumor, Count) ->
    receive
        {receiveRumour, List} ->
            io:format("~p ~p ~n", [self(), Rumor]),
            io:format("~p Count: ~n", [Count]),
            if
                Count == 5 ->
                    processKiller(self());
                true ->
                    spreadLineRumor(List, self()),
                    initiateLine(Rumor, Count + 1)
            end;
        stop ->
            io:format("~p Stopping~n", [self()])
    end.

initiate2D(Rumor, Count, NumCols) ->
    receive
        {receiveRumour, List} ->
            io:format("~p ~p ~n", [self(), Rumor]),
            io:format("~p Count: ~n", [Count]),
            if
                Count == 5 ->
                    processKiller(self());
                true ->
                    spread2DRumor(List, self(), NumCols),
                    initiate2D(Rumor, Count + 1, NumCols)
            end;
        stop ->
            io:format("~p Stopping~n", [self()])
    end.

initiate3D(Rumor, Count, NumCols) ->
    receive
        {receiveRumour, List} ->
            io:format("~p ~p ~n", [self(), Rumor]),
            io:format("~p Count: ~n", [Count]),
            if
                Count == 5 ->
                    processKiller(self());
                true ->
                    spread3DRumor(List, self(), NumCols),
                    initiate3D(Rumor, Count + 1, NumCols)
            end;
        stop ->
            io:format("~p Stopping~n", [self()])
    end.

spreadFullRumor(Nodes, NumNodes, List, Rumor, CurrPID) ->
    case NumNodes > 0 of
        true ->
            case checkIfEqual(CurrPID, nth(NumNodes, List)) of
                true ->
                    spreadFullRumor(Nodes, NumNodes - 1, List, Rumor, CurrPID);
                false ->
                    nth(NumNodes, List) ! {receiveRumour, List, Nodes},
                    spreadFullRumor(Nodes, NumNodes - 1, List, Rumor, CurrPID)
            end;
        false ->
            ""
    end.

spreadLineRumor(List, CurrPID) ->
    case checkIfEqual(CurrPID, nth(1, List)) of
        true ->
            nth(2, List) ! {receiveRumour, List};
        false ->
            case checkIfEqual(CurrPID, lists:last(List)) of
                true ->
                    nth(length(List) - 1, List) ! {receiveRumour, List};
                false ->
                    nth(string:str(List, [CurrPID]) + 1, List) ! {receiveRumour, List},
                    nth(string:str(List, [CurrPID]) - 1, List) ! {receiveRumour, List}
            end
    end.

spread2DRumor(List, CurrPID, NumCols) ->
    Index = string:str(List, [CurrPID]),
    case Index + NumCols =< length(List) of
        true ->
            nth(Index + NumCols, List) ! {receiveRumour, List};
        false ->
            ""
    end,
    case Index - NumCols >= 1 of
        true ->
            nth(Index - NumCols, List) ! {receiveRumour, List};
        false ->
            ""
    end,
    case Index rem NumCols == 1 of
        true ->
            case Index + 1 =< length(List) of
                true ->
                    nth(Index + 1, List) ! {receiveRumour, List};
                false ->
                    ""
            end;
        false ->
            case Index rem NumCols == 0 of
                true ->
                    case Index - 1 >= 1 of
                        true ->
                            nth(Index - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end;
                false ->
                    nth(Index + 1, List) ! {receiveRumour, List},
                    nth(Index - 1, List) ! {receiveRumour, List}
            end
    end.

spread3DRumor(List, CurrPID, NumCols) ->
    Index = string:str(List, [CurrPID]),
    case Index + NumCols =< length(List) of
        true ->
            nth(Index + NumCols, List) ! {receiveRumour, List};
        false ->
            ""
    end,
    case Index - NumCols >= 1 of
        true ->
            nth(Index - NumCols, List) ! {receiveRumour, List};
        false ->
            ""
    end,
    case Index rem NumCols == 1 of
        true ->
            case Index + 1 =< length(List) of
                true ->
                    nth(Index + 1, List) ! {receiveRumour, List};
                false ->
                    ""
            end,
            case Index + NumCols + 1 =< length(List) of
                true ->
                    nth(Index + NumCols + 1, List) ! {receiveRumour, List};
                false ->
                    ""
            end,
            case Index - NumCols + 1 >= 1 of
                true ->
                    nth(Index - NumCols + 1, List) ! {receiveRumour, List};
                false ->
                    ""
            end;
        false ->
            case Index rem NumCols == 0 of
                true ->
                    case Index - 1 >= 1 of
                        true ->
                            nth(Index - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end,
                    case Index + NumCols - 1 =< length(List) of
                        true ->
                            nth(Index + NumCols - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end,
                    case Index - NumCols - 1 >= 1 of
                        true ->
                            nth(Index - NumCols - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end;
                false ->
                    nth(Index + 1, List) ! {receiveRumour, List},
                    nth(Index - 1, List) ! {receiveRumour, List},
                    case Index + NumCols + 1 =< length(List) of
                        true ->
                            nth(Index + NumCols + 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end,
                    case Index - NumCols + 1 >= 1 of
                        true ->
                            nth(Index - NumCols + 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end,
                    case Index + NumCols - 1 =< length(List) of
                        true ->
                            nth(Index + NumCols - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end,
                    case Index - NumCols - 1 >= 1 of
                        true ->
                            nth(Index - NumCols - 1, List) ! {receiveRumour, List};
                        false ->
                            ""
                    end
            end
    end,
    RandIndex = nth(1, [rand:uniform(length(List)) || _ <- lists:seq(1, 1)]),
    case RandIndex >= 1 of
        true ->
            case RandIndex =< length(List) of
                true ->
                    nth(RandIndex, List) ! {receiveRumour, List};
                false ->
                    ""
            end;
        false ->
            ""
    end.

checkIfEqual(PID1, PID2) ->
    if
        PID1 == PID2 ->
            true;
        true ->
            false
    end.

processKiller(PIDToKill) ->
    exit(PIDToKill, kill).
