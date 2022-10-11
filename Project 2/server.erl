-module(server).
-import(lists, [nth/2, seq/2, append/1, last/1]).
-export([
    startFull/2,
    createFullList/3,
    spawnFull/4,
    initiateFull/6,
    spreadFullRumor/8,
    randomize/6,
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

startFull(AlgoName, NumNodes) ->
    case NumNodes >= 10 of
        true ->
            List = createFullList(1, NumNodes, AlgoName),
            RandList = [rand:uniform(NumNodes) || _ <- seq(1, NumNodes div 10)],
            Curr = nth(1, RandList),
            randomize(RandList, 1, List, NumNodes, Curr, AlgoName);
        false ->
            io:format("Please enter a number greater than 10.~n", [])
    end.

randomize(RandList, Index, List, NumNodes, Curr, AlgoName) ->
    case AlgoName == "Gossip" of 
        true ->
            case Index =< length(RandList) of
                true ->
                    nth(Curr, List) ! {receiveRumour, List, NumNodes},
                    case Index + 1 =< length(RandList) of
                        true ->
                            Curr2 = nth(Index + 1, RandList),
                            randomize(RandList, Index + 1, List, NumNodes, Curr2, AlgoName);
                        false ->
                            ""
                    end;
                false ->
                    ""
            end;
        false ->
            case Index =< length(RandList) of
                true ->
                    nth(Curr, List) ! {pushSumRumour, List, NumNodes},
                    case Index + 1 =< length(RandList) of
                        true ->
                            Curr2 = nth(Index + 1, RandList),
                            randomize(RandList, Index + 1, List, NumNodes, Curr2, AlgoName);
                        false ->
                            ""
                    end;
                false ->
                    ""
            end
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

createFullList(S, E, AlgoName) ->
    spawnFull(S, E, [], AlgoName).

createLineList(S, E) ->
    spawnLine(S, E, []).

create2DList(S, E, NumCols) ->
    spawn2D(S, E, NumCols, []).

create3DList(S, E, NumCols) ->
    spawn3D(S, E, NumCols, []).

spawnFull(S, E, L, AlgoName) ->
    case S =< E of
        true ->
            spawnFull(
                S + 1, E, append([L, [spawn(server, initiateFull, ["This is a rumor", 1, S, 1, AlgoName,0])]]), AlgoName
            );
        false ->
            L
    end.

spawnLine(S, E, L) ->
    case S =< E of
        true ->
            spawnLine(
                S + 1, E, append([L, [spawn(server, initiateLine, ["This is a rumor", 1])]])
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
                append([L, [spawn(server, initiate2D, ["This is a rumor", 1, NumCols])]])
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
                append([L, [spawn(server, initiate3D, ["This is a rumor", 1, NumCols])]])
            );
        false ->
            L
    end.

initiateFull(Rumor, Count, S, W, AlgoName,Counter) ->
    receive
        {receiveRumour, List, NumNodes} ->
            io:format("~p ~p ~n", [self(), Rumor]),
            io:format("~p Count: ~n", [Count]),
            if
                Count == 5 ->
                    processKiller(self());
                true ->
                    spreadFullRumor(length(List), NumNodes, List, Rumor, self(), S, W, AlgoName),
                    initiateFull(Rumor, Count + 1, S, W, AlgoName, Counter)
            end;
        {pushSumRumour, List, NumNodes} ->
            spreadFullRumor(length(List), NumNodes, List, Rumor, self(), S div 2, W , AlgoName),
            initiateFull(Rumor, Count + 1, S, W, AlgoName, Counter);
        {pushSumRumour, List, NumNodes, SReceived, WReceived} ->
            OldRatio = S / W,
            NewS = S + SReceived,
            NewW = W + WReceived,
            NewRatio = NewS / NewW,
            io:format("~p Ratio- ~n",[NewRatio]),
            io:format("~p Counter- ~n",[Counter]),
            case abs(OldRatio - NewRatio) =< 1 of
                true ->
                    Counter2=Counter+1,
                    if
                        Counter2==3->
                            processKiller(self());
                        true->
                            ""
                    end;
                false ->
                    Counter2=0
            end,
            spreadFullRumor(length(List), NumNodes, List, Rumor, self(), NewS div 2, NewW div 2, AlgoName),
            initiateFull(Rumor, Count + 1, S, W, AlgoName, Counter2);
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

spreadFullRumor(Nodes, NumNodes, List, Rumor, CurrPID, S, W, AlgoName) ->
    case NumNodes > 0 of
        true ->
            case checkIfEqual(CurrPID, nth(NumNodes, List)) of
                true ->
                    spreadFullRumor(Nodes, NumNodes - 1, List, Rumor, CurrPID, S, W, AlgoName);
                false ->
                    case AlgoName == "Gossip" of
                        true ->
                            nth(NumNodes, List) ! {receiveRumour, List, Nodes};
                        false ->
                            nth(NumNodes, List) ! {pushSumRumour, List, Nodes, S, W}
                    end,
                    spreadFullRumor(Nodes, NumNodes - 1, List, Rumor, CurrPID, S, W, AlgoName)
            end;
        false ->
            ""
    end.

spreadLineRumor(List, CurrPID) ->
    case checkIfEqual(CurrPID, nth(1, List)) of
        true ->
            nth(2, List) ! {receiveRumour, List};
        false ->
            case checkIfEqual(CurrPID, last(List)) of
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
    RandIndex = nth(1, [rand:uniform(length(List)) || _ <- seq(1, 1)]),
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
