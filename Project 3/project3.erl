-module(project3).
-import(lists, [nth/2, seq/2, append/1, last/1, sort/1]).
-export([
    start/2,
    createList/2,
    createdEncryptedPIDList/3,
    createdEncryptedList/3,
    spawnActors/3,
    encryption/4,
    encryptPIDs/4,
    encrypt/1,
    createRandomString/2,
    startSearch/9,
    initiate/2,
    fingerTable/11,
    calculateAvgHop/2
]).

start(NumNodes, NumRequests) ->
    NumNeighbors = list_to_integer(float_to_list(math:log2(NumNodes), [{decimals, 0}])),
    List = createList(1, NumNodes),
    EncryptedPIDs = createdEncryptedPIDList(1, NumNodes, List),
    EncryptedList = createdEncryptedList(1, NumNodes, List),
    SortedEncryptedList = sort(EncryptedPIDs),
    io:format("Encrypted List: ~p~n", [SortedEncryptedList]),
    RandString = createRandomString(6, "abcdefghijklmnopqrstuvwxyz0123456789"),
    EncryptedRandomString = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha, RandString)
    ],
    io:format("Encrypted Random String: ~p~n", [EncryptedRandomString]),
    Map = maps:from_list(EncryptedList),
    startSearch(
        1,
        NumNodes,
        Map,
        EncryptedPIDs,
        SortedEncryptedList,
        EncryptedRandomString,
        NumRequests,
        NumNeighbors,
        0
    ).

createList(S, E) ->
    spawnActors(S, E, []).

createdEncryptedPIDList(S, E, List) ->
    encryption(S, E, List, []).

createdEncryptedList(S, E, List) ->
    encryptPIDs(S, E, List, []).

spawnActors(S, E, L) ->
    case S =< E of
        true ->
            spawnActors(
                S + 1,
                E,
                append([L, [spawn(server, initiate, [E, 0])]])
            );
        false ->
            L
    end.

encryption(S, E, List, L) ->
    case S =< E of
        true ->
            encryption(S + 1, E, List, append([L, [encrypt(nth(S, List))]]));
        false ->
            L
    end.

encryptPIDs(S, E, List, L) ->
    case S =< E of
        true ->
            encryptPIDs(S + 1, E, List, append([L, [{encrypt(nth(S, List)), nth(S, List)}]]));
        false ->
            L
    end.

encrypt(PID) ->
    EncryptedStr = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha, pid_to_list(PID))
    ],
    EncryptedStr.

createRandomString(Length, AllowedChars) ->
    Str =
        lists:foldl(
            fun(_, Acc) ->
                [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
            end,
            [],
            lists:seq(1, Length)
        ),
    Str.

startSearch(
    S,
    E,
    Map,
    EncryptedPIDs,
    SortedEncryptedList,
    EncryptedRandomString,
    NumRequests,
    NumNeighbors,
    HopCount
) ->
    case S =< E of
        true ->
            SHAKey = nth(S, EncryptedPIDs),
            Node = maps:get(SHAKey, Map),
            Node !
                {searchKey, Map, EncryptedPIDs, SortedEncryptedList, EncryptedRandomString,
                    NumRequests, NumNeighbors, S, HopCount},
            startSearch(
                S + 1,
                E,
                Map,
                EncryptedPIDs,
                SortedEncryptedList,
                EncryptedRandomString,
                NumRequests,
                NumNeighbors,
                HopCount
            );
        false ->
            ""
    end.

initiate(NumNodes, RequestCount) ->
    receive
        {searchKey, Map, EncryptedPIDs, SortedEncryptedList, EncryptedRandomString, NumRequests,
            NumNeighbors, Index, HopCount} ->
            case RequestCount =< NumRequests of
                true ->
                    fingerTable(
                        Map,
                        EncryptedPIDs,
                        SortedEncryptedList,
                        EncryptedRandomString,
                        NumRequests,
                        1,
                        NumNeighbors,
                        NumNodes,
                        Index,
                        HopCount,
                        RequestCount
                    );
                false ->
                    ""
            end,
            initiate(NumNodes, RequestCount + 1)
    end.

fingerTable(
    Map,
    EncryptedPIDs,
    SortedEncryptedList,
    EncryptedRandomString,
    NumRequests,
    NeighborCount,
    NumNeighbors,
    NumNodes,
    Index,
    HopCount,
    RequestCount
) ->
    case NeighborCount =< NumNeighbors of
        true ->
            case Index == 1 of
                true ->
                    FirstNode = nth(1, SortedEncryptedList),
                    LastNode = nth(NumNodes, SortedEncryptedList),
                    case
                        (EncryptedRandomString >= LastNode) or (EncryptedRandomString =< FirstNode)
                    of
                        true ->
                            calculateAvgHop(HopCount, NumNodes);
                        false ->
                            ""
                    end;
                false ->
                    PrevNode = nth(Index - 1, SortedEncryptedList),
                    CurrNode = nth(Index, SortedEncryptedList),
                    case
                        (EncryptedRandomString >= PrevNode) and (EncryptedRandomString =< CurrNode)
                    of
                        true ->
                            calculateAvgHop(HopCount, NumNodes);
                        false ->
                            ""
                    end
            end,
            Offset = list_to_integer(
                float_to_list(math:pow(2, NeighborCount - 1), [{decimals, 0}])
            ),
            Temp = Index + Offset,
            Neighbor = Temp rem NumNodes,
            SHAKey = nth(Neighbor, EncryptedPIDs),
            Node = maps:get(SHAKey, Map),
            Node !
                {searchKey, Map, EncryptedPIDs, SortedEncryptedList, EncryptedRandomString,
                    NumRequests, NumNeighbors, Neighbor, HopCount + 1},
            fingerTable(
                Map,
                EncryptedPIDs,
                SortedEncryptedList,
                EncryptedRandomString,
                NumRequests,
                NeighborCount + 1,
                NumNeighbors,
                NumNodes,
                Index,
                HopCount,
                RequestCount + 1
            );
        false ->
            ""
    end.

calculateAvgHop(HopCount, NumNodes) ->
    io:format("Average hops: ~p~n", [HopCount / NumNodes]).
