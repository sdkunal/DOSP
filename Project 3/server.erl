-module(server).
-import(lists, [nth/2, seq/2, append/1, last/1, sort/1]).
-export([
    start/2,
    createList/2,
    createdEncryptedList/3,
    spawnActors/3,
    encryptPIDs/4,
    encrypt/1,
    initiate/0
]).

start(NumNodes, NumRequests) ->
    List = createList(1, NumNodes),
    EncryptedList = createdEncryptedList(1, NumNodes, List),
    SortedList = sort(List),
    % SortedEncryptedList = sort(EncryptedList),
    io:format("Map: ~p~n", [maps:from_list(EncryptedList)]),
    io:format("List: ~p~n", [SortedList]),
    % io:format("Encrypted List: ~p~n", [SortedEncryptedList]),
    io:format("Number of requests: ~p~n", [NumRequests]).

createList(S, E) ->
    spawnActors(S, E, []).

createdEncryptedList(S, E, List) ->
    encryptPIDs(S, E, List, []).

spawnActors(S, E, L) ->
    case S =< E of
        true ->
            spawnActors(
                S + 1,
                E,
                append([L, [spawn(server, initiate, [])]])
            );
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

initiate() ->
    receive
        {receiveRumour} ->
            io:format("Initiate~n", [])
    end.
