-module(server).
-import(lists, [nth/2, seq/2, append/1, last/1, sort/1]).
-export([start/2, createList/2, spawnActors/3, encrypt/0, initiate/0]).

start(NumNodes, NumRequests) ->
    List = createList(1, NumNodes),
    ListB = sort(List),
    io:format("List: ~p~n", [ListB]),
    io:format("Number of requests: ~p~n", [NumRequests]).
% nth(1, List) ! {receiveRumour, List, NumRequests}.

createList(S, E) ->
    spawnActors(S, E, []).

spawnActors(S, E, L) ->
    case S =< E of
        true ->
            spawnActors(
                S + 1,
                E,
                append([L, [encrypt()]])
            );
        false ->
            L
    end.

encrypt() ->
    EncryptedStr = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha256, pid_to_list(spawn(server, initiate, [])))
    ],
    EncryptedStr.

initiate() ->
    receive
        {receiveRumour} ->
            io:format("Initiate~n", [])
    end.
