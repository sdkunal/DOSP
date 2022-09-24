-module(bitcoinSimulator).
-import(string, [equal/2, substr/3]).
-export([start/0, spawner/2, master/2, bitcoin/0, repeat/1, sha/2, get_random_string/2]).

start() ->
    % master pid
    spawn(bitcoinSimulator, spawner, [4, self()]).
% spawner(4, First_Pid).

spawner(NumActors, First_Pid) ->
    case NumActors > 0 of
        true ->
            spawn(bitcoinSimulator, bitcoin, []),
            spawner(NumActors - 1, First_Pid);
        false ->
            ""
    end.

masterStart() ->
    receive
        finished ->
            io:format("Master finished~n")
    end.

master(0, First_Pid) ->
    First_Pid ! {stop},
    io:fwrite("Master Finished\n");
master(N, First_Pid) ->
    % First_Pid ! {hash, self()},
    receive
        {masterfunction} ->
            io:fwrite("Master function\n")
    end,
    master(N - 1, First_Pid).

bitcoin() ->
    receive
        {stop} ->
            io:fwrite("Bitcoin stop\n");
        {hash, Second_Pid} ->
            repeat(1000),
            Second_Pid ! {masterfunction},
            bitcoin()
        % Other ->
        %     Pid ! {self(), Other},
        %     bitcoin()
    end.

repeat(Num) ->
    case Num > 0 of
        true ->
            sha(get_random_string(6, "abcdefghijklmnopqrstuvwxyz0123456789"), 2),
            repeat(Num - 1);
        false ->
            io:fwrite("Repeat finished\n")
    end.

sha(Str, Zeros) ->
    EncrytedStr = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha256, Str)
    ],
    ZerosString = lists:flatten(lists:duplicate(Zeros, "0")),
    EncryptedStart = substr(EncrytedStr, 1, Zeros),
    Status = equal(ZerosString, EncryptedStart),
    if
        Status ->
            io:fwrite(
                "~p" ++ " -- " ++ integer_to_list(Zeros) ++ " -- " ++ Str ++ " -- " ++ EncrytedStr ++
                    "\n",
                [self()]
            );
        true ->
            ""
    end.

get_random_string(Length, AllowedChars) ->
    BCoin =
        "kdudhe:" ++
            lists:foldl(
                fun(_, Acc) ->
                    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end,
                [],
                lists:seq(1, Length)
            ),
    BCoin.
