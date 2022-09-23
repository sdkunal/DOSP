-module(bitcoinSimulator).
-import(string, [len/1, substr/3, equal/2]).
-export([bitcoin/0, start/0, hashComparison/2, sha/2, get_random_string/2, repeat/1]).

repeat(Num) ->
    %io:fwrite(integer_to_list(Num)),
    case Num > 0 of
        true ->
            sha(get_random_string(6, "abcdefghijklmnopqrstuvwxyz0123456789"), 1),
            %sha(curStr,3),
            % io:fwrite("\n"),
            repeat(Num - 1);
        false ->
            ""
    end.

sha(Str, Zeros) ->
    EncrytedStr = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha256, Str)
    ],
    Str1 = lists:flatten(lists:duplicate(Zeros, "0")),
    Str2 = substr(EncrytedStr, 1, Zeros),
    Status = equal(Str1, Str2),
    if
        Status ->
            io:fwrite(integer_to_list(Zeros) ++ "\t" ++ Str ++ "\t" ++ EncrytedStr ++ "\n");
        true ->
            ""
    end.

get_random_string(Length, AllowedChars) ->
    Str2 =
        "kdudhe:" ++
            lists:foldl(
                fun(_, Acc) ->
                    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end,
                [],
                lists:seq(1, Length)
            ),
    Str2.

bitcoin() ->
    receive
        {From, {}} ->
            From ! {self(), repeat(100)},
            bitcoin();
        {From, {stop}} ->
            From ! {self(), "Stopping~n"};
        {From, Other} ->
            From ! {self(), Other},
            bitcoin()
    end.

start() ->
    spawn(fun() -> bitcoin() end).

hashComparison(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.
