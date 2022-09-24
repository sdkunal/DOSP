-module(client).
-import(server,[start/1,serverStart/0]).
-export([createRandomString/2,createEncryptedString/1,childProcess/5]).


createRandomString(Length, AllowedChars) ->
    Str =
        "kdudhe:" ++
            lists:foldl(
                fun(_, Acc) ->
                    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end,
                [],
                lists:seq(1, Length)
            ),
    Str.

createEncryptedString(Str) ->
    EncryptedStr = [
        element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
     || <<C:4>> <= crypto:hash(sha256, Str)
    ],
    EncryptedStr.

childProcess(MasterPID, Length, AllowedChars, Zeros, NumCoins) ->
    case NumCoins > 0 of
        true ->
            RandomStr = createRandomString(Length, AllowedChars),
            EncryptedStr = createEncryptedString(RandomStr),
            ZerosString = lists:flatten(lists:duplicate(Zeros, "0")),
            EncryptedStart = string:substr(EncryptedStr, 1, Zeros),
            Status = string:equal(ZerosString, EncryptedStart),
            if
                Status ->
                    MasterPID ! {get, RandomStr, EncryptedStr, Zeros, self()};
                true ->
                    ""
            end,
            childProcess(MasterPID, Length, AllowedChars, Zeros, NumCoins - 1);
        false ->
            % "",
            MasterPID ! finished
    end.
