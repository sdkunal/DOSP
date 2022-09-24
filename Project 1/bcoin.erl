-module(bcoin).
% -import(string, [equal/2, substr/3]).
-export([
    start/0,
    masterStart/0,
    childProcess/1,
    % loop/2,
    createRandomString/2,
    createEncryptedString/1,
    childProcess2/5
]).

start() ->
    MasterPID = spawn(bcoin, masterStart, []),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]),
    spawn(bcoin, childProcess2, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 3, 2000000]).

masterStart() ->
    receive
        finished ->
            io:format("Master process ends~n", []);
        % {stringCreate, NumStrings, ChildPID} ->
        %     io:format("Child started working~n", []),
        %     loop(NumStrings, ChildPID),
        %     masterStart()
        {get, RandomStr, ChildPid} ->
            io:format("The string is: ~p from child: ~p~n", [RandomStr, ChildPid]),
            masterStart()
    end.

% loop(0, ChildPID) ->
%     ok;
% loop(NumStrings, ChildPID) ->
%     ChildPID ! {randomStrings, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 1},
%     loop(NumStrings - 1, ChildPID).

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

childProcess(MasterPID) ->
    % MasterPID ! {stringCreate, 100, self()},
    receive
        {randomStrings, Length, AllowedChars, Zeros} ->
            RandomStr = createRandomString(Length, AllowedChars),
            EncryptedStr = createEncryptedString(RandomStr),
            ZerosString = lists:flatten(lists:duplicate(Zeros, "0")),
            EncryptedStart = string:substr(EncryptedStr, 1, Zeros),
            Status = string:equal(ZerosString, EncryptedStart),
            if
                Status ->
                    io:format(
                        "~p" ++ " -- " ++ integer_to_list(Zeros) ++ " -- " ++ RandomStr ++ " -- " ++
                            EncryptedStr ++
                            "\n",
                        [self()]
                    );
                true ->
                    ""
            end
    end,
    MasterPID ! finished.

childProcess2(MasterPID, Length, AllowedChars, Zeros, NumCoins) ->
    case NumCoins > 0 of
        true ->
            RandomStr = createRandomString(Length, AllowedChars),
            EncryptedStr = createEncryptedString(RandomStr),
            ZerosString = lists:flatten(lists:duplicate(Zeros, "0")),
            EncryptedStart = string:substr(EncryptedStr, 1, Zeros),
            Status = string:equal(ZerosString, EncryptedStart),
            if
                Status ->
                    io:format(
                        "~p" ++ " -- " ++ integer_to_list(Zeros) ++ " -- " ++ RandomStr ++ " -- " ++
                            EncryptedStr ++
                            "\n",
                        [self()]
                    ),
                    MasterPID ! {get, RandomStr, self()};
                true ->
                    ""
            end,
            childProcess2(MasterPID, Length, AllowedChars, Zeros, NumCoins - 1);
        false ->
            % "",
            MasterPID ! finished
    end.
