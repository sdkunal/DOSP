-module(bcoin).
-import(string, [equal/2, substr/3]).
-export([start/0, masterStart/0, childProcess/1, loop/2]).

start() ->
    MasterPID = spawn(bcoin, masterStart, []),
    spawn(bcoin, childProcess, [MasterPID]).

masterStart() ->
    receive
        finished ->
            io:format("Master process ends~n", []);
        {stringCreate, NumStrings, ChildPID} ->
            io:format("Child started working~n", []),
            % case NumStrings > 0 of
            %     true ->
            % io:format("Inside Case True~n", []),

            loop(NumStrings, ChildPID),
            % RandomString =
            % ChildPID ! {randomStrings, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 2};
            % io:format("Created random string: ~p", [RandomString]);
            % ChildPID ! {sha, RandomString, 2};
            %     false ->
            %         ""
            % end,
            masterStart()
    end.

loop(0, ChildPID) ->
    ok;
loop(NumStrings, ChildPID) ->
    % io:fwrite("Hello\n"),
    ChildPID ! {randomStrings, 6, "abcdefghijklmnopqrstuvwxyz0123456789", 1},
    % io:fwrite("~p", [NumStrings]),
    % io:fwrite("\n"),
    loop(NumStrings - 1, ChildPID).

childProcess(MasterPID) ->
    MasterPID ! {stringCreate, 100, self()},
    receive
        {randomStrings, Length, AllowedChars, Zeros} ->
            io:fwrite("Inside Random String\n"),
            Str =
                "kdudhe:" ++
                    lists:foldl(
                        fun(_, Acc) ->
                            [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                        end,
                        [],
                        lists:seq(1, Length)
                    ),
            % Str;
            % {sha, Str, Zeros} ->
            % io:format("Inside SHA~n", []),
            EncryptedStr = [
                element(C + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F})
             || <<C:4>> <= crypto:hash(sha256, Str)
            ],
            % io:fwrite(EncryptedStr ++ "\n"),
            ZerosString = lists:flatten(lists:duplicate(Zeros, "0")),
            EncryptedStart = substr(EncryptedStr, 1, Zeros),
            Status = equal(ZerosString, EncryptedStart),
            if
                Status ->
                    io:format(
                        "~p" ++ " -- " ++ integer_to_list(Zeros) ++ " -- " ++ Str ++ " -- " ++
                            EncryptedStr ++
                            "\n",
                        [self()]
                    );
                true ->
                    % io:fwrite("did not match\n")\
                    ""
            end
    end,
    MasterPID ! finished.
