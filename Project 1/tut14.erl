-module(tut14).

-export([start/0, say_something/2,bitcoin/1,repeat/1]).

say_something(What, 0) ->
    done;

say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

repeat(Str)->
    io:format("~p~n", [Str]).
    % io:fwrite(Str++"\n").

bitcoin(Str) ->
    % receive
    %     {From, {}} ->
    %         From ! {self(), repeat(10)},
    %         bitcoin()
    % end.
    repeat(Str).

start() ->
    spawn(tut14, bitcoin, [hello]),
    spawn(tut14, bitcoin, [hey]).