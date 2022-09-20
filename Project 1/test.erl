-module(test).
-export([hello/0,hello2/0]).
hello()->io:fwrite("Hello World!\n").
hello2()->io:fwrite("Hello World2!\n").
%this is a comment
%to run -> install erlang, run "erl" on command prompt to launch erlang shell
%to run module - "c(test)."
%to run function from a module - test:hello()