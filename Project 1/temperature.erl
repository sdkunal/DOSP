-module(temperature).
-export([temperatureConverter/0,start/0,convert/2]).

temperatureConverter() ->
 receive
 {From, {toF, C}} ->
 From ! {self(), 32+C*9/5},
 temperatureConverter();

 {From, {toC, F}} ->
 From ! {self(), (F-32)*5/9},
 temperatureConverter();

 {From, {stop}} ->
 From ! {self(),"Stopping~n"};

 {From, Other} ->
 From ! {self(),Other},
 temperatureConverter()
 end.

start() ->
 spawn(fun() -> temperatureConverter() end).

convert(Pid, Request) ->
 Pid ! {self(), Request},
 receive
 {Pid, Response} -> Response
 end.