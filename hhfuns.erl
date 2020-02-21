-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

%% can be called functionally with
%% add(fun one/0, fun two/0).


%% recursive: over a list, add or subtract from each integer of a list
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

%% due to similarity of such operations, can define a map function which 
%% accepts a function as a paramter to use as the operator
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

%% next: anonymous functions (lambdas)
%% syntax is
%%   fun(Args1) -> Expression1, Exp2, ..., ExpN;
%%      (Args2) -> Expression1, Exp2, ..., ExpN;
%%      (Args3) -> Expression1, Exp2, ..., ExpN
%%      end

%% PrepareAlarm = fun(Room) ->
%%         io:format("Alarm set in ~s.~n", [Room]),
%%         fun() -> io:format("Alarm tripped in ~s! Call Batman!~n", [Room]) end
%%         end.
