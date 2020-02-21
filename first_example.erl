-module(first_example).
-compile(export_all).
% -export([add/2, hello/0, greet_and_add_two/1, greet/2]).

add(A,B) ->
    A + B.

%% Shows greetings
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").


greet_and_add_two(X) ->
    hello(),
    add(X, 2).

%% patern matching for greetings
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s", [Name]).

head([H|_]) -> H.
second([_,X|_]) -> X.

%% print date, but only if formatted correctly
valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is ~p/~p/~p, ~n", [Date, Y, M,
                                                                 D]),
    io:format("The Time tuple (~p) indicates: ~p:~p:~p. ~n", [Time, H, Min,
                                                              S]);
valid_time(_) ->
        io:format("Stop feeding me wrong data!~n").


%% using guards to be more expressive with pattern matching
old_enough(X) when X >= 18, X =< 104 -> true;   % can use , as andalso and ; as
                                                % orelse
old_enough(_) -> false.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does % This is Erlang's if's 'else' statement
    end.

%% purely academic example -- would work much better in function head
animal_help(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == beef -> "mooo";
              true -> "we live in a society"
            end,
    {Animal, "says " ++ Talk ++ "!"}.

%% insert function for sets
insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X, Set) of
        true -> Set;
        false -> [X|Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favourable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favourable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favourable in the US';
        _ ->
            'avoid beach'
    end.


%% Recursion example
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

%% e.g. to calculate the length of a list
len([]) -> 0;
len([_|T]) -> 1 + len(T).

%% for tail recursion
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).


tail_len(L) -> tail_len(L, 0).
tail_len([_|T], Acc) -> tail_len(T, Acc+1).


%% another recursive example for duplicating terms
duplicate(0,_) ->
    [];
duplicate(N, Term) when N > 0 ->
    [Term|duplicate(N-1, Term)].

%% now becomes easier to write a tail recursive version
tail_duplicate(N, Term) ->
    tail_duplicate(N, Term, []).

tail_duplicate(0,_,List) ->
    List;
tail_duplicate(N, Term, List) when N > 0 ->
    tail_duplicate(N-1, Term, [Term|List]).

%% reversing function
reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

%% tail recursion
tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

%% a sublist function
%% requires checks to ensure list is not empty
sublist(_, 0) -> [];
sublist([],_) -> [];
sublist([H|T], N) when N > 0 -> [H|sublist(T, N-1)].

%% or as in tail recursive form
tail_sublist(L, N) -> reverse(tail_sublist(L, N, [])).

tail_sublist(_, 0, SubList) -> SubList;
tail_sublist([], _, SubList) -> SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
    tail_sublist(T, N-1, [H|SubList]).


%% zipping function
%% 1> recursive:zip([a,b,c],[1,2,3]).
%%    [{a,1},{b,2},{c,3}]
zip([],[]) -> [];
zip([X|Xs], [Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].

%% lenient zip : finishes when one of the two lists is depleted
lenient_zip([],_) -> [];
lenient_zip(_,[]) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X,Y}|lenient_zip(Xs,Ys)].

%% tail zip
tail_zip(X, Y) -> tail_zip(X, Y, []).

tail_zip([], [], Acc) -> Acc;
tail_zip([X|Xs], [Y|Ys], Acc) -> tail_zip(Xs, Ys, [{X,Y}|Acc]).


