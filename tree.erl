-module(tree).
-export([empty/0, insert/3, lookup/2]).

empty() -> {node, 'nil'}.

%% insert into smaller when node is less than current node
%% else insert into larger if greater
%% if equl, replace the value with the new one
%% 
%% storage is a tagged tuple:
%% {node, {Key, Value, Smaller, Larger}}

insert(Key, Val, {node, 'nil'}) 
        -> {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};

insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key
        -> {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};

insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key
        -> {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};

insert(Key, Val, {node, {Key, _, Smaller, Larger}}) 
        -> {node, {Key, Val, Smaller, Larger}}.


%% lookup function : returns {ok, Value} if key in tree
%% else will return atom 'undefined'.

lookup(_, {node, 'nil'})
        -> undefined;

% pattern match Key == Key, thus don't need to check using a guard
lookup(Key, {node, {Key, Val, _, _}})
        -> {ok, Val};

lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey
        -> lookup(Key, Smaller);

lookup(Key, {node, {_, _, _, Larger}})
        -> lookup(Key, Larger).


%% from chpt 7: adding a has_value/2 function:
%% looks for a given value 'Val' in the tree
has_value(_, {node, 'nil'}) ->
    false;
has_value(Val, {node, {_, Val, _, _}}) ->
    true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.


%% with throws, can create a function with fewer comparisons
has_value_throw(Val, Tree) ->
    try has_value1(Val, Tree) of
        false -> false
    catch
        true -> true
    end.

has_value1(_, {node, 'nil'}) ->
    false;
has_value1(Val, {node, {_, Val, _, _}}) ->
    throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
    has_value1(Val, Left),
    has_value1(Val, Right).

