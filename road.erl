-module(road).
-compile(export_all).

%% using a recursive algorithm to find the shortest path through
%% road defined in road.txt
%%
%% text file is formatted A, B, X sequentially such that
%%
%%      A -------.--->
%%               |
%%               | X
%%               |
%%      B -------.--->
%% 
%% represents the nodes along two paths with intersection crosses

main([FileName]) -> 
    {ok, Bin} = file:read_file(FileName),
    ParsedMap = optimal_path(parse_map(Bin)),
    io:format("~p~n", [ParsedMap]),
    erlang:halt().

parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
    group_vals(Values, []).

%% mao values into {A, X, B} list
group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([A, B, X | Rest], Acc) ->
    group_vals(Rest, [{A, B, X} | Acc]).

%% shortest step algorithm, use similar to Dijkstras (?)
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
    OptA1 = {DistA + A, [{a, A} | PathA]},
    %% accumulate backwards as using a fold, which is tail recursive
    OptA2 = {DistB + B + X, [{x, X}, {b, B} | PathB]},
    OptB1 = {DistB + B, [{b, B} | PathB]},
    OptB2 = {DistA + A + X, [{x, X}, {a, A} | PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
    {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}},
                         Map),
    {_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
                       hd(element(2, B)) =/= {x, 0} -> B
                    end,
    lists:reverse(Path).
