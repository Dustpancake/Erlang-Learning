-module(dolphins).
-compile(export_all).

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thaks for all the fish!~n");
        _ ->
            io:format("Heh, we're smarter than you humans.~n")
    end.

%% spawn syntax
%% c(dolphins).
%% Dolphin = spawn(dolphins, dolphin1, []).
%%
%% Dolphin ! "oh, hello dolphin!".

%% instead of writing to io, use a reply
dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?";
        {From, _} ->
            From ! "Heh, we're smarter than you humans.~n";
        _ ->
            io:format("Catch all!")
    end.

%% and now recursively, to ensure runs forever
%% (using tail recursion to not blow up the stack)
dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
            %% exits
        _ ->
            io:format("Heh, we're smarter than you humans.~n"),
            dolphin3()
    end.

