-module(records).
-compile(export_all).

%% behave like structs in C, except expanded to tuples at compile time
-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).

%% create an instance:
first_robot() ->
    #robot{name="Mechatron",
           type=handmade,
           details=["Moved by a small man inside"]}.

%% can load record definitions from a module with
%% rr(Module)
%% in the erl shell. then run records:first_robot(). again
%%
%% use rf() to unload all/a specific record
%% rl to print all record defintions
%% rd to define one in the same way as -record()

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cards"}.

%% access syntax is Crusher#robot.hobbies.
%% for nested records (NestedBot#robot.details)#robot.name.
%% NB: parenthese are not mandatory

%% can use them for pattern matching
-record(user, {id, name, group, age}).

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

%% can extend without an issue
adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.

%% updating records
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.

%% to include from a .hrl header file
-include("records.hrl").
included() -> #included{some_field="Some value"}.

