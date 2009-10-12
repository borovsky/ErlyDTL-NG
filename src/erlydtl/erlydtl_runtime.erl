-module(erlydtl_runtime).

-compile(export_all).

format(Value) when is_atom(Value) ->
    atom_to_list(Value);
format(Value) when is_integer(Value) ->
    integer_to_list(Value);
format(Value) when is_float(Value) ->
    io_lib:format("~p", [Value]);
format(Value) ->
    Value.
    
extend_dict(Dict, Extendes) when is_list(Dict) ->
    Extendes ++ Dict;

extend_dict({GBSize, _} = Tree, Extendes)  when is_integer(GBSize) ->
    lists:foldl(fun({Key, Value}, Acc) ->
                        gb_trees:enter(Key, Value, Acc)
                end, Tree, Extendes);
extend_dict(Dict, Extendes) when is_tuple(Dict) ->
    Module = element(1, Dict),
    case Module of
        dict -> lists:foldl(fun({Key, Value}, Acc) ->
                            dict:store(Key, Value, Acc)
                    end, Dict, Extendes);
        Module ->
            throw(can_not_extend_tuples)
    end.

init_counter_stats(undefined) ->
    init_counter_stats([], undefined);

init_counter_stats(List) ->
    init_counter_stats(List, undefined).


init_counter_stats(undefined, Parent) ->
    init_counter_stats([], Parent);

init_counter_stats(List, Parent) ->
    [{"counter", 1}, 
     {"counter0", 0}, 
     {"revcounter", length(List)}, 
     {"revcounter0", length(List) - 1}, 
     {"first", true}, 
     {"last", length(List) =:= 1},
     {"parentloop", Parent}].

increment_counter_stats([{"counter", Counter},
                         {"counter0", Counter0},
                         {"revcounter", RevCounter},
                         {"revcounter0", RevCounter0},
                         {"first", _},
                         {"last", _},
                         {"parentloop", Parent}]) ->
    [{"counter", Counter + 1},
     {"counter0", Counter0 + 1},
     {"revcounter", RevCounter - 1},
     {"revcounter0", RevCounter0 - 1},
     {"first", false},
     {"last", RevCounter0 =:= 1},
     {"parentloop", Parent}].

is_true(false) ->
    false;
is_true(undefined) ->
    false;
is_true(_) ->
    true.

cycle(NamesTuple, Counters) when is_tuple(NamesTuple) ->
    format(element(erlang_el_runtime:get_value("counter0", Counters) rem
                   size(NamesTuple) + 1, NamesTuple));

cycle(NamesList, Counters) when is_list(NamesList) ->
    format(lists:nth(erlang_el_runtime:get_value("counter0", Counters) rem
                     length(NamesList) + 1, NamesList)).
