%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011,
%%% @doc
%%% Assuming a record  of `-record(simple, {one, two})' in mod_fake
%%% Usage example:
%%% ```
%%%   Rec = mod_fake:new(<<"simple">>),
%%%   Json = mochijson2:decode("{'one':1,'two':2}"),
%%%   SimpleRec = json_rec:to_rec(Json,mod_fake,Rec)
%%%
%%% '''
%%%
%%% The above code will take the json and transform it into the
%%% specified record. Trying to match the field of the record with the
%%% key in the json. If a match fails, then json_rec will fall back to
%%% using proplists
%%%
%%% The module MUST export module:new/1. new/1 should take a binary and return a record. Example:
%%% ```
%%% -module(mod_fake).
%%% -export([new/1]).
%%% -record(simple, {one,two}).
%%% new(<<"simple">>) -> #simple{};
%%% new(_) -> undefined.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(json_rec).

-export([
         to_json/2,
         to_rec/3
        ]).

to_json(Record, Module) ->
    jsx:encode(to_json1(Record, Module)).

to_json1(Record, Module) ->
    Fields = Module:'#info-'(element(1, Record)),
    to_json1(Fields, Module, Record, []).

to_json1([], _Module, _Record, Acc) ->
    Acc;
to_json1([Field | Rest], Module, Record, Acc) ->
    case {Module:field_type(Field, Record),
          Module:'#get-'(Field, Record)} of
        {_, undefined} ->
            Property = {Field, null}, 
            to_json1(Rest, Module, Record, [Property | Acc]);
        {undefined, Value} ->
            Property = {Field, check_val(Value)}, 
            to_json1(Rest, Module, Record, [Property | Acc]);
        {[_SubRecord], Value} ->
            %% list of records
            F = fun(E) -> to_json1(E, Module) end,
            Property = {Field, lists:map(F, Value)},
            to_json1(Rest, Module, Record, [Property | Acc]);
        {_SubRecord, Value} ->
            Property = {Field, to_json1(Value, Module)},
            to_json1(Rest, Module, Record, [Property | Acc])
    end.
            
check_val(true) -> true;
check_val(false) -> false;
check_val(null) -> null;
check_val(undefiend) -> null;
check_val(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
check_val(Val) -> Val.

to_rec(Json, Module, RecName) ->
    Pl = jsx:decode(Json, [{labels, atom}]),
    Record = Module:'#new-'(RecName),
    to_rec1(Pl, Module, Record).

to_rec1(Pl, Module, Record) ->
    F = fun({Key, Value}) ->
            case Module:field_type(Key, Record) of
                undefined ->
                    {Key, Value};
                [SubRecord] ->
                    SF = fun(E) ->
                            to_rec1(E, Module, SubRecord)
                    end,
                    NewValue = lists:map(SF, Value),
                    {Key, NewValue};
                SubRecord ->
                    {Key, to_rec1(Value, Module, SubRecord)}
            end
    end,
    Module:'#fromlist-'(lists:map(F, Pl), Record).
