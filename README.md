Making erlang record <~> json conversion easy.
--

A glue between [jiffy](https://github.com/davisp/jiffy) and [exprecs](git@github.com:uwiger/parse_trans.git). This is a modified (using jiffy instead of mochijson2) version of [json_rec](https://github.com/justinkirby/json_rec) which is explained in detail [here](http://blogs.openaether.org/?p=253 "exprecs, making json usable").

Quick example:
--

    -module(json_rec_model_example).
    -behaviour(json_rec_model).
    -export([field_type/2]). 

    %% define your record(s) that this module will work with
    -record(simple, {
          foo = 1 :: integer(),
          bar = 2 :: integer()
         }). 

    -record(deep, {
        answer = 42   :: integer(),
        single_simple :: #simple{},
        list_simple   :: [#simple{}]
         }).

    %% make these records accessible via exprecs. This is necessary in
    %% order to let json_rec move from json to #rec{} and back again for
    %% you
    -compile({parse_transform, exprecs}).
    -export_records([simple, deep]).

    %% this function is used to process deep nested records,
    %% if field holds a record return that record,
    %% if it holds a lists or records return that record inside a list

    field_type(single_simple, #deep{}) -> #simple{};
    field_type(list_simple, #deep{}) -> [#simple{}];
    field_type(_,_) -> undefined.


Once your have defined your records like this you can do:

    Simple = #simple{},
    json_rec:to_json(Simple, json_rec_model_example),
    > <<"{\"bar\":2,\"foo\":1}">>

Or: 
   
    Json = <<"{\"list_simple\":[{\"bar\":2,\"foo\":5},{\"bar\":2,\"foo\":1}],\"single_simple\":{\"bar\":2,\"foo\":1},\"answer\":42}">>.
    12> json_rec:to_rec(Json, json_rec_model_example, deep).
    > {deep,42,{simple,1,2},[{simple,5,2},{simple,1,2}]}
