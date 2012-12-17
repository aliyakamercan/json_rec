-module(json_rec_tests).
-include_lib("eunit/include/eunit.hrl").

-record(simple, {
          one,
          two
          }).

-record(simplet2l, {
          two
          }).

-record(deep, {
          simple,
          second = []
         }).


-compile({parse_transform, exprecs}).
-export_records([simple,simplet2l,deep]).
-export([field_type/2]).

field_type(simple, #deep{}) -> #simple{};
field_type(second, #deep{}) -> [#simple{}];
field_type(_FieldName, _Record) -> undefined.


simple_json_data() ->
    [<<"{\"one\":1,\"two\":2}">>,
     #simple{ one = 1, two = 2}].

deep_json_data() ->
    Simple = "{\"simple\":{\"one\":1,\"two\":2}",
    Deep = Simple++"}",
    [list_to_binary(Deep),
     #deep{ simple = #simple{ one = 1, two = 2}
            }].

deep_deep_json_data() ->
    [<<"{\"second\":[{\"two\":2,\"one\":1},{\"two\":2,\"one\":1},{\"two\":2,\"one\":1}],\"simple\":{\"two\":2,\"one\":1}}">>,
     #deep{ simple = #simple{ one = 1, two = 2},
            second  = [ #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2}
                      ]
          }
     ].


simple_test() ->
    [Json, Rec] = simple_json_data(),
    NewRec = json_rec:to_rec(Json,json_rec_tests,simple),
    ?assertEqual(Rec, NewRec).

deep_test() ->
    [Json, Rec] = deep_json_data(),
    NewRec = json_rec:to_rec(Json,json_rec_tests,deep),
    ?assertEqual(Rec, NewRec).

deep_deep_test()  ->
    [Json, Rec] = deep_deep_json_data(),
    New = json_rec:to_rec(Json,json_rec_tests,deep),
    ?assertEqual(Rec, New).

to_json_simple_test() ->
    [_Json, Rec] = simple_json_data(),
    Conv = json_rec:to_json(Rec, json_rec_tests),
    New = json_rec:to_rec(Conv,json_rec_tests,simple),
    ?assertEqual(Rec,New).

to_json_deep_test() ->
    [_Json, Rec] = deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    New = json_rec:to_rec(Conv, json_rec_tests, deep),
    ?assertEqual(Rec,New).

to_json_deep_deep_test() ->
    [_Json, Rec] = deep_deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    New = json_rec:to_rec(Conv, json_rec_tests, deep),
    ?assertEqual(Rec,New).


