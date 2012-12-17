%% -*- mode: erlang -*-
%%%-------------------------------------------------------------------
%%% @author  <jkirby@voalte.com>
%%% @copyright (C) 2011,
%%% @doc
%%%
%%%  This is an example of how the json_rec_model behaviour can be
%%%  used.
%%%
%%%  comments inline should explain the rest.
%%%
%%% @end
%%% Created :  1 Sep 2011 by  <jkirby@voalte.com>
%%%-------------------------------------------------------------------
-module(json_rec_model_example).


%% specifiy that your module is a json_rec_model. much like gen_server
-behaviour(json_rec_model).


%% the three functions required export
-export([
    field_type/2
        ]).

%% define your record(s) that this module will work with
-record(simple, {
          foo = 1,
          bar = 2
         }).

-record(deep, {
        answer = 42 :: integer(),
        single_simple :: #simple{},
        list_simple :: [#simple{}]
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
