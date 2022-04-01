-module(esqb_kv).

-compile([{no_auto_import,[get/2]}]).
%% API
-export([get/2]).
-export([get/3]).

-export([getn/2]).
-export([getn/3]).

-export([getpath/2]).
-export([getpath/3]).

get(Key, Struct) ->
  get(Key, Struct, undefined).

get(Key, [_|_] = Struct, Default) ->
  case lists:keyfind(Key, 1, Struct) of
    {_, Value} -> Value;
    false      -> Default
  end;
get(Key, #{} = Struct, Default) ->
  DefMap = #{Key => Default},
  Map0 = maps:merge(DefMap, Struct),
  maps:get(Key, Map0);
get(_, _, Default) -> Default.


getn(Pos, List) ->
  getn(Pos, List, undefined).

getn(Pos, [_|_] = List, Default) ->
  case Pos > length(List) or (Pos < 1) of
    false -> lists:nth(Pos, List);
    true  -> Default
  end;
getn(_, _, Default) -> Default.

getpath(KeyList, Struct) ->
  getpath(KeyList, Struct, undefined).

getpath([{Pos}|Keys], Struct, Default) ->
  case getn(Pos, Struct, Default) of
    Default   -> Default;
    NewStruct -> getpath(Keys, NewStruct, Default)
  end;
getpath([Key|Keys], Struct, Default) ->
  case get(Key, Struct, Default) of
    Default   -> Default;
    NewStruct -> getpath(Keys, NewStruct, Default)
  end;
getpath([], Value, _) -> Value.
