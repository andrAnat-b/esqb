-module(esqb).

-include("esqb_i.hrl").

%% API
-export([make_statement/2]).
-export([merge_dicts/2]).

-export([props_to_sql_dict/1]).
-export([make_query/2]).
-export([make_query/3]).
-export([make_args_from_dict/1]).
-export([make_subst/1]).

-export([substitute_property/2]).
-export([substitute_property/3]).

-export([substitute_existed_property/3]).

-export([substitute_custom_values/2]).




-define(NIL, '$nil').


-spec make_statement(Template :: list(), Params :: list() | #dict{}) ->
  {Query :: binary(), Args :: list(), Dict :: #dict{}}.

make_statement(Template, PropOrDict) ->
  {Query, Dict} = make_query(Template, PropOrDict),
  ArgsList      = make_args_from_dict(Dict),
  {Query, ArgsList, Dict}.


-spec make_query(Template :: list(), PropOrDict :: list() | #dict{}) -> {Query :: binary(), Dict :: #dict{}}.

make_query(Template, PropOrDict) ->
  make_query(Template, PropOrDict, []).


-spec make_query(Template :: list(), PropOrDict :: list() | #dict{}, Join :: string() | binary()) ->
  {Query :: binary(), Dict :: #dict{}}.

make_query(Template, #dict{} = Dict, Join) ->
  make_query(Template, Dict, [], Join);

make_query(Template, PropList, Join) ->
  make_query(Template, props_to_sql_dict(PropList), Join).


make_query([#tpl{prefix = Prefix, param = {Key}, type = Type, suffix = Suffix, strict = false}|Template], Dict, Acc, Join) ->
  {Subst, NewDict} = substitute_property(Key, Dict),
  make_query(Template, NewDict, [iolist_to_binary([Prefix,Subst, compute_suffix_and_type(Type, Suffix)])|Acc], Join);

make_query([#tpl{prefix = Prefix, param = {Key, Fun2ValOrDefault}, type = Type, suffix = Suffix, strict = false}|Template], Dict, Acc, Join) ->
  {Subst, NewDict} = substitute_property(Key, Dict, Fun2ValOrDefault),
  make_query(Template, NewDict, [iolist_to_binary([Prefix,Subst, compute_suffix_and_type(Type, Suffix)])|Acc], Join);

make_query([#tpl{prefix = Prefix, param = {Key}, type = Type, suffix = Suffix}|Template], Dict, Acc, Join) ->
  {Subst, NewDict} = substitute_existed_property(Key, Dict, ?NIL),
  case Subst of
    ?NIL  -> make_query(Template, NewDict, Acc, Join);
    Value -> make_query(Template, NewDict, [iolist_to_binary([Prefix, Value, compute_suffix_and_type(Type, Suffix)])|Acc], Join)
  end;

make_query([#tpl{prefix = Prefix, param = {Key, Fun2Val}, type = Type, suffix = Suffix}|Template], Dict, Acc, Join) ->
  {Subst, NewDict} = substitute_existed_property(Key, Dict, Fun2Val),
  case Subst of
    ?NIL  -> make_query(Template, NewDict, Acc, Join);
    Value -> make_query(Template, NewDict, [iolist_to_binary([Prefix, Value, compute_suffix_and_type(Type, Suffix)])|Acc], Join)
  end;

make_query([#group{group = Group, split = Split, start = St, stop = Fin}|Template], Dict, Acc, Join) ->
  {Subst, NewDict} = make_query(Group, Dict, Split),
  make_query(Template, NewDict, [Fin, Subst, St|Acc], Join);
make_query([Fun|Template], Dict, Acc, Join) when is_function(Fun, 1) ->
  {Subst, NewDict} = Fun(Dict),
  make_query(Template, NewDict, [Subst|Acc], Join);

make_query([Plain|Template], Dict, Acc, Join) ->
  make_query(Template, Dict, [Plain|Acc], Join);

make_query([], Dict, Acc, []) ->
  {iolist_to_binary(lists:reverse(Acc)), Dict};

make_query([], Dict, Acc, Join) ->
  {iolist_to_binary(lists:join(Join, lists:reverse(Acc))), Dict}.


-spec make_args_from_dict(Target :: #dict{}) -> List :: list().
make_args_from_dict(#dict{indexed = IndexedArgs}) ->
  %% @TODO probably keysort is unnecessary but kept for strict ordering
  OrderedList = lists:keysort(#prop.ord,lists:reverse(IndexedArgs)),
  [Value||#prop{value = Value}<-OrderedList].


-spec merge_dicts(Target :: #dict{}, Donor :: #dict{}) -> UpdatedTargetDict :: #dict{}.

merge_dicts(Target, Donor) ->
  Target#dict{props = Donor#dict.props ++ Target#dict.props}.


-spec substitute_property(Key::list(), Dict :: #dict{}) -> {Subst::binary(), Dict:: #dict{}}.

substitute_property(Key, Dict) ->
  substitute_property(Key, Dict, null).


-spec substitute_property(Key::list(), Dict :: #dict{}, Default :: any()) -> {Subst::binary(), Dict:: #dict{}}.

substitute_property(Key, #dict{idx = Idx, props = Props, indexed = Indexed} = Dict, Default) ->
  case lists:keyfind(Key, #prop.path, Indexed) of
    false -> case lists:keyfind(Key, #prop.path, Props) of
               false ->
                 Subst   = make_subst(Idx),
                 Value   = calculate_subst_value(Default, Default),
                 NewProp = #prop{ord = Idx, path = Key, value = Value, subst = Subst},
                 {Subst, Dict#dict{idx = Idx+1, indexed = [NewProp|Indexed]}};
               Prop ->
                 Subst   = make_subst(Idx),
                 Value   = calculate_subst_value(Default, Prop#prop.value),
                 NewProp = Prop#prop{ord = Idx, subst = Subst, value = Value},
                 NewDict = Dict#dict{idx = Idx+1, props = Props--[Prop], indexed = [NewProp|Indexed]},
                 {Subst, NewDict}
             end;
    Prop ->
      {Prop#prop.subst, Dict}
  end.


-spec substitute_existed_property(Key::list(), Dict :: #dict{}, Default :: any()) ->
  {Subst::binary(), Dict:: #dict{}}.

substitute_existed_property(Key, #dict{idx = Idx, props = Props, indexed = Indexed} = Dict, Fun2Val) ->
  case lists:keyfind(Key, #prop.path, Indexed) of
    false -> case lists:keyfind(Key, #prop.path, Props) of
               false ->
                 {?NIL, Dict};
               Prop ->
                 Subst   = make_subst(Idx),
                 Value   = calculate_subst_value(Fun2Val, Prop#prop.value),
                 NewProp = Prop#prop{ord = Idx, subst = Subst, value = Value},
                 NewDict = Dict#dict{idx = Idx+1, props = Props--[Prop], indexed = [NewProp|Indexed]},
                 {Subst, NewDict}
             end;
    Prop ->
      {Prop#prop.subst, Dict}
  end.


compute_suffix_and_type(<<>>, <<>>) ->
  <<>>;
compute_suffix_and_type(<<_/binary>> = Type, <<>>) ->
  <<"::", Type/binary>>;
compute_suffix_and_type(<<>>, <<_/binary>> = Suffix) ->
  <<" ", Suffix/binary>>;
compute_suffix_and_type(<<_/binary>> = Type, <<_/binary>> = Suffix) ->
  <<"::", Type/binary, " ", Suffix/binary>>.


calculate_subst_value(Fun, Value) when is_function(Fun, 1) -> Fun(Value);
calculate_subst_value(Fun, _)     when is_function(Fun, 0) -> Fun();
calculate_subst_value(_, Value)                            -> Value.

substitute_custom_values(KeyFunLis, Dict) ->
  substitute_custom_values(KeyFunLis, Dict, []).

substitute_custom_values([{Key, Value}|KeyFunLis], Dict, Acc) ->
  case substitute_existed_property(Key, Dict, fun(Default) -> Default end) of
    {?NIL, Dict} ->
      substitute_custom_values(KeyFunLis, Dict, Acc);
    {_, NewDict} ->
      substitute_custom_values(KeyFunLis, NewDict, [Value|Acc])
  end;
substitute_custom_values(_, Dict, Acc) ->
  {Acc, Dict}.


-spec props_to_sql_dict(List::list()) -> NewList::list().
props_to_sql_dict([_|_] = PropList) ->
  Props = prop_list_to_sql_tpl(PropList, [], []),
  #dict{props = Props};
props_to_sql_dict(_) ->
  #dict{}.


%%======================================================================================================================
prop_list_to_sql_tpl([_|_] = PropList, Context, Acc) ->
  case is_object(PropList) of
    true ->
      object_to_sql_tpl(PropList, Context, [#prop{path = Context, value = PropList}|Acc]);
    _ ->
      array_to_sql_tpl(PropList, Context, [#prop{path = Context, value = PropList}|Acc])
  end;

prop_list_to_sql_tpl(Entity, Context, Acc) ->
  [#prop{path = Context, value = Entity}|Acc].


%%======================================================================================================================
object_to_sql_tpl([{Key, Value}|PropList], Context, Acc) ->
  NewAcc = prop_list_to_sql_tpl(Value, Context++[Key], Acc),
  object_to_sql_tpl(PropList, Context, NewAcc);

object_to_sql_tpl([], _, Acc) -> Acc.


%%======================================================================================================================
array_to_sql_tpl(Array, Context, Acc) ->
  array_to_sql_tpl(Array, Context, Acc, 1).


array_to_sql_tpl([Item|Array], Context, Acc, Idx) ->
  NewAcc = prop_list_to_sql_tpl(Item, Context++[{Idx}], Acc),
  array_to_sql_tpl(Array, Context, NewAcc, Idx+1);
array_to_sql_tpl(_, _, Acc, _) -> Acc.


%%======================================================================================================================
make_subst(Idx) ->
  iolist_to_binary([<<"$">>, integer_to_binary(Idx)]).


is_object([_|_] = PropList)  ->
  Predicate = fun({_,_})-> true; (_)-> false end,
  lists:all(Predicate, PropList).







