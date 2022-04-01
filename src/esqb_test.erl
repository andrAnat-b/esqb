-module(esqb_test).
-include("esqb_i.hrl").

-export([tst/0, tst2/0, tst_json/0, tst_q1/0, tst_q2/0]).

tst() ->
  {Statement1, Params1, Dict1} = esqb:make_statement(tst_q1(), tst_json()),
  {Statement2, Params2, Dict2} = esqb:make_statement(tst_q2(), Dict1),
  StatementList = [Statement1, Statement2],
  ComputedArgs = esqb:make_args_from_dict(Dict2),
  Other = {Params1, Params2, Dict2},
  {StatementList, ComputedArgs, Other}.

tst2() ->
  {Statement1, Params1, Dict1} = esqb:make_statement(tst_qi1(), tst_json()),
  {Statement2, Params2, Dict2} = esqb:make_statement(tst_qu2(), Dict1),
  StatementList = [Statement1, Statement2],
  ComputedArgs = esqb:make_args_from_dict(Dict2),
  Other = {Params1, Params2, Dict2},
  {StatementList, ComputedArgs, Other}.

tst_json() ->
  [
    {<<"user_id">>, 15},
    {<<"opts">>, [
      {<<"auth">>, true},
      {<<"passwd">>, <<"pass1">>},
      {<<"matrix">>,[
        1,
        3,
        5,
        7
      ]}
    ]}
  ].


tst_q1() ->
  [
    "SELECT ",
    fun(Dict) ->
      List = ["data", "last_login_dt"],
      {lists:join(", ", List), Dict}
    end,
    " FROM",
    " users",
    " WHERE",
    #group{split = " AND", group = [
      #tpl{prefix = " id=",     param = {[<<"user_id">>]}, type = <<"int64">>},
      #tpl{prefix = " auth=",   param = {[<<"opts">>, <<"auth">>], false}},
      #tpl{prefix = " access=", param = {[<<"opts">>, <<"matrix">>, {4}]}}
    ]}
  ].
tst_q2() ->
  [
    "SELECT ",
    fun(Dict) ->
      {<<"*">>, Dict}
    end,
    " FROM",
    " data",
    " WHERE",
    #tpl{prefix = " id=",          param = {[<<"user_id">>]}},
    #tpl{prefix = " OR id=",       param = {[<<"user_id_2">>]}}, %% this part didn't added to request
    #tpl{prefix = " AND mtrx IN ", param = {[<<"opts">>, <<"matrix">>]}, type = <<"int[]">>}
  ].


tst_qi1() ->
  [
    "INSERT INTO ",
    "public.syslog (",
    fun(Dict) ->
      Keys = [
        {[<<"user_id">>],             "user_id"},
        {[unexist],                        "dt"},
        {[<<"opts">>, <<"passwd">>], "password"}
      ],
      {Subst, NewDict} = esqb:substitute_custom_values(Keys, Dict),
      erlang:display({'_______', NewDict}),
      {lists:join(", ", Subst), NewDict}
    end,
    ") VALUES",
    #group{
      start = " (",
      split = ", ",
      stop  = ") ",
      group = [
        #tpl{param = {[unexist]}},
        #tpl{param = {[<<"opts">>, <<"passwd">>]}},
        #tpl{param = {[<<"user_id">>]}}
      ]}
  ].

tst_qu2() ->
  [].
