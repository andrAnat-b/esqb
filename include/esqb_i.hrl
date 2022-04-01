-ifdef(ESQB).
-else.
-define(ESQB, true).

-record(dict, {props = [], idx = 1, indexed = []}).

-record(prop, {path = [], value, subst, ord = 0}).

-record(group, {
  group = [],
  start = " (",
  split = ", ",
  stop  = ") "
}).

-record(tpl, {
  prefix = <<>> :: string(),          %% Prefix for parameter ex: 'data='.
  
  param         :: parameter_tpl(),   %% Json_path-like path ex: [<<"key1">>, {index}, <<"key2">>]
  %% and default value or fun/1 for transform value ex: fun binary_to_integer/1.
  %% Example for parameter_tpl()^
  %%      ex1: {[<<"key1">>, {index}, <<"key2">>]}
  %%      ex2: {[<<"key1">>, {index}, <<"key2">>], null}
  %%      ex3: {[<<"key1">>, {index}, <<"key2">>], fun is_list/1}
  
  type   = <<>> :: string(),          %% common use case is type casting ex: <<"::int8">>, <<"::text">>
  suffix = <<>> :: string(),          %% common use case is aliasing ex: <<"as t">>, <<"as data">>
  
  strict = true :: boolean()          %% If set 'true' and 'param' undefined in dict than whole substitution
  %% will be skipped.
}).

-type parameter_tpl() :: simple_tpl() | extend_tpl().
-type simple_tpl()    :: {composite_key()}.
-type extend_tpl()    :: {composite_key(), default_val()}.
-type composite_key() :: [plain_key()|composite_key()].
-type default_val()   :: fun2val() | any().

-type fun2val()       :: fun_arity_1().
-type plain_key()     :: binary() | {integer()} | [].
-type fun_arity_1()  :: fun((X::composite_key()) -> any()).


-endif.
