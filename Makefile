PROJECT = esqb
PROJECT_DESCRIPTION = Erlang SQL Query Builder
PROJECT_VERSION = 0.1.0

DEPS += parse_trans

dep_parse_trans = git https://github.com/uwiger/parse_trans.git 3.4.1

ERLC_OPTS += '+{parse_transform, ct_expand}'
ERLC_OPTS += '+inline'
ERLC_OPTS += '+bin_opt_info'
ERLC_OPTS += '+warn_export_all'

FULL = 1

include erlang.mk
