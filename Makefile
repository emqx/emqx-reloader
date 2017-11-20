PROJECT = emqx_reloader
PROJECT_DESCRIPTION = EMQ X Reloader Plugin
PROJECT_VERSION = 2.3.0

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx X
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	 ./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_reloader.conf -i priv/emqx_reloader.schema -d data

