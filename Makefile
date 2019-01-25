PROJECT = emqx_reloader
PROJECT_DESCRIPTION = EMQ X Reloader Plugin
PROJECT_VERSION = 3.0

BUILD_DEPS = emqx
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_reloader.conf -i priv/emqx_reloader.schema -d data
