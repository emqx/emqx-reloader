PROJECT = emqx_reloader
PROJECT_DESCRIPTION = EMQ X Reloader Plugin
PROJECT_VERSION = 3.0

BUILD_DEPS = emqx
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

COVER = true

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_reloader.conf -i priv/emqx_reloader.schema -d data
