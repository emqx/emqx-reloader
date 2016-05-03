PROJECT = emqttd_reloader
PROJECT_DESCRIPTION = emqttd reloader
PROJECT_VERSION = 1.0

DEPS = lager

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk
