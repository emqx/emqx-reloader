PROJECT = emqttd_reloader
PROJECT_DESCRIPTION = emqttd reloader
PROJECT_VERSION = 1.1

DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
