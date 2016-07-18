PROJECT = emqttd_reloader
PROJECT_DESCRIPTION = emqttd reloader
PROJECT_VERSION = 2.0 

DEPS = gen_conf emqttd
dep_gen_conf = git https://github.com/emqtt/gen_conf master
dep_emqttd   = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
