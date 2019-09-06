%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% Notice that this file is copied from mochiweb project.
%%--------------------------------------------------------------------

-module(emqx_reloader_config).

-export([ reload/0
        , reload/1
        , reload/2
        , reload/3
        ]).

-type(application() :: atom()).

%% @doc Reload all app config file
-spec(reload() -> {'ok', [application()]}).
reload() -> reload([]).

%% @doc Reload all app config file, restart apps
-spec(reload([application()]) -> {'ok', [application()]}).
reload(RestartApps) ->
    AvailableApplications = [Application || {Application, _, _} <- application:loaded_applications()],
    reload(AvailableApplications, RestartApps).

%% @doc Reload specify app config file, restart apps
-spec(reload([application()], [application()]) -> {'ok', [application()]}).
reload(Applications, RestartApps) ->
    {ok, [[File]]} = init:get_argument(config),
    reload(Applications, RestartApps, File).

%% @doc Reload specify app , specify config file, restart apps
-spec(reload([application()], [application()], file:name_all()) -> {'ok', [application()]}).
reload(Applications, RestartApps, ConfigFile) ->
    {ok, Config} = check_config(ConfigFile),
    reload_ll(Applications, Config, RestartApps).

reload_ll(Applications, Config, RestartApps) ->
    case application_specs(Applications) of
        {incorrect_specs, IncorrectApps} ->
            logger:error("incorrect_specs error ~p", [IncorrectApps]),
            {error, {incorrect_specs, IncorrectApps}};
        Specs ->
            {change_application_data(Specs, Config, RestartApps), Applications}
    end.

application_specs(Applications) ->
    Specs = [{application, Application, make_application_spec(Application)} || Application <- Applications],
    IncorrectApplications = [Application || {_, Application, incorrect_spec} <- Specs],
    case IncorrectApplications of
        [] -> Specs;
        _ -> {incorrect_specs, IncorrectApplications}
    end.

change_application_data(Specs, Config, RestartApps) ->
    OldEnv = application_controller:prep_config_change(),
    ok = application_controller:change_application_data(Specs, Config),
    application_controller:config_change(OldEnv),
    maybe_restart_apps(RestartApps, OldEnv).

make_application_spec(Application) when is_atom(Application) ->
    {ok, LoadedAppSpec} = application:get_all_key(Application),
    case code:where_is_file(atom_to_list(Application) ++ ".app") of
        non_existing ->
            LoadedAppSpec;
        AppSpecPath when is_list(AppSpecPath) ->
            parse_app_file(AppSpecPath)
    end.

maybe_restart_apps(Applications, OldEnv) ->
    lists:foreach(
        fun(Application) ->
            CurrentEnv = application:get_all_env(Application),
            PreviousEnv = proplists:get_value(Application, OldEnv, []),
            case CurrentEnv of
                PreviousEnv ->
                    ok;
                _ ->
                    StartedApps = proplists:get_value(started, application_controller:info()),
                    ApplicationStartType = proplists:get_value(Application, StartedApps),
                    application:stop(Application),
                    application:start(Application, ApplicationStartType)
            end
        end,
    Applications).

check_config(File) ->
    Dirname = filename:dirname(File),
    Basename = filename:basename(File, ".config"),
    FileName = filename:join(Dirname, Basename) ++ ".config",
    {ok, Env} = parse_config(FileName),
    Files = [Term || Term <- Env, is_list(Term)],
    case check_config(Files, []) of
        ok -> {ok, Env};
        E -> E
    end.

check_config([], []) ->
    ok;

check_config([], Errors) ->
    {error, lists:reverse(Errors)};

check_config([File | Files], Errors) ->
    case parse_config(File) of
        {ok, _} -> check_config(Files, Errors);
        E -> check_config(Files, [E | Errors])
    end.

parse_config(File) ->
    case file:consult(File) of
        {ok, [Data]} -> {ok, Data};
        {error, E} -> {error, File, E}
    end.

parse_app_file(AppSpecPath) ->
    case file:consult(AppSpecPath) of
        {ok, [{application, _, AppSpec}]} -> AppSpec;
        {error, _Reason} -> incorrect_spec
    end.

