%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emq_reloader_config).

-export([reloader/0,
         reloader/1,
         reloader/2,
         reloader/3]).

-type(application() :: atom()).

%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------

%% @doc reload all app config file
-spec reloader() -> {'ok', [application()]}.
reloader() ->
    reloader([]).

%% @doc reload all app config file, restart apps
-spec reloader(RestartApps :: [application()]) -> {'ok', [application()]}.
reloader(RestartApps) ->
    AvailableApplications = [Application || {Application, _, _} <- application:loaded_applications()],
    reloader(AvailableApplications, RestartApps).

%% @doc reload specify app config file, restart apps
-spec reloader(Applications :: [application()], 
             RestartApps :: [application()]) -> {'ok', [application()]}.
reloader(Applications, RestartApps) ->
    {ok, [[File]]} = init:get_argument(config),
    reloader(Applications, RestartApps, File).

%% @doc reload specify app , specify config file, restart apps
-spec reloader(Applications:: [application()], 
             RestartApps :: [application()], 
             ConfigFile :: file:name_all()) -> {'ok', [application()]}.
reloader(Applications, RestartApps, ConfigFile) ->
    {ok, Config} = check_config(ConfigFile),
    reloader_ll(Applications, Config, RestartApps).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
reloader_ll(Applications, Config, RestartApps) ->
    case application_specs(Applications) of
        {incorrect_specs, IncorrectApps} ->
            lager:error("incorrect_specs error ~p", [IncorrectApps]),
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
