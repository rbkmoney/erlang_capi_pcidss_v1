%% @doc Top level supervisor.
%% @end

-module(capi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    LechiffreOpts = genlib_app:env(capi_pcidss, lechiffre_opts),
    LechiffreSpec = lechiffre:child_spec(lechiffre, LechiffreOpts),
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = genlib_app:env(capi_pcidss, health_check, #{}),
    AdditionalRoutes = [{'_', [erl_health_handle:get_route(HealthCheck), get_prometheus_route()]}],
    SwaggerSpec = capi_swagger_server:child_spec({AdditionalRoutes, LogicHandler}),
    UacConf = get_uac_config(),
    ok = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
        [LechiffreSpec] ++ LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []}.
get_logic_handler_info() ->
    case genlib_app:env(capi_pcidss, service_type) of
        real ->
            {capi_real_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.

get_uac_config() ->
    maps:merge(
        get_authorization_config(),
        #{access => capi_auth:get_access_config()}
    ).

get_authorization_config() ->
    case genlib_app:env(capi_pcidss, access_conf) of
        undefined ->
            exit(undefined_access_configuration);
        Config ->
            Config
    end.

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.
