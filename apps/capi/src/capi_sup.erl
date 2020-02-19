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
    HealthRoutes = [{'_', [erl_health_handle:get_route(genlib_app:env(capi_pcidss, health_checkers, []))]}],
    SwaggerSpec  = capi_swagger_server:child_spec({HealthRoutes, LogicHandler}),
    UacConf      = genlib_app:env(capi_pcidss, access_conf),
    ok           = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
            [LechiffreSpec] ++ LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    case genlib_app:env(capi_pcidss, service_type) of
        real ->
            {capi_real_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.
