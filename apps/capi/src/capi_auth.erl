-module(capi_auth).

-export([init_provider/2]).
-export([authorize_operation/2]).

-export([get_operation_access/2]).
-export([get_access_config/0]).

%%

-type context() :: uac:context().

-opaque provider() ::
    {bouncer, capi_bouncer_context:fragments(), woody_context:ctx()}
    | {legacy, context()}.

-type resolution() :: allowed | {forbidden, _Reason}.

-export_type([context/0]).
-export_type([provider/0]).
-export_type([resolution/0]).

-spec init_provider(
    ReqCtx :: swag_server:request_context(),
    WoodyCtx :: woody_context:ctx()
) -> provider().
init_provider(ReqCtx, WoodyCtx) ->
    % NOTE
    % We need to support both bouncer-based authorization as well as legacy ACL-based one. Non-zero
    % number of various access tokens will probably be in-flight at the time of service update
    % rollout. And if we ever receive such token it should be better to handle it through legacy
    % authz machinery, since we have no simple way to extract required bouncer context out of it.
    case capi_bouncer:extract_context_fragments(ReqCtx, WoodyCtx) of
        Fragments when Fragments /= undefined ->
            {bouncer, Fragments, WoodyCtx};
        undefined ->
            {legacy, get_auth_context(ReqCtx)}
    end.

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

%%

-spec authorize_operation(
    Prototype :: capi_bouncer_context:prototypes(),
    Provider :: provider()
) -> resolution().
authorize_operation(Prototype, {bouncer, Fragments0, WoodyCtx}) ->
    Fragments1 = capi_bouncer_context:build(Prototype, Fragments0, WoodyCtx),
    case capi_bouncer:judge(Fragments1, WoodyCtx) of
        allowed ->
            allowed;
        forbidden ->
            {forbidden, policy}
    end;
authorize_operation(Prototype, {legacy, AuthContext}) ->
    authorize_operation_legacy(Prototype, AuthContext).

authorize_operation_legacy(Prototype, AuthContext) ->
    % NOTE
    % Operation context prototype MUST be present here at all times.
    OpPrototype = #{id := OperationID} = proplists:get_value(operation, Prototype),
    case uac:authorize_operation(get_operation_access(OperationID, OpPrototype), AuthContext) of
        ok ->
            allowed;
        {error, Reason} ->
            {forbidden, Reason}
    end.

%%

-spec get_operation_access(swag_server:operation_id(), capi_bouncer_context:prototype_operation()) ->
    [{uac_acl:scope(), uac_acl:permission()}].
get_operation_access('CreatePaymentResource', _) ->
    [{[payment_resources], write}].

-spec get_access_config() -> map().
get_access_config() ->
    #{
        domain_name => <<"common-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

get_resource_hierarchy() ->
    #{
        payment_resources => #{}
    }.
