-module(capi_ct_helper).

-include_lib("capi_dummy_data.hrl").

-export([start_app/1]).
-export([start_app/2]).

-export([issue_token/2]).
-export([issue_token/3]).
-export([issue_token/4]).

%%

-type app_name() :: atom().

-spec start_app(app_name()) ->
    [app_name()].

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(scoper = AppName) ->
    start_app(AppName, [
        {storage, scoper_storage_logger}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) ->
    [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec issue_token( _, _) ->
        {ok, binary()} |
        {error,
            nonexistent_signee
        }.

issue_token(ACL, LifeTime) ->
    issue_token(?STRING, ACL, LifeTime, #{}).

-spec issue_token(_, _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(PartyID, ACL, LifeTime) ->
    issue_token(PartyID, ACL, LifeTime, #{}).

-spec issue_token(_, _, _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(PartyID, ACL, LifeTime, ExtraProperties) ->
    Claims = maps:merge(#{?STRING => ?STRING}, ExtraProperties),
    UniqueId = get_unique_id(),
    genlib:unwrap(
        uac_authorizer_jwt:issue(
            UniqueId,
            LifeTime,
            {PartyID, uac_acl:from_list(ACL)},
            Claims,
            capi_pcidss
        )
    ).

-spec get_unique_id() ->
    binary().

get_unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).
