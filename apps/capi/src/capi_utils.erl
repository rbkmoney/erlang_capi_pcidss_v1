-module(capi_utils).

-type deadline() :: woody:deadline().

-export_type([deadline/0]).

-export([deadline_to_binary/1]).
-export([deadline_from_binary/1]).
-export([deadline_from_timeout/1]).
-export([deadline_is_reached/1]).

-export([base64url_to_map/1]).
-export([map_to_base64url/1]).

-export([to_universal_time/1]).

-export([parse_lifetime/1]).

-spec deadline_to_binary(deadline()) -> binary() | undefined.
deadline_to_binary(undefined) ->
    undefined;
deadline_to_binary(Deadline) ->
    woody_deadline:to_binary(Deadline).

-spec deadline_from_binary(binary()) -> deadline() | undefined.
deadline_from_binary(undefined) ->
    undefined;
deadline_from_binary(Binary) ->
    woody_deadline:from_binary(Binary).

-spec deadline_from_timeout(timeout()) -> deadline().
deadline_from_timeout(Timeout) ->
    woody_deadline:from_timeout(Timeout).

-spec deadline_is_reached(deadline()) -> boolean().
deadline_is_reached(Deadline) ->
    woody_deadline:is_reached(Deadline).

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    jsx:decode(base64url:decode(Base64), [return_maps]).

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    base64url:encode(jsx:encode(Map)).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    Microsecs = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(Microsecs, microsecond).

-spec parse_lifetime
    (undefined) -> {error, bad_lifetime};
    (binary()) -> {ok, timeout()} | {error, bad_lifetime}.
parse_lifetime(undefined) ->
    {error, bad_lifetime};
parse_lifetime(Bin) ->
    %% lifetime string like '1ms', '30s', '2.6m' etc
    %% default unit - millisecond
    case re:split(Bin, <<"^(\\d+\\.\\d+|\\d+)([a-z]*)$">>) of
        [<<>>, NumberStr, <<>>, <<>>] ->
            {ok, genlib:to_int(NumberStr)};
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            case unit_factor(Unit) of
                {ok, Factor} ->
                    {ok, erlang:round(Number * Factor)};
                {error, _Reason} ->
                    {error, bad_lifetime}
            end;
        _Other ->
            {error, bad_lifetime}
    end.

unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.

to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>, to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-spec parse_lifetime_test() -> _.
parse_lifetime_test() ->
    {ok, 16 * 1000} = parse_lifetime(<<"16s">>),
    {ok, 32 * 60 * 1000} = parse_lifetime(<<"32m">>),
    {error, bad_lifetime} = parse_lifetime(undefined),
    {error, bad_lifetime} = parse_lifetime(<<"64h">>).

-endif.
