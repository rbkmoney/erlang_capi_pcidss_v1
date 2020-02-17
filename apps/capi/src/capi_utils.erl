-module(capi_utils).

-export([base64url_to_map/1]).
-export([map_to_base64url/1]).

-export([to_universal_time/1]).

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    jsx:decode(base64url:decode(Base64), [return_maps]).

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    base64url:encode(jsx:encode(Map)).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    {ok, {Date, Time, Usec, TZOffset}} = rfc3339:parse(Timestamp),
    Seconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    %% The following crappy code is a dialyzer workaround
    %% for the wrong rfc3339:parse/1 spec.
    {DateUTC, TimeUTC} = calendar:gregorian_seconds_to_datetime(
        case TZOffset of
            _ when is_integer(TZOffset) ->
                Seconds - (60 * TZOffset);
            _ ->
                Seconds
        end
    ),
    {ok, TimestampUTC} = rfc3339:format({DateUTC, TimeUTC, Usec, 0}),
    TimestampUTC.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.
to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>,        to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-endif.
