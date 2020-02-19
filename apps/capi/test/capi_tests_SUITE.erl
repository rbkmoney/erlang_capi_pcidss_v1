-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("damsel/include/dmsl_reporting_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("jose/include/jose_jwk.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    create_visa_payment_resource_ok_test/1,
    create_nspkmir_payment_resource_ok_test/1,
    create_euroset_payment_resource_ok_test/1,
    create_qw_payment_resource_ok_test/1,
    create_crypto_payment_resource_ok_test/1,
    create_applepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_plain_payment_resource_ok_test/1
]).

-define(CAPI_IP                     , "::").
-define(CAPI_PORT                   , 8080).
-define(CAPI_HOST_NAME              , "localhost").
-define(CAPI_URL                    , ?CAPI_HOST_NAME ++ ":" ++ integer_to_list(?CAPI_PORT)).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, payment_resources}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {payment_resources, [],
            [
                create_visa_payment_resource_ok_test,
                create_nspkmir_payment_resource_ok_test,
                create_euroset_payment_resource_ok_test,
                create_qw_payment_resource_ok_test,
                create_crypto_payment_resource_ok_test,
                create_applepay_tokenized_payment_resource_ok_test,
                create_googlepay_tokenized_payment_resource_ok_test,
                create_googlepay_plain_payment_resource_ok_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    SupPid = start_mocked_service_sup(),
    Apps =
        capi_ct_helper:start_app(woody)  ++
        capi_ct_helper:start_app(scoper) ++
        start_capi(Config),
    [{apps, lists:reverse(Apps)}, {suite_test_sup, SupPid} | Config].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(payment_resources, Config) ->
    BasePermissions = [
        {[payment_resources], write}
    ],
    Token = capi_ct_helper:issue_token(BasePermissions, unlimited),
    Context = get_context(Token),
    [{context, Context} | Config];

init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, start_mocked_service_sup()} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create_visa_payment_resource_ok_test(_) ->
    _.
create_visa_payment_resource_ok_test(Config) ->
    mock_services([
        {cds_storage, fun
            ('PutSession', _) -> {ok, ok};
            ('PutCard', [
                #'cds_PutCardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>}
            ]) ->
                {ok, #'cds_PutCardResult'{
                    bank_card = #cds_BankCard{
                        token = ?STRING,
                        bin = <<"411111">>,
                        last_digits = Mask
                    }
                }}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    CardHolder = <<"Alexander Weinerschnitzel">>,
    {ok, #{<<"paymentToolToken">> := PaymentToolToken,
    <<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"visa">>,
        <<"lastDigits">> := <<"1111">>,
        <<"bin">> := <<"411111">>,
        <<"cardNumberMask">> := <<"411111******1111">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"4111111111111111">>,
            <<"cardHolder">> => CardHolder,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }),
    {ok, {bank_card, BankCard}} = capi_crypto:decrypt_payment_tool_token(PaymentToolToken),
    CardHolder = BankCard#domain_BankCard.cardholder_name.

-spec create_nspkmir_payment_resource_ok_test(_) ->
    _.
create_nspkmir_payment_resource_ok_test(Config) ->
    mock_services([
        {cds_storage, fun
            ('PutSession', _) -> {ok, ok};
            ('PutCard', [
                #'cds_PutCardData'{pan = <<"22022002", _:6/binary, Mask:2/binary>>}
            ]) ->
                {ok, #'cds_PutCardResult'{
                    bank_card = #cds_BankCard{
                        token = ?STRING,
                        bin = <<"22022002">>,
                        last_digits = Mask
                    }
                }}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"NSPK MIR">>)} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"nspkmir">>,
        <<"cardNumberMask">> := <<"22022002******54">>,
        <<"lastDigits">> := <<"54">>,
        <<"bin">> := <<"22022002">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"2202200223948454">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_euroset_payment_resource_ok_test(_) ->
    _.
create_euroset_payment_resource_ok_test(Config) ->
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsPaymentTerminal">>,
        <<"provider">> := <<"euroset">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"PaymentTerminalData">>,
            <<"provider">> => <<"euroset">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_qw_payment_resource_ok_test(_) ->
    _.
create_qw_payment_resource_ok_test(Config) ->
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsDigitalWallet">>,
        <<"digitalWalletDetailsType">> := <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask">> := <<"+7******3210">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"DigitalWalletData">>,
            <<"digitalWalletType">> => <<"DigitalWalletQIWI">>,
            <<"phoneNumber">> => <<"+79876543210">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_crypto_payment_resource_ok_test(_) ->
    _.
create_crypto_payment_resource_ok_test(Config) ->
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsCryptoWallet">>,
        <<"cryptoCurrency">> := <<"bitcoinCash">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CryptoWalletData">>,
            <<"cryptoCurrency">> => <<"bitcoinCash">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_applepay_tokenized_payment_resource_ok_test(_) ->
    _.
create_applepay_tokenized_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_apple_pay, fun('Unwrap', _) -> {ok, ?UNWRAPPED_PAYMENT_TOOL(?APPLE_PAY_DETAILS)} end},
        {cds_storage, fun
            ('PutSession', _) -> {ok, ok};
            ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{<<"paymentSystem">> := <<"mastercard">>}}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"ApplePay">>,
                <<"merchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }).

-spec create_googlepay_tokenized_payment_resource_ok_test(_) ->
    _.
create_googlepay_tokenized_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_google_pay, fun('Unwrap', _) -> {ok, ?UNWRAPPED_PAYMENT_TOOL(?GOOGLE_PAY_DETAILS)} end},
        {cds_storage, fun
            ('PutSession', _) -> {ok, ok};
            ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"paymentSystem">> := <<"mastercard">>,
        <<"tokenProvider">> := <<"googlepay">>
    }}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"GooglePay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }).

-spec create_googlepay_plain_payment_resource_ok_test(_) ->
    _.
create_googlepay_plain_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_google_pay,
            fun('Unwrap', _) ->
                {ok, ?UNWRAPPED_PAYMENT_TOOL(
                    ?GOOGLE_PAY_DETAILS,
                    {card, #paytoolprv_Card{
                        pan = <<"5321301234567892">>,
                        exp_date = #paytoolprv_ExpDate{month = 10, year = 2028}
                    }}
                )}
            end
        },
        {cds_storage, fun
            ('PutSession', _) -> {ok, ok};
            ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {binbase,
            fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end
        }
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := Details = #{<<"paymentSystem">> := <<"mastercard">>}}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"GooglePay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }),
    false = maps:is_key(<<"tokenProvider">>, Details).

%%

start_capi(Config) ->
    KeySource = get_keysource("keys/local/jwk.json", Config),
    CapiEnv = [
        {ip, ?CAPI_IP},
        {port, ?CAPI_PORT},
        {service_type, real},
        {lechiffre_opts, #{
            encryption_key_path => KeySource,
            decryption_key_paths => [KeySource]
        }},
        {access_conf, #{
            jwt => #{
                keyset => #{
                    capi_pcidss => {pem_file, get_keysource("keys/local/private.pem", Config)}
                }
            },
            access => #{
                service_name => <<"common-api">>,
                resource_hierarchy => #{
                    payment_resources   => #{}
                }
            }
        }}
    ],
    capi_ct_helper:start_app(capi_pcidss, CapiEnv).

% TODO move it to `capi_dummy_service`, looks more appropriate
start_mocked_service_sup() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    SupPid.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

mock_services(Services, SupOrConfig) ->
    start_woody_client(mock_services_(Services, SupOrConfig)).

% TODO need a better name
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));

mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?CAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => {scoper_woody_event_handler, #{}},
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun (Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, capi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {capi_dummy_service, #{function => Fun}}}}.

start_woody_client(ServiceURLs) ->
    capi_ct_helper:start_app(capi_woody_client, [{service_urls, ServiceURLs}]).

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?CAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

get_context(Token) ->
    capi_client_lib:get_context(?CAPI_URL, Token, 10000, ipv4).

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).
