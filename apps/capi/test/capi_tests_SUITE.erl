-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("capi_bouncer_data.hrl").
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
    expiration_date_fail_test/1,
    create_visa_payment_resource_ok_test/1,
    create_nspkmir_payment_resource_ok_test/1,
    create_euroset_payment_resource_ok_test/1,
    create_qw_payment_resource_ok_test/1,
    create_crypto_payment_resource_ok_test/1,
    create_applepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_plain_payment_resource_ok_test/1,
    create_yandexpay_tokenized_payment_resource_ok_test/1,
    valid_until_payment_resource_test/1,
    check_support_decrypt_v2_test/1,
    authorization_forbidden_causes_client_error/1
]).

-define(CAPI_IP, "::").
-define(CAPI_PORT, 8080).
-define(CAPI_HOST_NAME, "localhost").
-define(CAPI_URL, ?CAPI_HOST_NAME ++ ":" ++ integer_to_list(?CAPI_PORT)).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, with_bouncer_auth},
        {group, with_legacy_auth}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {with_bouncer_auth, [], [
            {group, payment_resources},
            authorization_forbidden_causes_client_error
        ]},
        {with_legacy_auth, [], [
            {group, payment_resources}
        ]},
        {payment_resources, [], [
            expiration_date_fail_test,
            create_visa_payment_resource_ok_test,
            create_nspkmir_payment_resource_ok_test,
            create_euroset_payment_resource_ok_test,
            create_qw_payment_resource_ok_test,
            create_crypto_payment_resource_ok_test,
            create_applepay_tokenized_payment_resource_ok_test,
            create_googlepay_tokenized_payment_resource_ok_test,
            create_googlepay_plain_payment_resource_ok_test,
            create_yandexpay_tokenized_payment_resource_ok_test,
            valid_until_payment_resource_test,
            check_support_decrypt_v2_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    SupPid = start_mocked_service_sup(),
    Apps1 = capi_ct_helper:start_app(woody) ++ capi_ct_helper:start_app(scoper),
    Apps2 = mock_bouncer_client(SupPid),
    [{suite_apps, Apps1 ++ Apps2}, {suite_test_sup, SupPid} | Config].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = stop_applications(suite_apps, C),
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(with_bouncer_auth, Config) ->
    Apps = start_capi(Config, #{
        capi_pcidss => #{
            source => {pem_file, get_keysource("keys/local/private.pem", Config)},
            metadata => #{
                auth_method => user_session_token,
                user_realm => <<"external">>
            }
        }
    }),
    Token = capi_ct_helper:issue_token(capi_pcidss, ?STRING, [], unlimited),
    Context = get_context(Token),
    [{group_apps, Apps}, {context, Context} | Config];
init_per_group(payment_resources, Config) ->
    Apps = start_capi(Config, #{
        capi_pcidss => #{
            source => {pem_file, get_keysource("keys/local/private.pem", Config)}
        }
    }),
    BasePermissions = [
        {[payment_resources], write}
    ],
    Token = capi_ct_helper:issue_token(capi_pcidss, ?STRING, BasePermissions, unlimited),
    Context = get_context(Token),
    [{group_apps, Apps}, {context, Context} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    stop_applications(group_apps, C).

-spec stop_applications(group_apps | suite_apps, config()) -> _.
stop_applications(Key, C) ->
    [ok = application:stop(App) || App <- lists:reverse(proplists:get_value(Key, C, []))].

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, start_mocked_service_sup()} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create_visa_payment_resource_ok_test(_) -> _.
create_visa_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {cds_storage, fun
                ('PutSession', _) ->
                    {ok, ok};
                ('PutCard', {#'cds_PutCardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>}}) ->
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
        ],
        Config
    ),
    _ = mock_bouncer_arbiter(
        ?assertContextMatches(
            #bctx_v1_ContextFragment{
                env = #bctx_v1_Environment{
                    now = <<_/binary>>,
                    deployment = #bctx_v1_Deployment{id = ?TEST_DEPLOYMENT}
                },
                auth = #bctx_v1_Auth{
                    method = <<"SessionToken">>,
                    token = #bctx_v1_Token{id = <<_/binary>>}
                },
                user = #bctx_v1_User{
                    id = ?STRING,
                    realm = ?CTX_ENTITY(?TEST_USER_REALM)
                },
                capi = ?CTX_CAPI(?CTX_PARTY_OP(<<"CreatePaymentResource">>, ?STRING))
            }
        ),
        Config
    ),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    CardHolder = <<"Alexander Weinerschnitzel">>,
    {ok, #{
        <<"paymentToolToken">> := PaymentToolToken,
        <<"paymentToolDetails">> := #{
            <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
            <<"paymentSystem">> := <<"visa">>,
            <<"lastDigits">> := <<"1111">>,
            <<"bin">> := <<"411111">>,
            <<"cardNumberMask">> := <<"411111******1111">>
        }
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"4111111111111111">>,
            <<"cardHolder">> => CardHolder,
            <<"expDate">> => <<"03/20">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }),
    {ok, {{bank_card, BankCard}, _ValidUntil}} = capi_crypto:decrypt_payment_tool_token(PaymentToolToken),
    ?assertEqual(CardHolder, BankCard#domain_BankCard.cardholder_name).

-spec expiration_date_fail_test(_) -> _.
expiration_date_fail_test(Config) ->
    _ = mock_woody_client(
        [
            {cds_storage, fun
                ('PutSession', _) ->
                    {ok, ok};
                ('PutCard', {#'cds_PutCardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>}}) ->
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
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    CardHolder = <<"Alexander Weinerschnitzel">>,
    {error,
        {400, #{
            <<"code">> := <<"invalidRequest">>,
            <<"message">> := <<"Invalid expiration date">>
        }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"4111111111111111">>,
            <<"cardHolder">> => CardHolder,
            <<"expDate">> => <<"02/20">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_nspkmir_payment_resource_ok_test(_) -> _.
create_nspkmir_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {cds_storage, fun
                ('PutSession', _) ->
                    {ok, ok};
                ('PutCard', {#'cds_PutCardData'{pan = <<"22022002", _:6/binary, Mask:2/binary>>}}) ->
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
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolDetails">> := #{
            <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
            <<"paymentSystem">> := <<"nspkmir">>,
            <<"cardNumberMask">> := <<"22022002******54">>,
            <<"lastDigits">> := <<"54">>,
            <<"bin">> := <<"22022002">>
        }
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"2202200223948454">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_euroset_payment_resource_ok_test(_) -> _.
create_euroset_payment_resource_ok_test(Config) ->
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolDetails">> := #{
            <<"detailsType">> := <<"PaymentToolDetailsPaymentTerminal">>,
            <<"provider">> := <<"euroset">>
        }
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"PaymentTerminalData">>,
            <<"provider">> => <<"euroset">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_qw_payment_resource_ok_test(_) -> _.
create_qw_payment_resource_ok_test(Config) ->
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolDetails">> := #{
            <<"detailsType">> := <<"PaymentToolDetailsDigitalWallet">>,
            <<"digitalWalletDetailsType">> := <<"DigitalWalletDetailsQIWI">>,
            <<"phoneNumberMask">> := <<"+7******3210">>
        }
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"DigitalWalletData">>,
            <<"digitalWalletType">> => <<"DigitalWalletQIWI">>,
            <<"phoneNumber">> => <<"+79876543210">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_crypto_payment_resource_ok_test(_) -> _.
create_crypto_payment_resource_ok_test(Config) ->
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolDetails">> := #{
            <<"detailsType">> := <<"PaymentToolDetailsCryptoWallet">>,
            <<"cryptoCurrency">> := <<"bitcoinCash">>
        }
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CryptoWalletData">>,
            <<"cryptoCurrency">> => <<"bitcoinCash">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_applepay_tokenized_payment_resource_ok_test(_) -> _.
create_applepay_tokenized_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {payment_tool_provider_apple_pay, fun('Unwrap', _) ->
                {ok, ?UNWRAPPED_PAYMENT_TOOL(?APPLE_PAY_DETAILS)}
            end},
            {cds_storage, fun
                ('PutSession', _) -> {ok, ok};
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
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

-spec create_googlepay_tokenized_payment_resource_ok_test(_) -> _.
create_googlepay_tokenized_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {payment_tool_provider_google_pay, fun('Unwrap', _) ->
                {ok, ?UNWRAPPED_PAYMENT_TOOL(?GOOGLE_PAY_DETAILS)}
            end},
            {cds_storage, fun
                ('PutSession', _) -> {ok, ok};
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolDetails">> := #{
            <<"paymentSystem">> := <<"mastercard">>,
            <<"tokenProvider">> := <<"googlepay">>
        }
    }} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"GooglePay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }).

-spec create_googlepay_plain_payment_resource_ok_test(_) -> _.
create_googlepay_plain_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {payment_tool_provider_google_pay, fun('Unwrap', _) ->
                {ok,
                    ?UNWRAPPED_PAYMENT_TOOL(
                        ?GOOGLE_PAY_DETAILS,
                        {card, #paytoolprv_Card{
                            pan = <<"5321301234567892">>,
                            exp_date = #paytoolprv_ExpDate{month = 10, year = 2028}
                        }}
                    )}
            end},
            {cds_storage, fun
                ('PutSession', _) -> {ok, ok};
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
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
    ?assertEqual(error, maps:find(<<"tokenProvider">>, Details)).

-spec create_yandexpay_tokenized_payment_resource_ok_test(_) -> _.
create_yandexpay_tokenized_payment_resource_ok_test(Config) ->
    _ = mock_woody_client(
        [
            {payment_tool_provider_yandex_pay, fun('Unwrap', _) ->
                {ok, ?UNWRAPPED_PAYMENT_TOOL(?YANDEX_PAY_DETAILS)}
            end},
            {cds_storage, fun
                ('PutSession', _) -> {ok, ok};
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
        ],
        Config
    ),
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{
        <<"paymentToolToken">> := EncryptedToken,
        <<"paymentToolDetails">> := Details = #{
            <<"paymentSystem">> := <<"mastercard">>,
            <<"tokenProvider">> := <<"yandexpay">>
        }
    }} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"YandexPay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }),
    ?assertEqual(error, maps:find(<<"first6">>, Details)),
    {ok, {PaymentTool, _Deadline}} = capi_crypto:decrypt_payment_tool_token(EncryptedToken),
    ?assertMatch(
        {bank_card, #domain_BankCard{
            metadata = #{
                <<"com.rbkmoney.payment-tool-provider">> :=
                    {obj, #{
                        {str, <<"details">>} :=
                            {obj, #{
                                {str, <<"message_id">>} := {str, ?MESSAGE_ID}
                            }}
                    }}
            }
        }},
        PaymentTool
    ).

-spec valid_until_payment_resource_test(_) -> _.
valid_until_payment_resource_test(Config) ->
    _ = mock_bouncer_assert_party_op_ctx(<<"CreatePaymentResource">>, ?STRING, Config),
    {ok, #{
        <<"paymentToolToken">> := PaymentToolToken,
        <<"validUntil">> := ValidUntil
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CryptoWalletData">>,
            <<"cryptoCurrency">> => <<"bitcoinCash">>
        },
        <<"clientInfo">> => #{
            <<"fingerprint">> =>
                <<"test fingerprint">>
        }
    }),
    {ok, {_PaymentTool, DeadlineToken}} = capi_crypto:decrypt_payment_tool_token(PaymentToolToken),
    Deadline = capi_utils:deadline_from_binary(ValidUntil),
    ?assertEqual(Deadline, DeadlineToken).

-spec check_support_decrypt_v2_test(config()) -> _.
check_support_decrypt_v2_test(_Config) ->
    PaymentToolToken = <<
        "v2.eyJhbGciOiJFQ0RILUVTIiwiZW5jIjoiQTEyOEdDTSIsImVwayI6eyJhbGciOiJFQ0RILUVTIiwiY3J2IjoiUC0yNTYiLCJrdHkiOi"
        "JFQyIsInVzZSI6ImVuYyIsIngiOiJRanFmNFVrOTJGNzd3WXlEUjNqY3NwR2dpYnJfdVRmSXpMUVplNzVQb1R3IiwieSI6InA5cjJGV3F"
        "mU2xBTFJXYWhUSk8xY3VneVZJUXVvdzRwMGdHNzFKMFJkUVEifSwia2lkIjoia3hkRDBvclZQR29BeFdycUFNVGVRMFU1TVJvSzQ3dVp4"
        "V2lTSmRnbzB0MCJ9..j3zEyCqyfQjpEtQM.JAc3kqJm6zbn0fMZGlK_t14Yt4PvgOuoVL2DtkEgIXIqrxxWFbykKBGxQvwYisJYIUJJwt"
        "YbwvuGEODcK2uTC2quPD2Ejew66DLJF2xcAwE.MNVimzi8r-5uTATNalgoBQ"
    >>,
    {ok, {PaymentTool, ValidUntil}} = capi_crypto:decrypt_payment_tool_token(PaymentToolToken),
    ?assertEqual(
        {mobile_commerce, #domain_MobileCommerce{
            phone = #domain_MobilePhone{
                cc = <<"7">>,
                ctn = <<"9210001122">>
            },
            operator = megafone
        }},
        PaymentTool
    ),
    ?assertEqual(<<"2020-10-29T23:44:15.499Z">>, capi_utils:deadline_to_binary(ValidUntil)).

%%

-spec authorization_forbidden_causes_client_error(config()) -> _.
authorization_forbidden_causes_client_error(Config) ->
    _ = mock_bouncer_arbiter(
        fun(_) -> {ok, ?JUDGEMENT(?FORBIDDEN)} end,
        Config
    ),
    ?assertEqual(
        {error, {invalid_response_code, 401}},
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"PaymentTerminalData">>,
                <<"provider">> => <<"euroset">>
            },
            <<"clientInfo">> => #{
                <<"fingerprint">> => <<"test fingerprint">>
            }
        })
    ).

%%

start_capi(Config, AccessKeyset) ->
    JwkPublSource = {json, {file, get_keysource("keys/local/jwk.publ.json", Config)}},
    JwkPrivSource = {json, {file, get_keysource("keys/local/jwk.priv.json", Config)}},
    CapiEnv = [
        {ip, ?CAPI_IP},
        {port, ?CAPI_PORT},
        {deployment, ?TEST_DEPLOYMENT},
        {bouncer_ruleset_id, ?TEST_RULESET_ID},
        {lechiffre_opts, #{
            encryption_source => JwkPublSource,
            decryption_sources => [JwkPrivSource]
        }},
        {validation, #{
            now => {{2020, 3, 1}, {0, 0, 0}}
        }},
        {access_conf, #{
            jwt => #{
                keyset => AccessKeyset
            }
        }},
        {payment_tool_token_lifetime, <<"1024s">>}
    ],
    capi_ct_helper:start_app(capi_pcidss, CapiEnv).

% TODO move it to `capi_dummy_service`, looks more appropriate
start_mocked_service_sup() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    SupPid.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

mock_woody_client(Services, SupOrConfig) ->
    start_woody_client(mock_services(Services, SupOrConfig)).

% TODO need a better name
mock_services(Services, Config) when is_list(Config) ->
    mock_services(Services, ?config(test_sup, Config));
mock_services(Services, SupPid) when is_pid(SupPid) ->
    ServerRef = {dummy, lists:map(fun get_service_name/1, Services)},
    {ok, IP} = inet:parse_address(?CAPI_IP),
    ChildSpec = woody_server:child_spec(
        ServerRef,
        Options = #{
            ip => IP,
            port => 0,
            event_handler => {scoper_woody_event_handler, #{}},
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    {IP, Port} = woody_server:get_addr(ServerRef, Options),
    lists:foldl(
        fun(Service, Acc) ->
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

mock_bouncer_assert_party_op_ctx(Op, PartyID, Config) ->
    mock_bouncer_arbiter(
        ?assertContextMatches(
            #bctx_v1_ContextFragment{
                capi = ?CTX_CAPI(?CTX_PARTY_OP(Op, PartyID))
            }
        ),
        Config
    ).

mock_bouncer_arbiter(JudgeFun, SupOrConfig) ->
    start_bouncer_client(
        mock_services(
            [
                {
                    bouncer,
                    {bouncer_decisions_thrift, 'Arbiter'},
                    fun('Judge', {?TEST_RULESET_ID, Context}) ->
                        Fragments = decode_bouncer_context(Context),
                        Combined = combine_fragments(Fragments),
                        JudgeFun(Combined)
                    end
                }
            ],
            SupOrConfig
        )
    ).

mock_bouncer_client(SupOrConfig) ->
    start_bouncer_client(
        mock_services(
            [
                {
                    org_management,
                    {orgmgmt_auth_context_provider_thrift, 'AuthContextProvider'},
                    fun('GetUserContext', {UserID}) ->
                        {encoded_fragment, Fragment} = bouncer_client:bake_context_fragment(
                            bouncer_context_helpers:make_user_fragment(#{
                                id => UserID,
                                realm => #{id => ?TEST_USER_REALM},
                                orgs => [#{id => ?STRING, owner => #{id => UserID}, party => #{id => UserID}}]
                            })
                        ),
                        {ok, Fragment}
                    end
                }
            ],
            SupOrConfig
        )
    ).

decode_bouncer_context(#bdcs_Context{fragments = Fragments}) ->
    maps:map(fun(_, Fragment) -> decode_bouncer_fragment(Fragment) end, Fragments).

decode_bouncer_fragment(#bctx_ContextFragment{type = v1_thrift_binary, content = Content}) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(Content),
    {ok, Fragment, _} = thrift_strict_binary_codec:read(Codec, Type),
    Fragment.

combine_fragments(Fragments) ->
    [Fragment | Rest] = maps:values(Fragments),
    lists:foldl(fun combine_fragments/2, Fragment, Rest).

combine_fragments(Fragment1 = #bctx_v1_ContextFragment{}, Fragment2 = #bctx_v1_ContextFragment{}) ->
    combine_records(Fragment1, Fragment2).

combine_records(Record1, Record2) ->
    [Tag | Fields1] = tuple_to_list(Record1),
    [Tag | Fields2] = tuple_to_list(Record2),
    list_to_tuple([Tag | lists:zipwith(fun combine_fragment_fields/2, Fields1, Fields2)]).

combine_fragment_fields(undefined, V) ->
    V;
combine_fragment_fields(V, undefined) ->
    V;
combine_fragment_fields(V, V) ->
    V;
combine_fragment_fields(V1, V2) when is_tuple(V1), is_tuple(V2) ->
    combine_records(V1, V2);
combine_fragment_fields(V1, V2) when is_list(V1), is_list(V2) ->
    ordsets:union(V1, V2).

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, capi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {capi_dummy_service, #{function => Fun}}}}.

start_bouncer_client(ServiceURLs) ->
    ServiceClients = maps:map(fun(_, URL) -> #{url => URL} end, ServiceURLs),
    Acc = application:get_env(bouncer_client, service_clients, #{}),
    capi_ct_helper:start_app(bouncer_client, [{service_clients, maps:merge(Acc, ServiceClients)}]).

start_woody_client(ServiceURLs) ->
    capi_ct_helper:start_app(capi_woody_client, [{service_urls, ServiceURLs}]).

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?CAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

get_context(Token) ->
    capi_client_lib:get_context(?CAPI_URL, Token, 10000, ipv4).

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).
