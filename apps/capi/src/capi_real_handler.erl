-module(capi_real_handler).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
-include_lib("damsel/include/dmsl_geo_ip_thrift.hrl").
-include_lib("damsel/include/dmsl_reporting_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").

-behaviour(swag_server_logic_handler).

%% API callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(SWAG_HANDLER_SCOPE, swag_handler).

-define(DEFAULT_INVOICE_META, #{}).
-define(DEFAULT_INVOICE_TPL_META, #{}).
-define(DEFAULT_URL_LIFETIME, 60). % seconds

-define(payment_institution_ref(PaymentInstitutionID),
    #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key(), handler_opts()) ->
    Result :: false | {true, uac:context()}.

authorize_api_key(OperationID, ApiKey, _HandlerOpts) ->
    scoper:scope(?SWAG_HANDLER_SCOPE, #{operation_id => OperationID}, fun() ->
        _ = logger:debug("Api key authorization started"),
        case uac:authorize_api_key(ApiKey, get_verification_opts()) of
            {ok, Context} ->
                _ = logger:debug("Api key authorization successful"),
                {true, Context};
            {error, Error} ->
                _ = logger:info("Api key authorization failed due to ~p", [Error]),
                false
        end
    end).

get_verification_opts() ->
    #{}.

-type request_data() :: #{atom() | binary() => term()}.
-type handler_opts() :: swag_server:handler_opts(_).

-spec handle_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context(),
    handler_opts()
) ->
    {ok | error, swag_server:response()}.

handle_request(OperationID, Req, Context, _HandlerOpts) ->
    scoper:scope(
        ?SWAG_HANDLER_SCOPE,
        #{
            operation_id => OperationID
        },
        fun() -> handle_request_(OperationID, Req, Context) end
    ).

handle_request_(OperationID, Req, Context) ->
    try
        ReqContext = create_context(Req, get_auth_context(Context)),
        _ = logger:debug("Processing request"),
        OperationACL = capi_auth:get_operation_access(OperationID, Req),
        case uac:authorize_operation(OperationACL, get_auth_context(Context)) of
            ok ->
                process_request(OperationID, Req, Context, ReqContext);
            {error, _} = Error ->
                _ = logger:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {error, {401, #{}, general_error(<<"Unauthorized operation">>)}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec process_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context(),
    ReqCtx :: woody_context:ctx()
) ->
    {Code :: non_neg_integer(), Headers :: #{}, Response :: #{}}.

process_request('CreatePaymentResource' = OperationID, Req, Context, ReqCtx) ->
    Params = maps:get('PaymentResourceParams', Req),
    ClientInfo = enrich_client_info(maps:get(<<"clientInfo">>, Params), Context),
    PartyID = get_party_id(Context),
    try
        Data = maps:get(<<"paymentTool">>, Params), % "V" !!!!
        IdempotentKey = capi_bender:get_idempotent_key(OperationID, PartyID, undefined),
        {PaymentTool, PaymentSessionID} = case Data of
            #{<<"paymentToolType">> := <<"CardData">>} ->
                process_card_data(Data, IdempotentKey, ReqCtx);
            #{<<"paymentToolType">> := <<"PaymentTerminalData">>} ->
                process_payment_terminal_data(Data, ReqCtx);
            #{<<"paymentToolType">> := <<"DigitalWalletData">>} ->
                process_digital_wallet_data(Data, ReqCtx);
            #{<<"paymentToolType">> := <<"TokenizedCardData">>} ->
                process_tokenized_card_data(Data, IdempotentKey, ReqCtx);
            #{<<"paymentToolType">> := <<"CryptoWalletData">>} ->
                process_crypto_wallet_data(Data, ReqCtx)
        end,
        PaymentResource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSessionID,
            client_info = encode_client_info(ClientInfo)
        },
        EncryptedToken = capi_crypto:create_encrypted_payment_tool_token(IdempotentKey, PaymentTool),
        {ok, {201, #{}, decode_disposable_payment_resource(PaymentResource, EncryptedToken)}}
    catch
        Result ->
            Result
    end.

%%%

service_call(ServiceName, Function, Args, Context) ->
    capi_woody_client:call_service(ServiceName, Function, Args, Context).

create_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => capi_auth:get_subject_id(AuthContext),
        realm => ?REALM,
        email => capi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => capi_auth:get_claim(<<"name">>, AuthContext, undefined)
    }).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

parse_exp_date(undefined) ->
    undefined;
parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    #cds_ExpDate{
        month = genlib:to_int(Month),
        year = Year
    }.

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

get_peer_info(#{peer := Peer}) ->
    Peer.

get_party_id(Context) ->
    capi_auth:get_subject_id(get_auth_context(Context)).

decode_client_info(ClientInfo) ->
    #{
        <<"fingerprint">> => ClientInfo#domain_ClientInfo.fingerprint,
        <<"ip">> => ClientInfo#domain_ClientInfo.ip_address
    }.

encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address = maps:get(<<"ip">>, ClientInfo)
    }.

encode_content(json, Data) ->
    #'Content'{
        type = <<"application/json">>,
        data = jsx:encode(Data)
    }.

decode_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>});
decode_payment_tool_details({crypto_currency, CryptoCurrency}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCryptoWallet">>,
        <<"cryptoCurrency">> => convert_crypto_currency_to_swag(CryptoCurrency)
    }.

decode_bank_card_details(BankCard, V) ->
    LastDigits = decode_last_digits(BankCard#domain_BankCard.masked_pan),
    Bin = BankCard#domain_BankCard.bin,
    merge_and_compact(V, #{
        <<"lastDigits">>     => LastDigits,
        <<"bin">>            => Bin,
        <<"cardNumberMask">> => decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem" >> => genlib:to_binary(BankCard#domain_BankCard.payment_system),
        <<"tokenProvider" >> => decode_token_provider(BankCard#domain_BankCard.token_provider)
    }).

decode_token_provider(Provider) when Provider /= undefined ->
    genlib:to_binary(Provider);
decode_token_provider(undefined) ->
    undefined.

decode_payment_terminal_details(#domain_PaymentTerminal{
    terminal_type = Type
}, V) ->
    V#{
        <<"provider">> => genlib:to_binary(Type)
    }.

decode_digital_wallet_details(#domain_DigitalWallet{
    provider = qiwi,
    id = ID
}, V) ->
    V#{
        <<"digitalWalletDetailsType">> => <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask">> => mask_phone_number(ID)
    }.

-define(MASKED_PAN_MAX_LENGTH, 4).

decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

-define(PAN_LENGTH, 16).

decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

mask_phone_number(PhoneNumber) ->
    genlib_string:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).


process_woody_error(_Source, result_unexpected, _Details) ->
    {error, reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, reply_5xx(503)};
process_woody_error(_Source, result_unknown, _Details) ->
    {error, reply_5xx(504)}.

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, #{}, <<>>}.

enrich_client_info(ClientInfo, Context) ->
    ClientInfo#{<<"ip">> => prepare_client_ip(Context)}.

prepare_client_ip(Context) ->
    #{ip_address := IP} = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

process_card_data(Data, IdempotentKey, ReqCtx) ->
    PutCardData = encode_card_data(Data),
    SessionData = encode_session_data(Data),
    BankInfo = get_bank_info(PutCardData#cds_PutCardData.pan, ReqCtx),
    PaymentSystem = capi_bankcard:payment_system(BankInfo),
    case capi_bankcard:validate(PutCardData, SessionData, PaymentSystem) of
        ok ->
            put_card_data_to_cds(PutCardData, SessionData, IdempotentKey, BankInfo, ReqCtx);
        {error, Error} ->
            throw({ok, validation_error(Error)})
    end.


put_card_to_cds(PutCardData, BankInfo, ReqCtx) ->
    case service_call(cds_storage, 'PutCard', [PutCardData], ReqCtx) of
        {ok, #cds_PutCardResult{bank_card = BankCard}} ->
            ExpDate = PutCardData#cds_PutCardData.exp_date,
            CardholderName = PutCardData#cds_PutCardData.cardholder_name,
            {bank_card, expand_card_info(BankCard, BankInfo, ExpDate, CardholderName)};
        {exception, #cds_InvalidCardData{}} ->
            throw({ok, {400, #{}, logic_error(invalidRequest, <<"Card data is invalid">>)}})
    end.

put_session_to_cds(SessionID, SessionData, ReqCtx) ->
    {ok, ok} = service_call(cds_storage, 'PutSession', [SessionID, SessionData], ReqCtx),
    ok.

put_card_data_to_cds(PutCardData, SessionData, IdempotentKey, BankInfo, ReqCtx) ->
    BankCard = put_card_to_cds(PutCardData, BankInfo, ReqCtx),
    {bank_card, #domain_BankCard{token = Token}} = BankCard,
    RandomID = gen_random_id(),
    Hash = erlang:phash2(Token),
    {ok, SessionID} = capi_bender:gen_by_constant(IdempotentKey, RandomID, Hash, ReqCtx),
    ok = put_session_to_cds(SessionID, SessionData, ReqCtx),
    {BankCard, SessionID}.

expand_card_info(BankCard, BankInfo, ExpDate, CardholderName) ->
    #{
        payment_system  := PaymentSystem,
        bank_name       := BankName,
        issuer_country  := IssuerCountry,
        metadata        := Metadata
    } = BankInfo,
    #domain_BankCard{
        token = BankCard#cds_BankCard.token,
        bin = BankCard#cds_BankCard.bin,
        masked_pan = BankCard#cds_BankCard.last_digits,
        exp_date = encode_exp_date(ExpDate),
        payment_system = PaymentSystem,
        issuer_country = IssuerCountry,
        bank_name = BankName,
        cardholder_name = CardholderName,
        metadata = #{
            ?CAPI_NS => capi_msgp_marshalling:marshal(Metadata)
        }
    }.

encode_card_data(CardData) ->
    ExpDate = parse_exp_date(genlib_map:get(<<"expDate">>, CardData)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, CardData)),
    #cds_PutCardData{
        pan  = CardNumber,
        exp_date = ExpDate,
        cardholder_name = genlib_map:get(<<"cardHolder">>, CardData)
    }.

encode_session_data(CardData) ->
    #cds_SessionData{
        auth_data = {card_security_code, #cds_CardSecurityCode{
            value = genlib_map:get(<<"cvv">>, CardData)
        }}
    }.

encode_exp_date(#cds_ExpDate{month = Month, year = Year}) ->
    #domain_BankCardExpDate{
        month = Month,
        year = Year
    }.

process_payment_terminal_data(Data, _ReqCtx) ->
    PaymentTerminal = #domain_PaymentTerminal{
        terminal_type = binary_to_existing_atom(
            genlib_map:get(<<"provider">>, Data),
            utf8
        )
    },
    {{payment_terminal, PaymentTerminal}, <<>>}.

process_digital_wallet_data(Data, _ReqCtx) ->
    DigitalWallet = case Data of
        #{<<"digitalWalletType">> := <<"DigitalWalletQIWI">>} ->
            #domain_DigitalWallet{
                provider = qiwi,
                id = maps:get(<<"phoneNumber">>, Data)
            }
    end,
    {{digital_wallet, DigitalWallet}, <<>>}.

process_tokenized_card_data(Data, IdempotentKey, ReqCtx) ->
    CallResult = service_call(
        get_token_provider_service_name(Data),
        'Unwrap',
        [encode_wrapped_payment_tool(Data)],
        ReqCtx
    ),
    UnwrappedPaymentTool = case CallResult of
        {ok, Tool} ->
            Tool;
        {exception, #'InvalidRequest'{}} ->
            throw({ok, {400, #{}, logic_error(invalidRequest, <<"Tokenized card data is invalid">>)}})
    end,
    CardData = encode_tokenized_card_data(UnwrappedPaymentTool),
    SessionData = encode_tokenized_session_data(UnwrappedPaymentTool),
    BankInfo = get_bank_info(CardData#cds_PutCardData.pan, ReqCtx),
    PaymentSystem = capi_bankcard:payment_system(BankInfo),
    case capi_bankcard:validate(CardData, SessionData, PaymentSystem) of
        ok ->
            process_put_card_data_result(
                put_card_data_to_cds(
                    CardData,
                    SessionData,
                    IdempotentKey,
                    BankInfo,
                    ReqCtx
                ),
                UnwrappedPaymentTool
            );
        {error, Error} ->
            throw({ok, validation_error(Error)})
    end.

process_crypto_wallet_data(Data, _ReqCtx) ->
    #{<<"cryptoCurrency">> := CryptoCurrency} = Data,
    {{crypto_currency, convert_crypto_currency_from_swag(CryptoCurrency)}, <<>>}.

encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        auth_data = {auth_3ds, #paytoolprv_Auth3DS{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    }}
}) ->
    #cds_SessionData{
        auth_data = {auth_3ds, #cds_Auth3DS{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    };
encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{}}
}) ->
    #cds_SessionData{
        auth_data = {card_security_code, #cds_CardSecurityCode{
            %% TODO dirty hack for test GooglePay card data
            value = <<"">>
        }}
    }.

encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        dpan = DPAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #cds_PutCardData{
        pan  = DPAN,
        exp_date = #cds_ExpDate{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    };
encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{
        pan = PAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #cds_PutCardData{
        pan  = PAN,
        exp_date = #cds_ExpDate{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    }.

encode_wrapped_payment_tool(Data) ->
    #paytoolprv_WrappedPaymentTool{
        request = encode_payment_request(Data)
    }.

encode_payment_request(#{<<"provider" >> := <<"ApplePay">>} = Data) ->
    {apple, #paytoolprv_ApplePayRequest{
        merchant_id = maps:get(<<"merchantID">>, Data),
        payment_token = encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"GooglePay">>} = Data) ->
    {google, #paytoolprv_GooglePayRequest{
        gateway_merchant_id = maps:get(<<"gatewayMerchantID">>, Data),
        payment_token = encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"SamsungPay">>} = Data) ->
    {samsung, #paytoolprv_SamsungPayRequest{
        service_id = genlib_map:get(<<"serviceID">>, Data),
        reference_id = genlib_map:get(<<"referenceID">>, Data)
    }}.

get_token_provider_service_name(Data) ->
    case Data of
        #{<<"provider">> := <<"ApplePay">>} ->
            payment_tool_provider_apple_pay;
        #{<<"provider">> := <<"GooglePay">>} ->
            payment_tool_provider_google_pay;
        #{<<"provider">> := <<"SamsungPay">>} ->
            payment_tool_provider_samsung_pay
    end.

process_put_card_data_result(
    {{bank_card, BankCard}, SessionID},
    #paytoolprv_UnwrappedPaymentTool{
        card_info = #paytoolprv_CardInfo{
            payment_system = PaymentSystem,
            last_4_digits  = Last4
        },
        payment_data = PaymentData,
        details = PaymentDetails
    }
) ->
    {
        {bank_card, BankCard#domain_BankCard{
            payment_system = PaymentSystem,
            masked_pan     = genlib:define(Last4, BankCard#domain_BankCard.masked_pan),
            token_provider = get_payment_token_provider(PaymentDetails, PaymentData)
        }},
        SessionID
    }.

decode_disposable_payment_resource(PaymentResource, EncryptedToken) ->
    #domain_DisposablePaymentResource{
        payment_tool = PaymentTool,
        payment_session_id = PaymentSessionID,
        client_info = ClientInfo0
    } = PaymentResource,
    ClientInfo = decode_client_info(ClientInfo0),
    #{
        <<"paymentToolToken">> => EncryptedToken,
        <<"paymentSession">> => wrap_payment_session(ClientInfo, PaymentSessionID),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"clientInfo">> => ClientInfo
    }.

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

get_payment_token_provider(_PaymentDetails, {card, _}) ->
    % TODO
    % We deliberately hide the fact that we've got that payment tool from the likes of Google Chrome browser
    % in order to make our internal services think of it as if it was good ol' plain bank card. Without a
    % CVV though. A better solution would be to distinguish between a _token provider_ and an _origin_.
    undefined;

get_payment_token_provider({apple, _}, _PaymentData) ->
    applepay;
get_payment_token_provider({google, _}, _PaymentData) ->
    googlepay;
get_payment_token_provider({samsung, _}, _PaymentData) ->
    samsungpay.

wrap_payment_session(ClientInfo, PaymentSession) ->
    capi_utils:map_to_base64url(#{
        <<"clientInfo">> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    }).

-spec convert_crypto_currency_from_swag(binary()) -> atom().

convert_crypto_currency_from_swag(<<"bitcoinCash">>) ->
    bitcoin_cash;
convert_crypto_currency_from_swag(CryptoCurrency) when is_binary(CryptoCurrency) ->
    binary_to_existing_atom(CryptoCurrency, utf8).

-spec convert_crypto_currency_to_swag(atom()) -> binary().

convert_crypto_currency_to_swag(bitcoin_cash) ->
    <<"bitcoinCash">>;
convert_crypto_currency_to_swag(CryptoCurrency) when is_atom(CryptoCurrency) ->
    atom_to_binary(CryptoCurrency, utf8).

gen_random_id() ->
    Random = crypto:strong_rand_bytes(16),
    genlib_format:format_int_base(binary:decode_unsigned(Random), 62).

get_bank_info(CardDataPan, Context) ->
    case capi_bankcard:lookup_bank_info(CardDataPan, Context) of
        {ok, BankInfo} ->
            BankInfo;
        {error, _Reason} ->
            throw({ok, logic_error(invalidRequest, <<"Unsupported card">>)})
    end.

-spec validation_error
    (capi_bankcard:reason()) -> swag_server:response().

validation_error(unrecognized) ->
    Data = #{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => <<"Unrecognized bank card issuer">>},
    create_error_resp(400, Data);
validation_error({invalid, K, C}) ->
    Data = #{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => validation_msg(C, K)},
    create_error_resp(400, Data).

validation_msg(expiration, _Key) ->
    <<"Invalid expiration date">>;
validation_msg(luhn, Key) ->
    <<"Invalid ", (key_to_binary(Key))/binary, " checksum">>;
validation_msg({length, _}, Key) ->
    <<"Invalid ", (key_to_binary(Key))/binary, " length">>.

key_to_binary(cardnumber) ->
    <<"cardNumber">>;
key_to_binary(exp_date) ->
    <<"expDate">>;
key_to_binary(cvv) ->
    <<"cvv">>.

create_error_resp(Code, Data) ->
    create_error_resp(Code, #{}, Data).
create_error_resp(Code, Headers, Data) ->
    {Code, Headers, Data}.
