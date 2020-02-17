-module(capi_bankcard).

-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-export([lookup_bank_info/2]).
-export([validate/3]).
-export([payment_system/1]).

-type bank_info() :: #{
    payment_system := dmsl_domain_thrift:'BankCardPaymentSystem'(),
    bank_name      := binary(),
    issuer_country := dmsl_domain_thrift:'Residence'() | undefined,
    metadata       := map()
}.

-type lookup_error() ::
    notfound |
    {invalid,
        payment_system |
        issuer_country
    }.

-type cardholder_data() :: cds_proto_storage_thrift:'PutCardData'().
-type session_data() :: cds_proto_storage_thrift:'SessionData'().
-type payment_system() :: dmsl_domain_thrift:'BankCardPaymentSystem'().
-type reason() :: unrecognized |{invalid, cardnumber | cvv | exp_date, check()}.

-export_type([cardholder_data/0]).
-export_type([session_data/0]).
-export_type([payment_system/0]).
-export_type([reason/0]).

-spec lookup_bank_info(_PAN :: binary(), woody_context:ctx()) ->
    {ok, bank_info()} | {error, lookup_error()}.

lookup_bank_info(PAN, WoodyCtx) ->
    RequestVersion = {'last', #binbase_Last{}},
    case capi_woody_client:call_service(binbase, 'Lookup', [PAN, RequestVersion], WoodyCtx) of
        {ok, BinData} ->
            decode_bank_info(BinData);
        {exception, #'binbase_BinNotFound'{}} ->
            {error, notfound}
    end.

decode_bank_info(#'binbase_ResponseData'{bin_data = BinData, version = Version}) ->
    try
        {ok, #{
            payment_system => decode_payment_system(BinData#binbase_BinData.payment_system),
            bank_name      => BinData#binbase_BinData.bank_name,
            issuer_country => decode_issuer_country(BinData#binbase_BinData.iso_country_code),
            metadata       => #{
                <<"version">> => Version
            }
        }}
    catch
        {invalid, What} ->
            {error, {invalid, What}}
    end.

-define(invalid(What), erlang:throw({invalid, What})).

%% Payment system mapping
%%
%% List of known payment systems as of https://github.com/rbkmoney/binbase-data/commit/dcfabb1e.
%% Please keep in sorted order.
-spec decode_payment_system(binary()) ->
    dmsl_domain_thrift:'BankCardPaymentSystem'().

decode_payment_system(<<"AMERICAN EXPRESS">>)           -> amex;
decode_payment_system(<<"AMERICAN EXPRESS COMPANY">>)   -> amex;
decode_payment_system(<<"ATM CARD">>)                   -> ?invalid(payment_system);
decode_payment_system(<<"ATOS PRIVATE LABEL">>)         -> ?invalid(payment_system);
decode_payment_system(<<"AURA">>)                       -> ?invalid(payment_system);
decode_payment_system(<<"BANKCARD(INACTIVE)">>)         -> ?invalid(payment_system);
decode_payment_system(<<"BP FUEL CARD">>)               -> ?invalid(payment_system);
decode_payment_system(<<"CABAL">>)                      -> ?invalid(payment_system);
decode_payment_system(<<"CARNET">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"CHINA UNION PAY">>)            -> unionpay;
decode_payment_system(<<"CHJONES FUEL CARD">>)          -> ?invalid(payment_system);
decode_payment_system(<<"CIRRUS">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"COMPROCARD">>)                 -> ?invalid(payment_system);
decode_payment_system(<<"DANKORT">>)                    -> dankort;
decode_payment_system(<<"DFS/DCI">>)                    -> ?invalid(payment_system);
decode_payment_system(<<"DINACARD">>)                   -> ?invalid(payment_system);
decode_payment_system(<<"DINERS CLUB INTERNATIONAL">>)  -> dinersclub;
decode_payment_system(<<"DISCOVER">>)                   -> discover;
decode_payment_system(<<"DUET">>)                       -> ?invalid(payment_system);
decode_payment_system(<<"EBT">>)                        -> ?invalid(payment_system);
decode_payment_system(<<"EFTPOS">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"ELO">>)                        -> ?invalid(payment_system);
decode_payment_system(<<"ELO/DISCOVER">>)               -> ?invalid(payment_system);
decode_payment_system(<<"EUROSHELL FUEL CARD">>)        -> ?invalid(payment_system);
decode_payment_system(<<"FUEL CARD">>)                  -> ?invalid(payment_system);
decode_payment_system(<<"GE CAPITAL">>)                 -> ?invalid(payment_system);
decode_payment_system(<<"GLOBAL BC">>)                  -> ?invalid(payment_system);
decode_payment_system(<<"HIPERCARD">>)                  -> ?invalid(payment_system);
decode_payment_system(<<"HRG STORE CARD">>)             -> ?invalid(payment_system);
decode_payment_system(<<"JCB">>)                        -> jcb;
decode_payment_system(<<"LOCAL BRAND">>)                -> ?invalid(payment_system);
decode_payment_system(<<"LOCAL CARD">>)                 -> ?invalid(payment_system);
decode_payment_system(<<"LOYALTY CARD">>)               -> ?invalid(payment_system);
decode_payment_system(<<"LUKOIL FUEL CARD">>)           -> ?invalid(payment_system);
decode_payment_system(<<"MAESTRO">>)                    -> maestro;
decode_payment_system(<<"MASTERCARD">>)                 -> mastercard;
decode_payment_system(<<"NEWDAY">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"NSPK MIR">>)                   -> nspkmir;
decode_payment_system(<<"OUROCARD">>)                   -> ?invalid(payment_system);
decode_payment_system(<<"PAYPAL">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"PHH FUEL CARD">>)              -> ?invalid(payment_system);
decode_payment_system(<<"PRIVATE LABEL">>)              -> ?invalid(payment_system);
decode_payment_system(<<"PRIVATE LABEL CARD">>)         -> ?invalid(payment_system);
decode_payment_system(<<"PROSTIR">>)                    -> ?invalid(payment_system);
decode_payment_system(<<"RBS GIFT CARD">>)              -> ?invalid(payment_system);
decode_payment_system(<<"RED FUEL CARD">>)              -> ?invalid(payment_system);
decode_payment_system(<<"RED LIQUID FUEL CARD">>)       -> ?invalid(payment_system);
decode_payment_system(<<"RUPAY">>)                      -> ?invalid(payment_system);
decode_payment_system(<<"SBERCARD">>)                   -> ?invalid(payment_system);
decode_payment_system(<<"SODEXO">>)                     -> ?invalid(payment_system);
decode_payment_system(<<"STAR REWARDS">>)               -> ?invalid(payment_system);
decode_payment_system(<<"TROY">>)                       -> ?invalid(payment_system);
decode_payment_system(<<"UATP">>)                       -> ?invalid(payment_system);
decode_payment_system(<<"UK FUEL CARD">>)               -> ?invalid(payment_system);
decode_payment_system(<<"UNIONPAY">>)                   -> unionpay;
decode_payment_system(<<"VISA">>)                       -> visa;
decode_payment_system(<<"VISA/DANKORT">>)               -> visa; % supposedly 🤔
decode_payment_system(<<"VPAY">>)                       -> ?invalid(payment_system);
decode_payment_system(PaymentSystem) ->
    _ = logger:warning("unknown payment system encountered: ~s", [PaymentSystem]),
    ?invalid(payment_system).

%% Residence mapping
%%
-spec decode_issuer_country(binary() | undefined) ->
    dmsl_domain_thrift:'Residence'() | undefined.

decode_issuer_country(Residence) when is_binary(Residence) ->
    try
        {enum, Variants} = dmsl_domain_thrift:enum_info('Residence'),
        Variant = erlang:list_to_existing_atom(string:to_lower(erlang:binary_to_list(Residence))),
        element(1, lists:keyfind(Variant, 1, Variants))
    catch
        error:badarg ->
            _ = logger:warning("unknown residence encountered: ~s", [Residence]),
            ?invalid(issuer_country)
    end;
decode_issuer_country(undefined) ->
    undefined.

-spec payment_system(bank_info()) ->
    payment_system().

payment_system(BankInfo) ->
    maps:get(payment_system, BankInfo).

-spec validate(cardholder_data(), session_data() | undefined, payment_system()) ->
    ok | {error, reason()}.

validate(CardData, SessionData, PaymentSystem) ->
    Rulesets = get_payment_system_assertions(),
    Assertions = maps:get(PaymentSystem, Rulesets, []),
    validate_card_data(merge_data(CardData, SessionData), Assertions).

merge_data(CardData, undefined) ->
    convert_card_data(CardData);
merge_data(CardData, #cds_SessionData{auth_data = AuthData}) ->
    CVV = get_cvv_from_session_data(AuthData),
    CardDataMap = convert_card_data(CardData),
    CardDataMap#{cvv => maybe_undefined(CVV)}.

get_cvv_from_session_data({card_security_code, AuthData}) ->
    AuthData#cds_CardSecurityCode.value;
get_cvv_from_session_data(_) ->
    undefined.

%%

validate_card_data(CardData, Assertions) ->
    try run_assertions(CardData, Assertions) catch
        Reason ->
            {error, Reason}
    end.

run_assertions(CardData, Assertions) ->
    Env = #{now => calendar:universal_time()},
    genlib_map:foreach(
        fun(K, Checks) ->
            V = maps:get(K, CardData, undefined),
            lists:foreach(
                fun(C) -> check_value(V, C, Env) orelse throw({invalid, K, C}) end,
                Checks
            )
        end,
        Assertions
    ).

check_value(undefined, _, _) ->
    true;
check_value(V, {length, Ls}, _) ->
    lists:any(fun(L) -> check_length(V, L) end, Ls);
check_value(V, luhn, _) ->
    check_luhn(V, 0);
check_value({M, Y}, expiration, #{now := {{Y0, M0, _DD}, _Time}}) ->
    M >= 1 andalso
        M =< 12 andalso
        {Y, M} >= {Y0, M0}.

check_length(V, {range, L, U}) ->
    L =< byte_size(V) andalso byte_size(V) =< U;
check_length(V, L) ->
    byte_size(V) =:= L.

check_luhn(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
check_luhn(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            check_luhn(Rest, Sum + M div 10 + M rem 10);
        M ->
            check_luhn(Rest, Sum + M)
    end;
check_luhn(<<N, Rest/binary>>, Sum) ->
    check_luhn(Rest, Sum + N - $0).

% config

-type check() ::
    {length, [pos_integer() | {range, pos_integer(), pos_integer()}]} |
    luhn |
    expiration.

get_payment_system_assertions() ->
    #{

        visa => #{
            cardnumber => [{length, [13, 16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        mastercard => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        %% Maestro Global Rules
        %% https://www.mastercard.com/hr/merchants/_assets/Maestro_rules.pdf
        %%
        %% 6.2.1.3 Primary Account Number (PAN)
        %%
        %% The PAN must be no less than twelve (12) and no more than nineteen (19)
        %% digits in length. All digits of the PAN must be numeric. It is strongly
        %% recommended that Members issue Cards with a PAN of nineteen (19) digits.
        %%
        %% The IIN appears in the first six (6) digits of the PAN and must be assigned
        %% by the ISO Registration Authority, and must be unique.
        maestro => #{
            cardnumber => [{length, [{range, 12, 19}]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        nspkmir => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        amex => #{
            cardnumber => [{length, [15]}, luhn],
            cvv => [{length, [3, 4]}],
            exp_date => [expiration]
        },

        dinersclub => #{
            cardnumber => [{length, [{range, 14, 19}]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        discover => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        unionpay => #{
            cardnumber => [{length, [{range, 16, 19}]}],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        jcb => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        forbrugsforeningen => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        },

        dankort => #{
            cardnumber => [{length, [16]}, luhn],
            cvv => [{length, [3]}],
            exp_date => [expiration]
        }

    }.

convert_card_data(CardData) ->
    #cds_PutCardData {
        pan = PAN,
        cardholder_name = Cardholder,
        exp_date = #cds_ExpDate{
            month = Month,
            year = Year
        }
    } = CardData,
    #{
        cardnumber => PAN,
        cardholder => Cardholder,
        exp_date => {Month, Year}
    }.

maybe_undefined(<<>>) ->
    undefined;
maybe_undefined(CVV) ->
    CVV.
