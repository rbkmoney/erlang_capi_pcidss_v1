-define(STRING, <<"TEST">>).
-define(INTEGER, 10000).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).

-define(BANK_CARD, #cds_BankCard{
    token = ?STRING,
    bin = <<"532130">>,
    last_digits = <<"7892">>
}).

-define(BANK_NAME, <<"SAVINGS BANK OF THE GLORIOUS RUSSIAN FEDERATION">>).

-define(BINBASE_LOOKUP_RESULT, ?BINBASE_LOOKUP_RESULT(<<"MASTERCARD">>)).
-define(BINBASE_LOOKUP_RESULT(PaymentSystem), #'binbase_ResponseData'{
    bin_data = #'binbase_BinData'{
        payment_system = PaymentSystem,
        bank_name = ?BANK_NAME,
        iso_country_code = <<"KAZ">>,
        card_type = debit,
        bin_data_id = {i, 1}
    },
    version = ?INTEGER
}).

-define(UNWRAPPED_PAYMENT_TOOL(Details),
    ?UNWRAPPED_PAYMENT_TOOL(
        Details,
        {tokenized_card, #paytoolprv_TokenizedCard{
            dpan = <<"5321301234567892">>,
            exp_date = #paytoolprv_ExpDate{
                month = 10,
                year = 2028
            },
            auth_data =
                {auth_3ds, #paytoolprv_Auth3DS{
                    cryptogram = ?STRING,
                    eci = ?STRING
                }}
        }}
    )
).

-define(UNWRAPPED_PAYMENT_TOOL(Details, PaymentData), #paytoolprv_UnwrappedPaymentTool{
    payment_data = PaymentData,
    card_info = #paytoolprv_CardInfo{
        display_name = <<"Visa 1234">>,
        cardholder_name = ?STRING,
        last_4_digits = <<"1234">>,
        card_class = debit,
        payment_system = mastercard
    },
    details = Details
}).

-define(PUT_CARD_RESULT, #'cds_PutCardResult'{
    bank_card = ?BANK_CARD
}).

-define(MESSAGE_ID, <<
    "27FBD553651896F61FF58EBA63091A33FACDE10C662807FF4C1835A1EE89198917D4AB"
    "6A56A1F250983A8EA287E3E4CE65E4782006937142857475861835A10158FDE54B52B5"
>>).

-define(APPLE_PAY_DETAILS,
    {apple, #paytoolprv_ApplePayDetails{
        transaction_id = ?STRING,
        amount = ?INTEGER,
        currency_numeric_code = 643,
        device_id = ?STRING
    }}
).

-define(GOOGLE_PAY_DETAILS,
    {google, #paytoolprv_GooglePayDetails{
        message_id = ?MESSAGE_ID,
        message_expiration = ?TIMESTAMP
    }}
).

-define(YANDEX_PAY_DETAILS,
    {yandex, #paytoolprv_YandexPayDetails{
        message_id = ?MESSAGE_ID,
        message_expiration = ?TIMESTAMP
    }}
).

-define(TEST_USER_REALM, <<"external">>).
