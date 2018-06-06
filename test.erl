%%%-------------------------------------------------------------------
%%% @author Ludwik Ciechański
%%% @author Bartek Pietrzyk
%%% Created : 20. maj 2018 13:45
%%%-------------------------------------------------------------------
-module(test).
-author("Ludwik Ciechański").
-author("Bartek Pietrzyk").

%% API
-export([]).
-include("test_lib.hrl").

%% user interface
-export([gold_last_30/0, gbp_last_10/0]).
-export([n_last_rates_of_currency/2]).
-export([when_currency_was_the_most_valuable_in_30_days/1]).

% tests
-export([test/0, test_of_detect_encoding/0, test_of_parse_sax/0]).
-export([test_of_scan_file/0, test_of_simple_form/0, test_of_Unicode/0]).
-export([test_of_write/0, test_of_write_xsd_hrl_file/0]).

%%%-------------------------------------------------------------------
% utility functions

request_xml(Path) ->
    %% create HTTP client
    inets:start(),
    %% request XML data from server
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(Path),
    Body.

print_currency_info(#currency_value{date = Data, value = Cena}) ->
    io:format("Date    : ~s~nValue   : ~s~n~n", [formatDate(Data), Cena]).

print_gold_info(#gold_type{date = Data, price = Cena}) ->
    io:format("Date    : ~s~nPrice   : ~s~n~n", [formatDate(Data), Cena]).

formatDate(undefined) -> "<unknown>";
formatDate(Date) -> Date.

when_most_valuable([],{Max_date, Max_value}) -> {Max_date, Max_value};
when_most_valuable(Values, {Max_date, Max_value}) ->
    [#currency_value{date = Data, value = Cena} | T] = Values,
    if
        Cena > Max_value -> when_most_valuable(T, {Data, Cena});
        true             -> when_most_valuable(T, {Max_date, Max_value})
    end.

%%%-------------------------------------------------------------------
% user interface implementation

gold_last_30() ->
    Path = "http://api.nbp.pl/api/cenyzlota/last/30/?format=xml",
    Body = request_xml(Path),

    %% compile XSD
    {ok, Model} = erlsom:compile_xsd_file("ArrayOfCenaZlota.xsd"),

    %% parse XML
    {ok, Result, _} = erlsom:scan(Body, Model),
    GoldStores = Result#'ArrayOfCenaZlota'.'gold_store',

    %% print info for each record
    lists:foreach(fun print_gold_info/1, GoldStores).

gbp_last_10() ->
    Path = "http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/10/?format=xml",
    Body = request_xml(Path),

    %% compile XSD
    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),

    %% parse XML
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',

    %% print info for each record
    lists:foreach(fun print_currency_info/1, Currency_measurements).

n_last_rates_of_currency(N, Currency) ->
    Path = "http://api.nbp.pl/api/exchangerates/rates/a/"
        ++ Currency ++ "/last/" ++ integer_to_list(N) ++ "/?format=xml",
    Body = request_xml(Path),

    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',
    lists:foreach(fun print_currency_info/1, Currency_measurements).

when_currency_was_the_most_valuable_in_30_days(Currency) ->
    Path = "http://api.nbp.pl/api/exchangerates/rates/a/"
        ++ Currency ++ "/last/30/?format=xml",
    Body = request_xml(Path),

    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',
    % Date = lists:foreach(fun print_currency_info/1, Currency_measurements).
    [#currency_value{date = Data, value = Cena} | _] = Currency_measurements,
    Date = when_most_valuable(Currency_measurements, {Data, Cena}).
%%%-------------------------------------------------------------------

% examples of functions
test_of_scan_file() ->
    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Out, Rest} = erlsom:scan_file("a_gbp_last_10.xml", Model).

test_of_write() ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
        httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/10/?format=xml"),
    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    {ok,Xml} = erlsom:write(Result,Model,[]).

test_of_write_xsd_hrl_file() ->
    erlsom:write_xsd_hrl_file("ExchangeRatesSeries.xsd","file_with_records.txt",[]).

% potwierdzenie czy na pewno jest zestawienie z 10 dni wartosci waluty
test_of_parse_sax() ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
    httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/10/?format=xml"),
    erlsom:parse_sax(Body, 0, fun(Event, Acc) -> case Event of {startElement,_,"Rate",_,_} ->  Acc+1; _ -> Acc end end).


test_of_simple_form() ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
    httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/5/?format=xml"),
    erlsom:simple_form(Body).


% jak to przetestowac?
test_of_Unicode()->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
    httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/5/?format=xml"),
    erlsom_lib:toUnicode(Body).


test_of_detect_encoding()->
    erlsom_lib:detect_encoding("ExchangeRatesSeries.xsd").
    % erlsom_lib:find_xsd("ExchangeRatesSeries.xsd", "/", 'undefined', 'undefined').    

% ??
test() ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
    httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/5/?format=xml"),
    erlsom_ucs:from_utf16le(Body).

