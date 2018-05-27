%%%-------------------------------------------------------------------
%%% @author Ludwik Ciechański
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. maj 2018 13:45
%%%-------------------------------------------------------------------
-module(test).
-author("Ludwik Ciechański").

%% API
-export([]).
-include("test_lib.hrl").

%% user interface
-export([run/0,run2/0,n_last_rates_of_currency/2,when_currency_was_the_most_valuable_in_30_days/1]).

n_last_rates_of_currency(N,Currency) ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/"++ Currency ++ "/last/"++integer_to_list(N)++"/?format=xml"),

    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',
    lists:foreach(fun print_currency_info/1, Currency_measurements).

when_currency_was_the_most_valuable_in_30_days(Currency) ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/"++ Currency ++ "/last/30/?format=xml"),

    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',
    % Date = lists:foreach(fun print_currency_info/1, Currency_measurements).
    [#currency_value{date = Data, value = Cena}|_] = Currency_measurements,
    Date = when_most_valuable(Currency_measurements,{Data,Cena}). 

run2() ->
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request("http://api.nbp.pl/api/exchangerates/rates/a/gbp/last/10/?format=xml"),

    {ok, Model} = erlsom:compile_xsd_file("ExchangeRatesSeries.xsd"),
    {ok, Result, _} = erlsom:scan(Body, Model),
    Tmp = Result#'ExchangeRatesSeries'.'ExchangeRatesSeries/Rates',
    Currency_name = Result#'ExchangeRatesSeries'.'currency_name',
    Currency_code = Result#'ExchangeRatesSeries'.'currency_code',
    Currency_measurements = Tmp#'ExchangeRatesSeries/Rates'.'currency_value',
    lists:foreach(fun print_currency_info/1, Currency_measurements).
    
when_most_valuable([],{Max_date,Max_value}) -> 
    {Max_date,Max_value};
when_most_valuable(Values,{Max_date,Max_value}) ->
  [#currency_value{date = Data, value = Cena}|T] = Values,
  if
    Cena > Max_value ->
        when_most_valuable(T,{Data,Cena});
    true ->
        when_most_valuable(T,{Max_date,Max_value})
  end.

run() ->
    %% create HTTP client
    inets:start(),
      
      %% request XML data from server
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request("http://api.nbp.pl/api/cenyzlota/last/30/?format=xml"),
      
    %%file:write_file("src/golden_erlsom/dane.xml", Body).
    
    %% compile xsd
    {ok, Model} = erlsom:compile_xsd_file("ArrayOfCenaZlota.xsd"),
    
    %% parse XML
    %% {ok, #gold_store{gold=Golds}, _} = erlsom:scan(Body, Model).
    {ok, Result, _} = erlsom:scan(Body, Model),

    GoldStores = Result#'ArrayOfCenaZlota'.'gold_store',
      
    %% test
    lists:foreach(fun print_gold_info/1, GoldStores).


print_currency_info(#currency_value{date = Data, value = Cena}) ->
    io:format("Date    : ~s~n
                Value   : ~s~n~n", [formatDate(Data), Cena]).


print_gold_info(#gold_type{date = Data, price = Cena}) ->
  io:format("Date    : ~s~n
Price   : ~s~n~n", [formatDate(Data), Cena]).

formatDate(undefined) -> "<unknown>";
formatDate(Date) -> Date.
%%%-------------------------------------------------------------------