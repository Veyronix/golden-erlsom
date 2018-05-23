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
-export([run/0]).

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

print_gold_info(#gold_type{date = Data, price = Cena}) ->
  io:format("Date    : ~s~n
             Price   : ~s~n~n", [formatDate(Data), Cena]).

formatDate(undefined) -> "<unknown>";
formatDate(Date) -> Date.
%%%-------------------------------------------------------------------