%%%-------------------------------------------------------------------
% records for http://api.nbp.pl/api/cenyzlota/[...]
-record('ArrayOfCenaZlota',{anyAttribs,'gold_store'}).
-record('gold_store', {anyAttribs, 'gold_type'}).
-record('gold_type', {anyAttribs, 'date', 'price'}).

% records for http://api.nbp.pl/api/exchangerates/rates/a/[...]
-record('ExchangeRatesSeries',{anyAttribs,'table','currency_name','currency_code','ExchangeRatesSeries/Rates'}).
-record('ExchangeRatesSeries/Rates',{anyAttribs,'currency_value'}).
-record('currency_value',{anyAttribs,'something','date','value'}).
%%%-------------------------------------------------------------------