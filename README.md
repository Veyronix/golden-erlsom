# erlang_projekt
Repository to collaborate on project for laboratory _Programming in Erlang_

Main aim - show usage of **erlsom** on simple use cases

Actual functionality:
```erlang
% user interface
gold_last_30()
gbp_last_10()
n_last_rates_of_currency(N, Currency)
when_currency_was_the_most_valuable_in_30_days(Currency)
when_currency_was_the_most_valuable_in_n_days(N, Currency)

% test functions
test_of_scan_file() 
test_of_write()
test_of_write_xsd_hrl_file()
test_of_parse_sax()
test_of_simple_form()
test_of_toUnicode()
test_of_detect_encoding()
test_of_from_utf16le()
```

List of functions in **erlsom**

| Lp | Function                   | Done               |
|---:|:---------------------------| ------------------:|
| 1  | compile_xsd/1              |  |
| 2  | compile_xsd/2              |  |
| 3  | compile_xsd_file/1         | :heavy_check_mark: |
| 4  | compile_xsd_file/2         |  |
| 5  | scan/2                     | :heavy_check_mark: |
| 6  | scan/3                     |  |
| 7  | scan_file/2                | :heavy_check_mark: |
| 8  | write/2                    | :heavy_check_mark: |
| 9  | write/3                    |  |
| 10 | write_xsd_hrl_file/3       | :heavy_check_mark: |
| 11 | parse_sax/4                | :heavy_check_mark: |
| 12 | simple_form/1              | :heavy_check_mark: |
| 13 | simple_form/2              |  ??
| 14 | erlsom_lib:toUnicode/1     |  ??
| 15 | erlsom_lib:find_xsd/4      |  |
| 16 | erlsom_lib:detect_encoding/1 | :heavy_check_mark: |
| 17 | erlsom_ucs:from_utf8/1       | ??
| 18 | erlsom_ucs:from_utf16le      | ??
| 19 | erlsom_ucs:from_utf16be      | ??


