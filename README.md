
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finanzR

<!-- badges: start -->
<!-- badges: end -->

finanzR was developed as a personal library of scripts to facilitate
financial tracking in [Portfolio
Performance](https://www.portfolio-performance.info/).

Since these may also help others, I have decided to publish the library.

At the moment the package is in an early beta and testing phase, but
will be extended in the future.

If you have any suggestions or find any bugs, feel free to create a new
issue. Since this is my first R package, it is possible that there will
be changes in the architecture during the development.

## Installation

You can install the development version of finanzR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jak3sch/finanzR")
```

## Usage

For more detailed information, please refer to the
[documentation](https://jak3sch.github.io/finanzR/).

### Crypto

`{finanzR}` tries to solve the problem that staking cannot really be
mapped in Portfolio Performance. Currently the function only supports
staking at [Kraken](https://www.kraken.com/).

``` r
library(finanzR)

# kraken ledgers export
kraken_data <- data.frame(
  time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49", 
           "2022-04-18 09:07:51", "2022-04-19 02:46:48", "2022-04-25 09:11:56", 
           "2022-04-26 02:48:12"), 
  type = "staking",
  asset = c("TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S"),
  amount = c(0.4, 0.55, 10.76, 0.55, 10.77, 0.55, 10.79)
)

staking_data <- finanzR::kraken_staking(input = kraken_data, input_type = "data.frame")

print(staking_data)
#> # A tibble: 14 × 6
#>    date       time     type      symbol  amount  price
#>    <date>     <chr>    <chr>     <chr>    <dbl>  <dbl>
#>  1 2022-04-05 03:24:26 Dividende TRX/EUR   0.4  0.0645
#>  2 2022-04-11 17:51:44 Dividende ADA/EUR   0.55 0.954 
#>  3 2022-04-12 02:43:49 Dividende TRX/EUR  10.8  0.0535
#>  4 2022-04-18 09:07:51 Dividende ADA/EUR   0.55 0.856 
#>  5 2022-04-19 02:46:48 Dividende TRX/EUR  10.8  0.0571
#>  6 2022-04-25 09:11:56 Dividende ADA/EUR   0.55 0.820 
#>  7 2022-04-26 02:48:12 Dividende TRX/EUR  10.8  0.0610
#>  8 2022-04-05 03:24:26 Kauf      TRX/EUR   0.4  0.0645
#>  9 2022-04-11 17:51:44 Kauf      ADA/EUR   0.55 0.954 
#> 10 2022-04-12 02:43:49 Kauf      TRX/EUR  10.8  0.0535
#> 11 2022-04-18 09:07:51 Kauf      ADA/EUR   0.55 0.856 
#> 12 2022-04-19 02:46:48 Kauf      TRX/EUR  10.8  0.0571
#> 13 2022-04-25 09:11:56 Kauf      ADA/EUR   0.55 0.820 
#> 14 2022-04-26 02:48:12 Kauf      TRX/EUR  10.8  0.0610
```

The function takes the exported ledger data from Kraken. It searches the
CoinGecko api for the price of the coin on the day of the staking
transaction.

It then creates 2 transactions each for Portfolio Performance: a
dividend and a purchase.

Since we only have the day’s price, it’s not 100% accurate, but it’s
better than nothing.
