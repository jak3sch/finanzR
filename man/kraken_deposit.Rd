% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kraken_deposit.R
\name{kraken_deposit}
\alias{kraken_deposit}
\title{fdgfdgf}
\usage{
kraken_deposit(input, base_currency = "eur", input_type = "file", lang = "de")
}
\arguments{
\item{input}{A \code{string} with the path to the exported Kraken ledgers data or a \code{data.frame} with the same strucutre as the kraken ledgers export.}

\item{base_currency}{A \code{string} with the prefered currency in which the coin value will be returned. One of \code{c("eur", "usd")}. If the result will be imported in Portfolio Performance, this should be the same as the base currency set there.}

\item{input_type}{A \code{string} which defines what kind of input you are passing. If you are not passing a exported \code{ledgers.csv} from Kraken the data needs the columns \code{time}, \code{type}, \code{asset} and \code{amount.}}
}
\value{
A tibble with the following columns:
\itemize{
\item \code{date} (date): date of the staking transaction;
\item \code{time} (character) time of the staking transaction;
\item \code{type} (character): staking;
\item \code{asset} (character): Kraken coin abbreviation;
\item \code{amount} (double): amount of staked coins;
\item \code{fee} (double): transaction fee;
\item \code{price} (double): coin price in \code{base_currency}, as of \code{date};
\item \code{currency} (character): currency of coin value;
}
}
\description{
Adds coin values from CoinGecko API to Kraken staking transactions
}
\examples{
kraken_data <- data.frame(
  time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
         "2022-04-18 09:07:51", "2022-04-19 02:46:48", "2022-04-25 09:11:56",
         "2022-04-26 02:48:12"),
  type = "staking",
  asset = c("TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S"),
  amount = c(0.4, 0.55, 10.76, 0.55, 10.77, 0.55, 10.79)
)

r <- kraken_deposit(kraken_data, input_type = "data.frame")
head(r, 10)
}
