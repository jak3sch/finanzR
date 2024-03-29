% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kraken_ledgers_prepare.R
\name{kraken_ledgers_prepare}
\alias{kraken_ledgers_prepare}
\title{Basic data manipulation for ledgers.csv from kraken}
\usage{
kraken_ledgers_prepare(input)
}
\arguments{
\item{input}{A \code{string} with the path to the exported Kraken ledgers data or a \code{data.frame}. If you are not passing an exported \code{ledgers.csv} from Kraken, the data needs the columns:
\itemize{
\item \code{refid}: a unique id for each transaction
\item \code{type}: the type of the transaction (e.g. deposit, withdrawal, trade...)
\item \code{time}: date and time in the format YYYY-MM-DD HH:MM:SS
}}
}
\value{
Returns the input data with additional columns:
\itemize{
\item \code{date} (date): date of the staking transaction;
\item \code{time} (character): time of the staking transaction;
\item \code{symbol} (character): the symbol of the used asset
\item \code{currency} (character): currency transactions getting a new column with the used currency
\item \code{deposit} (true/false): transaction indicator
\item \code{dividend} (true/false): transaction indicator
\item \code{receive} (true/false): transaction indicator
\item \code{spend} (true/false): transaction indicator
\item \code{staking} (true/false): transaction indicator
\item \code{trade} (true/false): transaction indicator
\item \code{transfer} (true/false): transaction indicator
\item \code{withdrawal} (true/false): transaction indicator
}
}
\description{
Runs basic data manipulation and normalisation to an exported ledgers.csv from kraken.com.
}
\examples{
r <- kraken_ledgers_prepare()
head(r, 10)
}
