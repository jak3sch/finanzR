ledgers <- read.csv("ledgers.csv")

finanzR::kraken_ledgers_to_pp("ledgers.csv", base_currency = "eur")

base_currency <- "eur"

crypto_list <- crypto2::crypto_list(only_active = FALSE)
fiat_list <- crypto2::fiat_list(include_metals = FALSE)


# staking
staking <- finanzR::add_coin_price(input = transactions, coins = all_coins, base_currency = base_currency) %>%
  # handle multiple entries per transaction
  dplyr::group_by(refid) %>%
  dplyr::arrange(type) %>% # create same oder in each group
  dplyr::mutate(
    price = dplyr::case_when(
      # combine separate transfer/withdrawal entries
      type == "transfer" & subtype == "spottostaking" & transfer == FALSE ~ dplyr::# combine from to staking BVBYQBB-ZWZCD2-2NWII4
    ),
    fee = dplyr::case_when(
      type == "spend" & currency == FALSE ~ dplyr::lag(fee) # get fee from coin selling  transaction
    )
  ) %>%

  # handle staking deposits
  dplyr::mutate(
    note = ifelse(stringr::str_detect(asset, ".S"), paste("Staking:", note), note)
  )

# cleanup

