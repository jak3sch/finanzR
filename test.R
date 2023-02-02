finanzR::pp_create_import("kraken_staking", "ledgers.csv")

kraken_data <- data.frame(
  time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49", 
           "2022-04-18 09:07:51", "2022-04-19 02:46:48", "2022-04-25 09:11:56", 
           "2022-04-26 02:48:12"), 
  type = "staking",
  asset = c("TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S"),
  amount = c(0.4, 0.55, 10.76, 0.55, 10.77, 0.55, 10.79)
)

finanzR::kraken_staking(input = kraken_data, input_type = "data.frame")

# admin
devtools::install()
devtools::document()
devtools::load_all()
devtools::build_site()
devtools::build_readme()
pkgdown::build_site_github_pages()
pkgdown::deploy_to_branch()

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.
