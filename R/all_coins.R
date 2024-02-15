#' Load all supported Coins
#'
#' @description Load all supported crypto coins. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.
#'
#' @return A tibble with three columns:
#' * `coin_id` (character): coin IDs, ordered alphabetically;
#' * `symbol` (character): coin symbols;
#' * `name` (character): common names of the coins;
#'
#' @export
#'
#' @examples
#' r <- all_coins()
#' head(r, 10)
all_coins <- function() {
    kraken_cash_to_crypto_coin_names <-c('0x','1inch','Aave','Aavegotchi','Acala','Adventure Gold','Akash','Alchemix','Alchemy Pay','Algorand','Alien Worlds','Altair','Ambire AdEx','Ampleforth Governance Token','Ankr','ApeCoin','API3','Aptos','Aragon','Arbitrum','Arpa Chain','Astar','Audius','Augur','Augur v2','Avalanche','Axie Infinity','Badger DAO','Balancer','Bancor','Band Protocol','Barnbridge','Basic Attention Token','Basilisk','Biconomy','Bifrost','Bitcoin','Bitcoin Cash','BitDAO','Bittorrent','Blur','Bluzelle','Boba Network','Bonfida','Bricks','Cardano','Cartesi','Celer Network','Centrifuge','Chain','Chainlink','Chiliz','Chromia','Civic','Compound','Coin98','Convex Finance','Cosmos','COTI','Covalent','Crust Shadow','Curve','Dai*','Dash','Decentraland','Dent','Dogecoin','dYdX','Energy Web Token','Enjin Coin','Enzyme Finance','EOS','EthereumPoW**','Ethereum ("Ether")','Ethereum Classic','Ethereum Name Service','Euler','Fantom','Fetch.ai','Filecoin','Flare','Flow','Frax Share','Gala Games','Galxe','Gari Network','GensoKishi Metaverse','Gitcoin','GMX','Gnosis','Green Satoshi Token','Harvest Finance','Hashflow','HydraDX','ICON','IDEX','iExec','Immutable X','Injective Protocol','Integritee','Interlay','Internet Computer','Jasmy','JUNO','Karura','Kava','KeeperDAO','Keep Network','Keep3r Network','KILT','Kin','Kintsugi','Kusama','Kyber Network','Lido DAO','Liechtenstein Cryptoassets Exchange','LimeWire Token','Lisk','Litecoin','Livepeer','Loopring','Mango','Merit Circle','MXC','My Neighbor Alice','Maker','Marinade SOL','Marlin','Mask Network','Mina','Mirror Protocol','Monero','Moonbeam','Moons','Moonriver','Multichain','MultiversX','Nano','Near Protocol','Nodle','Numeraire','Nym','Ocean','OMG Network','Orca','Orchid','Origin Protocol','Oxygen','Parallel Finance','PAX Gold','Pepe','Perpetual Protocol','Phala','PlayDapp','Polkadot','Polkastarter','Polygon','Powerledger','pSTAKE','Qtum','Quant','Rarible','Raydium','REN Protocol','Render','Request','Ripple','Robonomics','Rocket Pool','Rubic','Saber','Samoyed Coin','Secret','SelfKey','Serum','Shiba Inu','Shiden','Siacoin','Solana','Songbird','Spell Token','Stacks','Star Atlas','Star Atlas DAO','Stargate Finance','Stella','Stellar Lumens','Step Finance','STEPN','Storj','Sui','Sushi','Radicle','Stafi Protocol','SuperFarm','SuperRare','Synapse','Synthetix','tBTC','Terra 2.0','Terra Classic','Tether EURt','TerraUSD Classic','Terra Virtua Kolect','Tether*','Tezos','The Graph','The Sandbox','Thorchain','Threshold','Tokemak','Tron','TrueFi','TrueUSD','Unifi Protocol DAO','Uniswap','Universal Market Access','USD Coin*','WAVES','Woo Network','Wrapped Bitcoin','Wrapped Axelar','Yearn Finance','Yield Guild Games','Zcash')

    kraken_cash_to_crypto_coin_symbols <- c('ZRX','1INCH','AAVE','GHST','ACA','AGLD','AKT','ALCX','ACH','ALGO','TLM','AIR','ADX','FORTH','ANKR','APE','API3','APT','ANT','ARB','ARPA','ASTR','AUDIO','REP','REPV2','AVAX','AXS','BADGER','BAL','BNT','BAND','BOND','BAT','BSX','BICO','BNC','BTC','BCH','BIT','BTT','BLUR','BLZ','BOBA','FIDA','BRICK','ADA','CTSI','CELR','CFG','XCN','LINK','CHZ','CHR','CVC','COMP','C98','CVX','ATOM','COTI','CQT','CSM','CRV','DAI','DASH','MANA','DENT','DOGE','DYDX','EWT','ENJ','MLN','EOS','ETHW','ETH','ETC','ENS','EUL','FTM','FET','FIL','FLR','FLOW','FXS','GALA','GAL','GARI','MV','GTC','GMX','GNO','GST','FARM','HFT','HDX','ICX','IDEX','RLC','IMX','INJ','TEER','INTR','ICP','JASMY','JUNO','KAR','KAVA','KEEP','KP3R','ROOK','KILT','KIN','KINT','KSM','KNC','LDO','LCX','LMWR','LSK','LTC','LPT','LRC','MNGO','MC','MXC','ALICE','MKR','MSOL','POND','MASK','MINA','MIR','XMR','GLMR','MOON','MOVR','MULTI','EGLD','NANO','NEAR','NODL','NMR','NYM','OCEAN','OMG','ORCA','OXT','OGN','OXY','PARA','PAXG','PEPE','PERP','PHA','PLA','DOT','POLS','MATIC','POWR','PSTAKE','QTUM','QNT','RARI','RAY','REN','RNDR','REQ','XRP','XRT','RPL','RBC','SBR','SAMO','SCRT','KEY','SRM','SHIB','SDN','SC','SOL','SGB','SPELL','STX','ATLAS','POLIS','STG','ALPHA','XLM','STEP','GMT','STORJ','SUI','SUSHI','RAD','FIS','SUPER','RARE','SYN','SNX','TBTC','LUNA2','LUNA','EURT','UST','TVK','USDT','XTZ','GRT','SAND','RUNE','T','TOKE','TRX','TRU','TUSD','UNFI','UNI','UMA','USDC','WAVES','WOO','WBTC','WAXL','YFI','YGG','ZEC')

    kraken_cash_to_crypto_coin_currencies <-c('USD', 'EUR', 'CAD', 'JPY', 'GBP', 'CHF', 'AUD')

    kraken_cash_to_crypto_pairs <- tibble::tibble(kraken_cash_to_crypto_coin_names, kraken_cash_to_crypto_coin_symbols, .name_repair = ~ c("name", "symbol")) %>%
        dplyr::mutate(
            symbol = tolower(symbol),
            name = stringr::str_replace_all(name, "\\(.*?\\)|\\*\\*?", ""), # remove everything inside (), all * and **
            # manual fix for joining with geckor
            name = dplyr::case_when(
                symbol == "adx" ~ "AdEx",
                symbol == "forth" ~ "Ampleforth Governance",
                symbol == "ankr" ~ "Ankr Network",
                symbol == "arpa" ~ "ARPA",
                #symbol == "repv2" ~ "Akash Network",
                symbol == "badger" ~ "Badger",
                symbol == "bnt" ~ "Bancor Network",
                symbol == "akt" ~ "Akash Network",
                symbol == "bond" ~ "BarnBridge",
                symbol == "bat" ~ "Basic Attention",
                symbol == "btt" ~ "BitTorrent",
                symbol == "brick" ~ "Brick",
                symbol == "xcn" ~ "Onyxcoin",
                symbol == "atom" ~ "Cosmos Hub",
                symbol == "crv" ~ "Curve DAO",
                symbol == "zrx" ~ "0x Protocol",
                symbol == "ewt" ~ "Energy Web",
                symbol == "mln" ~ "Enzyme",
                symbol == "eth" ~ "Ethereum",
                symbol == "gala" ~ "GALA",
                symbol == "gst" ~ "GSTCOIN",
                symbol == "rlc" ~ "iExec RLC",
                symbol == "imx" ~ "Immutable",
                symbol == "inj" ~ "Injective",
                symbol == "jasmy" ~ "JasmyCoin",
                symbol == "rook" ~ "Rook",
                symbol == "kilt" ~ "KILT Protocol",
                symbol == "knc" ~ "Kyber Network Crystal",
                symbol == "lcx" ~ "LCX",
                symbol == "lmwr" ~ "LimeWire",
                symbol == "msol" ~ "Marinade staked SOL",
                symbol == "mina" ~ "Mina Protocol",
                symbol == "moon" ~ "r/CryptoCurrency Moons",
                symbol == "near" ~ "NEAR Protocol",
                symbol == "nodl" ~ "Nodle Network",
                symbol == "ocean" ~ "Ocean Protocol",
                symbol == "pstake" ~ "pSTAKE Finance",
                symbol == "ren" ~ "Ren",
                symbol == "xrp" ~ "XRP",
                symbol == "xrt" ~ "Robonomics Network",
                symbol == "samo" ~ "Samoyedcoin",
                symbol == "sdn" ~ "Shiden Network",
                symbol == "spell" ~ "Spell",
                symbol == "xlm" ~ "Stellar",
                symbol == "gmt" ~ "GMT",
                symbol == "rad" ~ "Radworks",
                symbol == "fis" ~ "Stafi",
                symbol == "super" ~ "SuperVerse",
                symbol == "snx" ~ "Synthetix Network",
                symbol == "luna2" ~ "Terra",
                #symbol == "lunc" ~ "Terra Luna Classic",
                symbol == "eurt" ~ "Euro Tether",
                symbol == "ust" ~ "TerraUSD (Wormhole)",
                symbol == "tvk" ~ "Virtua",
                symbol == "rune" ~ "THORChain",
                symbol == "t" ~ "Threshold Network",
                symbol == "trx" ~ "TRON",
                symbol == "uma" ~ "UMA",
                symbol == "usdc" ~ "USDC",
                symbol == "waves" ~ "Waves",
                symbol == "woo" ~ "WOO",
                symbol == "yfi" ~ "yearn.finance",
                symbol == "keep" ~ "Keep Network",
                symbol == "oxt" ~ "Orchid Protocol",
                TRUE ~ name
            )
        ) %>%
        dplyr::left_join(
            geckor::supported_coins() %>%
                dplyr::select(coin_id, name) %>%

                # doppelte coins filtern
                dplyr::filter(!coin_id %in% c("dydx-chain", "dydx-wethdydx", "immutable-x", "saber-2", "alpha-finance")),
            by = "name"
        )

    # test for missing coins
    # test <- kraken_cash_to_crypto_pairs %>%
    #    filter(is.na(coin_id)) %>%
    #    pull(symbol)
    # test

    # test for multiple coins
    # test <- kraken_cash_to_crypto_pairs %>%
    #    group_by(symbol) %>%
    #    dplyr::mutate(count = n())

    return(kraken_cash_to_crypto_pairs)
}
