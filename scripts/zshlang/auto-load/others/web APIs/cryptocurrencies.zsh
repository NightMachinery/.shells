function crypto-prices() {
    local btc eth

    ##
    # gurl 'https://api.coinbase.com/v2/prices/BTC-USD/spot' | jq .
    # gurl 'https://api.coinbase.com/v2/prices/ETH-USD/spot' | jq .
    ##

    btc="$(gurl 'https://api.coinbase.com/v2/prices/BTC-USD/spot' | jqm "\"BTC: \" + .data.amount")" || return $?
    eth="$(gurl 'https://api.coinbase.com/v2/prices/ETH-USD/spot' | jqm "\"ETH: \" + .data.amount")" || return $?

    ec "$btc $eth"
}
aliasfn cryp crypto-prices
