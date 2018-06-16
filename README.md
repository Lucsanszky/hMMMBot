# BitMEX Market Maker Bot written in Haskell

## Requirements

Apart from the obvious requirement that the bot should be profitable, below is an itemised list of desired functionalities:

* Should be as high-performant as possible
* Should have an interface that allows strategy tweaking (ala: https://github.com/BitMEX/sample-market-maker)
* Should handle order placement and amending via bulk orders
* Should link all orders to stop-loss orders to facilitate order management (amending, canceling)
* Should automatically determine order size and price based on market and account data
* Should possibly use `orderBookL2` endpoint to stay up-to-date with the market
* Should switch between aggressive and passive market making based on market conditions
* Should automatically stop after it's been unprofitable for a certain time
* Should log trading behaviour for troubleshooting and analysis
* Should minimise inventory risk (as expected from a good market-maker)
* Should hold favorable positions at funding times
