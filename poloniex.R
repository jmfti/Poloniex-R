library(jsonlite)
library(httr)
library(digest)

poloniex.trading <- function(key, secret, command, args = list()) {
  req <- c(list(
    command = command,
    nonce = round(as.numeric(Sys.time()) * 1e4, 0)),
    args)
  
  ret <- POST("https://poloniex.com/tradingApi",
              add_headers(key=key, sign=hmac(secret, httr:::compose_query(req), "sha512")),
              body = req,
              encode = "form")
  stop_for_status(ret)
  content(ret, "text")
}

poloniex.public = function(command, args=list()) {
  
  req = c(list(command=command, nonce=round(as.numeric(Sys.time()) * 1e4, 0)), args)
  ret = GET("https://poloniex.com/public", query=req)
  stop_for_status(ret)
  content(ret, type="text")
}

poloniex.tradeHistory = function(currency, start) {
  data = poloniex.public("returnTradeHistory", list(currencyPair=currency, start=start))
  df = jsonlite::fromJSON(data)
  df$date = as.numeric(as.POSIXct(df$date))
  df
}

poloniex.returnTicker = function(){
  jsonlite::fromJSON(poloniex.public("returnTicker"))
}

poloniex.return24Volume = function(){
  jsonlite::fromJSON(poloniex.public("return24hVolume"))
}

poloniex.returnOrderBook = function(currency, depth=10){
  q=jsonlite::fromJSON(poloniex.public(command="returnOrderBook", list(currencyPair=currency, depth=depth)))
  q$asks = as.data.frame(q$asks, stringsAsFactors=F)
  names(q$asks) <- c("Price", "Amount")
  q$bids = as.data.frame(q$bids, stringsAsFactors=F)
  names(q$bids) <- names(q$asks)
  q$asks = as.data.frame(sapply(q$asks, as.numeric))
  q$bids = as.data.frame(sapply(q$bids, as.numeric))
  q
}

poloniex.returnChartData = function(currency, period=300, start=Sys.time() - 86400, end=Sys.time()){
  jsonlite::fromJSON(poloniex.public(command="returnChartData", list(currencyPair=currency, period=period, start=start, end=end)))
}

poloniex.returnCurrencies = function(){
  jsonlite::fromJSON(poloniex.public(command="returnCurrencies"))
}

poloniex.returnLoanOrders = function(currency){
  jsonlite::fromJSON(poloniex.public(command="returnLoanOrders", list(currency=currency)))
}

poloniex.returnBalances = function(){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnBalances"))
}

poloniex.returnCompleteBalances = function(){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnCompleteBalances"))
}

poloniex.returnDepositAddresses = function(){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnDepositAddresses"))
}

poloniex.generateNewAddress = function(currency){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "generateNewAddress", list(currency=currency)))
}

poloniex.returnDepositsWithdrawals = function(currency, start, end){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnDepositsWithdrawals", list(currency=currency, start=start, end=end)))
}

poloniex.returnOpenOrders = function(currency){
  poloniex.trading(poloniex.apikey, poloniex.secret, "returnOpenOrders", list(currencyPair=currency))
}

poloniex.returnTradeHistory = function(currency, start, end){
  df=jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnTradeHistory", 
                                         list(currencyPair=currency, start=start, end=end)))
  df$tradeID = as.numeric(df$tradeID)
  df$date = as.Date(df$date)
  df$rate = as.numeric(df$rate)
  df$amount = as.numeric(df$amount)
  df$total = as.numeric(df$total)
  df$fee = as.numeric(df$fee)
  df$orderNumber = as.numeric(df$orderNumber)
  df$type = as.factor(df$type)
  df$category = as.factor(df$category)
  df
}

poloniex.returnOrderTrades = function(order){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "returnOrderTrades", list(order=order)))
}

poloniex.buy = function(currency, rate, amount, option=F){
  if (option == F)
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "buy", list(currencyPair=currency, rate=rate, amount=amount)))
  else{
    switch(option, 
           fillOrKill={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "buy", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             fillOrKill=1)))
           },
           immediateOrCancel={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "buy", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             immediateOrCancel=1)))
           },
           postOnly={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "buy", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             postOnly=1)))
           })
  }
}

poloniex.sell = function(currency, rate, amount, option=F){
  if (option == F)
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "sell", list(currencyPair=currency, rate=rate, amount=amount)))
  else{
    switch(option, 
           fillOrKill={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "sell", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             fillOrKill=1)))
           },
           immediateOrCancel={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "sell", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             immediateOrCancel=1)))
           },
           postOnly={
             jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, 
                                                 "sell", list(currencyPair=currency, 
                                                             rate=rate, amount=amount,
                                                             postOnly=1)))
           })
  }
}

poloniex.cancelOrder = function(order){
  jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "cancelOrder", list(orderNumber=order)))
}

poloniex.moveOrder = function(order, rate, amount=F){
  if (amount == F)
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "moveOrder"), list(orderNumber=order, rate=rate))
  else
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "moveOrder"), list(orderNumber=order, rate=rate, amount=amount))
}

poloniex.withdraw = function(currency, address, amount, paymentID=F){
  if (paymentID==F)
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "withdraw"), list(currency=currency, address=address, amount=amount))
  else
    jsonlite::fromJSON(poloniex.trading(poloniex.apikey, poloniex.secret, "withdraw"), list(currency=currency, address=address, amount=amount, paymentId=paymentID))
}

poloniex.returnFeeInfo = function(){
  poloniex.trading(poloniex.apikey, poloniex.secret, "returnFeeInfo")
}

poloniex.panicSell = function(currency, coefficient=1){
  bestbid = poloniex.returnOrderBook(currency)$bids$Price[[1]]
  curr = strsplit(currency,"_")[[1]][[2]]
  amount = coefficient * as.numeric(poloniex.returnBalances()[[curr]])
  #print(amount)
  poloniex.sell(currency, bestbid * 0.9, amount)
}

poloniex.panicBuy = function(currency, coefficient=1){
  bestask = poloniex.returnOrderBook(currency)$asks$Price[[1]]
  amount = coefficient * as.numeric(poloniex.returnBalances()[["BTC"]]) / bestask
  #print(amount)
  poloniex.buy(currency, bestask * 1.1, amount)
}

poloniex.sellFirst = function(currency, btcs=0){
  bestask = poloniex.returnOrderBook(currency)$asks$Price[[1]]
  curr = strsplit(currency,"_")[[1]][[2]]
  if (btcs == 0)
    amount = as.numeric(poloniex.returnBalances()[[curr]] )
  else
    amount = btcs / bestask
  #print(amount)
  poloniex.sell(currency, bestask, amount)
}

poloniex.buyFirst = function(currency, btcs=0){
  bestbid = poloniex.returnOrderBook(currency)$bids$Price[[1]]
  if (btcs == 0)
    amount = as.numeric(poloniex.returnBalances()[["BTC"]] ) / bestbid
  else
    amount = btcs / bestbid
  
  poloniex.buy(currency, bestbid, amount)
}


