library(quantmod)
library(dplyr)
library(tidyr)
library(lubridate)

#### Load Data
getSymbols(c("BTC-USD", "MSTR", "ACWI"),
           src = "yahoo",
           from = "2021-01-01",
           auto.assign = TRUE)

#### Convert to monthly using to.monthly()
BTC   <- to.monthly(`BTC-USD`, indexAt = "lastof", drop.time = TRUE)
MSTR  <- to.monthly(MSTR, indexAt = "lastof", drop.time = TRUE)
ACWI  <- to.monthly(ACWI, indexAt = "lastof", drop.time = TRUE)

merged_data <- merge(BTC, MSTR, ACWI, join = "inner")
df <- data.frame(Date = index(merged_data), coredata(merged_data))
df <- df[,c(1,7,13,19)]

df <- df %>%
  mutate(Date = ceiling_date(Date, "month") - days(1),
         BTC_log_ret = log(BTC.USD.Adjusted) - lag(log(BTC.USD.Adjusted)),
         MSTR_log_ret = log(MSTR.Adjusted) - lag(log(MSTR.Adjusted)),
         ACWI_log_ret = log(ACWI.Adjusted) - lag(log(ACWI.Adjusted)))


model1 <- lm(MSTR_log_ret ~ BTC_log_ret + ACWI_log_ret,
             data = df %>% filter(Date >= as.Date("2024-01-01")))
model2 <- lm(MSTR_log_ret ~ BTC_log_ret,
             data = df %>% filter(Date >= as.Date("2024-01-01")))

summary(model1)
summary(model2)

plot(model1)

#### Load BTC holdings
MSTR_BTC_holdings <- readr::read_delim("btc-holdings-over-time.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

MSTR_BTC_holdings <- MSTR_BTC_holdings %>%
  mutate(DateTime = ceiling_date(DateTime, "month") - days(1))
colnames(MSTR_BTC_holdings) <- c("Date", "BTC_holdings")

df <- left_join(df, MSTR_BTC_holdings, by = "Date")

#### Compute MSTR premium (would like to do but Market Cap data is not as 
#### accessible as I want it to be)
df <- df %>%
  mutate(BTC_holdings_in_USD = BTC.USD.Adjusted*BTC_holdings)






#### Section for options calculations

solve_contracts <- function(delta, underlying_price, exposure){
  contracts <- exposure / exposure_calculator(delta, underlying_price, 1)
  return(contracts)
}

exposure_calculator <- function(delta, underlying_price, number_of_contracts){
  return(delta*underlying_price*number_of_contracts*100)  
}

delta_MSTR_option <- 0.36
underlying_price_MSTR <- 415.41
number_of_contracts <- 900
exposure <- exposure_calculator(delta_MSTR_option, underlying_price_MSTR, number_of_contracts) #what I want to hedge in MSTR


delta_BTC_ETF <- 0.71
underlying_price_BTC_ETF <- 62.79

contracts_needed <- solve_contracts(delta_BTC_ETF, underlying_price_BTC_ETF, exposure)
cat("Contracts needed:", contracts_needed, "\n") # Well this costs 4000€, I will go unhedged then and when rolling I will decrease the exposure drastically


