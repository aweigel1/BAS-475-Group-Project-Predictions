library(fpp3)

credit <- read.csv("credit.csv")

#Making data increasing
creditIncr <- transform(credit, credit_in_millions = rev(credit_in_millions))

#Creating tsibble
creditIncr %>% 
  mutate(Month = seq(as.Date("1981/1/1"), by = "month", length.out = nrow(creditIncr))) %>%
  mutate(Month = yearmonth(Month)) %>% 
  as_tsibble(index = Month) -> TSCredit

TSCredit %>% 
  autoplot()

#Differencing
TSCredit %>%
  features(credit_in_millions, unitroot_kpss)

TSCredit %>%
  features(credit_in_millions, unitroot_ndiffs)

TSCredit_Diff <- TSCredit %>%
  mutate(y = box_cox(credit_in_millions, lambda)) %>% 
  mutate(y = difference(y, 12)) %>% 
  mutate(y = difference(y))

TSCredit_Diff %>% 
  autoplot(y)

#Creating Train and Test Data
train <- TSCredit_Diff %>% 
  filter(Month < yearmonth("2021 Jan"))

test <- TSCredit_Diff %>% 
  filter(Month >= yearmonth("2021 Jan"))

#Models


TSCredit %>%
  model(lm = TSLM(credit_in_millions ~  trend() + season()),
        Drift= RW(credit_in_millions ~ drift()),
        hw = ETS(credit_in_millions),
        Mean = MEAN(credit_in_millions),
        NNETAR(sqrt(credit_in_millions))) %>%
  forecast(h = 20) %>%
  autoplot(credit_in_millions)

