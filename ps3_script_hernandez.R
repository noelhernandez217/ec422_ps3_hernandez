library(tidyverse)
library(fpp3)
library(glue)
library(ggplot)
library(fable)


retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts) # no gaps found, returns false for both 

head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)         

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)         
head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

# Q) Why do we sum Volume instead of taking the mean? 
# The reason we take the sum instead of the mean per month is because
# the mean would capture the average production of gas in that month. 
# That isn't useful for the time series because we are trying to identify 
# patterns in the monthly production of gas from month to month. That is 
# more identifying by looking at total production per month. 

# Q) Does the quarterly series still appear to be seasonal? -> YES! 

can_gas_q |> autoplot(Volume)

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
# 2.7 b 
# On the January of every year, we see a massive drop followed by a corrective rise. 
# The SA series also has this pattern. The seasonal adjustment doesn't change the overall
# trend of the data. However, it does capture the primary movements without the seasonality
# creating the big dips and spikes. 

can_gas |> 
  autoplot(Volume) + 
  labs(
    title = "Volume of Gas Produced through Time", 
    x = "Months", 
    y = "Volume of Gas Produced"
  ) + 
  theme_minimal()

can_gas |> 
  autoplot(Volume) + 
  labs(
    title = "Volume of Gas Produced through Time", 
    x = "Months", 
    y = "Volume of Gas Produced"
  ) + geom_smooth()
  theme_minimal()
  
# I applied the geom_smooth() so we can see the general trend without the 
# seasonality in the way. We get a more accurate picture of how gas production
# is increasing through time. 

# STARTING PART 3 - Data Transformations (log and growth rates)
  
# 3.1a - can_gas table does show that the volumes increase as the series grows. 
# there is greater seasonality swings in the middle section of the graph
# compared to the earlier years in the 60s-70s. Much smaller swings. 
  
  
# 3.1b - The shape of the trend changed where we see much more seasonality 
# in the movement of the graph in earlier years. The middle is elevated upwards. 
# The end of the graph has much lower seasonality and is more linear. 
  
can_gas <- can_gas |> mutate(log_volume = log(Volume))
head(can_gas)

can_gas |> 
  autoplot(log_volume) + 
  labs(
    title = "log(Volume) of Gas Produced through Time", 
    x = "Months", 
    y = "log(Volume)"
  ) + geom_smooth()
theme_minimal()

# 3.1c - The transformation has made this plot almost linear. We still see the effects of
# seasonality. Its been significantly minimized. 

log_retail <- retail_ts |> filter(series_id == "RSAFSNA") |> mutate(log_sales = log(sales))
head(log_retail)

log_retail |> 
  autoplot(log_sales) + 
  labs(
    title = "log(sales) through time", 
    x = "Months", 
    y = "log(sales)"
  ) + geom_smooth()
theme_minimal()

# 3.2 - Growth Rates

# 3.2a 

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

head(can_gas)
     
can_gas |> 
  autoplot(growth_direct) + 
  labs(
    title = "Growth Rate of Gas Volume through time", 
    x = "Months", 
    y = "Growth Rate of Volume"
  ) + geom_smooth()
theme_minimal()

# I think the log difference approximation is pretty good. We see the linear trend
# and we capture the movement around the line and the general trend. 

# 3.3. SA vs. NSA Growth Rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# What this is doing is taking the log of each month. This gives us the % change in sales per month.
# When we take the differences of each month, we get the % change from january to february.
# We can see how the percentage changes from month to month don't very as heavily as the raw changes 
# sales from month to month. This is because the logs are removing the wild swings from the pure retail 
# data to unmask actual underlying behavior. 

# Part 4 – Seeing, Decomposing, and Modeling Seasonality

# 4.1 - gg_season() - A Season Plot
# 4.1a 

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b - same plot but for SA series

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# Contrasting the plots, in just the RSAFSNA, there is significantly more seasonality. 
# We see greater swings, especially in April/May and November/December. In the second plot
# the lines become straight. There are some dips in the 2020 year because of covid but they level
# out and maintain their linear behavior afterwards. 

# 4.1c - Do the same thing for can_gas and log(Volume)

can_gas |> 
  gg_season(log_volume) + 
  labs(
    title = "Seasonal Patterns in Canada Gas Production", 
    y = "log volume"
  ) + 
  theme_minimal()

# The seasonality follows this kind of convex shape. Gas demand is high in 
# early months and later months. The middle of the year (April-Sept) there is less
# demand for gas in earlier years. As we get closer to the 2000s, gas demand is more consistent. 
# Since natural gas is used for heating, it makes sense that the colder months see more demand. 

# 4.2 - gg_subseries() – A Subseries Plot

# subseries plot of the logged can_gas series: 
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3. Seasonally Adjusting a Series with X-13ARIMA-SEATS

install.packages("seasonal")

# 4.3a. Add the following to your script to run X-13ARIMA-SEATS on the NSA series, and 
# plot the components:

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# NOTE: This is literally decomposing the target variable at each interval into 
# the components: trend, cycles, seasonality, and irregular! 

# Quick skim of the data shows that the month of December has the highest seasonality. 
# This makes sense in the retail sales context because of Christmas. It seems to be trending down. 

december_seasonality <- retail_nsa_x13 |> filter(month(Month) == 12)
head(december_seasonality)

ggplot(december_seasonality, aes(x = Month, y = seasonal)) +
  geom_line() +
  labs(
    title = "December Seasonality through Time",
    x = "Year",
    y = "Seasonal Component"
  ) +
  theme_minimal()


# 4.3b 

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# I dont think I was very close using the default settings. The earlier years are the closest. 
# Once we enter the years 2010 and onwards to 2025, we begin to lose a lot of accuracy when compared to 
# the Bereau. I'm sure the Bureau's model ajusted for things like COVID and other global factors that are influencing 
# the seasonality component. 

# 4.4. A Simple Model – Linear Trend + Seasonal Dummies

# 4.4a. Add code to your script to fit a linear trend plus seasonal dummy variables model to the 
# log of the can_gas “Volume” variable:

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# What is the estimated average monthly growth rate of gas production? 
# Which month has the most significant seasonal effect (largest t-statistic in absolute value)?

# The average monthly growth rate is 0.004 x 100 = 0.04% for gas production. 
# season()year6 has the highest statistic |-4.777| = 4.77 which corresponds to the month of June. 

# 4.4b 

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# The plotted values capture most of the trend past the year 1990. Before that, the seasonal swings
# in the earlier years are harder to capture and are more convex. There is some seasonality between 
# 1980 and 1990 that is harder to capture since the swings are greater. 

# 4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# The residuals do not look like noise. The test statistic is very large which makes the p-value essentially 0. 
# The goal with this test is to fail to reject since the null is we captured all the pattern 
# and our alternative hypothesis is that there is still pattern left to capture. 
# I think 5% is just convention for a lot of tests. 

# Part 5 – A First Forecast Evaluation

# 5.1a - An Expanding-Window TS-CV

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# What are the dates of the first and last forecasts that are evaluated? 
# Report the MAE and RMSE of each model in a table. 
# Which model forecasts more accurately? Do the two metrics agree?

# The seasonal naive performs the best and has the lowest error for both the MAE and RMSE metrics. 
# It has an MAE of 0.562 compared to 4.01 and an RMSE of 0.702 compared to 4.36. Both metrics agree 
# better performance from seasonal naive and it having higher forecast accuracy. 

# 5.2. Is the Difference Significant?
# 5.2a - Diebold-Mariano test

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# The statistic itself is DM = -31.278 which means the seasonal naive has a smaller average absolute error. 
# The super small p-value < 2.2e-16 means the difference is statistically significant. 
# This confirms that seasonal naive is the better model on average. 

# 5.3. A Rolling Window
# 5.3a

gas_cv_fc_rolling <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

 gas_cv_fc_rolling |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# Expanding window results 
# 1 snaive Test  0.562 0.702
# 2 tslm   Test  4.01  4.36 

# Rolling window results
# 1 snaive Test  0.562 0.702
# 2 tslm   Test  2.09  2.42 

# I guess this makes sense for seasonal naive since it just reports the previous seasonal value. 
# We won't see its error change at all. But for the linear model, it makes sense that we would 
# get lower error since were only operating on more recent values in the window to compute our target variable. 
# We see its error drop but its still a lot higher than seasonal naive error. 

 







