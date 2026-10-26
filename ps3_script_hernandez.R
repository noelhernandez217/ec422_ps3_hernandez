library(tidyverse)
library(fpp3)

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

# The RSAFSNA is significantly noisier. This is probably because 
