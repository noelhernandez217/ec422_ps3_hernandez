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
  
  
only_vol <- subset(can_gas, select = Volume) # select wasn't working?
log_vol <- log(only_vol, base = exp(1))
can_gas <- mutate(can_gas, log_volume = log_vol)  
can_gas

can_gas |> 
  autoplot(log_volume) + 
  labs(
    title = "log(Volume) of Gas Produced through Time", 
    x = "Months", 
    y = "% of Gas Produced"
  ) + geom_smooth()
theme_minimal()





