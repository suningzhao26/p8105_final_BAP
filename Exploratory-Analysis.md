Exploratory Analysis
================

``` r
library(tidyverse)
library(modelr)
library(mgcv)
library(purrr)
```

### Air Quality Index across year in NY state

#### Load the data

``` r
air_daily_df = 
  read_csv("./data/air_daily.csv") %>% 
  separate(col = date, into = c('year','month','day'), sep = "-" , convert = TRUE) %>% 
  drop_na(aqi) %>% 
  drop_na(county)
```

    ## New names:
    ## Rows: 114549 Columns: 21
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (5): county_code, state, county, category, defining_parameter dbl (15): ...1,
    ## state_code, aqi, mean_ozone, max_ozone, mean_co, max_co, me... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

#### Calculate the mean aqi for each county each year

``` r
aqi_year_df = 
  air_daily_df %>% 
  select(county_code, state, county, year, aqi) %>% 
  group_by(county,year) %>% 
  summarize(
    aqi_mean = mean(aqi)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

#### Figure 1: Aqi for different county from 2003-2012

``` r
aqi_state_graph =
  aqi_year_df %>% 
  group_by(county) %>% 
  ggplot(aes(x = year, y = aqi_mean, color = county)) +
  geom_point(alpha=.3) +
  geom_line() +
  labs(
    title = "Air Quality Index by county in NY state, 2003-2012",
    x = "Year",
    y = "Air Quality Index"
  )+
  scale_x_continuous(breaks = 2003:2012 )
```

#### BRFSS

``` r
brfss_air_df = 
  read_csv("./data/brfss_with_air.csv") %>% 
  drop_na(county)
```

    ## New names:
    ## Rows: 69305 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): state, county, category, defining_parameter dbl (25): ...1, county_code,
    ## year, month, day, cvdinfr2, cvdcrhd2, cvdstrk2... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
