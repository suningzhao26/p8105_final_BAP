Air Quality Index across year in NY state
================

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
  select(state_code, county_code, state, county, year, aqi) %>% 
  group_by(state_code, county_code,county,year) %>% 
  summarize(
    aqi_mean = mean(aqi)
  )
```

    ## `summarise()` has grouped output by 'state_code', 'county_code', 'county'. You
    ## can override using the `.groups` argument.

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
  scale_x_continuous(breaks = 2003:2012 )+
  scale_color_viridis(
    name = "Location", 
    discrete = TRUE
  )

aqi_state_graph
```

<img src="Exp-Air-Quality-Index-across-year-in-NY-state_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

##### Figure 2: Mean AQI for different county from 2003-2012

``` r
aqi_county_graph = 
  aqi_year_df %>% 
  group_by(county) %>% 
  summarize(
    aqi_all = mean(aqi_mean),
    max = max(aqi_mean),
    min = min(aqi_mean)
  ) %>% 
  mutate(county = fct_reorder(county, aqi_all)) %>% 
  ggplot(aes(x = county, y = aqi_all)) +
  geom_point()+
  geom_errorbar(mapping = aes(ymin = min, ymax = max)) +
  labs( x = "County",  y = "Air Quality Index", title = "Mean AQI for different county from 2003-2012") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

aqi_county_graph
```

<img src="Exp-Air-Quality-Index-across-year-in-NY-state_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

#### Figure 3: Map for Aqi in NY state

``` r
air_county_df = 
  aqi_year_df %>% 
  group_by(state_code, county_code,county) %>% 
  summarize(
    aqi_all = mean(aqi_mean),
    max = max(aqi_mean),
    min = min(aqi_mean)
  ) %>% 
  mutate(
    fips = str_c(state_code,county_code)
  )
```

    ## `summarise()` has grouped output by 'state_code', 'county_code'. You can
    ## override using the `.groups` argument.

``` r
county_plot_map = 
  plot_usmap(regions = "county", include = c("NY"), data = air_county_df, values = "aqi_all") +
  scale_fill_continuous(
    low = "white", high = "Red", name = "Air Quality Index", label = scales::comma, limits = c(0,60)
  ) + 
  theme(legend.position = "right")

county_plot_map
```

<img src="Exp-Air-Quality-Index-across-year-in-NY-state_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

#### Figure 4: Unhealthy air quality days in counties among 10 years

``` r
air_quality_day_df = 
  air_daily_df %>% 
  group_by(state_code, county_code,county) %>% 
  mutate(
    aqi_status = case_when(
      category %in% c("Good", "Moderate") ~ "Healthy",
      category %in% c("Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy") ~ "Unhealthy"
    )
  ) 

Unhealthy_air_graph = 
  air_quality_day_df %>% 
  filter(aqi_status == "Unhealthy") %>% 
  group_by(county) %>% 
  summarize(
    unhealthy_days = n()
  ) %>% 
  mutate(
    county = fct_reorder(county, unhealthy_days)
    ) %>% 
  ggplot(aes(y = county, x = unhealthy_days, fill = unhealthy_days)) +
  geom_col() +
  labs(
    title = "Unhealthy air quality days in counties among 10 years",
    x = "Unhealthy air quality days",
    y = "County"
  ) +
  scale_fill_viridis(option = "turbo")

Unhealthy_air_graph 
```

<img src="Exp-Air-Quality-Index-across-year-in-NY-state_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />
