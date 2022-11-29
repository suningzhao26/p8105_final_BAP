data_import_cleaning
================

check variables in daily parameters, and daily aqi tables:

``` r
daily_ny_2022_co = read_csv("./data/daily_parameter/co/daily_42101_2022.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
```

    ## Rows: 99645 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
daily_ny_2022_no2 = read_csv("./data/daily_parameter/no2/daily_42602_2022.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
```

    ## Rows: 92635 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
daily_aqi = read_csv("./data/daily_aqi/daily_aqi_by_county_2022.csv")
```

    ## Rows: 202786 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
daily_aqi_data = daily_aqi %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
```

## Data integrating and cleaning for daily aqi

### Daily aqi for each county in New York State for the last 15 years.

``` r
full_aqi = 
  tibble(
    files = list.files("data/daily_aqi/"),
    path = str_c("data/daily_aqi/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
  select(-files, -path) %>% 
  select(state = state_name, county = county_name, date, aqi, category, defining_parameter, defining_site, site_num = number_of_sites_reporting)
```

    ## Rows: 310395 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 315619 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 320858 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 319311 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 319231 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 318186 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 318859 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 320535 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 321071 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 326801 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 327537 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 325331 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 324338 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 325888 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 202786 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## daily parameter value

manipulate a relatively small file firstly:

``` r
sample_ozone = read_csv("./data/daily_parameter/ozone/daily_ozone_2019.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## New names:
    ## Rows: 10107 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

There is a large file storage complication which would block the push.
So we need to make a pre-processing for `ozone`, `pm2.5`, and `so2`
folder (those folders contain raw data files that exceed 100 M). This
process is in another document.

After the pre-processing, files in `ozone`, `pm2.5`, and `so2` folder
contain data for New York State, others contain data across the US.

## Data integrating and cleaning for 6 daily parameter values

### Ozone, 15 years, New York state

``` r
full_ozone = 
  tibble(
    files = list.files("data/daily_parameter/ozone/"),
    path = str_c("data/daily_parameter/ozone/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## New names:
    ## Rows: 12688 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13156 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13051 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 12341 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 12297 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10705 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10821 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10710 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10683 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10740 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10363 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10107 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 9906 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 10464 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 7663 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

### CO, 15 years, New York State

``` r
full_co = 
  tibble(
    files = list.files("data/daily_parameter/co/"),
    path = str_c("data/daily_parameter/co/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## Rows: 252960 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 236820 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 225671 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 226463 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 222576 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 216762 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 214950 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 212648 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 210362 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 204927 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 197686 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 188056 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 180206 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 173542 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 99645 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### NO2, 15 years, New York State

``` r
full_no2 = 
  tibble(
    files = list.files("data/daily_parameter/no2/"),
    path = str_c("data/daily_parameter/no2/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## Rows: 138190 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 134240 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 135583 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 131506 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 134196 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 138807 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 149395 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 154940 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 157607 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 154821 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 154178 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 155285 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 158302 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 158390 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 92635 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### SO2, 15 years, New York State

``` r
full_so2 = 
  tibble(
    files = list.files("data/daily_parameter/so2/"),
    path = str_c("data/daily_parameter/so2/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## New names:
    ## Rows: 18485 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17725 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17642 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17164 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 16493 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13180 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13750 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13628 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13681 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 16425 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 15703 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 15711 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 13028 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 12368 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6512 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

### PM2.5, 15 years, New York State

``` r
full_pm2_5 = 
  tibble(
    files = list.files("data/daily_parameter/pm2.5/"),
    path = str_c("data/daily_parameter/pm2.5/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## New names:
    ## Rows: 4157 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4211 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4019 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 3182 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 2507 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 2491 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4983 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6388 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6347 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6608 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6671 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 7203 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6428 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 6667 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4684 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

### PM10, 15 years, New York State

``` r
full_pm10 = 
  tibble(
    files = list.files("data/daily_parameter/pm10/"),
    path = str_c("data/daily_parameter/pm10/", files)
  ) %>% 
  mutate(data = map(path, read_csv)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
  select(state = state_name, city = city_name, county = county_name, site = local_site_name, date = date_local, parameter_name, max = x1st_max_value, mean = arithmetic_mean, units_of_measure, observation_count, latitude, longitude)
```

    ## Rows: 149958 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 147927 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 148480 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 153307 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 160027 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 166329 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 165254 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 166450 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 168837 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 169699 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 172447 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 171922 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 169226 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 170616 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 96376 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
