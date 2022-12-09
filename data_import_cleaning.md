data_import_cleaning
================

check variables in daily parameters, and daily aqi tables:

``` r
daily_ny_2008_co = read_csv("./data/daily_parameter/co/daily_42101_2008.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
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

``` r
daily_ny_2008_no2 = read_csv("./data/daily_parameter/no2/daily_42602_2008.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
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

``` r
daily_aqi = read_csv("./data/daily_aqi/daily_aqi_by_county_2008.csv")
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

``` r
daily_aqi_data = daily_aqi %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")
```

## Data integrating and cleaning for daily aqi

### Daily aqi for each county in New York State from 2003 to 2010.

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

    ## Rows: 290543 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 298489 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 301669 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 302941 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 309109 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): State Name, county Name, State Code, County Code, Category, Defini...
    ## dbl  (2): AQI, Number of Sites Reporting
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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

## daily parameter value

manipulate a relatively small file firstly:

``` r
sample_ozone = read_csv("./data/daily_parameter/ozone/daily_ozone_2008.csv") %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York") %>% 
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
    ## • `` -> `...1`

There is a large file storage complication which would block the push.
So we need to make a pre-processing for `ozone`, `pm2.5`, and `so2`
folder (those folders contain raw data files that exceed 100 M). This
process is in another document.

After the pre-processing, files in `ozone`, `pm2.5`, and `so2` folder
contain data for New York State, others contain data across the US.

## Data integrating and cleaning for 6 daily parameter values

### Ozone, 10 years, New York state

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
    ## Rows: 11085 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 11158 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 11424 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 11793 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 12187 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... lgl
    ## (1): method_code date (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
    ## • `` -> `...1`

### CO, 10 years, New York State

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

    ## Rows: 308449 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 302244 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 291837 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 273410 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 260464 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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

### NO2, 10 years, New York State

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

    ## Rows: 145028 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 146877 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 145028 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 144190 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 140212 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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

### SO2, 10 years, New York State

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
    ## Rows: 17002 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17741 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17883 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 17972 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 19444 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (16): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (12): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
    ## • `` -> `...1`

### PM2.5, 10 years, New York State

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
    ## Rows: 4104 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4031 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4041 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 3972 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 4159 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (15): county_code, site_num, datum, parameter_name, sample_duration, po... dbl
    ## (13): ...1, state_code, parameter_code, poc, latitude, longitude, obser... date
    ## (2): date_local, date_of_last_change
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
    ## • `` -> `...1`

### PM10, 10 years, New York State

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

    ## Rows: 145658 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 150286 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 155269 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 153742 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 150135 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): State Code, County Code, Site Num, Datum, Parameter Name, Sample ...
    ## dbl  (10): Parameter Code, POC, Latitude, Longitude, Observation Count, Obse...
    ## date  (2): Date Local, Date of Last Change
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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

## Extract daily parameter mean value from different sites for each county.

### Ozone (unit of measure: parts per million)

``` r
ozone = full_ozone %>% 
  group_by(county, date) %>% 
  summarize(
    mean_ozone = mean(mean),
    max_ozone = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

### CO (unit of measure: parts per million)

``` r
co = full_co %>% 
  group_by(county, date) %>% 
  summarize(
    mean_co = mean(mean),
    max_co = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

### NO2 (unit of measure: parts per million)

``` r
no2 = full_no2 %>% 
  group_by(county, date) %>% 
  summarize(
    mean_no2 = mean(mean),
    max_no2 = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

### SO2 (unit of measure: parts per million)

``` r
so2 = full_so2 %>% 
  group_by(county, date) %>% 
  summarize(
    mean_so2 = mean(mean),
    max_so2 = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

### PM2.5 (unit of measure: Micrograms/cubic meter (LC))

``` r
pm2_5 = full_pm2_5 %>% 
  group_by(county, date) %>% 
  summarize(
    mean_pm2_5 = mean(mean),
    max_pm2_5 = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

### PM10 (unit of measure: Micrograms/cubic meter (25 C))

``` r
pm10 = full_pm10 %>% 
  group_by(county, date) %>% 
  summarize(
    mean_pm10 = mean(mean),
    max_pm10 = mean(max)
  )
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

## Merge daily aqi and daily parameter tables

``` r
air_daily = 
  left_join(full_aqi, ozone, by = c("county", "date")) %>% 
  left_join(co, by = c("county", "date")) %>% 
  left_join(no2, by = c("county", "date")) %>%
  left_join(so2, by = c("county", "date")) %>%
  left_join(pm2_5, by = c("county", "date")) %>%
  left_join(pm10, by = c("county", "date")) %>% 
  separate(defining_site, into = c("state_code", "county_code", "site_code"), sep = "-") %>% 
  select(state_code, county_code, everything(), -site_num, -site_code)
```

``` r
write.csv(air_daily, file = ("./data/air_daily.csv"))
```

Dataset from brfss (raw datasets deleted, too large for push):

Code chunks used:

``` r
b2003 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/cdbrfs03.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack, aservist, asdrvist, asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd2, cvdinfr2, cvdstrk2, educag,sex, smoker2, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2003$physhlth[b2003$physhlth == "88"] <- 0
b2003$physhlth[b2003$physhlth == "77"] <- NA
b2003$physhlth[b2003$physhlth == "99"] <- NA

b2003$menthlth[b2003$menthlth == "88"] <- 0
b2003$menthlth[b2003$menthlth == "77"] <- NA
b2003$menthlth[b2003$menthlth == "99"] <- NA

b2003$cvdcrhd2[b2003$cvdcrhd2 == "2"] <- 0
b2003$cvdcrhd2[b2003$cvdcrhd2 == "7"] <- NA
b2003$cvdcrhd2[b2003$cvdcrhd2 == "9"] <- NA

b2003$cvdinfr2[b2003$cvdinfr2 == "2"] <- 0
b2003$cvdinfr2[b2003$cvdinfr2 == "7"] <- NA
b2003$cvdinfr2[b2003$cvdinfr2 == "9"] <- NA

b2003$cvdstrk2[b2003$cvdstrk2 == "2"] <- 0
b2003$cvdstrk2[b2003$cvdstrk2 == "7"] <- NA
b2003$cvdstrk2[b2003$cvdstrk2 == "9"] <- NA

b2003$asthma2[b2003$asthma2 == "2"] <- 0
b2003$asthma2[b2003$asthma2 == "7"] <- NA
b2003$asthma2[b2003$asthma2 == "9"] <- NA

b2003$asthnow[b2003$asthnow == "2"] <- 0
b2003$asthnow[b2003$asthnow == "7"] <- NA
b2003$asthnow[b2003$asthnow == "9"] <- NA

b2003$educag[b2003$educag == "9"] <- NA

b2003$sex[b2003$sex == "2"] <- 0

b2003$smoker2[b2003$smoker2 == "9"] <- NA

b2003$asthmage[b2003$asthmage == "99"] <- NA
b2003$asthmage[b2003$asthmage == "98"] <- NA
b2003$asthmage[b2003$asthmage == "97"] <- 10

b2003$asattack[b2003$asattack == "2"] <- 0
b2003$asattack[b2003$asattack == "7"] <- NA
b2003$asattack[b2003$asattack == "9"] <- NA

b2003$aservist[b2003$aservist == "88"] <- 0
b2003$aservist[b2003$aservist == "98"] <- NA
b2003$aservist[b2003$aservist == "99"] <- NA

b2003$asdrvist[b2003$asdrvist == "88"] <- 0
b2003$asdrvist[b2003$asdrvist == "98"] <- NA
b2003$asdrvist[b2003$asdrvist == "99"] <- NA

b2003$asymptom[b2003$asymptom == "8"] <- 0
b2003$asymptom[b2003$asymptom == "7"] <- NA
b2003$asymptom[b2003$asymptom == "9"] <- NA

b2003$asthmst[b2003$asthmst == "9"] <- NA

b2003$ltasthm[b2003$ltasthm == "9"] <- NA

b2003$casthma[b2003$casthma == "9"] <- NA

b2003$race2[b2003$race2 == "9"] <- NA

b2003$ageg5yr[b2003$ageg5yr == "14"] <- NA

b2003$incomg[b2003$incomg == "9"] <- NA

b2003 = 
  b2003 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd2, heart_attack = cvdinfr2, stroke = cvdstrk2, education = educag,sex, smoker = smoker2, race = race2, age = ageg5yr, income = incomg)

b2003 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2003.csv"))
```

``` r
b2004 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS04.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,aservist, asdrvist, asattack,asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd2, cvdinfr2, cvdstrk2, educag,sex, smoker2, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2004$physhlth[b2004$physhlth == "88"] <- 0
b2004$physhlth[b2004$physhlth == "77"] <- NA
b2004$physhlth[b2004$physhlth == "99"] <- NA

b2004$menthlth[b2004$menthlth == "88"] <- 0
b2004$menthlth[b2004$menthlth == "77"] <- NA
b2004$menthlth[b2004$menthlth == "99"] <- NA

b2004$cvdcrhd2[b2004$cvdcrhd2 == "2"] <- 0
b2004$cvdcrhd2[b2004$cvdcrhd2 == "7"] <- NA
b2004$cvdcrhd2[b2004$cvdcrhd2 == "9"] <- NA

b2004$cvdinfr2[b2004$cvdinfr2 == "2"] <- 0
b2004$cvdinfr2[b2004$cvdinfr2 == "7"] <- NA
b2004$cvdinfr2[b2004$cvdinfr2 == "9"] <- NA

b2004$cvdstrk2[b2004$cvdstrk2 == "2"] <- 0
b2004$cvdstrk2[b2004$cvdstrk2 == "7"] <- NA
b2004$cvdstrk2[b2004$cvdstrk2 == "9"] <- NA

b2004$asthma2[b2004$asthma2 == "2"] <- 0
b2004$asthma2[b2004$asthma2 == "7"] <- NA
b2004$asthma2[b2004$asthma2 == "9"] <- NA

b2004$asthnow[b2004$asthnow == "2"] <- 0
b2004$asthnow[b2004$asthnow == "7"] <- NA
b2004$asthnow[b2004$asthnow == "9"] <- NA

b2004$educag[b2004$educag == "9"] <- NA

b2004$sex[b2004$sex == "2"] <- 0

b2004$smoker2[b2004$smoker2 == "9"] <- NA

b2004$asthmage[b2004$asthmage == "99"] <- NA
b2004$asthmage[b2004$asthmage == "98"] <- NA
b2004$asthmage[b2004$asthmage == "97"] <- 10

b2004$asattack[b2004$asattack == "2"] <- 0
b2004$asattack[b2004$asattack == "7"] <- NA
b2004$asattack[b2004$asattack == "9"] <- NA

b2004$aservist[b2004$aservist == "88"] <- 0
b2004$aservist[b2004$aservist == "98"] <- NA
b2004$aservist[b2004$aservist == "99"] <- NA

b2004$asdrvist[b2004$asdrvist == "88"] <- 0
b2004$asdrvist[b2004$asdrvist == "98"] <- NA
b2004$asdrvist[b2004$asdrvist == "99"] <- NA

b2004$asymptom[b2004$asymptom == "8"] <- 0
b2004$asymptom[b2004$asymptom == "7"] <- NA
b2004$asymptom[b2004$asymptom == "9"] <- NA

b2004$asthmst[b2004$asthmst == "9"] <- NA

b2004$ltasthm[b2004$ltasthm == "9"] <- NA

b2004$casthma[b2004$casthma == "9"] <- NA

b2004$race2[b2004$race2 == "9"] <- NA

b2004$ageg5yr[b2004$ageg5yr == "14"] <- NA

b2004$incomg[b2004$incomg == "9"] <- NA

b2004 = 
  b2004 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma,  asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd2, heart_attack = cvdinfr2, stroke = cvdstrk2, education = educag,sex, smoker = smoker2, race = race2, age = ageg5yr, income = incomg)

b2004 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2004.csv"))
```

``` r
b2005 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS05.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack,aservist, asdrvist, asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd3, cvdinfr3, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2005$physhlth[b2005$physhlth == "88"] <- 0
b2005$physhlth[b2005$physhlth == "77"] <- NA
b2005$physhlth[b2005$physhlth == "99"] <- NA

b2005$menthlth[b2005$menthlth == "88"] <- 0
b2005$menthlth[b2005$menthlth == "77"] <- NA
b2005$menthlth[b2005$menthlth == "99"] <- NA

b2005$cvdcrhd3[b2005$cvdcrhd3 == "2"] <- 0
b2005$cvdcrhd3[b2005$cvdcrhd3 == "7"] <- NA
b2005$cvdcrhd3[b2005$cvdcrhd3 == "9"] <- NA

b2005$cvdinfr3[b2005$cvdinfr3 == "2"] <- 0
b2005$cvdinfr3[b2005$cvdinfr3 == "7"] <- NA
b2005$cvdinfr3[b2005$cvdinfr3 == "9"] <- NA

b2005$cvdstrk3[b2005$cvdstrk3 == "2"] <- 0
b2005$cvdstrk3[b2005$cvdstrk3 == "7"] <- NA
b2005$cvdstrk3[b2005$cvdstrk3 == "9"] <- NA

b2005$asthma2[b2005$asthma2 == "2"] <- 0
b2005$asthma2[b2005$asthma2 == "7"] <- NA
b2005$asthma2[b2005$asthma2 == "9"] <- NA

b2005$asthnow[b2005$asthnow == "2"] <- 0
b2005$asthnow[b2005$asthnow == "7"] <- NA
b2005$asthnow[b2005$asthnow == "9"] <- NA

b2005$educag[b2005$educag == "9"] <- NA

b2005$sex[b2005$sex == "2"] <- 0

b2005$smoker3[b2005$smoker3 == "9"] <- NA

b2005$asthmage[b2005$asthmage == "99"] <- NA
b2005$asthmage[b2005$asthmage == "98"] <- NA
b2005$asthmage[b2005$asthmage == "97"] <- 10

b2005$asattack[b2005$asattack == "2"] <- 0
b2005$asattack[b2005$asattack == "7"] <- NA
b2005$asattack[b2005$asattack == "9"] <- NA

b2005$aservist[b2005$aservist == "88"] <- 0
b2005$aservist[b2005$aservist == "98"] <- NA
b2005$aservist[b2005$aservist == "99"] <- NA

b2005$asdrvist[b2005$asdrvist == "88"] <- 0
b2005$asdrvist[b2005$asdrvist == "98"] <- NA
b2005$asdrvist[b2005$asdrvist == "99"] <- NA

b2005$asymptom[b2005$asymptom == "8"] <- 0
b2005$asymptom[b2005$asymptom == "7"] <- NA
b2005$asymptom[b2005$asymptom == "9"] <- NA

b2005$asthmst[b2005$asthmst == "9"] <- NA

b2005$ltasthm[b2005$ltasthm == "9"] <- NA

b2005$casthma[b2005$casthma == "9"] <- NA

b2005$race2[b2005$race2 == "9"] <- NA

b2005$ageg5yr[b2005$ageg5yr == "14"] <- NA

b2005$incomg[b2005$incomg == "9"] <- NA

b2005 = 
  b2005 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd3, heart_attack = cvdinfr3, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2005 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2005.csv"))
```

``` r
b2006 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS06.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack,aservist, asdrvist,asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd3, cvdinfr3, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2006$physhlth[b2006$physhlth == "88"] <- 0
b2006$physhlth[b2006$physhlth == "77"] <- NA
b2006$physhlth[b2006$physhlth == "99"] <- NA

b2006$menthlth[b2006$menthlth == "88"] <- 0
b2006$menthlth[b2006$menthlth == "77"] <- NA
b2006$menthlth[b2006$menthlth == "99"] <- NA

b2006$cvdcrhd3[b2006$cvdcrhd3 == "2"] <- 0
b2006$cvdcrhd3[b2006$cvdcrhd3 == "7"] <- NA
b2006$cvdcrhd3[b2006$cvdcrhd3 == "9"] <- NA

b2006$cvdinfr3[b2006$cvdinfr3 == "2"] <- 0
b2006$cvdinfr3[b2006$cvdinfr3 == "7"] <- NA
b2006$cvdinfr3[b2006$cvdinfr3 == "9"] <- NA

b2006$cvdstrk3[b2006$cvdstrk3 == "2"] <- 0
b2006$cvdstrk3[b2006$cvdstrk3 == "7"] <- NA
b2006$cvdstrk3[b2006$cvdstrk3 == "9"] <- NA

b2006$asthma2[b2006$asthma2 == "2"] <- 0
b2006$asthma2[b2006$asthma2 == "7"] <- NA
b2006$asthma2[b2006$asthma2 == "9"] <- NA

b2006$asthnow[b2006$asthnow == "2"] <- 0
b2006$asthnow[b2006$asthnow == "7"] <- NA
b2006$asthnow[b2006$asthnow == "9"] <- NA

b2006$educag[b2006$educag == "9"] <- NA

b2006$sex[b2006$sex == "2"] <- 0

b2006$smoker3[b2006$smoker3 == "9"] <- NA

b2006$asthmage[b2006$asthmage == "99"] <- NA
b2006$asthmage[b2006$asthmage == "98"] <- NA
b2006$asthmage[b2006$asthmage == "97"] <- 10

b2006$asattack[b2006$asattack == "2"] <- 0
b2006$asattack[b2006$asattack == "7"] <- NA
b2006$asattack[b2006$asattack == "9"] <- 

b2006$aservist[b2006$aservist == "88"] <- 0
b2006$aservist[b2006$aservist == "98"] <- NA
b2006$aservist[b2006$aservist == "99"] <- NA

b2006$asdrvist[b2006$asdrvist == "88"] <- 0
b2006$asdrvist[b2006$asdrvist == "98"] <- NA
b2006$asdrvist[b2006$asdrvist == "99"] <- NA

b2006$asymptom[b2006$asymptom == "8"] <- 0
b2006$asymptom[b2006$asymptom == "7"] <- NA
b2006$asymptom[b2006$asymptom == "9"] <- NA

b2006$asthmst[b2006$asthmst == "9"] <- NA

b2006$ltasthm[b2006$ltasthm == "9"] <- NA

b2006$casthma[b2006$casthma == "9"] <- NA

b2006$race2[b2006$race2 == "9"] <- NA

b2006$ageg5yr[b2006$ageg5yr == "14"] <- NA

b2006$incomg[b2006$incomg == "9"] <- NA

b2006 = 
  b2006 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd3, heart_attack = cvdinfr3, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2006 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2006.csv"))
```

``` r
b2007 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS07.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack,aservist, asdrvist, asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2007$physhlth[b2007$physhlth == "88"] <- 0
b2007$physhlth[b2007$physhlth == "77"] <- NA
b2007$physhlth[b2007$physhlth == "99"] <- NA

b2007$menthlth[b2007$menthlth == "88"] <- 0
b2007$menthlth[b2007$menthlth == "77"] <- NA
b2007$menthlth[b2007$menthlth == "99"] <- NA

b2007$cvdcrhd4[b2007$cvdcrhd4 == "2"] <- 0
b2007$cvdcrhd4[b2007$cvdcrhd4 == "7"] <- NA
b2007$cvdcrhd4[b2007$cvdcrhd4 == "9"] <- NA

b2007$cvdinfr4[b2007$cvdinfr4 == "2"] <- 0
b2007$cvdinfr4[b2007$cvdinfr4 == "7"] <- NA
b2007$cvdinfr4[b2007$cvdinfr4 == "9"] <- NA

b2007$cvdstrk3[b2007$cvdstrk3 == "2"] <- 0
b2007$cvdstrk3[b2007$cvdstrk3 == "7"] <- NA
b2007$cvdstrk3[b2007$cvdstrk3 == "9"] <- NA

b2007$asthma2[b2007$asthma2 == "2"] <- 0
b2007$asthma2[b2007$asthma2 == "7"] <- NA
b2007$asthma2[b2007$asthma2 == "9"] <- NA

b2007$asthnow[b2007$asthnow == "2"] <- 0
b2007$asthnow[b2007$asthnow == "7"] <- NA
b2007$asthnow[b2007$asthnow == "9"] <- NA

b2007$educag[b2007$educag == "9"] <- NA

b2007$sex[b2007$sex == "2"] <- 0

b2007$smoker3[b2007$smoker3 == "9"] <- NA

b2007$asthmage[b2007$asthmage == "99"] <- NA
b2007$asthmage[b2007$asthmage == "98"] <- NA
b2007$asthmage[b2007$asthmage == "97"] <- 10

b2007$asattack[b2007$asattack == "2"] <- 0
b2007$asattack[b2007$asattack == "7"] <- NA
b2007$asattack[b2007$asattack == "9"] <- NA

b2007$aservist[b2007$aservist == "88"] <- 0
b2007$aservist[b2007$aservist == "98"] <- NA
b2007$aservist[b2007$aservist == "99"] <- NA

b2007$asdrvist[b2007$asdrvist == "88"] <- 0
b2007$asdrvist[b2007$asdrvist == "98"] <- NA
b2007$asdrvist[b2007$asdrvist == "99"] <- NA

b2007$asymptom[b2007$asymptom == "8"] <- 0
b2007$asymptom[b2007$asymptom == "7"] <- NA
b2007$asymptom[b2007$asymptom == "9"] <- NA

b2007$asthmst[b2007$asthmst == "9"] <- NA

b2007$ltasthm[b2007$ltasthm == "9"] <- NA

b2007$casthma[b2007$casthma == "9"] <- NA

b2007$race2[b2007$race2 == "9"] <- NA

b2007$ageg5yr[b2007$ageg5yr == "14"] <- NA

b2007$incomg[b2007$incomg == "9"] <- NA

b2007 = 
  b2007 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2007 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2007.csv"))
```

``` r
b2008 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS08.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,aservist, asdrvist, asattack,asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2008$physhlth[b2008$physhlth == "88"] <- 0
b2008$physhlth[b2008$physhlth == "77"] <- NA
b2008$physhlth[b2008$physhlth == "99"] <- NA

b2008$menthlth[b2008$menthlth == "88"] <- 0
b2008$menthlth[b2008$menthlth == "77"] <- NA
b2008$menthlth[b2008$menthlth == "99"] <- NA

b2008$cvdcrhd4[b2008$cvdcrhd4 == "2"] <- 0
b2008$cvdcrhd4[b2008$cvdcrhd4 == "7"] <- NA
b2008$cvdcrhd4[b2008$cvdcrhd4 == "9"] <- NA

b2008$cvdinfr4[b2008$cvdinfr4 == "2"] <- 0
b2008$cvdinfr4[b2008$cvdinfr4 == "7"] <- NA
b2008$cvdinfr4[b2008$cvdinfr4 == "9"] <- NA

b2008$cvdstrk3[b2008$cvdstrk3 == "2"] <- 0
b2008$cvdstrk3[b2008$cvdstrk3 == "7"] <- NA
b2008$cvdstrk3[b2008$cvdstrk3 == "9"] <- NA

b2008$asthma2[b2008$asthma2 == "2"] <- 0
b2008$asthma2[b2008$asthma2 == "7"] <- NA
b2008$asthma2[b2008$asthma2 == "9"] <- NA

b2008$asthnow[b2008$asthnow == "2"] <- 0
b2008$asthnow[b2008$asthnow == "7"] <- NA
b2008$asthnow[b2008$asthnow == "9"] <- NA

b2008$educag[b2008$educag == "9"] <- NA

b2008$sex[b2008$sex == "2"] <- 0

b2008$smoker3[b2008$smoker3 == "9"] <- NA

b2008$asthmage[b2008$asthmage == "99"] <- NA
b2008$asthmage[b2008$asthmage == "98"] <- NA
b2008$asthmage[b2008$asthmage == "97"] <- 10

b2008$asattack[b2008$asattack == "2"] <- 0
b2008$asattack[b2008$asattack == "7"] <- NA
b2008$asattack[b2008$asattack == "9"] <- NA

b2008$aservist[b2008$aservist == "88"] <- 0
b2008$aservist[b2008$aservist == "98"] <- NA
b2008$aservist[b2008$aservist == "99"] <- NA

b2008$asdrvist[b2008$asdrvist == "88"] <- 0
b2008$asdrvist[b2008$asdrvist == "98"] <- NA
b2008$asdrvist[b2008$asdrvist == "99"] <- NA

b2008$asymptom[b2008$asymptom == "8"] <- 0
b2008$asymptom[b2008$asymptom == "7"] <- NA
b2008$asymptom[b2008$asymptom == "9"] <- NA

b2008$asthmst[b2008$asthmst == "9"] <- NA

b2008$ltasthm[b2008$ltasthm == "9"] <- NA

b2008$casthma[b2008$casthma == "9"] <- NA

b2008$race2[b2008$race2 == "9"] <- NA

b2008$ageg5yr[b2008$ageg5yr == "14"] <- NA

b2008$incomg[b2008$incomg == "9"] <- NA

b2008 = 
  b2008 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2008 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2008.csv"))
```

``` r
b2009 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS09.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack,aservist,asdrvist,asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2009$physhlth[b2009$physhlth == "88"] <- 0
b2009$physhlth[b2009$physhlth == "77"] <- NA
b2009$physhlth[b2009$physhlth == "99"] <- NA

b2009$menthlth[b2009$menthlth == "88"] <- 0
b2009$menthlth[b2009$menthlth == "77"] <- NA
b2009$menthlth[b2009$menthlth == "99"] <- NA

b2009$cvdcrhd4[b2009$cvdcrhd4 == "2"] <- 0
b2009$cvdcrhd4[b2009$cvdcrhd4 == "7"] <- NA
b2009$cvdcrhd4[b2009$cvdcrhd4 == "9"] <- NA

b2009$cvdinfr4[b2009$cvdinfr4 == "2"] <- 0
b2009$cvdinfr4[b2009$cvdinfr4 == "7"] <- NA
b2009$cvdinfr4[b2009$cvdinfr4 == "9"] <- NA

b2009$cvdstrk3[b2009$cvdstrk3 == "2"] <- 0
b2009$cvdstrk3[b2009$cvdstrk3 == "7"] <- NA
b2009$cvdstrk3[b2009$cvdstrk3 == "9"] <- NA

b2009$asthma2[b2009$asthma2 == "2"] <- 0
b2009$asthma2[b2009$asthma2 == "7"] <- NA
b2009$asthma2[b2009$asthma2 == "9"] <- NA

b2009$asthnow[b2009$asthnow == "2"] <- 0
b2009$asthnow[b2009$asthnow == "7"] <- NA
b2009$asthnow[b2009$asthnow == "9"] <- NA

b2009$educag[b2009$educag == "9"] <- NA

b2009$sex[b2009$sex == "2"] <- 0

b2009$smoker3[b2009$smoker3 == "9"] <- NA

b2009$asthmage[b2009$asthmage == "99"] <- NA
b2009$asthmage[b2009$asthmage == "98"] <- NA
b2009$asthmage[b2009$asthmage == "97"] <- 10

b2009$asattack[b2009$asattack == "2"] <- 0
b2009$asattack[b2009$asattack == "7"] <- NA
b2009$asattack[b2009$asattack == "9"] <- NA

b2009$aservist[b2009$aservist == "88"] <- 0
b2009$aservist[b2009$aservist == "98"] <- NA
b2009$aservist[b2009$aservist == "99"] <- NA

b2009$asdrvist[b2009$asdrvist == "88"] <- 0
b2009$asdrvist[b2009$asdrvist == "98"] <- NA
b2009$asdrvist[b2009$asdrvist == "99"] <- NA

b2009$asymptom[b2009$asymptom == "8"] <- 0
b2009$asymptom[b2009$asymptom == "7"] <- NA
b2009$asymptom[b2009$asymptom == "9"] <- NA

b2009$asthmst[b2009$asthmst == "9"] <- NA

b2009$ltasthm[b2009$ltasthm == "9"] <- NA

b2009$casthma[b2009$casthma == "9"] <- NA

b2009$race2[b2009$race2 == "9"] <- NA

b2009$ageg5yr[b2009$ageg5yr == "14"] <- NA

b2009$incomg[b2009$incomg == "9"] <- NA

b2009 = 
  b2009 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2009 %>% 
write.csv(file = ("./data/brfss/brfss_ny_2009.csv"))
```

``` r
b2010 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/CDBRFS10.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode, year = iyear, month = imonth, day = iday, asthma2, asthnow, asthmage,asattack,aservist, asdrvist, asymptom, asthmst, ltasthm, casthma, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2010$physhlth[b2010$physhlth == "88"] <- 0
b2010$physhlth[b2010$physhlth == "77"] <- NA
b2010$physhlth[b2010$physhlth == "99"] <- NA

b2010$menthlth[b2010$menthlth == "88"] <- 0
b2010$menthlth[b2010$menthlth == "77"] <- NA
b2010$menthlth[b2010$menthlth == "99"] <- NA

b2010$cvdcrhd4[b2010$cvdcrhd4 == "2"] <- 0
b2010$cvdcrhd4[b2010$cvdcrhd4 == "7"] <- NA
b2010$cvdcrhd4[b2010$cvdcrhd4 == "9"] <- NA

b2010$cvdinfr4[b2010$cvdinfr4 == "2"] <- 0
b2010$cvdinfr4[b2010$cvdinfr4 == "7"] <- NA
b2010$cvdinfr4[b2010$cvdinfr4 == "9"] <- NA

b2010$cvdstrk3[b2010$cvdstrk3 == "2"] <- 0
b2010$cvdstrk3[b2010$cvdstrk3 == "7"] <- NA
b2010$cvdstrk3[b2010$cvdstrk3 == "9"] <- NA

b2010$asthma2[b2010$asthma2 == "2"] <- 0
b2010$asthma2[b2010$asthma2 == "7"] <- NA
b2010$asthma2[b2010$asthma2 == "9"] <- NA

b2010$asthnow[b2010$asthnow == "2"] <- 0
b2010$asthnow[b2010$asthnow == "7"] <- NA
b2010$asthnow[b2010$asthnow == "9"] <- NA

b2010$educag[b2010$educag == "9"] <- NA

b2010$sex[b2010$sex == "2"] <- 0

b2010$smoker3[b2010$smoker3 == "9"] <- NA

b2010$asthmage[b2010$asthmage == "99"] <- NA
b2010$asthmage[b2010$asthmage == "98"] <- NA
b2010$asthmage[b2010$asthmage == "97"] <- 10

b2010$asattack[b2010$asattack == "2"] <- 0
b2010$asattack[b2010$asattack == "7"] <- NA
b2010$asattack[b2010$asattack == "9"] <- NA

b2010$aservist[b2010$aservist == "88"] <- 0
b2010$aservist[b2010$aservist == "98"] <- NA
b2010$aservist[b2010$aservist == "99"] <- NA

b2010$asdrvist[b2010$asdrvist == "88"] <- 0
b2010$asdrvist[b2010$asdrvist == "98"] <- NA
b2010$asdrvist[b2010$asdrvist == "99"] <- NA

b2010$asymptom[b2010$asymptom == "8"] <- 0
b2010$asymptom[b2010$asymptom == "7"] <- NA
b2010$asymptom[b2010$asymptom == "9"] <- NA

b2010$asthmst[b2010$asthmst == "9"] <- NA

b2010$ltasthm[b2010$ltasthm == "9"] <- NA

b2010$casthma[b2010$casthma == "9"] <- NA

b2010$race2[b2010$race2 == "9"] <- NA

b2010$ageg5yr[b2010$ageg5yr == "14"] <- NA

b2010$incomg[b2010$incomg == "9"] <- NA

b2010 =
  b2010 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma2, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthmst, asthma_history = ltasthm, asthma_current = casthma, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2010 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2010.csv"))
```

``` r
b2011 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/LLCP2011.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode1, year = iyear, month = imonth, day = iday, asthma3, asthnow, asthmage,asattack,aservist, asdrvist, asymptom, asthms1, ltasth1, casthm1, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2011$physhlth[b2011$physhlth == "88"] <- 0
b2011$physhlth[b2011$physhlth == "77"] <- NA
b2011$physhlth[b2011$physhlth == "99"] <- NA

b2011$menthlth[b2011$menthlth == "88"] <- 0
b2011$menthlth[b2011$menthlth == "77"] <- NA
b2011$menthlth[b2011$menthlth == "99"] <- NA

b2011$cvdcrhd4[b2011$cvdcrhd4 == "2"] <- 0
b2011$cvdcrhd4[b2011$cvdcrhd4 == "7"] <- NA
b2011$cvdcrhd4[b2011$cvdcrhd4 == "9"] <- NA

b2011$cvdinfr4[b2011$cvdinfr4 == "2"] <- 0
b2011$cvdinfr4[b2011$cvdinfr4 == "7"] <- NA
b2011$cvdinfr4[b2011$cvdinfr4 == "9"] <- NA

b2011$cvdstrk3[b2011$cvdstrk3 == "2"] <- 0
b2011$cvdstrk3[b2011$cvdstrk3 == "7"] <- NA
b2011$cvdstrk3[b2011$cvdstrk3 == "9"] <- NA

b2011$asthma3[b2011$asthma3 == "2"] <- 0
b2011$asthma3[b2011$asthma3 == "7"] <- NA
b2011$asthma3[b2011$asthma3 == "9"] <- NA

b2011$asthnow[b2011$asthnow == "2"] <- 0
b2011$asthnow[b2011$asthnow == "7"] <- NA
b2011$asthnow[b2011$asthnow == "9"] <- NA

b2011$educag[b2011$educag == "9"] <- NA

b2011$sex[b2011$sex == "2"] <- 0

b2011$smoker3[b2011$smoker3 == "9"] <- NA

b2011$asthmage[b2011$asthmage == "99"] <- NA
b2011$asthmage[b2011$asthmage == "98"] <- NA
b2011$asthmage[b2011$asthmage == "97"] <- 10

b2011$asattack[b2011$asattack == "2"] <- 0
b2011$asattack[b2011$asattack == "7"] <- NA
b2011$asattack[b2011$asattack == "9"] <- NA

b2011$aservist[b2011$aservist == "88"] <- 0
b2011$aservist[b2011$aservist == "98"] <- NA
b2011$aservist[b2011$aservist == "99"] <- NA

b2011$asdrvist[b2011$asdrvist == "88"] <- 0
b2011$asdrvist[b2011$asdrvist == "98"] <- NA
b2011$asdrvist[b2011$asdrvist == "99"] <- NA

b2011$asymptom[b2011$asymptom == "8"] <- 0
b2011$asymptom[b2011$asymptom == "7"] <- NA
b2011$asymptom[b2011$asymptom == "9"] <- NA

b2011$asthms1[b2011$asthms1 == "9"] <- NA

b2011$ltasth1[b2011$ltasth1 == "9"] <- NA

b2011$casthm1[b2011$casthm1 == "9"] <- NA

b2011$race2[b2011$race2 == "9"] <- NA

b2011$ageg5yr[b2011$ageg5yr == "14"] <- NA

b2011$incomg[b2011$incomg == "9"] <- NA

b2011 = 
  b2011 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma3, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthms1, asthma_history = ltasth1, asthma_current = casthm1, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg) 

b2011 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2011.csv"))
```

``` r
b2012 = read_xpt("C:/Users/Suning Zhao/Desktop/CU/Fall 2022/Data Science/Final Project/Original datasets/LLCP2012.XPT") %>% 
  janitor::clean_names() %>% 
  filter(state == 36) %>% 
  select(state_code = state, county_code = ctycode1, year = iyear, month = imonth, day = iday, asthma3, asthnow, asthmage,asattack,aservist, asdrvist, asymptom, asthms1, ltasth1, casthm1, physhlth, menthlth, cvdcrhd4, cvdinfr4, cvdstrk3, educag,sex, smoker3, race2, ageg5yr, incomg) %>% 
  drop_na(county_code) 

b2012$physhlth[b2012$physhlth == "88"] <- 0
b2012$physhlth[b2012$physhlth == "77"] <- NA
b2012$physhlth[b2012$physhlth == "99"] <- NA

b2012$menthlth[b2012$menthlth == "88"] <- 0
b2012$menthlth[b2012$menthlth == "77"] <- NA
b2012$menthlth[b2012$menthlth == "99"] <- NA

b2012$cvdcrhd4[b2012$cvdcrhd4 == "2"] <- 0
b2012$cvdcrhd4[b2012$cvdcrhd4 == "7"] <- NA
b2012$cvdcrhd4[b2012$cvdcrhd4 == "9"] <- NA

b2012$cvdinfr4[b2012$cvdinfr4 == "2"] <- 0
b2012$cvdinfr4[b2012$cvdinfr4 == "7"] <- NA
b2012$cvdinfr4[b2012$cvdinfr4 == "9"] <- NA

b2012$cvdstrk3[b2012$cvdstrk3 == "2"] <- 0
b2012$cvdstrk3[b2012$cvdstrk3 == "7"] <- NA
b2012$cvdstrk3[b2012$cvdstrk3 == "9"] <- NA

b2012$asthma3[b2012$asthma3 == "2"] <- 0
b2012$asthma3[b2012$asthma3 == "7"] <- NA
b2012$asthma3[b2012$asthma3 == "9"] <- NA

b2012$asthnow[b2012$asthnow == "2"] <- 0
b2012$asthnow[b2012$asthnow == "7"] <- NA
b2012$asthnow[b2012$asthnow == "9"] <- NA

b2012$educag[b2012$educag == "9"] <- NA

b2012$sex[b2012$sex == "2"] <- 0

b2012$smoker3[b2012$smoker3 == "9"] <- NA

b2012$asthmage[b2012$asthmage == "99"] <- NA
b2012$asthmage[b2012$asthmage == "98"] <- NA
b2012$asthmage[b2012$asthmage == "97"] <- 10

b2012$asattack[b2012$asattack == "2"] <- 0
b2012$asattack[b2012$asattack == "7"] <- NA
b2012$asattack[b2012$asattack == "9"] <- NA

b2012$aservist[b2012$aservist == "88"] <- 0
b2012$aservist[b2012$aservist == "98"] <- NA
b2012$aservist[b2012$aservist == "99"] <- NA

b2012$asdrvist[b2012$asdrvist == "88"] <- 0
b2012$asdrvist[b2012$asdrvist == "98"] <- NA
b2012$asdrvist[b2012$asdrvist == "99"] <- NA

b2012$asymptom[b2012$asymptom == "8"] <- 0
b2012$asymptom[b2012$asymptom == "7"] <- NA
b2012$asymptom[b2012$asymptom == "9"] <- NA

b2012$asthms1[b2012$asthms1 == "9"] <- NA

b2012$ltasth1[b2012$ltasth1 == "9"] <- NA

b2012$casthm1[b2012$casthm1 == "9"] <- NA

b2012$race2[b2012$race2 == "9"] <- NA

b2012$ageg5yr[b2012$ageg5yr == "14"] <- NA

b2012$incomg[b2012$incomg == "9"] <- NA

b2012 = 
  b2012 %>% 
  select(state_code, county_code, year, month, day, asthma = asthma3, asthma_now = asthnow, asthma_age = asthmage,asthma_attack = asattack,asthma_symptom = asymptom, asthma_status = asthms1, asthma_history = ltasth1, asthma_current = casthm1, asthma_emergency = aservist, asthma_visit = asdrvist, physical_health = physhlth, mental_health = menthlth, coronary_heart_disease = cvdcrhd4, heart_attack = cvdinfr4, stroke = cvdstrk3, education = educag,sex, smoker = smoker3, race = race2, age = ageg5yr, income = incomg)

b2012 %>% 
  write.csv(file = ("./data/brfss/brfss_ny_2012.csv"))
```

``` r
brfss_indi =  
  bind_rows(b2003, b2004, b2005, b2006, b2007, b2008, b2009, b2010, b2011, b2012) %>%  
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  )

write.csv(brfss_indi, file = ("./data/brfss_indi.csv"))
```

``` r
brfs_for_merge = read_csv("./data/brfss_indi.csv") %>% 
  mutate(
    date = as.Date(paste(year,month,day,sep="-"),"%Y-%m-%d")
  ) 
```

    ## New names:
    ## Rows: 65143 Columns: 27
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," dbl
    ## (27): ...1, state_code, county_code, year, month, day, asthma, asthma_no...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
air_daily1 = 
  read_csv("./data/air_daily.csv") %>%
  mutate(county_code = as.numeric(county_code))
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

``` r
brfss_with_air = 
  left_join(brfs_for_merge, air_daily1, by = c("county_code", "date"))

brfss_with_air$county_code[brfss_with_air$county_code == "1"] <- "001"
brfss_with_air$county_code[brfss_with_air$county_code == "5"] <- "005"
brfss_with_air$county_code[brfss_with_air$county_code == "7"] <- "007"
brfss_with_air$county_code[brfss_with_air$county_code == "13"] <- "013"
brfss_with_air$county_code[brfss_with_air$county_code == "15"] <- "015"
brfss_with_air$county_code[brfss_with_air$county_code == "27"] <- "027"
brfss_with_air$county_code[brfss_with_air$county_code == "29"] <- "029"
brfss_with_air$county_code[brfss_with_air$county_code == "31"] <- "031"
brfss_with_air$county_code[brfss_with_air$county_code == "33"] <- "033"
brfss_with_air$county_code[brfss_with_air$county_code == "43"] <- "043"
brfss_with_air$county_code[brfss_with_air$county_code == "45"] <- "045"
brfss_with_air$county_code[brfss_with_air$county_code == "47"] <- "047"
brfss_with_air$county_code[brfss_with_air$county_code == "53"] <- "053"
brfss_with_air$county_code[brfss_with_air$county_code == "55"] <- "055"
brfss_with_air$county_code[brfss_with_air$county_code == "59"] <- "059"
brfss_with_air$county_code[brfss_with_air$county_code == "61"] <- "061"
brfss_with_air$county_code[brfss_with_air$county_code == "63"] <- "063"
brfss_with_air$county_code[brfss_with_air$county_code == "65"] <- "065"
brfss_with_air$county_code[brfss_with_air$county_code == "67"] <- "067"
brfss_with_air$county_code[brfss_with_air$county_code == "71"] <- "071"
brfss_with_air$county_code[brfss_with_air$county_code == "75"] <- "075"
brfss_with_air$county_code[brfss_with_air$county_code == "79"] <- "079"
brfss_with_air$county_code[brfss_with_air$county_code == "81"] <- "081"
brfss_with_air$county_code[brfss_with_air$county_code == "83"] <- "083"
brfss_with_air$county_code[brfss_with_air$county_code == "85"] <- "085"
brfss_with_air$county_code[brfss_with_air$county_code == "87"] <- "087"
brfss_with_air$county_code[brfss_with_air$county_code == "89"] <- "089"
brfss_with_air$county_code[brfss_with_air$county_code == "91"] <- "091"
brfss_with_air$county_code[brfss_with_air$county_code == "93"] <- "093"

write.csv(brfss_with_air, file = ("./data/brfss_with_air.csv"))
```

``` r
air_daily2 = 
  air_daily1 %>% 
  separate(col = date, into = c('year','month','day'), sep = "-" , convert = TRUE) %>%
  group_by(county_code, county, year, month) %>% 
  summarize(
    mean_aqi_month = mean(aqi),
    max_aqi_month = max(aqi),
    min_aqi_month = min(aqi),
    mean_ozone_month = mean(mean_ozone),
    mean_pm2_5_month = mean(mean_pm2_5),
    mean_pm10_month = mean(mean_pm10),
    mean_co_month = mean(mean_co),
    mean_no2_month = mean(mean_no2),
    mean_so2_month = mean(mean_so2)
  ) 
```

    ## `summarise()` has grouped output by 'county_code', 'county', 'year'. You can
    ## override using the `.groups` argument.

``` r
brfss_with_air2 = 
  left_join(brfs_for_merge, air_daily2, by = c("county_code", "year","month"))

brfss_with_air2$county_code[brfss_with_air2$county_code == "1"] <- "001"
brfss_with_air2$county_code[brfss_with_air2$county_code == "5"] <- "005"
brfss_with_air2$county_code[brfss_with_air2$county_code == "7"] <- "007"
brfss_with_air2$county_code[brfss_with_air2$county_code == "13"] <- "013"
brfss_with_air2$county_code[brfss_with_air2$county_code == "15"] <- "015"
brfss_with_air2$county_code[brfss_with_air2$county_code == "27"] <- "027"
brfss_with_air2$county_code[brfss_with_air2$county_code == "29"] <- "029"
brfss_with_air2$county_code[brfss_with_air2$county_code == "31"] <- "031"
brfss_with_air2$county_code[brfss_with_air2$county_code == "33"] <- "033"
brfss_with_air2$county_code[brfss_with_air2$county_code == "43"] <- "043"
brfss_with_air2$county_code[brfss_with_air2$county_code == "45"] <- "045"
brfss_with_air2$county_code[brfss_with_air2$county_code == "47"] <- "047"
brfss_with_air2$county_code[brfss_with_air2$county_code == "53"] <- "053"
brfss_with_air2$county_code[brfss_with_air2$county_code == "55"] <- "055"
brfss_with_air2$county_code[brfss_with_air2$county_code == "59"] <- "059"
brfss_with_air2$county_code[brfss_with_air2$county_code == "61"] <- "061"
brfss_with_air2$county_code[brfss_with_air2$county_code == "63"] <- "063"
brfss_with_air2$county_code[brfss_with_air2$county_code == "65"] <- "065"
brfss_with_air2$county_code[brfss_with_air2$county_code == "67"] <- "067"
brfss_with_air2$county_code[brfss_with_air2$county_code == "71"] <- "071"
brfss_with_air2$county_code[brfss_with_air2$county_code == "75"] <- "075"
brfss_with_air2$county_code[brfss_with_air2$county_code == "79"] <- "079"
brfss_with_air2$county_code[brfss_with_air2$county_code == "81"] <- "081"
brfss_with_air2$county_code[brfss_with_air2$county_code == "83"] <- "083"
brfss_with_air2$county_code[brfss_with_air2$county_code == "85"] <- "085"
brfss_with_air2$county_code[brfss_with_air2$county_code == "87"] <- "087"
brfss_with_air2$county_code[brfss_with_air2$county_code == "89"] <- "089"
brfss_with_air2$county_code[brfss_with_air2$county_code == "91"] <- "091"
brfss_with_air2$county_code[brfss_with_air2$county_code == "93"] <- "093"

write.csv(brfss_with_air2, file = ("./data/brfss_with_air2.csv"))
```
