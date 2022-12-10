Statistical Analysis
================

``` r
asthma_df = 
  read_csv("data/brfss_with_air.csv") %>%
  mutate(
    #outcome
    asthma = as.numeric(asthma),
    asthma_now = as.numeric(asthma_now),
    #predictors below
    aqi = as.numeric(aqi),
    mental_health = as.numeric(mental_health),
    physical_health = as.numeric(physical_health),
    county = as.factor(county),
    age = as.factor(age),
    race = as.factor(race),
    smoker = as.factor(smoker),
    income = as.factor(income)
    )
```

    ## New names:
    ## Rows: 65143 Columns: 48
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (5): county_code, state, county, category, defining_parameter dbl (42): ...1,
    ## ...2, state_code.x, year, month, day, asthma, asthma_now, a... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `...1.x` -> `...2`
    ## • `...1.y` -> `...30`

\#linear regression

``` r
linear_fit_1a =  asthma_df %>%
  lm(asthma ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .)
```

``` r
linear_fit_1a =
  linear_fit_1a %>% 
  broom::tidy()
```

``` r
fit_linear1b = 
  asthma_df %>% 
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .) 
```

``` r
fit_linear1b %>% 
  broom::tidy()
```

    ## # A tibble: 57 × 5
    ##    term            estimate std.error statistic    p.value
    ##    <chr>              <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 (Intercept)      0.706     0.152       4.63  0.00000443
    ##  2 mental_health    0.00558   0.00230     2.42  0.0158    
    ##  3 physical_health  0.00461   0.00216     2.14  0.0330    
    ##  4 sex             -0.111     0.0428     -2.58  0.00999   
    ##  5 smoker2          0.223     0.0869      2.57  0.0104    
    ##  6 smoker3          0.109     0.0637      1.71  0.0886    
    ##  7 smoker4          0.0673    0.0587      1.15  0.252     
    ##  8 race2           -0.117     0.0688     -1.70  0.0898    
    ##  9 race3           -0.229     0.143      -1.60  0.109     
    ## 10 race4           -0.413     0.503      -0.821 0.412     
    ## # … with 47 more rows

``` r
asthma_df2 = 
  read_csv("data/brfss_with_air2.csv") %>%
  mutate(
    #outcome
    asthma = as.numeric(asthma),
    asthma_now = as.numeric(asthma_now),
    #predictors below
    mean_aqi_month = as.numeric(mean_aqi_month),
    mental_health = as.numeric(mental_health),
    physical_health = as.numeric(physical_health),
    county = as.factor(county),
    age = as.factor(age),
    race = as.factor(race),
    smoker = as.factor(smoker),
    income = as.factor(income)
    )
```

    ## New names:
    ## Rows: 65143 Columns: 39
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): county_code, county dbl (36): ...1, ...2, state_code, year, month, day,
    ## asthma, asthma_now, ast... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `...1` -> `...2`

``` r
linear_fit_2_emergency =  asthma_df2 %>%
  lm(asthma_emergency ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_aqi_month, data = .)
```

``` r
linear_fit_2_emergency =
  linear_fit_2_emergency %>% 
  broom::tidy()
```

``` r
linear_fit_2attack =  asthma_df2 %>%
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_aqi_month, data = .)
```

``` r
linear_fit_2attack =
  linear_fit_2attack %>% 
  broom::tidy()
```

``` r
fit_logistic2a = 
  asthma_df2 %>% 
  glm(asthma ~ sex + smoker + race + age + income + county + mean_aqi_month, data = ., family = binomial()) 
```

``` r
fit_logistic2b = 
  asthma_df2 %>% 
  glm(asthma_now ~ year + sex + smoker + race + age + income + county + mean_aqi_month, data = ., family = binomial()) 
```

``` r
fit_logistic2b 
```

    ## 
    ## Call:  glm(formula = asthma_now ~ year + sex + smoker + race + age + 
    ##     income + county + mean_aqi_month, family = binomial(), data = .)
    ## 
    ## Coefficients:
    ##        (Intercept)                year                 sex             smoker2  
    ##         -5.185e+01           2.636e-02          -3.876e-01          -1.601e-01  
    ##            smoker3             smoker4               race2               race3  
    ##         -1.344e-01          -1.959e-01           3.784e-02           1.511e-01  
    ##              race4               race5               race6               race7  
    ##         -6.501e-01           3.307e-01          -1.588e-01          -1.858e-01  
    ##              race8                age2                age3                age4  
    ##         -1.142e-01           2.777e-01           4.232e-01           5.293e-01  
    ##               age5                age6                age7                age8  
    ##          5.870e-01           6.466e-01           6.565e-01           6.753e-01  
    ##               age9               age10               age11               age12  
    ##          5.566e-01           6.579e-01           5.591e-01           6.706e-01  
    ##              age13             income2             income3             income4  
    ##          5.719e-01          -2.281e-01          -5.621e-01          -5.106e-01  
    ##            income5         countyBronx        countyBroome    countyChautauqua  
    ##         -6.857e-01          -6.748e-03          -1.937e-02           4.590e-01  
    ##      countyChemung      countyDutchess          countyErie         countyEssex  
    ##         -5.610e-02          -2.596e-01           2.504e-01          -1.335e-02  
    ##     countyFranklin      countyHerkimer     countyJefferson         countyKings  
    ##          2.144e-01           9.384e-01           4.957e-01          -2.001e-02  
    ##      countyMadison        countyMonroe        countyNassau      countyNew York  
    ##         -9.997e-01          -1.293e-02          -1.130e-01          -1.421e-01  
    ##      countyNiagara        countyOneida      countyOnondaga        countyOrange  
    ##          1.885e-01           3.167e-01           3.178e-01          -1.549e-01  
    ##       countyOswego        countyPutnam        countyQueens    countyRensselaer  
    ##          3.687e-01           1.357e-01          -9.811e-02           1.621e-01  
    ##     countyRichmond      countyRockland      countySaratoga   countySchenectady  
    ##         -6.729e-02          -2.839e-01           3.094e-01          -6.634e-03  
    ## countySt. Lawrence       countySteuben       countySuffolk      countyTompkins  
    ##          2.734e-01           7.514e-01           1.261e-01          -1.966e-01  
    ##       countyUlster         countyWayne   countyWestchester      mean_aqi_month  
    ##         -1.481e-02           9.786e-01          -2.121e-03          -3.554e-04  
    ## 
    ## Degrees of Freedom: 6038 Total (i.e. Null);  5975 Residual
    ##   (59104 observations deleted due to missingness)
    ## Null Deviance:       7367 
    ## Residual Deviance: 7122  AIC: 7250

``` r
  fit_logistic2b  %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term               |  log_OR |    OR | p.value |
|:-------------------|--------:|------:|--------:|
| (Intercept)        | -51.853 | 0.000 |   0.017 |
| year               |   0.026 | 1.027 |   0.015 |
| sex                |  -0.388 | 0.679 |   0.000 |
| smoker2            |  -0.160 | 0.852 |   0.269 |
| smoker3            |  -0.134 | 0.874 |   0.187 |
| smoker4            |  -0.196 | 0.822 |   0.040 |
| race2              |   0.038 | 1.039 |   0.700 |
| race3              |   0.151 | 1.163 |   0.480 |
| race4              |  -0.650 | 0.522 |   0.202 |
| race5              |   0.331 | 1.392 |   0.452 |
| race6              |  -0.159 | 0.853 |   0.604 |
| race7              |  -0.186 | 0.830 |   0.362 |
| race8              |  -0.114 | 0.892 |   0.261 |
| age2               |   0.278 | 1.320 |   0.083 |
| age3               |   0.423 | 1.527 |   0.005 |
| age4               |   0.529 | 1.698 |   0.000 |
| age5               |   0.587 | 1.799 |   0.000 |
| age6               |   0.647 | 1.909 |   0.000 |
| age7               |   0.657 | 1.928 |   0.000 |
| age8               |   0.675 | 1.965 |   0.000 |
| age9               |   0.557 | 1.745 |   0.000 |
| age10              |   0.658 | 1.931 |   0.000 |
| age11              |   0.559 | 1.749 |   0.001 |
| age12              |   0.671 | 1.955 |   0.000 |
| age13              |   0.572 | 1.772 |   0.002 |
| income2            |  -0.228 | 0.796 |   0.042 |
| income3            |  -0.562 | 0.570 |   0.000 |
| income4            |  -0.511 | 0.600 |   0.000 |
| income5            |  -0.686 | 0.504 |   0.000 |
| countyBronx        |  -0.007 | 0.993 |   0.975 |
| countyBroome       |  -0.019 | 0.981 |   0.988 |
| countyChautauqua   |   0.459 | 1.582 |   0.145 |
| countyChemung      |  -0.056 | 0.945 |   0.894 |
| countyDutchess     |  -0.260 | 0.771 |   0.316 |
| countyErie         |   0.250 | 1.284 |   0.229 |
| countyEssex        |  -0.013 | 0.987 |   0.980 |
| countyFranklin     |   0.214 | 1.239 |   0.653 |
| countyHerkimer     |   0.938 | 2.556 |   0.147 |
| countyJefferson    |   0.496 | 1.642 |   0.169 |
| countyKings        |  -0.020 | 0.980 |   0.920 |
| countyMadison      |  -1.000 | 0.368 |   0.016 |
| countyMonroe       |  -0.013 | 0.987 |   0.950 |
| countyNassau       |  -0.113 | 0.893 |   0.584 |
| countyNew York     |  -0.142 | 0.868 |   0.469 |
| countyNiagara      |   0.188 | 1.207 |   0.517 |
| countyOneida       |   0.317 | 1.373 |   0.255 |
| countyOnondaga     |   0.318 | 1.374 |   0.173 |
| countyOrange       |  -0.155 | 0.856 |   0.510 |
| countyOswego       |   0.369 | 1.446 |   0.322 |
| countyPutnam       |   0.136 | 1.145 |   0.765 |
| countyQueens       |  -0.098 | 0.907 |   0.632 |
| countyRensselaer   |   0.162 | 1.176 |   0.577 |
| countyRichmond     |  -0.067 | 0.935 |   0.790 |
| countyRockland     |  -0.284 | 0.753 |   0.497 |
| countySaratoga     |   0.309 | 1.363 |   0.261 |
| countySchenectady  |  -0.007 | 0.993 |   0.983 |
| countySt. Lawrence |   0.273 | 1.314 |   0.439 |
| countySteuben      |   0.751 | 2.120 |   0.080 |
| countySuffolk      |   0.126 | 1.134 |   0.535 |
| countyTompkins     |  -0.197 | 0.821 |   0.543 |
| countyUlster       |  -0.015 | 0.985 |   0.958 |
| countyWayne        |   0.979 | 2.661 |   0.128 |
| countyWestchester  |  -0.002 | 0.998 |   0.992 |
| mean_aqi_month     |   0.000 | 1.000 |   0.898 |
