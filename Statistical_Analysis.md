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
    ## Rows: 65143 Columns: 46
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): state, county, category, defining_parameter dbl (41): ...1, ...2,
    ## state_code.x, county_code, year, month, day, asthma, ... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `...1.x` -> `...2`
    ## • `...1.y` -> `...28`

\#linear regression

``` r
linear_fit_1a =  asthma_df %>%
  lm(asthma ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .)
```

``` r
linear_fit_1a =
  linear_fit_1a %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

``` r
fit_linear1b = 
  asthma_df %>% 
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .) 
```

``` r
fit_linear1b %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term               | log_OR |    OR | p.value |
|:-------------------|-------:|------:|--------:|
| (Intercept)        |  0.706 | 2.026 |   0.000 |
| mental_health      |  0.006 | 1.006 |   0.016 |
| physical_health    |  0.005 | 1.005 |   0.033 |
| sex                | -0.111 | 0.895 |   0.010 |
| smoker2            |  0.223 | 1.250 |   0.010 |
| smoker3            |  0.109 | 1.115 |   0.089 |
| smoker4            |  0.067 | 1.070 |   0.252 |
| race2              | -0.117 | 0.890 |   0.090 |
| race3              | -0.229 | 0.795 |   0.109 |
| race4              | -0.413 | 0.662 |   0.412 |
| race5              |  0.264 | 1.302 |   0.322 |
| race6              | -0.137 | 0.872 |   0.387 |
| race7              |  0.054 | 1.056 |   0.667 |
| race8              | -0.015 | 0.985 |   0.831 |
| age2               | -0.015 | 0.985 |   0.884 |
| age3               |  0.109 | 1.115 |   0.276 |
| age4               |  0.104 | 1.110 |   0.300 |
| age5               |  0.040 | 1.041 |   0.674 |
| age6               | -0.074 | 0.929 |   0.469 |
| age7               | -0.087 | 0.917 |   0.374 |
| age8               | -0.030 | 0.970 |   0.769 |
| age9               | -0.046 | 0.955 |   0.679 |
| age10              | -0.125 | 0.883 |   0.282 |
| age11              | -0.280 | 0.756 |   0.030 |
| age12              | -0.201 | 0.818 |   0.123 |
| age13              | -0.160 | 0.852 |   0.305 |
| income2            | -0.014 | 0.987 |   0.845 |
| income3            | -0.039 | 0.962 |   0.613 |
| income4            | -0.131 | 0.877 |   0.089 |
| income5            | -0.132 | 0.877 |   0.045 |
| countyBronx        | -0.109 | 0.897 |   0.423 |
| countyChautauqua   | -0.015 | 0.985 |   0.930 |
| countyDutchess     | -0.323 | 0.724 |   0.037 |
| countyErie         | -0.199 | 0.819 |   0.095 |
| countyJefferson    | -0.452 | 0.636 |   0.033 |
| countyKings        | -0.220 | 0.802 |   0.067 |
| countyMonroe       | -0.137 | 0.872 |   0.260 |
| countyNassau       | -0.162 | 0.851 |   0.203 |
| countyNew York     | -0.170 | 0.843 |   0.148 |
| countyNiagara      | -0.246 | 0.782 |   0.120 |
| countyOneida       | -0.134 | 0.874 |   0.445 |
| countyOnondaga     | -0.196 | 0.822 |   0.130 |
| countyOrange       | -0.264 | 0.768 |   0.103 |
| countyOswego       |  0.093 | 1.097 |   0.729 |
| countyQueens       | -0.139 | 0.870 |   0.269 |
| countyRensselaer   | -0.233 | 0.792 |   0.162 |
| countyRichmond     | -0.036 | 0.964 |   0.863 |
| countySaratoga     | -0.392 | 0.676 |   0.015 |
| countySchenectady  | -0.199 | 0.819 |   0.283 |
| countySt. Lawrence | -0.255 | 0.775 |   0.422 |
| countySteuben      |  0.278 | 1.321 |   0.306 |
| countySuffolk      | -0.261 | 0.770 |   0.030 |
| countyTompkins     | -0.715 | 0.489 |   0.008 |
| countyUlster       | -0.221 | 0.801 |   0.169 |
| countyWayne        | -0.649 | 0.523 |   0.003 |
| countyWestchester  | -0.207 | 0.813 |   0.102 |
| aqi                |  0.001 | 1.001 |   0.269 |

\#logistic regression

``` r
fit_logistic1a = 
  asthma_df %>% 
  glm(asthma ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = ., family = binomial()) 
```

``` r
fit_logistic1a %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term               | log_OR |    OR | p.value |
|:-------------------|-------:|------:|--------:|
| (Intercept)        | -1.149 | 0.317 |   0.000 |
| mental_health      |  0.013 | 1.013 |   0.000 |
| physical_health    |  0.034 | 1.034 |   0.000 |
| sex                | -0.346 | 0.708 |   0.000 |
| smoker2            |  0.149 | 1.161 |   0.038 |
| smoker3            |  0.188 | 1.207 |   0.000 |
| smoker4            |  0.010 | 1.010 |   0.829 |
| race2              |  0.214 | 1.239 |   0.000 |
| race3              | -0.395 | 0.673 |   0.000 |
| race4              |  0.022 | 1.022 |   0.935 |
| race5              |  0.094 | 1.099 |   0.643 |
| race6              |  0.127 | 1.136 |   0.426 |
| race7              |  0.639 | 1.895 |   0.000 |
| race8              |  0.145 | 1.156 |   0.006 |
| age2               | -0.416 | 0.660 |   0.000 |
| age3               | -0.352 | 0.703 |   0.000 |
| age4               | -0.470 | 0.625 |   0.000 |
| age5               | -0.545 | 0.580 |   0.000 |
| age6               | -0.638 | 0.529 |   0.000 |
| age7               | -0.607 | 0.545 |   0.000 |
| age8               | -0.599 | 0.550 |   0.000 |
| age9               | -0.673 | 0.510 |   0.000 |
| age10              | -0.752 | 0.472 |   0.000 |
| age11              | -0.821 | 0.440 |   0.000 |
| age12              | -0.981 | 0.375 |   0.000 |
| age13              | -1.252 | 0.286 |   0.000 |
| income2            | -0.253 | 0.776 |   0.000 |
| income3            | -0.214 | 0.807 |   0.000 |
| income4            | -0.317 | 0.728 |   0.000 |
| income5            | -0.283 | 0.754 |   0.000 |
| countyBronx        | -0.062 | 0.940 |   0.565 |
| countyBroome       | -0.932 | 0.394 |   0.387 |
| countyChautauqua   | -0.221 | 0.802 |   0.135 |
| countyChemung      | -0.010 | 0.990 |   0.964 |
| countyDutchess     | -0.142 | 0.868 |   0.285 |
| countyErie         | -0.156 | 0.856 |   0.127 |
| countyEssex        |  0.150 | 1.161 |   0.560 |
| countyFranklin     |  0.569 | 1.767 |   0.016 |
| countyHerkimer     | -0.005 | 0.995 |   0.984 |
| countyJefferson    |  0.201 | 1.222 |   0.238 |
| countyKings        | -0.193 | 0.824 |   0.052 |
| countyMadison      |  0.212 | 1.236 |   0.340 |
| countyMonroe       |  0.003 | 1.003 |   0.978 |
| countyNassau       | -0.159 | 0.853 |   0.129 |
| countyNew York     | -0.115 | 0.891 |   0.242 |
| countyNiagara      | -0.208 | 0.812 |   0.129 |
| countyOneida       |  0.062 | 1.064 |   0.646 |
| countyOnondaga     |  0.005 | 1.005 |   0.965 |
| countyOrange       |  0.159 | 1.172 |   0.206 |
| countyOswego       |  0.177 | 1.194 |   0.312 |
| countyPutnam       |  0.104 | 1.110 |   0.638 |
| countyQueens       | -0.380 | 0.684 |   0.000 |
| countyRensselaer   |  0.139 | 1.150 |   0.336 |
| countyRichmond     | -0.196 | 0.822 |   0.143 |
| countyRockland     | -0.032 | 0.969 |   0.881 |
| countySaratoga     |  0.029 | 1.029 |   0.830 |
| countySchenectady  |  0.066 | 1.068 |   0.668 |
| countySt. Lawrence | -0.543 | 0.581 |   0.159 |
| countySteuben      |  0.102 | 1.108 |   0.569 |
| countySuffolk      | -0.205 | 0.814 |   0.043 |
| countyTompkins     |  0.250 | 1.284 |   0.149 |
| countyUlster       | -0.018 | 0.982 |   0.899 |
| countyWayne        | -0.383 | 0.682 |   0.127 |
| countyWestchester  | -0.178 | 0.837 |   0.102 |
| aqi                |  0.001 | 1.001 |   0.221 |

``` r
fit_logistic1b = 
  asthma_df %>% 
  glm(asthma_now ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = ., family = binomial()) 
```

``` r
fit_logistic1b %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term               | log_OR |        OR | p.value |
|:-------------------|-------:|----------:|--------:|
| (Intercept)        |  0.804 |     2.234 |   0.001 |
| mental_health      |  0.005 |     1.005 |   0.227 |
| physical_health    |  0.034 |     1.034 |   0.000 |
| sex                | -0.401 |     0.670 |   0.000 |
| smoker2            | -0.207 |     0.813 |   0.172 |
| smoker3            | -0.098 |     0.907 |   0.365 |
| smoker4            | -0.106 |     0.900 |   0.298 |
| race2              |  0.081 |     1.084 |   0.429 |
| race3              |  0.160 |     1.173 |   0.471 |
| race4              | -0.838 |     0.432 |   0.113 |
| race5              |  0.123 |     1.131 |   0.788 |
| race6              | -0.366 |     0.693 |   0.252 |
| race7              | -0.165 |     0.848 |   0.449 |
| race8              | -0.139 |     0.870 |   0.194 |
| age2               |  0.269 |     1.309 |   0.106 |
| age3               |  0.367 |     1.443 |   0.018 |
| age4               |  0.447 |     1.564 |   0.003 |
| age5               |  0.504 |     1.655 |   0.001 |
| age6               |  0.556 |     1.743 |   0.000 |
| age7               |  0.544 |     1.722 |   0.000 |
| age8               |  0.540 |     1.717 |   0.000 |
| age9               |  0.421 |     1.524 |   0.007 |
| age10              |  0.539 |     1.714 |   0.001 |
| age11              |  0.449 |     1.567 |   0.012 |
| age12              |  0.516 |     1.676 |   0.009 |
| age13              |  0.283 |     1.328 |   0.156 |
| income2            | -0.145 |     0.865 |   0.230 |
| income3            | -0.475 |     0.622 |   0.000 |
| income4            | -0.344 |     0.709 |   0.007 |
| income5            | -0.477 |     0.620 |   0.000 |
| countyBronx        | -0.040 |     0.960 |   0.856 |
| countyBroome       | 10.509 | 36654.712 |   0.957 |
| countyChautauqua   |  0.330 |     1.390 |   0.308 |
| countyChemung      |  0.078 |     1.081 |   0.867 |
| countyDutchess     | -0.239 |     0.787 |   0.377 |
| countyErie         |  0.183 |     1.200 |   0.390 |
| countyEssex        |  0.040 |     1.041 |   0.939 |
| countyFranklin     |  0.152 |     1.164 |   0.754 |
| countyHerkimer     |  0.860 |     2.362 |   0.190 |
| countyJefferson    |  0.484 |     1.622 |   0.197 |
| countyKings        | -0.037 |     0.964 |   0.857 |
| countyMadison      | -1.132 |     0.322 |   0.008 |
| countyMonroe       |  0.008 |     1.008 |   0.969 |
| countyNassau       | -0.184 |     0.832 |   0.388 |
| countyNew York     | -0.159 |     0.853 |   0.428 |
| countyNiagara      |  0.168 |     1.183 |   0.575 |
| countyOneida       |  0.172 |     1.187 |   0.547 |
| countyOnondaga     |  0.276 |     1.318 |   0.248 |
| countyOrange       | -0.260 |     0.771 |   0.300 |
| countyOswego       |  0.497 |     1.644 |   0.215 |
| countyPutnam       |  0.147 |     1.159 |   0.748 |
| countyQueens       | -0.128 |     0.880 |   0.544 |
| countyRensselaer   |  0.034 |     1.034 |   0.910 |
| countyRichmond     | -0.067 |     0.935 |   0.807 |
| countyRockland     | -0.181 |     0.835 |   0.678 |
| countySaratoga     |  0.240 |     1.271 |   0.393 |
| countySchenectady  | -0.089 |     0.915 |   0.780 |
| countySt. Lawrence | -0.575 |     0.563 |   0.458 |
| countySteuben      |  0.681 |     1.976 |   0.117 |
| countySuffolk      |  0.054 |     1.055 |   0.797 |
| countyTompkins     | -0.247 |     0.781 |   0.471 |
| countyUlster       | -0.014 |     0.987 |   0.963 |
| countyWayne        |  0.507 |     1.660 |   0.443 |
| countyWestchester  | -0.041 |     0.959 |   0.852 |
| aqi                |  0.000 |     1.000 |   0.850 |

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
    ## Rows: 65143 Columns: 37
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): county dbl (35): ...1, ...2, state_code, county_code, year, month, day,
    ## asthma, as... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `...1` -> `...2`

``` r
linear_fit_2a =  asthma_df2 %>%
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_aqi_month, data = .)
```

``` r
linear_fit_2a =
  linear_fit_2a %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
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
