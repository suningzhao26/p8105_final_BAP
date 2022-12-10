Linear Regression
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

\#logistic regression \##logistic regression with asthma(ever had
asthma) as outcome

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

\##logistic regression with asthma now as outcome

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
