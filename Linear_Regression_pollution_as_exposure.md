Linear_Regression_Pollution
================

``` r
asthma_df2 = 
  read_csv("./data/brfss_with_air2.csv") %>%
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
linear_fit_so2_emergency =  asthma_df2 %>%
  lm(asthma_emergency ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_so2_month, data = .)
```

``` r
linear_fit_so2_emergency =
  linear_fit_so2_emergency %>% 
  broom::tidy()
```

``` r
print(linear_fit_so2_emergency, n=60)
```

    ## # A tibble: 44 × 5
    ##    term              estimate std.error statistic  p.value
    ##    <chr>                <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)        1.09       0.976     1.11   0.266   
    ##  2 mental_health      0.0237     0.0163    1.45   0.148   
    ##  3 physical_health    0.0130     0.0146    0.890  0.374   
    ##  4 sex               -0.263      0.288    -0.913  0.362   
    ##  5 smoker2            0.0415     0.598     0.0695 0.945   
    ##  6 smoker3            0.570      0.434     1.31   0.189   
    ##  7 smoker4            0.0926     0.399     0.232  0.816   
    ##  8 race2              0.982      0.514     1.91   0.0566  
    ##  9 race3             -0.638      0.885    -0.721  0.471   
    ## 10 race4             -1.34       2.89     -0.463  0.644   
    ## 11 race5             -0.549      2.05     -0.268  0.789   
    ## 12 race6              0.397      1.45      0.274  0.784   
    ## 13 race7              0.558      0.895     0.624  0.533   
    ## 14 race8              0.0293     0.532     0.0552 0.956   
    ## 15 age2              -1.56       0.767    -2.03   0.0429  
    ## 16 age3              -0.745      0.711    -1.05   0.295   
    ## 17 age4              -0.514      0.734    -0.700  0.484   
    ## 18 age5              -0.512      0.671    -0.763  0.446   
    ## 19 age6              -0.888      0.697    -1.27   0.203   
    ## 20 age7              -0.750      0.686    -1.09   0.275   
    ## 21 age8              -0.232      0.714    -0.324  0.746   
    ## 22 age9              -1.77       0.766    -2.31   0.0215  
    ## 23 age10             -0.876      0.811    -1.08   0.281   
    ## 24 age11             -1.71       0.837    -2.04   0.0422  
    ## 25 age12             -0.848      0.910    -0.933  0.352   
    ## 26 age13             -1.62       1.13     -1.43   0.153   
    ## 27 income2           -0.472      0.488    -0.967  0.334   
    ## 28 income3           -0.645      0.529    -1.22   0.224   
    ## 29 income4           -1.13       0.533    -2.12   0.0343  
    ## 30 income5           -1.51       0.445    -3.39   0.000759
    ## 31 countyBronx       -0.521      0.836    -0.623  0.533   
    ## 32 countyChautauqua  -0.418      1.01     -0.415  0.678   
    ## 33 countyErie         0.360      0.686     0.524  0.601   
    ## 34 countyMonroe       0.00983    0.700     0.0140 0.989   
    ## 35 countyNassau       0.476      0.713     0.667  0.505   
    ## 36 countyNew York    -0.185      0.763    -0.242  0.809   
    ## 37 countyNiagara      0.183      0.896     0.204  0.839   
    ## 38 countyOnondaga     0.321      0.744     0.431  0.667   
    ## 39 countyQueens       0.228      0.750     0.304  0.761   
    ## 40 countyRensselaer   0.864      0.969     0.891  0.373   
    ## 41 countySchenectady -0.375      1.06     -0.354  0.723   
    ## 42 countySuffolk     -0.0865     0.698    -0.124  0.901   
    ## 43 countyUlster       0.919      0.936     0.982  0.327   
    ## 44 mean_so2_month     0.166      0.0466    3.56   0.000406

``` r
linear_fit_co_emergency =  asthma_df2 %>%
  lm(asthma_emergency ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_co_month, data = .)
```

``` r
linear_fit_co_emergency =
  linear_fit_co_emergency %>% 
  broom::tidy()
```

``` r
print(linear_fit_co_emergency, n=60)
```

    ## # A tibble: 41 × 5
    ##    term              estimate std.error statistic p.value
    ##    <chr>                <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)        1.18       1.15      1.03   0.302  
    ##  2 mental_health      0.00829    0.0168    0.492  0.623  
    ##  3 physical_health    0.0232     0.0153    1.52   0.130  
    ##  4 sex               -0.0327     0.304    -0.108  0.914  
    ##  5 smoker2            0.0519     0.592     0.0877 0.930  
    ##  6 smoker3            0.721      0.448     1.61   0.109  
    ##  7 smoker4           -0.0844     0.407    -0.207  0.836  
    ##  8 race2              0.648      0.473     1.37   0.172  
    ##  9 race3             -0.627      0.875    -0.716  0.474  
    ## 10 race4             -1.16       2.96     -0.391  0.696  
    ## 11 race5             -1.47       2.90     -0.505  0.614  
    ## 12 race6             -0.246      1.00     -0.245  0.807  
    ## 13 race7              0.0705     0.866     0.0814 0.935  
    ## 14 race8              0.388      0.484     0.801  0.423  
    ## 15 age2              -1.26       0.700    -1.80   0.0733 
    ## 16 age3              -0.617      0.682    -0.905  0.366  
    ## 17 age4              -0.401      0.688    -0.583  0.560  
    ## 18 age5              -0.111      0.655    -0.170  0.865  
    ## 19 age6              -0.886      0.688    -1.29   0.199  
    ## 20 age7              -0.721      0.657    -1.10   0.273  
    ## 21 age8               0.0281     0.690     0.0408 0.967  
    ## 22 age9              -1.30       0.791    -1.64   0.101  
    ## 23 age10             -0.873      0.801    -1.09   0.276  
    ## 24 age11             -1.36       0.885    -1.54   0.124  
    ## 25 age12             -1.88       0.901    -2.08   0.0377 
    ## 26 age13             -1.45       1.23     -1.18   0.237  
    ## 27 income2           -0.0835     0.483    -0.173  0.863  
    ## 28 income3           -0.562      0.539    -1.04   0.298  
    ## 29 income4           -1.08       0.537    -2.02   0.0442 
    ## 30 income5           -1.39       0.457    -3.05   0.00242
    ## 31 countyBronx       -0.0495     0.946    -0.0524 0.958  
    ## 32 countyErie         0.146      0.705     0.207  0.836  
    ## 33 countyKings       -1.03       1.19     -0.870  0.385  
    ## 34 countyMonroe      -0.370      0.727    -0.509  0.611  
    ## 35 countyNew York     0.281      0.907     0.310  0.757  
    ## 36 countyNiagara     -0.136      0.994    -0.137  0.891  
    ## 37 countyOnondaga    -0.557      0.782    -0.712  0.477  
    ## 38 countyQueens       0.0294     0.794     0.0371 0.970  
    ## 39 countySchenectady -0.537      1.08     -0.495  0.621  
    ## 40 countySuffolk      0.375      0.853     0.440  0.660  
    ## 41 mean_co_month      1.64       1.76      0.927  0.354

``` r
linear_fit_co_emergency =  asthma_df2 %>%
  lm(asthma_emergency ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_co_month, data = .)
```

``` r
linear_fit_co_emergency =
  linear_fit_co_emergency %>% 
  broom::tidy()
```

``` r
print(linear_fit_co_emergency, n=60)
```

    ## # A tibble: 41 × 5
    ##    term              estimate std.error statistic p.value
    ##    <chr>                <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)        1.18       1.15      1.03   0.302  
    ##  2 mental_health      0.00829    0.0168    0.492  0.623  
    ##  3 physical_health    0.0232     0.0153    1.52   0.130  
    ##  4 sex               -0.0327     0.304    -0.108  0.914  
    ##  5 smoker2            0.0519     0.592     0.0877 0.930  
    ##  6 smoker3            0.721      0.448     1.61   0.109  
    ##  7 smoker4           -0.0844     0.407    -0.207  0.836  
    ##  8 race2              0.648      0.473     1.37   0.172  
    ##  9 race3             -0.627      0.875    -0.716  0.474  
    ## 10 race4             -1.16       2.96     -0.391  0.696  
    ## 11 race5             -1.47       2.90     -0.505  0.614  
    ## 12 race6             -0.246      1.00     -0.245  0.807  
    ## 13 race7              0.0705     0.866     0.0814 0.935  
    ## 14 race8              0.388      0.484     0.801  0.423  
    ## 15 age2              -1.26       0.700    -1.80   0.0733 
    ## 16 age3              -0.617      0.682    -0.905  0.366  
    ## 17 age4              -0.401      0.688    -0.583  0.560  
    ## 18 age5              -0.111      0.655    -0.170  0.865  
    ## 19 age6              -0.886      0.688    -1.29   0.199  
    ## 20 age7              -0.721      0.657    -1.10   0.273  
    ## 21 age8               0.0281     0.690     0.0408 0.967  
    ## 22 age9              -1.30       0.791    -1.64   0.101  
    ## 23 age10             -0.873      0.801    -1.09   0.276  
    ## 24 age11             -1.36       0.885    -1.54   0.124  
    ## 25 age12             -1.88       0.901    -2.08   0.0377 
    ## 26 age13             -1.45       1.23     -1.18   0.237  
    ## 27 income2           -0.0835     0.483    -0.173  0.863  
    ## 28 income3           -0.562      0.539    -1.04   0.298  
    ## 29 income4           -1.08       0.537    -2.02   0.0442 
    ## 30 income5           -1.39       0.457    -3.05   0.00242
    ## 31 countyBronx       -0.0495     0.946    -0.0524 0.958  
    ## 32 countyErie         0.146      0.705     0.207  0.836  
    ## 33 countyKings       -1.03       1.19     -0.870  0.385  
    ## 34 countyMonroe      -0.370      0.727    -0.509  0.611  
    ## 35 countyNew York     0.281      0.907     0.310  0.757  
    ## 36 countyNiagara     -0.136      0.994    -0.137  0.891  
    ## 37 countyOnondaga    -0.557      0.782    -0.712  0.477  
    ## 38 countyQueens       0.0294     0.794     0.0371 0.970  
    ## 39 countySchenectady -0.537      1.08     -0.495  0.621  
    ## 40 countySuffolk      0.375      0.853     0.440  0.660  
    ## 41 mean_co_month      1.64       1.76      0.927  0.354

``` r
linear_fit_co_attack =  asthma_df2 %>%
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + mean_co_month, data = .)
```

``` r
linear_fit_co_attack =
  linear_fit_co_attack %>% 
  broom::tidy()
```

``` r
print(linear_fit_co_attack, n=60)
```

    ## # A tibble: 41 × 5
    ##    term              estimate std.error statistic   p.value
    ##    <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)        0.791     0.197      4.02   0.0000698
    ##  2 mental_health      0.00455   0.00290    1.57   0.117    
    ##  3 physical_health    0.00790   0.00263    3.00   0.00282  
    ##  4 sex               -0.0719    0.0526    -1.37   0.172    
    ##  5 smoker2            0.201     0.102      1.96   0.0506   
    ##  6 smoker3            0.0289    0.0773     0.374  0.709    
    ##  7 smoker4            0.0424    0.0702     0.603  0.547    
    ##  8 race2             -0.171     0.0812    -2.11   0.0357   
    ##  9 race3             -0.306     0.149     -2.04   0.0415   
    ## 10 race4             -0.546     0.506     -1.08   0.281    
    ## 11 race5              0.514     0.496      1.04   0.301    
    ## 12 race6             -0.142     0.171     -0.830  0.407    
    ## 13 race7              0.132     0.148      0.893  0.372    
    ## 14 race8             -0.0173    0.0834    -0.208  0.835    
    ## 15 age2              -0.0757    0.120     -0.633  0.527    
    ## 16 age3              -0.0384    0.117     -0.330  0.742    
    ## 17 age4               0.0293    0.117      0.250  0.803    
    ## 18 age5              -0.00174   0.112     -0.0156 0.988    
    ## 19 age6              -0.140     0.118     -1.19   0.235    
    ## 20 age7              -0.173     0.112     -1.54   0.125    
    ## 21 age8              -0.155     0.119     -1.30   0.193    
    ## 22 age9              -0.0951    0.135     -0.704  0.482    
    ## 23 age10             -0.138     0.140     -0.986  0.325    
    ## 24 age11             -0.410     0.154     -2.67   0.00791  
    ## 25 age12             -0.323     0.154     -2.10   0.0363   
    ## 26 age13             -0.374     0.210     -1.78   0.0754   
    ## 27 income2            0.0234    0.0837     0.280  0.780    
    ## 28 income3           -0.0921    0.0926    -0.995  0.320    
    ## 29 income4           -0.0782    0.0933    -0.838  0.403    
    ## 30 income5           -0.138     0.0788    -1.75   0.0804   
    ## 31 countyBronx       -0.0462    0.162     -0.285  0.775    
    ## 32 countyErie        -0.177     0.120     -1.47   0.142    
    ## 33 countyKings       -0.247     0.203     -1.22   0.224    
    ## 34 countyMonroe      -0.116     0.125     -0.928  0.354    
    ## 35 countyNew York    -0.163     0.155     -1.05   0.295    
    ## 36 countyNiagara     -0.178     0.170     -1.05   0.295    
    ## 37 countyOnondaga    -0.190     0.135     -1.41   0.159    
    ## 38 countyQueens      -0.134     0.136     -0.990  0.323    
    ## 39 countySchenectady -0.231     0.185     -1.25   0.213    
    ## 40 countySuffolk     -0.177     0.146     -1.22   0.225    
    ## 41 mean_co_month      0.0547    0.302      0.181  0.856
