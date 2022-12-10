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

\#linear regression \##asthma attack as the outcome

``` r
linear_fit_attack =  asthma_df %>%
  lm(asthma_attack ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .)
```

``` r
linear_fit_attack =
  linear_fit_attack %>% 
  broom::tidy()
```

``` r
print(linear_fit_attack, n=60)
```

    ## # A tibble: 57 × 5
    ##    term                estimate std.error statistic    p.value
    ##    <chr>                  <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 (Intercept)         0.706     0.152       4.63   0.00000443
    ##  2 mental_health       0.00558   0.00230     2.42   0.0158    
    ##  3 physical_health     0.00461   0.00216     2.14   0.0330    
    ##  4 sex                -0.111     0.0428     -2.58   0.00999   
    ##  5 smoker2             0.223     0.0869      2.57   0.0104    
    ##  6 smoker3             0.109     0.0637      1.71   0.0886    
    ##  7 smoker4             0.0673    0.0587      1.15   0.252     
    ##  8 race2              -0.117     0.0688     -1.70   0.0898    
    ##  9 race3              -0.229     0.143      -1.60   0.109     
    ## 10 race4              -0.413     0.503      -0.821  0.412     
    ## 11 race5               0.264     0.266       0.991  0.322     
    ## 12 race6              -0.137     0.158      -0.865  0.387     
    ## 13 race7               0.0544    0.126       0.431  0.667     
    ## 14 race8              -0.0151    0.0706     -0.213  0.831     
    ## 15 age2               -0.0155    0.106      -0.146  0.884     
    ## 16 age3                0.109     0.100       1.09   0.276     
    ## 17 age4                0.104     0.101       1.04   0.300     
    ## 18 age5                0.0399    0.0949      0.421  0.674     
    ## 19 age6               -0.0735    0.101      -0.724  0.469     
    ## 20 age7               -0.0867    0.0974     -0.890  0.374     
    ## 21 age8               -0.0300    0.102      -0.293  0.769     
    ## 22 age9               -0.0457    0.110      -0.414  0.679     
    ## 23 age10              -0.125     0.116      -1.08   0.282     
    ## 24 age11              -0.280     0.129      -2.18   0.0298    
    ## 25 age12              -0.201     0.130      -1.54   0.123     
    ## 26 age13              -0.160     0.156      -1.03   0.305     
    ## 27 income2            -0.0136    0.0696     -0.195  0.845     
    ## 28 income3            -0.0389    0.0769     -0.506  0.613     
    ## 29 income4            -0.131     0.0769     -1.70   0.0891    
    ## 30 income5            -0.132     0.0657     -2.01   0.0454    
    ## 31 countyBronx        -0.109     0.135      -0.802  0.423     
    ## 32 countyChautauqua   -0.0154    0.176      -0.0878 0.930     
    ## 33 countyDutchess     -0.323     0.154      -2.09   0.0368    
    ## 34 countyErie         -0.199     0.119      -1.67   0.0951    
    ## 35 countyJefferson    -0.452     0.212      -2.14   0.0329    
    ## 36 countyKings        -0.220     0.120      -1.83   0.0671    
    ## 37 countyMonroe       -0.137     0.121      -1.13   0.260     
    ## 38 countyNassau       -0.162     0.127      -1.27   0.203     
    ## 39 countyNew York     -0.170     0.118      -1.45   0.148     
    ## 40 countyNiagara      -0.246     0.158      -1.56   0.120     
    ## 41 countyOneida       -0.134     0.176      -0.764  0.445     
    ## 42 countyOnondaga     -0.196     0.129      -1.52   0.130     
    ## 43 countyOrange       -0.264     0.161      -1.63   0.103     
    ## 44 countyOswego        0.0927    0.267       0.347  0.729     
    ## 45 countyQueens       -0.139     0.126      -1.11   0.269     
    ## 46 countyRensselaer   -0.233     0.167      -1.40   0.162     
    ## 47 countyRichmond     -0.0363    0.210      -0.173  0.863     
    ## 48 countySaratoga     -0.392     0.160      -2.44   0.0148    
    ## 49 countySchenectady  -0.199     0.185      -1.07   0.283     
    ## 50 countySt. Lawrence -0.255     0.317      -0.804  0.422     
    ## 51 countySteuben       0.278     0.272       1.02   0.306     
    ## 52 countySuffolk      -0.261     0.120      -2.17   0.0300    
    ## 53 countyTompkins     -0.715     0.269      -2.65   0.00816   
    ## 54 countyUlster       -0.221     0.161      -1.38   0.169     
    ## 55 countyWayne        -0.649     0.218      -2.97   0.00306   
    ## 56 countyWestchester  -0.207     0.127      -1.64   0.102     
    ## 57 aqi                 0.000839  0.000758    1.11   0.269

\##asthma emergency as the outcome

``` r
linear_fit_emergency =  asthma_df %>%
  lm(asthma_emergency ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .)
```

``` r
linear_fit_emergency = 
linear_fit_emergency %>% 
  broom::tidy() 
```

``` r
print(linear_fit_emergency, n=60)
```

    ## # A tibble: 57 × 5
    ##    term                estimate std.error statistic   p.value
    ##    <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)         1.47       0.829     1.77    0.0768   
    ##  2 mental_health       0.00896    0.0124    0.721   0.471    
    ##  3 physical_health     0.0273     0.0116    2.34    0.0194   
    ##  4 sex                -0.139      0.232    -0.596   0.551    
    ##  5 smoker2             0.205      0.471     0.436   0.663    
    ##  6 smoker3             0.703      0.346     2.03    0.0427   
    ##  7 smoker4             0.216      0.318     0.679   0.497    
    ##  8 race2               0.860      0.375     2.29    0.0223   
    ##  9 race3              -0.523      0.780    -0.671   0.503    
    ## 10 race4              -0.778      2.75     -0.283   0.777    
    ## 11 race5              -0.168      1.46     -0.115   0.908    
    ## 12 race6              -0.290      0.866    -0.335   0.738    
    ## 13 race7               0.238      0.690     0.345   0.730    
    ## 14 race8               0.516      0.384     1.34    0.179    
    ## 15 age2               -0.895      0.585    -1.53    0.127    
    ## 16 age3               -0.242      0.548    -0.442   0.659    
    ## 17 age4               -0.0656     0.550    -0.119   0.905    
    ## 18 age5               -0.0176     0.519    -0.0339  0.973    
    ## 19 age6               -0.355      0.555    -0.640   0.522    
    ## 20 age7               -0.584      0.532    -1.10    0.273    
    ## 21 age8                0.146      0.554     0.264   0.792    
    ## 22 age9               -0.917      0.602    -1.52    0.128    
    ## 23 age10              -0.516      0.627    -0.824   0.410    
    ## 24 age11              -0.954      0.680    -1.40    0.162    
    ## 25 age12              -1.21       0.713    -1.69    0.0912   
    ## 26 age13              -1.42       0.854    -1.67    0.0959   
    ## 27 income2            -0.431      0.376    -1.15    0.252    
    ## 28 income3            -0.916      0.418    -2.19    0.0287   
    ## 29 income4            -1.27       0.414    -3.07    0.00220  
    ## 30 income5            -1.54       0.355    -4.32    0.0000177
    ## 31 countyBronx         0.0310     0.741     0.0419  0.967    
    ## 32 countyChautauqua   -0.502      0.960    -0.522   0.602    
    ## 33 countyDutchess     -0.465      0.843    -0.551   0.581    
    ## 34 countyErie          0.281      0.653     0.431   0.667    
    ## 35 countyJefferson    -0.836      1.16     -0.722   0.471    
    ## 36 countyKings        -0.188      0.657    -0.286   0.775    
    ## 37 countyMonroe       -0.210      0.659    -0.318   0.751    
    ## 38 countyNassau        0.205      0.696     0.295   0.768    
    ## 39 countyNew York      0.781      0.643     1.22    0.225    
    ## 40 countyNiagara      -0.358      0.863    -0.414   0.679    
    ## 41 countyOneida       -0.0447     0.932    -0.0479  0.962    
    ## 42 countyOnondaga     -0.267      0.704    -0.379   0.705    
    ## 43 countyOrange       -0.0527     0.883    -0.0597  0.952    
    ## 44 countyOswego       -0.434      1.46     -0.297   0.767    
    ## 45 countyQueens        0.163      0.689     0.236   0.813    
    ## 46 countyRensselaer    0.293      0.911     0.321   0.748    
    ## 47 countyRichmond      0.0514     1.15      0.0447  0.964    
    ## 48 countySaratoga      0.187      0.857     0.218   0.827    
    ## 49 countySchenectady  -0.329      1.01     -0.325   0.746    
    ## 50 countySt. Lawrence -0.977      1.74     -0.563   0.574    
    ## 51 countySteuben      -0.262      1.49     -0.176   0.860    
    ## 52 countySuffolk      -0.0218     0.656    -0.0333  0.973    
    ## 53 countyTompkins     -0.379      1.47     -0.257   0.797    
    ## 54 countyUlster       -0.00119    0.869    -0.00137 0.999    
    ## 55 countyWayne        -0.247      1.19     -0.207   0.836    
    ## 56 countyWestchester   0.399      0.693     0.576   0.565    
    ## 57 aqi                 0.000465   0.00414   0.112   0.911

\##doctor visits as the outcome

``` r
linear_fit_visit =  asthma_df %>%
  lm(asthma_visit ~ mental_health + physical_health + sex + smoker + race + age + income + county + aqi, data = .)
```

``` r
linear_fit_visit =
  linear_fit_visit %>% 
  broom::tidy()
```

``` r
print(linear_fit_visit, n=60)
```

    ## # A tibble: 57 × 5
    ##    term               estimate std.error statistic   p.value
    ##    <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)        -0.285     0.936     -0.304  0.761    
    ##  2 mental_health      -0.00297   0.0141    -0.211  0.833    
    ##  3 physical_health     0.0574    0.0132     4.34   0.0000165
    ##  4 sex                -0.241     0.262     -0.917  0.359    
    ##  5 smoker2             0.352     0.531      0.662  0.508    
    ##  6 smoker3             0.445     0.392      1.14   0.257    
    ##  7 smoker4             0.988     0.359      2.76   0.00603  
    ##  8 race2               0.227     0.427      0.530  0.596    
    ##  9 race3              -0.393     0.880     -0.446  0.655    
    ## 10 race4              -0.262     3.10      -0.0846 0.933    
    ## 11 race5               0.514     1.64       0.313  0.755    
    ## 12 race6              -0.195     0.977     -0.199  0.842    
    ## 13 race7              -0.514     0.779     -0.660  0.510    
    ## 14 race8               0.373     0.435      0.859  0.391    
    ## 15 age2                0.0214    0.657      0.0326 0.974    
    ## 16 age3                0.508     0.619      0.821  0.412    
    ## 17 age4                0.540     0.621      0.870  0.385    
    ## 18 age5                1.78      0.585      3.03   0.00251  
    ## 19 age6                0.621     0.628      0.990  0.323    
    ## 20 age7                0.304     0.601      0.506  0.613    
    ## 21 age8                0.921     0.627      1.47   0.143    
    ## 22 age9                0.630     0.679      0.928  0.354    
    ## 23 age10               0.465     0.708      0.657  0.511    
    ## 24 age11               0.509     0.768      0.663  0.507    
    ## 25 age12              -0.225     0.812     -0.277  0.782    
    ## 26 age13              -0.0452    0.984     -0.0460 0.963    
    ## 27 income2            -0.693     0.427     -1.62   0.105    
    ## 28 income3            -0.704     0.472     -1.49   0.137    
    ## 29 income4            -1.03      0.468     -2.19   0.0286   
    ## 30 income5            -0.991     0.402     -2.46   0.0140   
    ## 31 countyBronx        -0.618     0.836     -0.739  0.460    
    ## 32 countyChautauqua   -0.159     1.08      -0.147  0.883    
    ## 33 countyDutchess     -0.318     0.951     -0.334  0.738    
    ## 34 countyErie          0.809     0.736      1.10   0.272    
    ## 35 countyJefferson    -0.126     1.31      -0.0963 0.923    
    ## 36 countyKings         0.616     0.741      0.832  0.406    
    ## 37 countyMonroe       -0.0482    0.746     -0.0647 0.948    
    ## 38 countyNassau        0.0949    0.781      0.122  0.903    
    ## 39 countyNew York     -0.0785    0.725     -0.108  0.914    
    ## 40 countyNiagara      -0.0747    0.974     -0.0767 0.939    
    ## 41 countyOneida        4.06      1.05       3.86   0.000123 
    ## 42 countyOnondaga     -0.296     0.794     -0.372  0.710    
    ## 43 countyOrange        0.208     0.996      0.208  0.835    
    ## 44 countyOswego        0.361     1.65       0.219  0.827    
    ## 45 countyQueens       -0.128     0.780     -0.164  0.870    
    ## 46 countyRensselaer   -0.404     1.03      -0.393  0.694    
    ## 47 countyRichmond      4.24      1.30       3.27   0.00112  
    ## 48 countySaratoga      0.192     0.985      0.196  0.845    
    ## 49 countySchenectady  -0.0388    1.14      -0.0339 0.973    
    ## 50 countySt. Lawrence  1.46      1.96       0.745  0.456    
    ## 51 countySteuben       0.537     1.68       0.320  0.749    
    ## 52 countySuffolk      -0.0356    0.740     -0.0481 0.962    
    ## 53 countyTompkins     -0.194     1.66      -0.117  0.907    
    ## 54 countyUlster        0.883     0.980      0.901  0.368    
    ## 55 countyWayne        -0.901     1.35      -0.669  0.504    
    ## 56 countyWestchester   0.193     0.786      0.245  0.807    
    ## 57 aqi                 0.00457   0.00468    0.978  0.328
