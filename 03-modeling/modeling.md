

``` r
library(glmnet)
```

```
## Warning: package 'glmnet' was built under R version 4.5.2
```

```
## Loading required package: Matrix
```

```
## Loaded glmnet 4.1-10
```

``` r
library(jtools)
```

```
## Warning: package 'jtools' was built under R version 4.5.2
```

``` r
library(caret)
```

```
## Warning: package 'caret' was built under R version 4.5.2
```

```
## Loading required package: ggplot2
```

```
## Loading required package: lattice
```

``` r
library(earth)
```

```
## Warning: package 'earth' was built under R version 4.5.2
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 4.5.2
```

```
## Loading required package: plotmo
```

```
## Warning: package 'plotmo' was built under R version 4.5.2
```

```
## Loading required package: plotrix
```

```
## Warning: package 'plotrix' was built under R version 4.5.2
```

``` r
library(ranger)
```

```
## Warning: package 'ranger' was built under R version 4.5.2
```

```
## ranger 0.18.0 using 2 threads (default). Change with num.threads in ranger() and predict(), options(Ncpus = N), options(ranger.num.threads = N) or environment variable R_RANGER_NUM_THREADS.
```

``` r
load("1_Data clean and transform DATA.RData") # κάνω load τα δεδομένα μου
rm(final_data,final_data1,auto_separate,clean_mixed_genres) # αφαιρώ οτι δεν μου είναι απαραίτητο


# Generalized Linear Model (GLM) - Gaussian Family

# - Εξαρτημένη μεταβλητή (Y): Rating 
# - Ανεξάρτητες μεταβλητές (X): Genre_1, Genre_2, Popularity_Trend κλπ. (ΣΥΝΟΛΟ 10 ΑΝΕΞΑΡΤΗΤΕΣ)

glm<-glm(Rating ~
                  Genre_1+
                  Genre_2+
                  Popularity_Trend+
                  count_mix+
                  Era+
                  Runtime_Cat+
                  Votes_Per_Year+
                  engagement_score+
                  net_sentiment_ratio+
                  Is_Series
                  ,data = final_data2,family = gaussian)

summ(glm) # summary του model 
```

```
## MODEL INFO:
## Observations: 4078
## Dependent Variable: Rating
## Type: Linear regression 
## 
## MODEL FIT:
## χ²(66) = 2538.11, p = 0.00
## Pseudo-R² (Cragg-Uhler) = 0.47
## Pseudo-R² (McFadden) = 0.19
## AIC = 10575.43, BIC = 11004.74 
## 
## Standard errors:MLE
## ------------------------------------------------------------
##                                  Est.   S.E.   t val.      p
## ----------------------------- ------- ------ -------- ------
## (Intercept)                      7.02   0.35    19.85   0.00
## Genre_1Adventure                 0.13   0.10     1.31   0.19
## Genre_1Animation                 0.49   0.09     5.60   0.00
## Genre_1Biography                 0.52   0.08     6.15   0.00
## Genre_1Comedy                    0.32   0.06     5.61   0.00
## Genre_1Crime                     0.25   0.07     3.75   0.00
## Genre_1Documentary               1.05   0.08    12.80   0.00
## Genre_1Drama                     0.49   0.06     7.67   0.00
## Genre_1Fantasy                   0.47   0.22     2.17   0.03
## Genre_1Film-Noir                 0.85   0.88     0.97   0.33
## Genre_1Game-Show                 0.15   0.21     0.68   0.50
## Genre_1History                  -0.07   0.88    -0.08   0.93
## Genre_1Horror                   -0.26   0.09    -2.82   0.00
## Genre_1Music                     1.11   0.52     2.15   0.03
## Genre_1Musical                   1.03   0.51     2.02   0.04
## Genre_1Mystery                   0.03   0.24     0.11   0.91
## Genre_1News                      1.31   0.88     1.49   0.14
## Genre_1Reality-TV               -0.23   0.21    -1.10   0.27
## Genre_1Romance                   0.71   0.34     2.09   0.04
## Genre_1Sci-Fi                    0.02   0.28     0.07   0.94
## Genre_1Short                     0.54   0.08     6.47   0.00
## Genre_1Sport                     1.08   0.88     1.22   0.22
## Genre_1Talk-Show                 0.07   0.34     0.21   0.84
## Genre_1Thriller                 -0.23   0.16    -1.44   0.15
## Genre_1War                      -1.53   0.88    -1.74   0.08
## Genre_1Western                   0.34   0.40     0.85   0.40
## Genre_2 Adventure               -0.56   0.11    -5.23   0.00
## Genre_2 Animation               -3.12   0.89    -3.51   0.00
## Genre_2 Biography               -0.26   0.18    -1.46   0.14
## Genre_2 Comedy                  -0.25   0.10    -2.53   0.01
## Genre_2 Crime                   -0.15   0.11    -1.41   0.16
## Genre_2 Documentary              0.96   0.63     1.52   0.13
## Genre_2 Drama                   -0.13   0.10    -1.32   0.19
## Genre_2 Fantasy                 -0.54   0.13    -4.02   0.00
## Genre_2 Film-Noir               -0.22   0.25    -0.87   0.38
## Genre_2 Game-Show                0.03   0.22     0.15   0.88
## Genre_2 History                 -0.01   0.14    -0.05   0.96
## Genre_2 Horror                  -0.88   0.12    -7.02   0.00
## Genre_2 Music                   -0.17   0.18    -0.95   0.34
## Genre_2 Musical                 -0.46   0.31    -1.48   0.14
## Genre_2 Mystery                 -0.48   0.12    -3.98   0.00
## Genre_2 News                    -0.46   0.63    -0.73   0.46
## Genre_2 Reality-TV              -0.10   0.26    -0.37   0.71
## Genre_2 Romance                 -0.48   0.12    -4.19   0.00
## Genre_2 Sci-Fi                  -0.65   0.14    -4.63   0.00
## Genre_2 Short                   -0.34   0.13    -2.75   0.01
## Genre_2 Sport                   -0.20   0.18    -1.10   0.27
## Genre_2 Talk-Show               -0.04   0.37    -0.12   0.91
## Genre_2 Thriller                -0.69   0.12    -5.70   0.00
## Genre_2 War                     -0.00   0.23    -0.00   1.00
## Genre_2 Western                 -0.69   0.23    -2.99   0.00
## Genre_2None                     -0.26   0.10    -2.53   0.01
## Popularity_TrendDown            -0.24   0.32    -0.76   0.45
## Popularity_TrendUp              -0.32   0.32    -1.01   0.31
## Popularity_TrendUnknown         -0.82   0.32    -2.59   0.01
## count_mix                        0.07   0.02     3.43   0.00
## EraRetro                        -0.29   0.08    -3.47   0.00
## EraMillennium                   -0.34   0.08    -4.42   0.00
## EraDigital                      -0.44   0.07    -6.06   0.00
## EraStreaming                    -0.74   0.07   -10.31   0.00
## Runtime_CatStandard              0.42   0.11     3.85   0.00
## Runtime_CatEpic                  0.76   0.12     6.46   0.00
## Runtime_CatMarathon              1.19   0.18     6.56   0.00
## Votes_Per_Year                   0.00   0.00     8.54   0.00
## engagement_score                -2.03   0.51    -3.96   0.00
## net_sentiment_ratio              0.84   0.08    10.96   0.00
## Is_Series1                       1.14   0.11    10.44   0.00
## ------------------------------------------------------------
## 
## Estimated dispersion parameter = 0.77
```

``` r
1-glm$deviance/glm$null.deviance  # υπολογισμός R squared
```

```
## [1] 0.451095
```

``` r
# Elastic Net Regularized Regression

fitControl <- trainControl(method = "cv", number = 10)  #Χωρίζουμε τα δεδομένα σε 10 μέρη
# Εκπαιδεύουμε τα 9 και τεστάρουμε στο 1 επαναλαμβάνοντας την διαδικασία 10 φορές

set.seed(123) #Ορίζουμε το random seed σε 123 ώστε να παίρνουμε πάντα τα ίδια ακριβώς αποτελέσματα σε κάθε εκτέλεση

elastic_net_model <- train(Rating ~ Genre_1 + Genre_2 + Popularity_Trend + count_mix + 
                             Era + Runtime_Cat + Votes_Per_Year + engagement_score + 
                             net_sentiment_ratio + Is_Series,
                           data = final_data2,
                           method = "glmnet",
                           trControl = fitControl,
                           tuneLength = 10) #train στις ίδιες μεταβλητές με πρίν

print(elastic_net_model) # Μας δείχνει την ακρίβεια (RMSE, Rsquared) για κάθε δοκιμή που έκανε.
```

```
## glmnet 
## 
## 4078 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 3669, 3670, 3670, 3671, 3672, 3671, ... 
## Resampling results across tuning parameters:
## 
##   alpha  lambda        RMSE       Rsquared   MAE      
##   0.1    0.0002208011  0.8884193  0.4288528  0.6523053
##   0.1    0.0005100792  0.8884173  0.4288549  0.6523015
##   0.1    0.0011783491  0.8883736  0.4289115  0.6522228
##   0.1    0.0027221392  0.8883163  0.4289532  0.6520837
##   0.1    0.0062884947  0.8883813  0.4287845  0.6520528
##   0.1    0.0145272383  0.8889938  0.4279239  0.6527682
##   0.1    0.0335598047  0.8907925  0.4257534  0.6550133
##   0.1    0.0775275016  0.8949359  0.4215309  0.6595704
##   0.1    0.1790985841  0.9054860  0.4127215  0.6716802
##   0.1    0.4137409584  0.9288625  0.4000296  0.6964845
##   0.2    0.0002208011  0.8883828  0.4288885  0.6522453
##   0.2    0.0005100792  0.8883637  0.4289086  0.6522135
##   0.2    0.0011783491  0.8882884  0.4290079  0.6520678
##   0.2    0.0027221392  0.8882417  0.4290191  0.6519014
##   0.2    0.0062884947  0.8882204  0.4289452  0.6518305
##   0.2    0.0145272383  0.8895118  0.4272542  0.6533380
##   0.2    0.0335598047  0.8918090  0.4247361  0.6560306
##   0.2    0.0775275016  0.8993893  0.4172252  0.6642287
##   0.2    0.1790985841  0.9157290  0.4049465  0.6823816
##   0.2    0.4137409584  0.9609907  0.3706113  0.7280998
##   0.3    0.0002208011  0.8884492  0.4288454  0.6522270
##   0.3    0.0005100792  0.8883959  0.4288983  0.6521542
##   0.3    0.0011783491  0.8882489  0.4290577  0.6519632
##   0.3    0.0027221392  0.8881855  0.4290659  0.6517940
##   0.3    0.0062884947  0.8882956  0.4288137  0.6519312
##   0.3    0.0145272383  0.8899828  0.4266809  0.6538669
##   0.3    0.0335598047  0.8932602  0.4232697  0.6576166
##   0.3    0.0775275016  0.9045120  0.4122144  0.6701703
##   0.3    0.1790985841  0.9279650  0.3944725  0.6947351
##   0.3    0.4137409584  0.9908506  0.3397459  0.7549312
##   0.4    0.0002208011  0.8884335  0.4288605  0.6521960
##   0.4    0.0005100792  0.8883547  0.4289430  0.6520924
##   0.4    0.0011783491  0.8882064  0.4291012  0.6518710
##   0.4    0.0027221392  0.8880674  0.4291904  0.6516232
##   0.4    0.0062884947  0.8884785  0.4285645  0.6521001
##   0.4    0.0145272383  0.8905373  0.4260413  0.6544629
##   0.4    0.0335598047  0.8952052  0.4212684  0.6596715
##   0.4    0.0775275016  0.9092526  0.4079062  0.6755158
##   0.4    0.1790985841  0.9421921  0.3803242  0.7093043
##   0.4    0.4137409584  1.0148342  0.3162673  0.7756384
##   0.5    0.0002208011  0.8883852  0.4289476  0.6521620
##   0.5    0.0005100792  0.8882993  0.4290248  0.6520200
##   0.5    0.0011783491  0.8881945  0.4291045  0.6518146
##   0.5    0.0027221392  0.8879902  0.4292645  0.6515273
##   0.5    0.0062884947  0.8887744  0.4281777  0.6523970
##   0.5    0.0145272383  0.8910087  0.4255455  0.6549960
##   0.5    0.0335598047  0.8976315  0.4186689  0.6622649
##   0.5    0.0775275016  0.9136673  0.4041685  0.6797706
##   0.5    0.1790985841  0.9571772  0.3638614  0.7237231
##   0.5    0.4137409584  1.0339559  0.3038246  0.7927684
##   0.6    0.0002208011  0.8883412  0.4289829  0.6521341
##   0.6    0.0005100792  0.8882758  0.4290462  0.6519741
##   0.6    0.0011783491  0.8881786  0.4291133  0.6517863
##   0.6    0.0027221392  0.8879861  0.4292461  0.6515229
##   0.6    0.0062884947  0.8891775  0.4276604  0.6528158
##   0.6    0.0145272383  0.8915132  0.4250242  0.6555542
##   0.6    0.0335598047  0.9003923  0.4156569  0.6653044
##   0.6    0.0775275016  0.9186339  0.3997605  0.6846452
##   0.6    0.1790985841  0.9700409  0.3501314  0.7357868
##   0.6    0.4137409584  1.0513478  0.3011889  0.8083676
##   0.7    0.0002208011  0.8884028  0.4289179  0.6521669
##   0.7    0.0005100792  0.8882510  0.4290706  0.6519331
##   0.7    0.0011783491  0.8881539  0.4291337  0.6517436
##   0.7    0.0027221392  0.8880070  0.4291997  0.6515565
##   0.7    0.0062884947  0.8895209  0.4272258  0.6532116
##   0.7    0.0145272383  0.8920793  0.4244483  0.6562233
##   0.7    0.0335598047  0.9028347  0.4130967  0.6681486
##   0.7    0.0775275016  0.9243837  0.3941911  0.6905945
##   0.7    0.1790985841  0.9813229  0.3388680  0.7458824
##   0.7    0.4137409584  1.0715835  0.2956742  0.8262812
##   0.8    0.0002208011  0.8883451  0.4289761  0.6520907
##   0.8    0.0005100792  0.8882273  0.4290943  0.6518982
##   0.8    0.0011783491  0.8881206  0.4291656  0.6516835
##   0.8    0.0027221392  0.8880408  0.4291413  0.6516102
##   0.8    0.0062884947  0.8897940  0.4268894  0.6535276
##   0.8    0.0145272383  0.8927122  0.4238153  0.6570116
##   0.8    0.0335598047  0.9049935  0.4109792  0.6706758
##   0.8    0.0775275016  0.9304950  0.3879612  0.6969622
##   0.8    0.1790985841  0.9924655  0.3273509  0.7553289
##   0.8    0.4137409584  1.0948581  0.2820601  0.8464268
##   0.9    0.0002208011  0.8883711  0.4289427  0.6521166
##   0.9    0.0005100792  0.8882088  0.4291122  0.6518604
##   0.9    0.0011783491  0.8880550  0.4292354  0.6515949
##   0.9    0.0027221392  0.8881014  0.4290535  0.6516679
##   0.9    0.0062884947  0.8900547  0.4265791  0.6538002
##   0.9    0.0145272383  0.8934409  0.4230812  0.6578362
##   0.9    0.0335598047  0.9071897  0.4088236  0.6731416
##   0.9    0.0775275016  0.9365134  0.3817406  0.7032921
##   0.9    0.1790985841  1.0022173  0.3184406  0.7640933
##   0.9    0.4137409584  1.1194885  0.2572631  0.8679829
##   1.0    0.0002208011  0.8883257  0.4289985  0.6520472
##   1.0    0.0005100792  0.8881908  0.4291286  0.6518253
##   1.0    0.0011783491  0.8879932  0.4293017  0.6515207
##   1.0    0.0027221392  0.8881738  0.4289532  0.6517338
##   1.0    0.0062884947  0.8903344  0.4262484  0.6540936
##   1.0    0.0145272383  0.8942954  0.4221920  0.6587742
##   1.0    0.0335598047  0.9092729  0.4068362  0.6753425
##   1.0    0.0775275016  0.9426151  0.3752824  0.7095010
##   1.0    0.1790985841  1.0114107  0.3106938  0.7725245
##   1.0    0.4137409584  1.1423794  0.2391150  0.8885917
## 
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were alpha = 0.6 and lambda = 0.002722139.
```

``` r
print(elastic_net_model$bestTune) #Εμφάνιση των Βέλτιστων Παραμέτρων alpha , lambda
```

```
##    alpha      lambda
## 54   0.6 0.002722139
```

``` r
coef(elastic_net_model$finalModel, elastic_net_model$bestTune$lambda) #Εξαγωγή των Τελικών Συντελεστών
```

```
## 67 x 1 sparse Matrix of class "dgCMatrix"
##                         s=0.002722139
## (Intercept)              6.711683e+00
## Genre_1Adventure         1.000620e-01
## Genre_1Animation         5.341411e-01
## Genre_1Biography         4.828610e-01
## Genre_1Comedy            2.739039e-01
## Genre_1Crime             2.283938e-01
## Genre_1Documentary       9.984963e-01
## Genre_1Drama             4.341849e-01
## Genre_1Fantasy           4.050476e-01
## Genre_1Film-Noir         7.743730e-01
## Genre_1Game-Show         8.845743e-02
## Genre_1History          -1.825771e-02
## Genre_1Horror           -3.031644e-01
## Genre_1Music             1.044875e+00
## Genre_1Musical           9.228199e-01
## Genre_1Mystery           .           
## Genre_1News              1.147222e+00
## Genre_1Reality-TV       -2.522206e-01
## Genre_1Romance           6.178614e-01
## Genre_1Sci-Fi            .           
## Genre_1Short             5.113392e-01
## Genre_1Sport             9.120264e-01
## Genre_1Talk-Show         .           
## Genre_1Thriller         -2.688125e-01
## Genre_1War              -1.479604e+00
## Genre_1Western           3.546116e-01
## Genre_2 Adventure       -4.511941e-01
## Genre_2 Animation       -2.818548e+00
## Genre_2 Biography       -1.088351e-01
## Genre_2 Comedy          -1.445183e-01
## Genre_2 Crime           -2.765742e-02
## Genre_2 Documentary      1.000571e+00
## Genre_2 Drama            6.470975e-04
## Genre_2 Fantasy         -3.743270e-01
## Genre_2 Film-Noir        .           
## Genre_2 Game-Show        1.321432e-01
## Genre_2 History          1.376209e-01
## Genre_2 Horror          -7.208303e-01
## Genre_2 Music           -8.667105e-03
## Genre_2 Musical         -2.571116e-01
## Genre_2 Mystery         -3.249268e-01
## Genre_2 News            -2.511436e-01
## Genre_2 Reality-TV       6.178341e-02
## Genre_2 Romance         -3.298644e-01
## Genre_2 Sci-Fi          -5.133321e-01
## Genre_2 Short           -2.567534e-01
## Genre_2 Sport           -3.314323e-02
## Genre_2 Talk-Show        4.660462e-02
## Genre_2 Thriller        -5.483327e-01
## Genre_2 War              1.267704e-01
## Genre_2 Western         -5.051340e-01
## Genre_2None             -1.115691e-01
## Popularity_TrendDown     7.499106e-02
## Popularity_TrendUp       .           
## Popularity_TrendUnknown -4.981896e-01
## count_mix                6.462071e-02
## EraRetro                -2.176644e-01
## EraMillennium           -2.642367e-01
## EraDigital              -3.697561e-01
## EraStreaming            -6.707284e-01
## Runtime_CatStandard      2.451392e-01
## Runtime_CatEpic          5.738490e-01
## Runtime_CatMarathon      1.024101e+00
## Votes_Per_Year           7.324626e-06
## engagement_score        -2.030694e+00
## net_sentiment_ratio      8.510271e-01
## Is_Series1               9.702430e-01
```

``` r
# Multivariate Adaptive Regression Splines(MARS)

fitControl2 <- trainControl(method = "cv", number = 10)#Χωρίζουμε τα δεδομένα σε 10 μέρη
# Εκπαιδεύουμε τα 9 και τεστάρουμε στο 1 επαναλαμβάνοντας την διαδικασία 10 φορές

set.seed(123) #Ορίζουμε το random seed σε 123 ώστε να παίρνουμε πάντα τα ίδια ακριβώς αποτελέσματα σε κάθε εκτέλεση


mars_model <- train(Rating ~ Genre_1 + Genre_2 + Popularity_Trend + count_mix + 
                      Era + Runtime_Cat + Votes_Per_Year + engagement_score + 
                      net_sentiment_ratio + Is_Series,
                    data = final_data2,
                    method = "earth",
                    trControl = fitControl2,
                    tuneLength = 10) #train στις ίδιες μεταβλητές με πρίν

print(mars_model) #Εμφάνιση των Βέλτιστων Παραμέτρων
```

```
## Multivariate Adaptive Regression Spline 
## 
## 4078 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 3669, 3670, 3670, 3671, 3672, 3671, ... 
## Resampling results across tuning parameters:
## 
##   nprune  RMSE       Rsquared   MAE      
##    2      1.0732861  0.1684104  0.8201441
##    5      0.9855821  0.2980056  0.7458434
##    8      0.9341094  0.3691397  0.7023864
##   11      0.9237481  0.3830766  0.6897017
##   14      0.9100241  0.4012404  0.6739008
##   17      0.9037186  0.4096900  0.6628827
##   20      0.8933030  0.4228170  0.6518892
##   23      0.8895766  0.4274208  0.6494606
##   26      0.8793805  0.4400387  0.6406351
##   29      0.8741382  0.4465897  0.6364175
## 
## Tuning parameter 'degree' was held constant at a value of 1
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were nprune = 29 and degree = 1.
```

``` r
summary(mars_model$finalModel) #Hinge Functions
```

```
## Call: earth(x=matrix[4078,66], y=c(8.6,8,9,8.2,9...), keepxy=TRUE, degree=1, nprune=29)
## 
##                                 coefficients
## (Intercept)                         44.22938
## Genre_1Animation                     0.40590
## Genre_1Biography                     0.36581
## Genre_1Documentary                   0.90579
## Genre_1Drama                         0.26908
## Genre_1Horror                       -0.38678
## Genre_1Short                         0.36090
## Genre_2 Adventure                   -0.45739
## Genre_2 Drama                        0.19226
## Genre_2 Horror                      -0.45486
## Genre_2 Sci-Fi                      -0.36954
## Genre_2 Thriller                    -0.34752
## Popularity_TrendUnknown             -0.47732
## EraRetro                            -0.48856
## EraMillennium                       -0.59240
## EraDigital                          -0.67389
## EraStreaming                        -0.91920
## Runtime_CatStandard                  0.40181
## Runtime_CatEpic                      0.70112
## Runtime_CatMarathon                  1.15136
## Is_Series1                           1.15765
## h(27762-Votes_Per_Year)             -0.00002
## h(Votes_Per_Year-27762)              0.00001
## h(engagement_score-0.000317058)   -856.53203
## h(0.0446429-engagement_score)     -828.48966
## h(engagement_score-0.0446429)      869.38343
## h(engagement_score-0.130435)       -13.23489
## h(-0.1-net_sentiment_ratio)         -2.90719
## h(net_sentiment_ratio- -0.1)         0.64911
## 
## Selected 29 of 37 terms, and 23 of 66 predictors (nprune=29)
## Termination condition: RSq changed by less than 0.001 at 37 terms
## Importance: Popularity_TrendUnknown, net_sentiment_ratio, engagement_score, Is_Series1, EraStreaming, ...
## Number of terms at each degree of interaction: 1 28 (additive model)
## GCV 0.7599961    RSS 3013.23    GRSq 0.449443    RSq 0.4644636
```

``` r
importance_mars <- varImp(mars_model, scale = FALSE) #Υπολογισμός Σημαντικότητας Μεταβλητών


#Random Forest (Μέθοδος Ranger)

fitControl3 <- trainControl(method = "cv", number = 5) #Εδώ άλλαξα σε 5-fold (αντί για 10) ιατί το rf ήταν βαρύ

set.seed(123) #Ορίζουμε το random seed σε 123 ώστε να παίρνουμε πάντα τα ίδια ακριβώς αποτελέσματα σε κάθε εκτέλεση



rf_model <- train(Rating ~ Genre_1 + Genre_2 + Popularity_Trend + count_mix + 
                    Era + Runtime_Cat + Votes_Per_Year + engagement_score + 
                    net_sentiment_ratio + Is_Series,
                  data = final_data2,
                  method = "ranger",
                  trControl = fitControl3,
                  tuneLength = 5,
                  importance = 'impurity') #train στις ίδιες μεταβλητές με πρίν

print(rf_model) #Εμφάνιση Αποτελεσμάτων
```

```
## Random Forest 
## 
## 4078 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 3262, 3262, 3262, 3263, 3263 
## Resampling results across tuning parameters:
## 
##   mtry  splitrule   RMSE       Rsquared   MAE      
##    2    variance    0.9755014  0.4093435  0.7449257
##    2    extratrees  0.9909428  0.3881886  0.7568030
##   18    variance    0.8504743  0.4763815  0.6198186
##   18    extratrees  0.8652883  0.4583349  0.6310240
##   34    variance    0.8579853  0.4674763  0.6246740
##   34    extratrees  0.8646580  0.4605397  0.6262983
##   50    variance    0.8633217  0.4616621  0.6293099
##   50    extratrees  0.8648965  0.4611767  0.6264965
##   66    variance    0.8670950  0.4573506  0.6325668
##   66    extratrees  0.8655715  0.4610176  0.6263050
## 
## Tuning parameter 'min.node.size' was held constant at a value of 5
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were mtry = 18, splitrule = variance and min.node.size = 5.
```

``` r
varImp(rf_model, scale = FALSE) #Σημαντικότητα Μεταβλητών
```

```
## ranger variable importance
## 
##   only 20 most important variables shown (out of 66)
## 
##                         Overall
## Votes_Per_Year           780.35
## engagement_score         753.95
## Runtime_CatStandard      419.57
## Is_Series1               403.94
## Popularity_TrendUnknown  343.39
## net_sentiment_ratio      327.71
## EraStreaming             143.32
## count_mix                134.41
## Genre_1Documentary       117.10
## Genre_1Horror            100.58
## Genre_1Animation         100.16
## Genre_2 Drama             76.68
## Genre_2None               72.77
## Popularity_TrendDown      69.15
## EraDigital                67.65
## Genre_1Drama              67.28
## Genre_2 Adventure         67.00
## Genre_1Comedy             56.76
## EraMillennium             50.67
## Genre_1Crime              50.42
```

