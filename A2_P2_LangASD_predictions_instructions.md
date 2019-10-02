Welcome to the second exciting part of the Language Development in ASD exercise
-------------------------------------------------------------------------------

In this exercise we will delve more in depth with different practices of
model comparison and model selection, by first evaluating your models
from last time against some new data. Does the model generalize well?
Then we will learn to do better by cross-validating models and
systematically compare them.

The questions to be answered (in a separate document) are: 1- Discuss
the differences in performance of your model in training and testing
data 2- Which individual differences should be included in a model that
maximizes your ability to explain/predict new data? 3- Predict a new
kid's performance (Bernie) and discuss it against expected performance
of the two groups

Learning objectives
-------------------

-   Critically appraise the predictive framework (contrasted to the
    explanatory framework)
-   Learn the basics of machine learning workflows: training/testing,
    cross-validation, feature selections

Let's go
--------

N.B. There are several datasets for this exercise, so pay attention to
which one you are using!

1.  The (training) dataset from last time (the awesome one you produced
    :-) ).
2.  The (test) datasets on which you can test the models from last time:

-   Demographic and clinical data:
    <https://www.dropbox.com/s/ra99bdvm6fzay3g/demo_test.csv?dl=1>
-   Utterance Length data:
    <https://www.dropbox.com/s/uxtqqzl18nwxowq/LU_test.csv?dl=1>
-   Word data:
    <https://www.dropbox.com/s/1ces4hv8kh0stov/token_test.csv?dl=1>

### Exercise 1) Testing model performance

How did your models from last time perform? In this exercise you have to
compare the results on the training data () and on the test data. Report
both of them. Compare them. Discuss why they are different.

-   recreate the models you chose last time (just write the model code
    again and apply it to your training data (from the first
    assignment))
-   calculate performance of the model on the training data: root mean
    square error is a good measure. (Tip: google the function rmse())
-   create the test dataset (apply the code from assignment 1 to clean
    up the 3 test datasets)
-   test the performance of the models on the test data (Tips: google
    the functions "predict()")
-   optional: predictions are never certain, can you identify the
    uncertainty of the predictions? (e.g. google predictinterval())

\[HERE GOES YOUR ANSWER\]

### Exercise 2) Model Selection via Cross-validation (N.B: ChildMLU!)

One way to reduce bad surprises when testing a model on new data is to
train the model via cross-validation.

In this exercise you have to use cross-validation to calculate the
predictive error of your models and use this predictive error to select
the best possible model.

-   Use cross-validation to compare your model from last week with the
    basic model (Child MLU as a function of Time and Diagnosis, and
    don't forget the random effects!)
-   (Tips): google the function "createFolds"; loop through each fold,
    train both models on the other folds and test them on the fold)

-   Now try to find the best possible predictive model of ChildMLU, that
    is, the one that produces the best cross-validated results.

-   Bonus Question 1: What is the effect of changing the number of
    folds? Can you plot RMSE as a function of number of folds?
-   Bonus Question 2: compare the cross-validated predictive error
    against the actual predictive error on the test data

Setting up the basic model
==========================

    # Basic model of Child_MLU as a function of Time and Diagnosis incl. random effects.
    basic_model <-  lmer(CHI_MLU ~ Visit * Diagnosis + (1 + Visit|Child.ID), data=train)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00252572
    ## (tol = 0.002, component 1)

Loop
====

Make a cross-validated version of the model. (Tips: google the function
"createFolds"; loop through each fold, train a model on the other folds
and test it on the fold.

    # The data from train is seperated into 5 folds (k) but with respect to Child.ID meaning that all rows concerning the same child will be in the same fold. That is done with unique()
    # In each fold, there will be numbers representing Child.ID's and not rows (the rows for each child are "grouped" under the child number)
    fold <- createFolds(unique(train$Child.ID), k = 5, list = TRUE, returnTrain = FALSE)
    fold

    ## $Fold1
    ##  [1]  9 15 16 18 28 30 36 40 44 52 53 60
    ## 
    ## $Fold2
    ##  [1]  2  5 10 19 25 29 37 41 42 54 55 58
    ## 
    ## $Fold3
    ##  [1]  3  7  8 13 17 20 31 38 39 43 47 48 59
    ## 
    ## $Fold4
    ##  [1]  6 12 14 21 23 26 32 34 46 50 51 56
    ## 
    ## $Fold5
    ##  [1]  1  4 11 22 24 27 33 35 45 49 57 61

    # Set counter
    n = 1

    # List without data
    RMSETrain = NULL
    RMSETest = NULL

    # loop
    for(f in fold) {
      
      # making 2 subsets. For each fold, the Child.ID's appearing here will not be included in the subset TrainSet - thereby excluding the given fold from the trainSet
      TrainSet = subset(train,!(Child.ID %in% f))
      # This given fold will then be the only fold included in the subset Testset
      TestSet = subset(train,(Child.ID %in% f))
      
      # Specify model with data from the subset TrainSet
      Model <-  lmer(CHI_MLU ~ Visit * Diagnosis + (1+Visit|Child.ID),TrainSet, REML=FALSE)
      
      # calculate performance of the model on the training data
      Train_pred <- predict(Model)
      RMSETrain[n] = rmse(TrainSet$CHI_MLU,Train_pred)
      
      # calculate performance of the model on the test data
      Test_pred <- predict(Model, newdata=TestSet, allow.new.levels = TRUE)
      RMSETest[n] = rmse(TestSet$CHI_MLU,Test_pred)
      n = n+1
      }

    RMSETest

    ## [1] 0.5718741 0.9066792 0.7061945 0.7362895 0.7052595

    RMSETrain

    ## [1] 0.3601298 0.3508746 0.3495515 0.3418207 0.3393727

    # Using the package cvsm
    # Set seed for reproducibility

    # Fold data 
    data_folded <- groupdata2::fold(train, k = 5,
                 cat_col = 'Diagnosis',
                 id_col = 'Child.ID') %>% mutate(scaled_mlu = scale(CHI_MLU))

    CV1 <- cvms::cross_validate(data_folded, "scaled_mlu ~ Visit + Diagnosis + (1+Visit|Child.ID)", 
                          fold_cols = '.folds',
                          control = lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxfun = 1000)),
                          REML = FALSE
                          )

    head(CV1)

    ## # A tibble: 1 x 21
    ##    RMSE   MAE   r2m   r2c   AIC  AICc   BIC Predictions Results
    ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
    ## 1 0.850 0.654 0.233 0.799  526.  527.  552. <tibble [3~ <tibbl~
    ## # ... with 12 more variables: Coefficients <list>, Folds <int>, `Fold
    ## #   Columns` <int>, `Convergence Warnings` <int>, `Singular Fit
    ## #   Messages` <int>, `Other Warnings` <int>, `Warnings and
    ## #   Messages` <list>, Family <chr>, Link <chr>, Dependent <chr>,
    ## #   Fixed <chr>, Random <chr>

    View(CV1$Results[[1]])
    #- Report the results and comment on them. 


    #- Now try to find the best possible predictive model of ChildMLU, that is, the one that produces the best cross-validated results.
    CHI_MLU_models <- c("CHI_MLU ~ Visit * Diagnosis + (1+Visit|Child.ID)", "CHI_MLU ~ Visit * Diagnosis + Visit * verbalIQ1 + Visit * Ados1 +(1+ Visit | Child.ID)", "CHI_MLU ~ Visit * Diagnosis * verbalIQ1 + (1+Visit|Child.ID)", "CHI_MLU ~ Visit * Diagnosis * Ados1 + (1+Visit|Child.ID)")

    CV2 <- cvms::cross_validate(data_folded, CHI_MLU_models, 
                          fold_cols = '.folds',
                          control = lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxfun = 1000)),
                          family = 'gaussian',
                          REML = FALSE)

    head(CV2)

    ## # A tibble: 4 x 21
    ##    RMSE   MAE   r2m   r2c   AIC  AICc   BIC Predictions Results
    ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
    ## 1 0.752 0.599 0.360 0.815  460.  461.  489. <tibble [3~ <tibbl~
    ## 2 0.556 0.424 0.662 0.814  406.  407.  450. <tibble [3~ <tibbl~
    ## 3 0.544 0.422 0.676 0.814  404.  405.  447. <tibble [3~ <tibbl~
    ## 4 0.682 0.535 0.505 0.816  444.  445.  487. <tibble [3~ <tibbl~
    ## # ... with 12 more variables: Coefficients <list>, Folds <int>, `Fold
    ## #   Columns` <int>, `Convergence Warnings` <int>, `Singular Fit
    ## #   Messages` <int>, `Other Warnings` <int>, `Warnings and
    ## #   Messages` <list>, Family <chr>, Link <chr>, Dependent <chr>,
    ## #   Fixed <chr>, Random <chr>

    #The model with lowest RMSE is the best af predicting.

    # Bonus Question 1: What is the effect of changing the number of folds? Can you plot RMSE as a function of number of folds?
    # Bonus Question 2: compare the cross-validated predictive error against the actual predictive error on the test data

\[HERE GOES YOUR ANSWER\]

### Exercise 3) Assessing the single child

Let's get to business. This new kiddo - Bernie - has entered your
clinic. This child has to be assessed according to his group's average
and his expected development.

Bernie is one of the six kids in the test dataset, so make sure to
extract that child alone for the following analysis.

You want to evaluate:

-   how does the child fare in ChildMLU compared to the average TD child
    at each visit? Define the distance in terms of absolute difference
    between this Child and the average TD.

<!-- -->

    # Making a table with relevant information extracted from data.

    # Making subset with Bernie only - we know he is child no. 2 by revisiting the old dataframe
    test_bernie <- subset(test, Child.ID == 102)

    mean_train <- train %>% filter(Diagnosis == "TD") %>% group_by(Visit) %>% summarise(mean(CHI_MLU))
    mean_train

    ## # A tibble: 6 x 2
    ##   Visit `mean(CHI_MLU)`
    ##   <dbl>           <dbl>
    ## 1     1            1.31
    ## 2     2            1.76
    ## 3     3            2.23
    ## 4     4            2.73
    ## 5     5            2.97
    ## 6     6            2.93

    colnames(mean_train)[2] <- "Mean_MLU_TD"
    test_bernie$Mean_MLU_TD <- mean_train$Mean_MLU_TD

    test_bernie <- test_bernie %>% select(Visit, CHI_MLU, Mean_MLU_TD)

    test_bernie$distance <- (test_bernie$CHI_MLU - test_bernie$Mean_MLU_TD)
    colnames(test_bernie)[2] <- "Bernie_MLU"
    test_bernie

    ##    Visit Bernie_MLU Mean_MLU_TD  distance
    ## 7      1   1.984456    1.310174 0.6742815
    ## 8      2   2.544444    1.762800 0.7816445
    ## 9      3   3.353191    2.229037 1.1241544
    ## 10     4   3.183099    2.734681 0.4484175
    ## 11     5   3.173252    2.969893 0.2033593
    ## 12     6   3.448413    2.927875 0.5205373

-   how does the child fare compared to the model predictions at Visit
    6? Is the child below or above expectations? (tip: use the predict()
    function on Bernie's data only and compare the prediction with the
    actual performance of the child)

<!-- -->

    # Making a subset only with Bernie
    Bernie <- subset(test, Child.ID == 102)

    # The best mdoel from cross validation
    best_model <- lmer(CHI_MLU ~ Visit * Diagnosis * Ados1 + (1+Visit|Child.ID), data = train)

    # Adding a column with the predicted values for Bernie with the model.
    Bernie$Bernie_pred <- predict(best_model,Bernie, allow.new.levels = TRUE)

    # Using pipes and and summarise to isolate bernies MLU, the predicted MLU of Bernie and the difference.
    Bernie %>% filter(Visit == "6") %>% 
      summarise(
        difference = CHI_MLU - Bernie_pred,
        Bernie_MLU = CHI_MLU, 
        Bernie_pred)

    ##   difference Bernie_MLU Bernie_pred
    ## 1  0.2524106   3.448413    3.196002

    # He is doing better than we predicted at visit 6.

\[HERE GOES YOUR ANSWER\]

### OPTIONAL: Exercise 4) Model Selection via Information Criteria

Another way to reduce the bad surprises when testing a model on new data
is to pay close attention to the relative information criteria between
the models you are comparing. Let's learn how to do that!

Re-create a selection of possible models explaining ChildMLU (the ones
you tested for exercise 2, but now trained on the full dataset and not
cross-validated).

Then try to find the best possible predictive model of ChildMLU, that
is, the one that produces the lowest information criterion.

-   Bonus question for the optional exercise: are information criteria
    correlated with cross-validated RMSE? That is, if you take AIC for
    Model 1, Model 2 and Model 3, do they co-vary with their
    cross-validated RMSE?

### OPTIONAL: Exercise 5): Using Lasso for model selection

Welcome to the last secret exercise. If you have already solved the
previous exercises, and still there's not enough for you, you can expand
your expertise by learning about penalizations. Check out this tutorial:
<http://machinelearningmastery.com/penalized-regression-in-r/> and make
sure to google what penalization is, with a focus on L1 and L2-norms.
Then try them on your data!

### Make as csv

    write.csv(train,"train_correct.csv")
    write.csv(test,"test_correct.csv")
