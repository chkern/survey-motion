# Completion Conditions and Response Behavior in Smartphone Surveys: A Prediction Approach Using Acceleration Data

This repository contains code for training prediction models (01-train1.R), predicting motion conditions (x-predict.R) and comparing predicted groups (x-compare.R) based on acceleration data from smartphone sensors.

Supplementary material to the paper: 

https://doi.org/10.1177/0894439320971233

### Pre-trained random forest for predicting motion conditions

The pre-trained random forest model is provided as a train object (`caret` package) in src/rf.rds. When used with `predict()` it requires the following aggregated SMotion variables (acceleration features) as input: SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95. See 02-predict.R and 04-predict.R for examples. 

### Citation 

``` {.r}
@Article{Kern2020,
  title    = {Completion Conditions and Response Behavior in Smartphone Surveys: A Prediction Approach Using Acceleration Data},
  author   = {Kern, C. AND Hoehne, J. K. AND Schlosser, S. AND Revilla, M.},
  journal  = {Social Science Computer Review},
  year     = {2020},
  doi      = {10.1177/0894439320971233},
  url      = {https://doi.org/10.1177/0894439320971233}
}
```
