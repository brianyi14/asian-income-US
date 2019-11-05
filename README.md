# Asian American Income in the United States

Brian Yi

## Introduction

**Purpose:** My goal is to practice and develop my data cleaning, transforming, and exploratory data analysis skills in R. We want to identify overall trends and patterns within our dataset to see what is responsible for the higher median income for Asian Americans in the US. Under the assumption that the factors responsible for the higher income isn't related to race, this knowledge can be useful for anyone who wants to improve their chances of earning a higher income. From an industry view, if a company measures an individual's potential according to their predicted income, they can even use the patterns we discover as metrics to hiring the right employees!

Due to my own Asian ethnic background, I am interested in the factors most responsible for Asian Americans supposedly having a higher median income in the US. I choose the Adult dataset that was extracted from the 1994 US Census Bureau database. This dataset contains various social, economic, educational, and ethnic variables that can be used to predict someone's salary.

**Method of Approach:** Throughout this report, we focus on the following predictors for `salary`: `education_level`, `working_class`, `occupation`, `marital_status`, and `hours_per_week`. We look at each predictor individually, and our analysis of each variable follows a general two step procedure. First, we see if there is a correlation between the predictor and `salary` of the entire US population. *(For example: If our predictor is `education_level`, we determine if there is a correlation between higher education and higher income for the population as a whole.)* Next, we determine if Asians occupy the respective fields of the predictor that correlate to higher salary. *(Following the same example: If higher education corresponds to higher income, we determine whether Asians have a higher average eductation. If Asians indeed have a higher eductation than average, since higher education correlates with higher income, then we know that `education_level` is a variable responsible for the higher median income among Asians.)*

**Results:** We discover that `education_level` and `work_hours` are the only predictors of the five with a definitive correlation with `price`. The other predictors will require a slightly more statistical analysis, which does not fall under the focus of our project, if we want to know whether they are responsible for the higher median income for Asians.
