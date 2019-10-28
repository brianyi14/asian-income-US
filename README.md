# Asian American Income in the United States

Brian Yi

## Introduction

In this project, we will mainly focus on data munging and exploratory data analysis.

The dataset we will be looking at today is the Adult dataset that was extracted from the 1994 US Census Bureau database. This dataset contains various social, economic, and ethnic variables that can be used to predict whether someone had an annual salary of more or less than 50K. The dataset can be found here: [Dataset Link](https://archive.ics.uci.edu/ml/datasets/Adult).

Due to my own Asian ethnic background, I wanted to determine the factors most responsible for Asian Americans supposedly having a higher income in comparison to that of the other races. Throughout this report, we will focus on the following variables as predictors for salary: `education_level`, `working_class`, `occupation`, `marital_status`, and `hours_per_week`. Keep note that aside from `hours_per_week` being numerical, the other four variables we will be investigating are categorical.

We will look at each variable individually, and our analysis of each variable will follow a general two step procedure (some will be combined into one step). We will first see if there is a correlation between the variable we are investigating and the salary of the entire US population sample. Next, we will determine if Asians occupy the respective fields of that variable that correlate to higher salary. Now let's get to tidying this dataset!

