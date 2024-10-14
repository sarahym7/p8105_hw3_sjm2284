Homework 3
================
Sarahy Martinez
10/13/2023

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Problem 1

``` r
library(p8105.datasets)
data("ny_noaa")
```

## Problem 2

``` r
accelerometer_data = read_csv(file = "./data/nhanes_accel.csv", na = c("", "NA", 999)) %>%
  janitor::clean_names() %>%
  mutate(seqn=as.character(seqn)) %>% 
  drop_na()
```

    ## Rows: 250 Columns: 1441
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1441): SEQN, min1, min2, min3, min4, min5, min6, min7, min8, min9, min1...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
demographics_data = read_csv(file = "./data/nhanes_covar.csv",na = c("", "NA", 999),
                    skip = 4, col_names =TRUE) %>% 
                    janitor::clean_names() %>% 
                     mutate(seqn=as.character(seqn)) %>% 
                    filter(age >21) %>% 
                    drop_na()
```

    ## Rows: 250 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): SEQN, sex, age, BMI, education
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
combined_nhanes = 
  left_join(accelerometer_data, demographics_data, by= "seqn") %>% 
  janitor::clean_names() %>%
  relocate(seqn,sex,age,bmi,education) %>% 
  drop_na(sex,age,bmi,education)
```
