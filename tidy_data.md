Tidy Data and Relational Datasets
================
Maggie Ramatowski
2026-11-17

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Print only 5 lines of each tibble:

``` r
options(tibble.print_min = 5)
```

load dataset and clean variable names

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") |> 
  janitor::clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## 4 10026  72.7 male            20             6            18            16
    ## 5 10035  60.4 male             4             0             1             2
    ## # ℹ 1,082 more rows

# ‘Pivot_longer’

``` r
pulse_tidy_df= 
  pivot_longer(
    pulse_df, 
    bdi_score_bl:bdi_score_12m, 
    names_to = "visit",
    values_to = "bdi")

pulse_tidy_df
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit           bdi
    ##   <dbl> <dbl> <chr> <chr>         <dbl>
    ## 1 10003  48.0 male  bdi_score_bl      7
    ## 2 10003  48.0 male  bdi_score_01m     1
    ## 3 10003  48.0 male  bdi_score_06m     2
    ## 4 10003  48.0 male  bdi_score_12m     0
    ## 5 10015  72.5 male  bdi_score_bl      6
    ## # ℹ 4,343 more rows

``` r
pulse_tidy_df=
  pivot_longer(
    pulse_df, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi")

pulse_tidy_df
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <chr> <dbl>
    ## 1 10003  48.0 male  bl        7
    ## 2 10003  48.0 male  01m       1
    ## 3 10003  48.0 male  06m       2
    ## 4 10003  48.0 male  12m       0
    ## 5 10015  72.5 male  bl        6
    ## # ℹ 4,343 more rows

``` r
mutate(pulse_tidy_df, visit= replace(visit, visit== "bl", "00m"), visit= factor(visit))
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <fct> <dbl>
    ## 1 10003  48.0 male  00m       7
    ## 2 10003  48.0 male  01m       1
    ## 3 10003  48.0 male  06m       2
    ## 4 10003  48.0 male  12m       0
    ## 5 10015  72.5 male  00m       6
    ## # ℹ 4,343 more rows

## Practice

load litters data set and clean variable names

``` r
litters_df= read_csv("./data/FAS_litters.csv") |> 
  janitor::clean_names()
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): Group, Litter Number, GD0 weight, GD18 weight
    ## dbl (4): GD of Birth, Pups born alive, Pups dead @ birth, Pups survive
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

retain only litter number and weights in dataset

``` r
select(litters_df, litter_number, gd0_weight,gd18_weight) |> 
  pivot_longer(gd0_weight:gd18_weight, 
               names_to = "gd",
               values_to = "weight") |> 
  mutate(
    gd= recode_values(gd, 
                "gd0_weight" ~ 0,
                "gd18_weight" ~ 18))
```

    ## # A tibble: 98 × 3
    ##   litter_number    gd weight
    ##   <chr>         <dbl> <chr> 
    ## 1 #85               0 19.7  
    ## 2 #85              18 34.7  
    ## 3 #1/2/95/2         0 27    
    ## 4 #1/2/95/2        18 42    
    ## 5 #5/5/3/83/3-3     0 26    
    ## # ℹ 93 more rows

# ‘Pivot_wider’

create a sample dataset

``` r
analysis_result= 
  tibble(
    group= c("treatment", "treatment", "placebo", "placebo"),
    time= c("pre", "post", "pre", "post"),
    mean= c(4, 8, 3.5, 4)
  )

analysis_result
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

``` r
pivot_wider(analysis_result, 
            names_from = "time", 
            values_from = "mean") |> 
  knitr::kable()
```

| group     | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |    8 |
| placebo   | 3.5 |    4 |

# Binding Rows

load data

``` r
fellowship_ring= 
  readxl::read_excel("./data/LotR_Words.xlsx", range= "B3:D6") |> 
  mutate(movie= "fellowship_ring")

two_towers=
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie= "two_towers")

return_king=
  readxl::read_excel("./data/LotR_Words.xlsx", range= "J3:L6") |> 
  mutate(movie= "return_king")
```

``` r
lotr_tidy= bind_rows(fellowship_ring, two_towers, return_king) |> 
  janitor::clean_names() |> 
  pivot_longer(female:male, 
               names_to = "gender",
               values_to = "words") |> 
  mutate(race= str_to_lower(race)) |> 
  select(movie, everything())

lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

# Joining Datasets

load and clean pups and litters datasets

``` r
pup_df= read_csv("./data/FAS_pups.csv", skip= 3, na=c("NA", "", ".")) |> 
  janitor::clean_names() |> 
  mutate(
    sex= recode_values(sex, 
                       1~ "male",
                       2~ "female"),
    sex= as.factor(sex))
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df= read.csv("./data/FAS_litters.csv", na= c("NA", "", ".")) |> 
  janitor::clean_names() |> 
  separate(group, into = c("dose", "day_of_tx"), sep= 3) |> 
  relocate(litter_number) |> 
  mutate(
    wt_gain= gd18_weight - gd0_weight, 
    dose= str_to_lower(dose))
```

join data sets

``` r
fas_df= left_join(pup_df, litters_df, by= "litter_number")

fas_df
```

    ## # A tibble: 313 × 15
    ##   litter_number sex   pd_ears pd_eyes pd_pivot pd_walk dose  day_of_tx
    ##   <chr>         <fct>   <dbl>   <dbl>    <dbl>   <dbl> <chr> <chr>    
    ## 1 #85           male        4      13        7      11 con   7        
    ## 2 #85           male        4      13        7      12 con   7        
    ## 3 #1/2/95/2     male        5      13        7       9 con   7        
    ## 4 #1/2/95/2     male        5      13        8      10 con   7        
    ## 5 #5/5/3/83/3-3 male        5      13        8      10 con   7        
    ## # ℹ 308 more rows
    ## # ℹ 7 more variables: gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <int>,
    ## #   pups_born_alive <int>, pups_dead_birth <int>, pups_survive <int>,
    ## #   wt_gain <dbl>

## Practice

load and clean data sets

``` r
survey_os= read_csv("./data/surv_os.csv", na= c("", "NA", ".")) |> 
  janitor::clean_names() |> 
  rename(
    id= what_is_your_uni, os= what_operating_system_do_you_use)
```

    ## Rows: 173 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): What is your UNI?, What operating system do you use?
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
survey_git= read_csv("./data/surv_program_git.csv", na=c("", "NA", ".")) |> 
  janitor::clean_names() |> 
  rename(
    id= what_is_your_uni,
    degree= what_is_your_degree_program,
    git= which_most_accurately_describes_your_experience_with_git
  )
```

    ## Rows: 135 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): What is your UNI?, What is your degree program?, Which most accurat...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

combine datasets: gives error that matching variable is not unique

``` r
survey_all= left_join(survey_os, survey_git, by= "id")
```

    ## Warning in left_join(survey_os, survey_git, by = "id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 7 of `x` matches multiple rows in `y`.
    ## ℹ Row 66 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

use below code to investigate the error:

``` r
survey_os |> slice(7)
```

    ## # A tibble: 1 × 2
    ##   id         os        
    ##   <chr>      <chr>     
    ## 1 student_15 Windows 10

``` r
survey_git |> filter(id=="student 15")
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: id <chr>, degree <chr>, git <chr>

``` r
survey_all= left_join(survey_os, survey_git, by= "id")
```

    ## Warning in left_join(survey_os, survey_git, by = "id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 7 of `x` matches multiple rows in `y`.
    ## ℹ Row 66 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.
