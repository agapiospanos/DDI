DDI - Drug to Drug Interaction Assessment R Package
================

<img src="man/figures/DDI-logo.png" width=250 align="right" style="margin-left:20px; margin-right: 20px;"/>

## Description

A package that provides patient data evaluation algorithm for the DDI
(Drug to Drug Interaction) criteria. The patient data are parsed and
their ids are classified based on whether the meet the conditions or not
for each DDI criterion. The package exports excel files that contains
the patient ids and their status (1: if they fulfill the conditions for
the criterion and 0: if they do not fulfill the conditions for the
criterion.)

We have included 68 criteria so far: DDI-1 to DDI-68

## Installation

The package is still under active development and has not been submitted
to the CRAN yet. You can install the DDI package from GitHub repository
as follows:

Installation using R package
**[devtools](https://cran.r-project.org/package=devtools)**:

``` r
install.packages("devtools")
devtools::install_github("agapiospanos/DDI")
```

## Input data format

I will soon provide an excel file as a template for the input data
format.

## Basic usage examples

Provided that you have the input data in the format specifed above you
can use the package by calling the eval\_crit function and using the
correct arguments. For example to evaluate the data for the DDI-1
criterion you can call the following function. This function will
display a popup window to choose a folder that the excel file will be
exported. Then another popup window will appear to choose the excel file
that contains the patient data.

``` r
eval_crit(selected = c("DDI1"))
```

You can also get a single excel file for all the DDI criteria using the
command below:

``` r
eval_crit(selected = c("all"))
```

## Advanced usage examples

You can disable the excel file output and get a list with the results by
using the argument excel\_out = FALSE as shown below:

``` r
output_data <- eval_crit(selected = c("all"), excel_out = F)

# getting the full data
output_data$all_criteria

# getting only the sum of DDI criteria met
output_data$sumdata
```

If you want to get a signle excel file for all the DDI criteria
excluding some of them (e.g. DDI1 and DDI24) use the command below:

``` r
eval_crit(selected = c("all"), exclude = c("DDI1", "DDI24"))
```

You can also get an excel file with the sums of DDI criteria met. You
just have to set the show\_only\_sum argument to TRUE. The column
ddi.found indicates if at least one criterion is met (with 1) or no DDI
criterion is met (with 0). The column ddi.count indicates the number of
DDI criteria that each patient meets.

``` r
eval_crit(selected = c("all"), show_only_sum = T)
```

Finally, if you want to get multiple files for each DDI criterion and
not get the single excel file use the command below:

``` r
eval_crit(selected = c("all"), single_excel = F)
```
