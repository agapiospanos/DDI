DDI - Drug to Drug Interaction Assessment R Package
================

Description
-----------

A package that provides patient data evaluation algorithm for the DDI (Drug to Drug Interaction) criteria. The patient data are parsed and their ids are classified based on whether the meet the conditions or not for each DDI criterion. The package exports excel files that contains the patient ids and their status (1: if they fulfill the conditions for the criterion and 0: if they do not fulfill the conditions for the criterion.)

Till now the following DDI criteria have been incorporated in the package.

DDI-1 DDI-2-3 DDI-6 DDI-12 DDI-16 DDI-24

Installation
------------

The package is still under active development and has not been submitted to the CRAN yet. You can install the DDI package from GitHub repository as follows:

Installation using R package **[devtools](https://cran.r-project.org/package=devtools)**:

``` r
install.packages("devtools")
devtools::install_github("agapiospanos/DDI")
```

Input data format
-----------------

I will soon provide an excel file as a template for the input data format.

Basic usage examples
--------------------

Provided that you have the input data in the format specifed above you can use the package by calling the eval\_crit function and using the correct arguments. For example to evaluate the data for the DDI-1 criterion you can call the following function. This function will display a popup window to choose a folder that the excel file will be exported. Then another popup window will appear to choose the excel file that contains the patient data.

``` r
eval_crit(selected = c("DDI1"))
```

You can also get a single excel file for all the DDI criteria using the command below:

``` r
eval_crit(selected = c("all"))
```

If you want to get a signle excel file for all the DDI criteria excluding some of them (e.g. DDI1 and DDI24) use the command below:

``` r
eval_crit(selected = c("all"), exclude = c("DDI1", "DDI2_3"))
```

Finally, if you want to get multiple files for each DDI criterion and not get the single excel file use the command below:

``` r
eval_crit(selected = c("all"), single_excel = F)
```