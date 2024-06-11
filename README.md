NOTE 1: This repo is no longer maintained, as this packge has been superseded by Trvis Gerke's version, which
can be found at:
https://github.com/tgerke/ggconsort 

You may also find useful the {consort} package on CRAN here:
https://cran.r-project.org/web/packages/consort/index.html

If you are doing a manual one-off consort diagram, the guide at Rpubs may still be helpful:
https://rpubs.com/phiggins/461686


# ggconsort <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of {ggconsort} is to make it easy to create CONSORT diagrams for the transparent reporting of participant allocation in randomized, controlled clinical trials. This is done by creating a standardized status table, and using this table as the source for the creation of a custom ggplot that creates a standard CONSORT diagram.

## Installation

You can install the current version of ggconsort from github with:

``` r
library(remotes)
remotes::install_github("higgi13425/ggconsort")
```

**To Do List:**
1. Store example status_table files as proper *.rda files in a data folder, as per Chapter 14 of R packages here: https://r-pkgs.org/data.html 
2. make 'bad' status_table files (small, ~ 10 rows) that violate some of the rules - wrong names, too many/too few columns, blanks instead of NAs, etc.
3. create a validate_status_table() function that checks a submitted status table to make sure it can be used, and returns helpful error messages on how to fix the table. 
4. update validate_status_table() to fix simple things, like automatically fix fuzzy matched column names, case for Yes/YES/yes, change No to NA, convert 1/0 to Yes/NA, etc.
5. Ben Gerber will try a "diagrammer" branch
6. fix drawing size for top_tbl


Someday (not any day soon), one might be able to install the released version of ggconsort from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ggconsort")
```
## Website
A crude pkgdown website is now available at https://higgi13425.github.io/ggconsort/


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggconsort)
## basic example code
```

