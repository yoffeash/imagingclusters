COPDGene/DECAMP Radiologic Clustering Project
================
Samuel Ash
2018-08-13

-   [Overview](#overview)
-   [Load Libraries and Data](#load-libraries-and-data)
    -   [Libraries](#libraries)
    -   [Data](#data)
-   [Analysis](#analysis)

Overview
========

This document describes the initial results of the imaging clusters from COPDGene based on prior DECAMP clustering. This includes the baseline clinical characteristics and biomarkers as well as longitudinal analyses including change in FEV1, change in 6WMD, mortality, and exacerbation rates.

Load Libraries and Data
=======================

Libraries
---------

``` r
library(devtools)
library(tidyverse)
library(janitor)
library(cowplot)
library(survival)
library(survminer)
library(readxl)
library(ggfortify)
library(ggpubr)
library(rmarkdown)
```

Data
----

Analysis
========
