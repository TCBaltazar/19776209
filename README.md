# Purpose

This work folder was created to be used in the evaluation for the
Financial Econometrics course presented at Stellebosch University for
the purpose of completing my Mcom (Economics). It outlines … and for
this practical, I will attempt to do …

``` r
library(pacman)
pacman::p_load(tidyverse, fmxdat, Texevier, )
```

    ## Warning in pacman::p_load(tidyverse, fmxdat, Texevier, ): Failed to install/load:

# Description

This folder was created using the fmxdat package
(<https://github.com/Nicktz/fmxdat>) with the following code:

``` r
fmxdat::make_project()
```

In addition, the folders used for the seperate questions (see *Question
1* and *Question 2*) were created using the Texevier package
(<https://github.com/Nicktz/Texevier>):

``` r
Texevier::create_template_html(directory = 'Questions', template_name = 'Question_1')
# Alternatively, for knitting to PDF: Texevier::create_template(directory = 'Questions', template_name = 'Question_1')
```

## Data

The relevant data used in completing this practical was sourced from:
<https://www.fmx.nfkatzke.com/FMX_data_2021.zip>

*PROVIDE SHORT DESCRIPTION OF THE DATA*
