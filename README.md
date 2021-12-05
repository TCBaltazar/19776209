# Purpose

This work folder was created to be used in the evaluation for the
Financial Econometrics course presented at Stellebosch University for
the purpose of completing my Mcom (Economics). It outlines my workflow,
and rationale, while completing the tutorial.

Below, the r setup used throughout. Importantly, the packages loaded for
each question vary depending on the respective requirements.

``` r
knitr::opts_chunk$set(fig.align = "left", fig.height = 3, fig.pos = "H", fig.width = 5,
                      message=FALSE, warning=FALSE, comment = NA)
library(pacman)
pacman::p_load(fmxdat, Texevier, knitr, kableExtra, tidyverse, readr)
```

# Description

This folder was created using the fmxdat package
(<https://github.com/Nicktz/fmxdat>) with the following code:

``` r
fmxdat::make_project()
```

In addition, the folders used for the seperate questions (inside the
“root” *Questions* folder) were created using the Texevier package
(<https://github.com/Nicktz/Texevier>):

``` r
Texevier::create_template_html(directory = 'Questions', template_name = 'Question_1')
Texevier::create_template_html(directory='Questions', template_name = 'Question_2')
Texevier::create_template_html(directory='Questions', template_name = 'Question_3')
Texevier::create_template_html(directory='Questions', template_name = 'Question_4')
Texevier::create_template_html(directory='Questions', template_name = 'Question_5')
Texevier::create_template_html(directory='Questions', template_name = 'Question_6')
```

# Data

The relevant data used in completing this practical was sourced from:
<https://www.fmx.nfkatzke.com/FMX_data_2021.zip>.

The data sourced was all in .rds format, with accompanying txt files
providing brief descriptions for the relevant datasets. Data used to
complete this practical was placed in the “data” folder. These datasets,
however, are not available on this github (due to storage constraints),
and were excluded from the commits by updating the .gitignore file:
specifying that objects in “data/” should be ignored. In this folder,
there is a README.md file which provides an overview of the data. The
data used to complete questions will be specified when these questions
are addressed.

Data in .rds format was uploaded to the environment using code in the
following format:

``` r
somedat <- read_rds("data/some_data.rds")
```

# Question 1: Yield Spread

## Introduction

Bond yields are an important economic indicator, and can be seen as a
sign of investor sentiment about the economy. Interest rates and bond
yields have a positive relationship, and, to a large extent, lower
yields on Advanced Economy bonds can lead investors to search for yield
elsewhere. This means that when there is a “risk-on” sentiment in global
markets, and investors shift more capital into Emerging Markets, this
greater demand for bonds should drive up their prices and, by extension,
decrease the yield to investors. This sections of the README aims to
investigate trends in yield spreads using the data provided.

## Data

``` r
SA_bonds <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/SA_Bonds.rds") %>%
    rename("ZA_3M"=SA_3M)
BE_Infl <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/BE_Infl.rds")
bonds_2y <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/bonds_2y.rds")
bonds_10y <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/bonds_10y.rds")
usdzar <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/usdzar.rds")
ZA_Infl <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/ZA_Infl.rds")
VIX <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/IV.rds")
ZAR_IY <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/ZAR_IY.rds")
```

The data sourced includes information on bond yields for SA (SA_bonds),
as well as a wide basket of Advanced and Emerging economies (bonds_2y
and bonds_10y). Data on the USD:ZAR spot rate is provided in “usdzar”,
VIX is a composite volatility index including indicators for the US, EU,
and EME’s. ZA_Infl contains historical inflation rates for SA, and
BE_Infl is Break-Even Inflation, which gives an insight into the
market’s pricing of future inflation. Finally, ZAR_IY is the 6-month
forward implied yield for SA.

## Exploratory Analysis

Firstly, due to constraints on the time periods available for many of
the countries, and the reduced time frame available for the Break-Even
inflation variable, the sample was restricted to consider only dates
from 2012-05-06 (ie the first observation for BE_Infl).

Calculating bond spreads was done using the formula:
*S**p**r**e**a**d* = *Y**i**e**l**d*<sub>10*Y**r*</sub> − *Y**i**e**l**d*<sub>2*Y**r*</sub>

``` r
BRIC <- c("Brazil", "CHINA", "India", "Russia")
bonds_10y$Name <- gsub("_10Yr", "", bonds_10y$Name)
bonds_2y$Name <- gsub("_2yr", "", bonds_2y$Name) # remove unnecessary labels

BRICS_10Yr <- left_join(SA_bonds %>% arrange(date) %>% select(date, ZA_10Yr) %>% filter(date>ymd(20120506)), 
                        bonds_10y %>% filter(Name %in% BRIC) %>% spread(Name, Bond_10Yr) %>% 
    setNames(c("date", "BRA_10Yr", "CHN_10Yr", "IND_10Yr", "RUS_10Yr")) %>% filter(date>ymd(20120506)), by="date") %>%
    gather(Tickers, Val, -date)

SA_2Yr <- SA_bonds %>% arrange(date) %>% select(date, ZA_2Yr) %>% filter(date>ymd(20120506)) 

BRIC_2Yr <- bonds_2y %>% filter(Name %in% BRIC) %>% spread(Name, Bond_2Yr) %>% 
    setNames(c("date", "BRA_2Yr", "CHN_2Yr", "IND_2Yr", "RUS_2Yr")) %>% filter(date>ymd(20120506))

BRICS_2Yr <- left_join(SA_2Yr, BRIC_2Yr, by="date") %>% gather(Tickers, Val, -date)
```

``` r
BRICS_2Yr %>% ggplot() +
    geom_line(aes(date, Val, color=Tickers))  +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="Bond Yields",
         subtitle="BRICS 2Yr Bond Yields (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-8-1.png" alt="BRICS 2Yr Bond Yields (2012-2021)"  />
<p class="caption">
BRICS 2Yr Bond Yields (2012-2021)
</p>

</div>

``` r
BRICS_10Yr %>% ggplot() + geom_line(aes(date, Val, color=Tickers)) +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="Bond Yields",
         subtitle="BRICS 10Yr Bond Yields (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-9-1.png" alt="BRICS 10Yr Bond Yields (2012-2021)"  />
<p class="caption">
BRICS 10Yr Bond Yields (2012-2021)
</p>

</div>

``` r
BE_Infl %>% spread(Name, Price) %>%
    ggplot() + geom_line(aes(date, SAGGBE10)) +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="Break-Even Inflation", subtitle="BE Inflation (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-10-1.png" alt="BE Inflation (2012-2021)"  />
<p class="caption">
BE Inflation (2012-2021)
</p>

</div>

``` r
ZA_Infl %>% spread(Name, Price) %>% filter(date>ymd(20120506)) %>%
    ggplot() + geom_line(aes(date, ZAR_Infl)) +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="ZA Inflation", subtitle="ZA Inflation (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-11-1.png" alt="ZA Inflation (2012-2021)"  />
<p class="caption">
ZA Inflation (2012-2021)
</p>

</div>

``` r
usdzar %>% spread(Name, Price) %>% rename("USDZAR"= SouthAfrica_Cncy) %>% filter(date>ymd(20120506)) %>%
    ggplot() + geom_line(aes(date, USDZAR)) +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="USDZAR Spot", subtitle="USDZAR Spot (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-12-1.png" alt="USDZAR Spot (2012-2021)"  />
<p class="caption">
USDZAR Spot (2012-2021)
</p>

</div>

``` r
VIX %>% filter(Name=="VXEEM", date>ymd(20120506)) %>% spread(Name, Price) %>%
    ggplot() + geom_line(aes(date, VXEEM)) +fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="VXEEM", subtitle="Emerging Market Volatility (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-13-1.png" alt="Emerging Market Volatility (2012-2021)"  />
<p class="caption">
Emerging Market Volatility (2012-2021)
</p>

</div>

``` r
consolidated_SA <- left_join(SA_2Yr, 
                          VIX %>% filter(Name=="VXEEM", date>ymd(20120506)) %>% spread(Name, Price),
                          by="date") %>%
    left_join(., SA_bonds %>% arrange(date) %>% select(date, ZA_10Yr) %>% filter(date>ymd(20120506)),
              by="date") %>% 
    left_join(., BE_Infl %>% spread(Name, Price), by="date") %>%
    left_join(., usdzar %>% spread(Name, Price) %>% rename("USDZAR"= SouthAfrica_Cncy) %>% filter(date>ymd(20120506)), by="date") %>% 
    left_join(., ZA_Infl %>% spread(Name, Price) %>% filter(date>ymd(20120506)), by="date") %>%
    mutate(Spread=ZA_10Yr-ZA_2Yr)
```

``` r
consolidated_SA %>% ggplot() + geom_line(aes(date, Spread)) + fmxdat::theme_fmx() +fmxdat::fmx_cols()+
    labs(x="Year", y="Spread", subtitle="ZA Yield Spread (2012-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-15-1.png" alt="ZA Yield Spread (2012-2021)"  />
<p class="caption">
ZA Yield Spread (2012-2021)
</p>

</div>

As can be seen from the figure above, yield spreads have increased
significantly in SA since the COVID-19 pandemic, which coincides with a
sharp increase in the EME volatility index. The next section provides
descriptive statistics for the variables included in the consolidated SA
dataset, after which a correlation analysis will be performed to
determine the influence of the other variables on bond yield spreads in
SA.

### Descriptive Statistics

Tables 1-8 below provide descriptive statistics for the relevant
variables.

``` r
pastecs::stat.desc(SA_bonds[, -1])%>% kable(caption="Summary Statistics: SA Bonds (Full Sample)",  format="html", digits=2)
```

<table>
<caption>
Summary Statistics: SA Bonds (Full Sample)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
ZA_3M
</th>
<th style="text-align:right;">
ZA_10Yr
</th>
<th style="text-align:right;">
ZA_2Yr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
5715.00
</td>
<td style="text-align:right;">
5715.00
</td>
<td style="text-align:right;">
5715.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
6.03
</td>
<td style="text-align:right;">
3.90
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
13.60
</td>
<td style="text-align:right;">
15.27
</td>
<td style="text-align:right;">
12.95
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
10.27
</td>
<td style="text-align:right;">
9.24
</td>
<td style="text-align:right;">
9.04
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
43740.68
</td>
<td style="text-align:right;">
52162.85
</td>
<td style="text-align:right;">
44320.87
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
7.14
</td>
<td style="text-align:right;">
8.72
</td>
<td style="text-align:right;">
7.25
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
7.65
</td>
<td style="text-align:right;">
9.13
</td>
<td style="text-align:right;">
7.76
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
5.81
</td>
<td style="text-align:right;">
2.44
</td>
<td style="text-align:right;">
3.45
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
2.41
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.86
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.24
</td>
</tr>
</tbody>
</table>

``` r
# Since 2011
pastecs::stat.desc(SA_bonds %>% filter(date>ymd(20120506)) %>%
            .[, -1]) %>%  kable(caption="Summary Statistics: SA Bonds (Reduced Sample)",  format="html", digits=2)
```

<table>
<caption>
Summary Statistics: SA Bonds (Reduced Sample)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
ZA_3M
</th>
<th style="text-align:right;">
ZA_10Yr
</th>
<th style="text-align:right;">
ZA_2Yr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
2475.00
</td>
<td style="text-align:right;">
2475.00
</td>
<td style="text-align:right;">
2475.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
6.03
</td>
<td style="text-align:right;">
3.90
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
7.38
</td>
<td style="text-align:right;">
12.42
</td>
<td style="text-align:right;">
8.97
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
4.04
</td>
<td style="text-align:right;">
6.39
</td>
<td style="text-align:right;">
5.07
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
14778.77
</td>
<td style="text-align:right;">
21180.26
</td>
<td style="text-align:right;">
16048.68
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
6.14
</td>
<td style="text-align:right;">
8.72
</td>
<td style="text-align:right;">
6.52
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
5.97
</td>
<td style="text-align:right;">
8.56
</td>
<td style="text-align:right;">
6.48
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.98
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.99
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.15
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(BE_Infl %>% spread(Name, Price) %>% arrange(date) %>% .[, -1]) %>% kable(caption="Summary Statistics: BE Inflation", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: BE Inflation
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
SAGGBE10
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
2475.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
4.50
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
7.88
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
3.38
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
15061.74
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
6.09
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
6.09
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
0.39
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
0.63
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(bonds_2y %>% filter(Name %in% BRIC) %>% 
            arrange(date) %>% spread(Name, Bond_2Yr) %>% 
            filter(date>ymd(20120506)) %>%
            .[,-1]) %>% kable(caption="Summary Statistics: BRIC 2yr Bonds", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: BRIC 2yr Bonds
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Brazil
</th>
<th style="text-align:right;">
CHINA
</th>
<th style="text-align:right;">
India
</th>
<th style="text-align:right;">
Russia
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
2475.00
</td>
<td style="text-align:right;">
2475.00
</td>
<td style="text-align:right;">
2475.00
</td>
<td style="text-align:right;">
2475.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
-3.58
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
4.30
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
4.32
</td>
<td style="text-align:right;">
4.38
</td>
<td style="text-align:right;">
9.93
</td>
<td style="text-align:right;">
17.50
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
7.90
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
6.08
</td>
<td style="text-align:right;">
13.20
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
3760.55
</td>
<td style="text-align:right;">
7080.74
</td>
<td style="text-align:right;">
16862.42
</td>
<td style="text-align:right;">
18880.28
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
1.55
</td>
<td style="text-align:right;">
2.70
</td>
<td style="text-align:right;">
7.08
</td>
<td style="text-align:right;">
7.28
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
6.81
</td>
<td style="text-align:right;">
7.63
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
1.80
</td>
<td style="text-align:right;">
4.37
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
2.09
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.27
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(bonds_10y %>% filter(Name %in% BRIC) %>%
                       arrange(date) %>% spread(Name, Bond_10Yr)  %>%
                       .[,-1])  %>% kable(caption="Summary Statistics: BRIC 10yr Bonds", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: BRIC 10yr Bonds
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Brazil
</th>
<th style="text-align:right;">
CHINA
</th>
<th style="text-align:right;">
India
</th>
<th style="text-align:right;">
Russia
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
3017.00
</td>
<td style="text-align:right;">
4165.00
</td>
<td style="text-align:right;">
5948.00
</td>
<td style="text-align:right;">
3016.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
2931.00
</td>
<td style="text-align:right;">
1783.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2932.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
2.47
</td>
<td style="text-align:right;">
4.95
</td>
<td style="text-align:right;">
5.43
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
7.29
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
12.23
</td>
<td style="text-align:right;">
16.06
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
4.96
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
7.28
</td>
<td style="text-align:right;">
10.63
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
13358.30
</td>
<td style="text-align:right;">
14536.20
</td>
<td style="text-align:right;">
46100.40
</td>
<td style="text-align:right;">
24359.46
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
4.44
</td>
<td style="text-align:right;">
3.43
</td>
<td style="text-align:right;">
7.66
</td>
<td style="text-align:right;">
7.88
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
4.43
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
7.75
</td>
<td style="text-align:right;">
8.08
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
2.27
</td>
<td style="text-align:right;">
2.11
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
1.51
</td>
<td style="text-align:right;">
1.45
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.18
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(usdzar %>% spread(Name, Price) %>%
                       arrange(date)  %>%
                       .[,-1]) %>% kable(caption="Summary Statistics: USDZAR Spot", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: USDZAR Spot
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
SouthAfrica_Cncy
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
8305.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
2.50
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
19.08
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
16.59
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
66913.44
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
7.28
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
8.06
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
15.45
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
3.93
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.49
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(ZA_Infl %>% spread(Name, Price) %>%
                       arrange(date) %>% filter(date>ymd(20120506)) %>%
                       .[,-1]) %>% kable(caption="Summary Statistics: ZA Inflation", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: ZA Inflation
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
ZAR_Infl
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
114.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
2.10
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
7.00
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
4.90
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
567.00
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
5.00
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
4.97
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
1.10
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
1.05
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.21
</td>
</tr>
</tbody>
</table>

``` r
pastecs::stat.desc(VIX %>% filter(Name=="VXEEM") %>% spread(Name, Price) %>% arrange(date) %>%
    filter(date>ymd(20120506)) %>%
    .[,-1]) %>% kable(caption="Summary Statistics: VIX (Emerging Markets)", format="html", digits=2)
```

<table>
<caption>
Summary Statistics: VIX (Emerging Markets)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
VXEEM
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nbr.val
</td>
<td style="text-align:right;">
2475.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.null
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
nbr.na
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
min
</td>
<td style="text-align:right;">
13.28
</td>
</tr>
<tr>
<td style="text-align:left;">
max
</td>
<td style="text-align:right;">
92.46
</td>
</tr>
<tr>
<td style="text-align:left;">
range
</td>
<td style="text-align:right;">
79.18
</td>
</tr>
<tr>
<td style="text-align:left;">
sum
</td>
<td style="text-align:right;">
55177.27
</td>
</tr>
<tr>
<td style="text-align:left;">
median
</td>
<td style="text-align:right;">
21.16
</td>
</tr>
<tr>
<td style="text-align:left;">
mean
</td>
<td style="text-align:right;">
22.29
</td>
</tr>
<tr>
<td style="text-align:left;">
SE.mean
</td>
<td style="text-align:right;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left;">
CI.mean.0.95
</td>
<td style="text-align:right;">
0.25
</td>
</tr>
<tr>
<td style="text-align:left;">
var
</td>
<td style="text-align:right;">
41.09
</td>
</tr>
<tr>
<td style="text-align:left;">
std.dev
</td>
<td style="text-align:right;">
6.41
</td>
</tr>
<tr>
<td style="text-align:left;">
coef.var
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
</tbody>
</table>

For the VIX data, only EME volatility was considered, and for the bond
yields (2 and 10 year), the sample was restricted to only consider the
BRIC economies. For the remainder of this discussion, focus will be on
the domestic (South African) economy.

## Correlation Analysis

``` r
consolidated_SA <- consolidated_SA %>% select(-ZA_2Yr, -ZA_10Yr, -ZAR_Infl) # to avoid unnecessary correlations (inflation captured by BE_infl, Spread captures bond yields at diff. maturities)

# Saving objects for corrplot
M <- consolidated_SA %>% select(-date)
M1 <- cor(M)


cor.mtest <- function(mat, ...) {
 mat <- as.matrix(mat)
n <- ncol(mat)
p.mat<- matrix(NA, n, n)
diag(p.mat) <- 0
for (i in 1:(n - 1)) {
     for (j in (i + 1):n) {
         tmp <- cor.test(mat[, i], mat[, j], ...)
           p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
       }
   }
   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
   p.mat
 }
p.mat <- cor.mtest(M)
```

``` r
corrplot(M1, type="lower", order="hclust",  p.mat = p.mat, sig.level = 0.01, insig = "blank")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-25-1.png" alt="Correlation Plot"  />
<p class="caption">
Correlation Plot
</p>

</div>

``` r
cor1 <- cor.test(consolidated_SA$Spread, consolidated_SA$SAGGBE10, method="pearson")
cor2 <- cor.test(consolidated_SA$Spread, consolidated_SA$USDZAR, method="pearson")
cor3 <- cor.test(consolidated_SA$Spread, consolidated_SA$VXEEM, method="pearson")
```

The yield spread on SA bonds is therefore positively related to the EM
volatility index (0.342602, p-value=4.1763533^{-69}), the USDZAR spot
rate (0.3693363, p-value=7.6125429^{-81}), and negatively related to the
Break-Even inflation measure (-0.5098304, p-value=7.1673854^{-164}). The
stronger US dollar vis-a-vis the Rand, and the greater perceived ‘risk’
in EM’s (measured by the VXEEM index) since the onset of the COVID-19
pandemic, can therefore be seen as two potential causes of the
significantly higher yield spreads seen in SA since 2020.

# Question 2: Portfolio Construction

``` r
pacman::p_load(modelsummary, gt, knitr, kableExtra, tidyverse, lubridate)
```

## Introduction

In this section, data on the ALSI and SWIX top 40 Indexes is used to
conduct analysis. The performance of stocks will be compared, and
stratification (using data on the USDZAR rate) will be used to compare
the return profiles of these indeces during periods of low and high
currency volatility.

## Data

``` r
T40 <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/T40.rds")
Reb_Days <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/Rebalance_days.rds")
USDZAR <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/usdzar.rds")
```

T40 is a variable containing data on daily stock returns, as well as
information on the ALSI (J200) and SWIX (J400) stock indexes. In
addition, to supplement this analysis, data on the USDZAR spot rate is
loaded (called USDZAR) in order to perform stratification, and analyze
the behavior of the indexes during periods of currency volatility.

``` r
T40_info <- left_join(T40 %>% select(Short.Name) %>% unique() %>% mutate(N=(1:92)),
                      T40 %>% select(Tickers) %>% unique() %>% mutate(N=(1:92)),
                      by="N") %>% select(-N) %>% arrange(Tickers) # Keeping descriptions of stocks

T40 <- T40 %>% rename("ALSI"=J200) %>% rename("SWIX"=J400) %>% select(-Short.Name) # renaming for convenience

T40$Tickers <- gsub(" SJ Equity", "", T40$Tickers) # simplifying

sum(is.na(T40$Return)) # no missing values for returns
```

    [1] 0

Firstly, although there were no missing values for the individual stock
returns (see above), there were 2004 missing values for the J400, and
1720 missing values for the J200. Before proceeding, need to impute
these missing returns; using the mean value of the respected index, and
the coalesce function.

``` r
T40 <- T40 %>% group_by(date) %>% 
    mutate(Avg1=mean(SWIX, na.rm=T)) %>%
    mutate(Avg1=coalesce(Avg1, 0)) %>%    # Avg1 creates a column for the mean of the J400
    ungroup() %>%
    mutate(SWIX=coalesce(SWIX, Avg1)) %>%
    mutate(Avg2=mean(ALSI, na.rm=T)) %>%  # Avg2 creates a column for the mean of the J200
    mutate(Avg2=coalesce(Avg2, 0)) %>%
    ungroup() %>%
    mutate(ALSI=coalesce(ALSI, Avg2)) %>%
    select(-Avg1, -Avg2)

sum(is.na(T40$SWIX))  # zero NA's
```

    [1] 0

``` r
sum(is.na(T40$ALSI))  # zero NA's
```

    [1] 0

There are now no missing values for the respective indexes. Secondly,
for this discussion only stocks that have data for the full sample
period (2 January 2008-29 October 2021) will be considered. Initially,
there were 92 tickers in total.

``` r
T40 <- T40 %>% arrange(date) %>%
    filter(!is.na(Return)) %>%
    mutate(YearMonth=format(date, "%Y%B"))

# Considering only indexes with data before 2008-01-01, and using this as common start date

T40_low <- T40 %>% group_by(Tickers) %>%
    filter(date<ymd(20080103)) %>%
   ungroup() %>%
    pull(Tickers) %>% unique() # Tickers available from start date

T40_high <- T40 %>% group_by(Tickers) %>% 
    filter(date>ymd(20211028)) %>% 
    ungroup() %>% pull(Tickers) %>% unique() # Tickers available up to end date

T40 <- T40 %>% filter(Tickers %in% T40_low & Tickers %in% T40_high) %>%
    filter(date>ymd(20080101)) #%>% pull(Tickers) %>% unique() only 20 Tickers have data for full sample period (20080102:20211029)
```

After restricting observations to those only available for the full
sample period, there are 20 tickers remaining, out of the initial 92.

Another issue that was found was that many of the stocks experienced
significant changes in their market capitalization over the sample
period. Specifically, AMS (2016), GFI (2016/17), ANG (2018), EXX (2019),
INP (2020), and INL (2020). The first four are all in the Resources
sector, which could explain why their market capitalization decreased so
much in similar periods (ie when commodity prices crashed), whilst the
latter two are Financials, whose drop in market cap happened in the
period of the COVID pandemic.

These stocks that have some NA’s as values for the Index_Name were all
Large_Caps, after some period (where NA’s present), they became
Mid_Caps. Therefore need to classify them as such for the periods where
information is missing.

``` r
sum(is.na(T40$Index_Name)) # 340 missing values
```

    [1] 340

``` r
T40 <- T40 %>% mutate(Size=coalesce(Index_Name, "Mid_Caps")) %>%
    select(-Index_Name)

sum(is.na(T40$Size)) # No missing values
```

    [1] 0

## Exploratory Analysis

``` r
T40 %>% ggplot() + geom_line(aes(date, Return), color="steelblue", size=1.2, alpha=0.8) +fmxdat::theme_fmx() + fmxdat::fmx_cols() + facet_wrap(~Sector) + labs(x="Year")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-33-1.png" alt="Returns by Sector"  />
<p class="caption">
Returns by Sector
</p>

</div>

``` r
T40 %>% ggplot() + geom_line(aes(date, Return), color="steelblue", size=1.2, alpha=0.8) + fmxdat::theme_fmx() + fmxdat::fmx_cols() + facet_wrap(~Size) + labs(x="Year")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-34-1.png" alt="Returns by Size"  />
<p class="caption">
Returns by Size
</p>

</div>

``` r
T40 %>% select(date, SWIX, ALSI) %>%
  gather(Index, Return, -date) %>% ggplot() +
  geom_line(aes(date, Return), color="steelblue", size=1.2, alpha=0.8) +
  fmxdat::theme_fmx() + fmxdat::fmx_cols() + facet_wrap(~Index) + 
  labs(x="Year")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-35-1.png" alt="Returns by Index"  />
<p class="caption">
Returns by Index
</p>

</div>

## Stratification Analysis

``` r
USDZAR <- USDZAR %>% filter(date>ymd(20080101)) %>% # need to restrict T40 data for Tickers that have data until this date. End date is the same as in previous sections
    spread(Name, Price) %>% rename("USDZAR"= SouthAfrica_Cncy)
```

First loading and visualizing currency data.

``` r
USDZAR %>% ggplot() + geom_line(aes(date, USDZAR)) + fmxdat::theme_fmx() + fmxdat::fmx_cols()+
    labs(x="Year", y="USDZAR Spot", subtitle="USDZAR Spot (2008-2021)")
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-37-1.png" alt="USDZAR Spot (2008-2021)"  />
<p class="caption">
USDZAR Spot (2008-2021)
</p>

</div>

Then, defining periods of high and low currency volatility.

``` r
ZARSD <- USDZAR %>% mutate(YearMonth=format(date, "%Y%B")) %>%
    group_by(YearMonth) %>% dplyr::summarize(SD=sd(USDZAR)*sqrt(252)) %>% # 252 trading days in a year
    mutate(TopQtile=quantile(SD,0.8), BotQtile=quantile(SD,0.2))

Hi_Vol <- ZARSD %>% filter(SD>TopQtile) %>% pull(YearMonth)
Lo_Vol <- ZARSD %>% filter(SD<BotQtile) %>% pull(YearMonth)
```

Below, the periods in question.

``` r
Vol_df <-list(Hi_Vol, Lo_Vol) 
data.frame(Vol_df) %>% kable(col.names=c("High_Volatility", "Low Volatility"), caption="Currency Volatility Months", format="html")
```

<table>
<caption>
Currency Volatility Months
</caption>
<thead>
<tr>
<th style="text-align:left;">
High_Volatility
</th>
<th style="text-align:left;">
Low Volatility
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2008December
</td>
<td style="text-align:left;">
2008May
</td>
</tr>
<tr>
<td style="text-align:left;">
2008November
</td>
<td style="text-align:left;">
2010April
</td>
</tr>
<tr>
<td style="text-align:left;">
2008October
</td>
<td style="text-align:left;">
2010August
</td>
</tr>
<tr>
<td style="text-align:left;">
2009January
</td>
<td style="text-align:left;">
2010December
</td>
</tr>
<tr>
<td style="text-align:left;">
2009March
</td>
<td style="text-align:left;">
2010February
</td>
</tr>
<tr>
<td style="text-align:left;">
2011September
</td>
<td style="text-align:left;">
2010June
</td>
</tr>
<tr>
<td style="text-align:left;">
2012May
</td>
<td style="text-align:left;">
2010March
</td>
</tr>
<tr>
<td style="text-align:left;">
2013May
</td>
<td style="text-align:left;">
2010November
</td>
</tr>
<tr>
<td style="text-align:left;">
2015December
</td>
<td style="text-align:left;">
2010October
</td>
</tr>
<tr>
<td style="text-align:left;">
2016April
</td>
<td style="text-align:left;">
2011April
</td>
</tr>
<tr>
<td style="text-align:left;">
2016August
</td>
<td style="text-align:left;">
2011February
</td>
</tr>
<tr>
<td style="text-align:left;">
2016February
</td>
<td style="text-align:left;">
2011July
</td>
</tr>
<tr>
<td style="text-align:left;">
2016January
</td>
<td style="text-align:left;">
2011June
</td>
</tr>
<tr>
<td style="text-align:left;">
2016June
</td>
<td style="text-align:left;">
2011March
</td>
</tr>
<tr>
<td style="text-align:left;">
2016May
</td>
<td style="text-align:left;">
2012April
</td>
</tr>
<tr>
<td style="text-align:left;">
2016November
</td>
<td style="text-align:left;">
2012February
</td>
</tr>
<tr>
<td style="text-align:left;">
2016September
</td>
<td style="text-align:left;">
2012June
</td>
</tr>
<tr>
<td style="text-align:left;">
2017April
</td>
<td style="text-align:left;">
2012March
</td>
</tr>
<tr>
<td style="text-align:left;">
2017December
</td>
<td style="text-align:left;">
2012November
</td>
</tr>
<tr>
<td style="text-align:left;">
2017March
</td>
<td style="text-align:left;">
2012September
</td>
</tr>
<tr>
<td style="text-align:left;">
2017October
</td>
<td style="text-align:left;">
2013December
</td>
</tr>
<tr>
<td style="text-align:left;">
2018August
</td>
<td style="text-align:left;">
2013February
</td>
</tr>
<tr>
<td style="text-align:left;">
2018June
</td>
<td style="text-align:left;">
2013March
</td>
</tr>
<tr>
<td style="text-align:left;">
2018September
</td>
<td style="text-align:left;">
2013November
</td>
</tr>
<tr>
<td style="text-align:left;">
2019January
</td>
<td style="text-align:left;">
2013October
</td>
</tr>
<tr>
<td style="text-align:left;">
2019June
</td>
<td style="text-align:left;">
2014April
</td>
</tr>
<tr>
<td style="text-align:left;">
2020April
</td>
<td style="text-align:left;">
2014August
</td>
</tr>
<tr>
<td style="text-align:left;">
2020August
</td>
<td style="text-align:left;">
2014July
</td>
</tr>
<tr>
<td style="text-align:left;">
2020March
</td>
<td style="text-align:left;">
2014June
</td>
</tr>
<tr>
<td style="text-align:left;">
2020May
</td>
<td style="text-align:left;">
2014May
</td>
</tr>
<tr>
<td style="text-align:left;">
2020November
</td>
<td style="text-align:left;">
2015January
</td>
</tr>
<tr>
<td style="text-align:left;">
2021June
</td>
<td style="text-align:left;">
2018March
</td>
</tr>
<tr>
<td style="text-align:left;">
2021September
</td>
<td style="text-align:left;">
2019November
</td>
</tr>
</tbody>
</table>

Comparing the relative performance of the two indexes during periods of
currency volatility (as specified above).

``` r
T40_Indeces <- T40 %>% select(date, SWIX, ALSI) %>%
        gather(Index, Return, -date) %>% mutate(YearMonth=format(date, "%Y%B"))

Perf_Comparisons_Indeces <- function(T40_Indeces, YMs, Alias) {

    Unconditional_SD <- T40_Indeces %>% group_by(Index) %>%
        mutate(Full_SD=sd(Return)*sqrt(252)) %>% # 252 trading days p/year
        filter(YearMonth %in% YMs) %>%
        summarise(SD=sd(Return)*sqrt(252), across(.cols=starts_with("Full"), .fns=max)) %>%
        arrange(desc(SD)) %>% mutate(Period=Alias) %>%
        #group_by(Index) %>%
        mutate(Ratio=SD/Full_SD)
    Unconditional_SD
}

perf_hi_Indeces <- Perf_Comparisons_Indeces(T40_Indeces, YMs=Hi_Vol, Alias="High_Vol")
perf_lo_Indeces <- Perf_Comparisons_Indeces(T40_Indeces, YMs=Lo_Vol, Alias = "Low_Vol")
```

### Results

``` r
left_join(perf_hi_Indeces, perf_lo_Indeces, by="Index") %>% kable(caption="Index Performance: Currency Volatility",  format="html", digits=2, col.names=c("Index", "SD_H", "Full_SD_H", "Period", "Ratio_H", "SD_L", "Full_SD_L", "Period", "Ratio_L"))
```

<table>
<caption>
Index Performance: Currency Volatility
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD_H
</th>
<th style="text-align:right;">
Full_SD_H
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio_H
</th>
<th style="text-align:right;">
SD_L
</th>
<th style="text-align:right;">
Full_SD_L
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio_L
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SWIX
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:left;">
High_Vol
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:left;">
Low_Vol
</td>
<td style="text-align:right;">
0.71
</td>
</tr>
<tr>
<td style="text-align:left;">
ALSI
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:left;">
High_Vol
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:left;">
Low_Vol
</td>
<td style="text-align:right;">
0.89
</td>
</tr>
</tbody>
</table>

From the above table, one can see that, during periods of high currency
volatility, the SWIX (J400) index exhibits higher volatility, whereas
during periods of lower currency volatility, the ALSI (J200) has a lower
volatility, as implied by the standard deviation.

# Question 3: Volatility Comparison

``` r
pacman::p_load(modelsummary, gt, knitr, kableExtra, tidyverse,
               lubridate, psych, broom,
               FactoMineR, factoextra, devtools, rmsfuns,
               huxtable, fmxdat)
```

    Error in get(genname, envir = envir) : object 'testthat_print' not found

## Introduction

This discussion aims to compare the concentration of returns among the
ALSI (J200) constituents by using Principal Component Analysis (PCA) to
study the concentration, and commonality fo returns within this index.
In addition, monthly returns volatility will be calculated and this data
will be used for stratification in order to compare return source
concentrations for periods of high and low market volatility.

## Data

``` r
T40 <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/T40.rds")

T40$Tickers <- gsub(" SJ Equity", "", T40$Tickers)

T40 <- T40 %>% select(-Short.Name, -J400) 

T40 <- T40 %>% group_by(Tickers) %>% arrange(date) %>% 
    mutate(Avg1=mean(J200, na.rm=T)) %>%
    mutate(Avg1=coalesce(Avg1, 0)) %>%
    mutate(J200=coalesce(J200, Avg1)) %>%
    mutate(across(.cols=J200, .fns=~log(.)-log(lag(.)), .names="{col}_Returns")) %>%
    filter(date>first(date)) %>%
    select(-Avg1) %>%
    mutate(Avg2=mean(J200_Returns, na.rm=T)) %>%
    mutate(Avg2=coalesce(Avg2, 0)) %>%
    mutate(J200_Returns=coalesce(J200_Returns, Avg2)) %>%
    select(-Avg2) %>%
    ungroup()
# using average J200 Returns, to replace NA's
```

For this question, data on the ALSI and SWIX top 40 Indexes is used to
conduct analysis. This is contained in the T40 dataframe, with the
variable of interest (J200 index) first being cleared of NA’s by
replacing them with the mean value of the index. Returns are then
calculated for the J200 index.

## Exploratory Analysis

``` r
T40 %>% ggplot() + geom_line(aes(date, J200_Returns), alpha=0.9) +
    labs(x="Year") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-44-1.png" alt="J200 Returns"  />
<p class="caption">
J200 Returns
</p>

</div>

``` r
T40 %>% ggplot() + geom_line(aes(date, J200_Returns), alpha=0.9) +
labs(x="Year") + theme_bw() + facet_wrap(~Sector)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-45-1.png" alt="J200 Returns by Sector"  />
<p class="caption">
J200 Returns by Sector
</p>

</div>

The data appears to be centered around the mean, however there do appear
to be some significant values that could potentially bias our results
(outliers).

Removing outliers and plotting.

``` r
T40 %>% mutate(J200_Returns=ifelse(J200_Returns>0.25, 0.25,
                        ifelse(J200_Returns< -0.25, -0.25, J200_Returns))) %>% 
    ggplot() + geom_line(aes(date, J200_Returns), alpha=0.9) + 
    labs(x="Year") + theme_bw() 
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-46-1.png" alt="J200 Returns, sans Outliers"  />
<p class="caption">
J200 Returns, sans Outliers
</p>

</div>

``` r
T40 %>% mutate(J200_Returns=ifelse(J200_Returns>0.25, 0.25,
                        ifelse(J200_Returns< -0.25, -0.25, J200_Returns))) %>% 
    ggplot() + geom_line(aes(date, J200_Returns), alpha=0.9) + 
    labs(x="Year") + theme_bw() + facet_wrap(~Sector)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-47-1.png" alt="J200 Returns by Sector, sans Outliers"  />
<p class="caption">
J200 Returns by Sector, sans Outliers
</p>

</div>

## Principal Component Analysis (PCA)

To perform a PCA, first need to elminate outliers and remove columns
with only NA’s. Although the returns data appears to already be
centered, it must still be demeaned and scaled.

``` r
data_wide <- T40 %>% select(date, Tickers, J200_Returns) %>%
    mutate(J200_Returns=ifelse(J200_Returns>0.25, 0.25,
                        ifelse(J200_Returns< -0.25, -0.25, J200_Returns))) %>%
    mutate(J200_Returns=J200_Returns-mean(J200_Returns)) %>%
    spread(Tickers, J200_Returns)

data_wide <- data_wide[, colSums(is.na(data_wide)) < nrow(data_wide)] %>% 
.[, colSums(is.na(.) | . == 0, na.rm = TRUE) <= 150] %>% select(-date)

data_wide[is.na(data_wide)] <- 0


df_PCA <- PCA(data_wide, graph = FALSE, scale.unit = TRUE)
data_wide <- unlist(data_wide)
```

A Scree plot can be used to determine the amount of variation explained
by each Principal Component (PC).

``` r
fviz_screeplot(df_PCA, ncp = 10)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-49-1.png" alt="Scree Plot"  />
<p class="caption">
Scree Plot
</p>

</div>

This indicates that the majority of the variation in the data is
explained by the first PC, with the relative variation explained by
subsequent components beginning to taper (ie curve flatten) after the
fourth component.

The plot below shows the relationship between all variables and the two
principal components; positively correlated variables are grouped
together, and negatively correlated variables are positioned in opposite
quadrants.

``` r
fviz_pca_var(df_PCA, col.var = "steelblue") + theme_minimal()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-50-1.png" alt="Variables-PCA"  />
<p class="caption">
Variables-PCA
</p>

</div>

From this PCA analysis, one can extract the contribution of each
variable as well as its quality of representation (given in the tables
below).

``` r
df_PCA$var$contrib %>% kable(caption="Contribution of Variables",  format="html", digits=2)
```

<table>
<caption>
Contribution of Variables
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
12.25
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.27
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
25.63
</td>
<td style="text-align:right;">
6.13
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
3.34
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.59
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
27.64
</td>
<td style="text-align:right;">
15.95
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
4.60
</td>
<td style="text-align:right;">
21.59
</td>
<td style="text-align:right;">
5.92
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
2.16
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
7.13
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
1.69
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
18.45
</td>
<td style="text-align:right;">
24.38
</td>
<td style="text-align:right;">
4.80
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
12.69
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.53
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.43
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
7.27
</td>
<td style="text-align:right;">
12.74
</td>
<td style="text-align:right;">
21.12
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
1.20
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
6.23
</td>
<td style="text-align:right;">
15.13
</td>
<td style="text-align:right;">
19.89
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
2.81
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
3.12
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
2.16
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
30.83
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
13.76
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
9.86
</td>
<td style="text-align:right;">
6.32
</td>
<td style="text-align:right;">
34.97
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
5.75
</td>
<td style="text-align:right;">
1.67
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
1.64
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
14.03
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
9.50
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
1.94
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
9.90
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
32.90
</td>
</tr>
</tbody>
</table>

``` r
df_PCA$var$cos2 %>% kable(caption="Cos2: Quality of Representation",  format="html", digits=2)
```

<table>
<caption>
Cos2: Quality of Representation
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.31
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.34
</td>
</tr>
</tbody>
</table>

And, finally, plotting each variables contribution to the PC’s.

``` r
fviz_contrib(df_PCA, choice = "var", axes = 1)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-53-1.png" alt="Contribution of Variables to PC1"  />
<p class="caption">
Contribution of Variables to PC1
</p>

</div>

``` r
fviz_contrib(df_PCA, choice = "var", axes = 2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-54-1.png" alt="Contribution of Variables to PC2"  />
<p class="caption">
Contribution of Variables to PC2
</p>

</div>

``` r
fviz_contrib(df_PCA, choice = "var", axes = 1:2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-55-1.png" alt="Contribution of Variables to PC's 1 &amp; 2"  />
<p class="caption">
Contribution of Variables to PC’s 1 & 2
</p>

</div>

### Stratification by Volatility

In order to do stratification by period of volatility in returns, one
must first identify the periods of low vs high volatility, and then
perform the PCA analysis for both periods.

``` r
Vol <- T40 %>% mutate(YearMonth=format(date, "%Y%B")) %>%
    group_by(YearMonth) %>% dplyr::summarize(SD=sd(J200_Returns)*sqrt(252)) %>% # 252 trading days in a year
    mutate(TopQtile=quantile(SD,0.8), BotQtile=quantile(SD,0.2))

Hi_Vol_PCA <- Vol %>% filter(SD>TopQtile) %>% pull(YearMonth)
Lo_Vol_PCA <- Vol %>% filter(SD<BotQtile) %>% pull(YearMonth)


T40_Hi_Vol <- T40 %>% mutate(YearMonth=format(date, "%Y%B")) %>% group_by(YearMonth) %>%
    filter(YearMonth %in% Hi_Vol_PCA) %>% ungroup()

T40_Lo_Vol <- T40 %>% mutate(YearMonth=format(date, "%Y%B")) %>% group_by(YearMonth) %>%
    filter(YearMonth %in% Lo_Vol_PCA) %>% ungroup()
```

The periods of high and low volatility extracted can be seen below.

``` r
Vol_df_PCA <-list(Hi_Vol_PCA, Lo_Vol_PCA) 
data.frame(Vol_df_PCA) %>% kable(col.names=c("High_Volatility", "Low Volatility"), caption="Currency Volatility Months", format="html")
```

<table>
<caption>
Currency Volatility Months
</caption>
<thead>
<tr>
<th style="text-align:left;">
High_Volatility
</th>
<th style="text-align:left;">
Low Volatility
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2008August
</td>
<td style="text-align:left;">
2010April
</td>
</tr>
<tr>
<td style="text-align:left;">
2008December
</td>
<td style="text-align:left;">
2010August
</td>
</tr>
<tr>
<td style="text-align:left;">
2008March
</td>
<td style="text-align:left;">
2010July
</td>
</tr>
<tr>
<td style="text-align:left;">
2008November
</td>
<td style="text-align:left;">
2010November
</td>
</tr>
<tr>
<td style="text-align:left;">
2008October
</td>
<td style="text-align:left;">
2010October
</td>
</tr>
<tr>
<td style="text-align:left;">
2008September
</td>
<td style="text-align:left;">
2010September
</td>
</tr>
<tr>
<td style="text-align:left;">
2009February
</td>
<td style="text-align:left;">
2011April
</td>
</tr>
<tr>
<td style="text-align:left;">
2009January
</td>
<td style="text-align:left;">
2011February
</td>
</tr>
<tr>
<td style="text-align:left;">
2009March
</td>
<td style="text-align:left;">
2011January
</td>
</tr>
<tr>
<td style="text-align:left;">
2009May
</td>
<td style="text-align:left;">
2011July
</td>
</tr>
<tr>
<td style="text-align:left;">
2011September
</td>
<td style="text-align:left;">
2011May
</td>
</tr>
<tr>
<td style="text-align:left;">
2012June
</td>
<td style="text-align:left;">
2011November
</td>
</tr>
<tr>
<td style="text-align:left;">
2013March
</td>
<td style="text-align:left;">
2012April
</td>
</tr>
<tr>
<td style="text-align:left;">
2014December
</td>
<td style="text-align:left;">
2012August
</td>
</tr>
<tr>
<td style="text-align:left;">
2016December
</td>
<td style="text-align:left;">
2012December
</td>
</tr>
<tr>
<td style="text-align:left;">
2016March
</td>
<td style="text-align:left;">
2012February
</td>
</tr>
<tr>
<td style="text-align:left;">
2016May
</td>
<td style="text-align:left;">
2012January
</td>
</tr>
<tr>
<td style="text-align:left;">
2016September
</td>
<td style="text-align:left;">
2012July
</td>
</tr>
<tr>
<td style="text-align:left;">
2017December
</td>
<td style="text-align:left;">
2012May
</td>
</tr>
<tr>
<td style="text-align:left;">
2018December
</td>
<td style="text-align:left;">
2012November
</td>
</tr>
<tr>
<td style="text-align:left;">
2018March
</td>
<td style="text-align:left;">
2013February
</td>
</tr>
<tr>
<td style="text-align:left;">
2019June
</td>
<td style="text-align:left;">
2013January
</td>
</tr>
<tr>
<td style="text-align:left;">
2019March
</td>
<td style="text-align:left;">
2013November
</td>
</tr>
<tr>
<td style="text-align:left;">
2019September
</td>
<td style="text-align:left;">
2013October
</td>
</tr>
<tr>
<td style="text-align:left;">
2020April
</td>
<td style="text-align:left;">
2014August
</td>
</tr>
<tr>
<td style="text-align:left;">
2020June
</td>
<td style="text-align:left;">
2014July
</td>
</tr>
<tr>
<td style="text-align:left;">
2020March
</td>
<td style="text-align:left;">
2014June
</td>
</tr>
<tr>
<td style="text-align:left;">
2020May
</td>
<td style="text-align:left;">
2014May
</td>
</tr>
<tr>
<td style="text-align:left;">
2020November
</td>
<td style="text-align:left;">
2017August
</td>
</tr>
<tr>
<td style="text-align:left;">
2020September
</td>
<td style="text-align:left;">
2017July
</td>
</tr>
<tr>
<td style="text-align:left;">
2021August
</td>
<td style="text-align:left;">
2017May
</td>
</tr>
<tr>
<td style="text-align:left;">
2021June
</td>
<td style="text-align:left;">
2017October
</td>
</tr>
<tr>
<td style="text-align:left;">
2021September
</td>
<td style="text-align:left;">
2018July
</td>
</tr>
</tbody>
</table>

First, creating “wide” data frames for the high and low volatility
periods.

``` r
# high volatility period
data_wide_Hi_Vol <- T40_Hi_Vol %>% select(date, Tickers, J200_Returns) %>%
    mutate(J200_Returns=ifelse(J200_Returns>0.25, 0.25,
                        ifelse(J200_Returns< -0.25, -0.25, J200_Returns))) %>%
    mutate(J200_Returns=J200_Returns-mean(J200_Returns)) %>%  # demeaning the returns and removing extreme observations
    spread(Tickers, J200_Returns)   

data_wide_Hi_Vol <- data_wide_Hi_Vol[, colSums(is.na(data_wide_Hi_Vol)) < nrow(data_wide_Hi_Vol)] %>% 
.[, colSums(is.na(.) | . == 0, na.rm = TRUE) <= 150] %>% select(-date) # removing columns with NA's

data_wide_Hi_Vol[is.na(data_wide_Hi_Vol)] <- 0  # setting remaining NA's equal to zero


df_PCA_Hi_Vol <- PCA(data_wide_Hi_Vol, graph = FALSE, scale.unit = TRUE) # the dataframe

# low volatility period
data_wide_Lo_Vol <- T40_Lo_Vol %>% select(date, Tickers, J200_Returns) %>%
    mutate(J200_Returns=ifelse(J200_Returns>0.25, 0.25,
                        ifelse(J200_Returns< -0.25, -0.25, J200_Returns))) %>%
    mutate(J200_Returns=J200_Returns-mean(J200_Returns)) %>%  # demeaning the returns and removing extreme observations
    spread(Tickers, J200_Returns)

data_wide_Lo_Vol <- data_wide_Lo_Vol[, colSums(is.na(data_wide_Lo_Vol)) < nrow(data_wide_Lo_Vol)] %>% 
.[, colSums(is.na(.) | . == 0, na.rm = TRUE) <= 150] %>% select(-date) # removing columns with NA's

data_wide_Lo_Vol[is.na(data_wide_Lo_Vol)] <- 0 # setting remaining NA's equal to zero


df_PCA_Lo_Vol <- PCA(data_wide_Lo_Vol, graph = FALSE, scale.unit = TRUE) # the dataframe
```

The PCA analysis conducted below follows on from the one done in the
previous section, differentiating between periods of high and low
volatility in returns.

*Scree Plots*

``` r
fviz_screeplot(df_PCA_Hi_Vol, ncp = 10)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-59-1.png" alt="Scree Plot (High Volatility)"  />
<p class="caption">
Scree Plot (High Volatility)
</p>

</div>

``` r
fviz_screeplot(df_PCA_Lo_Vol, ncp = 10)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-60-1.png" alt="Scree Plot(Low Volatility)"  />
<p class="caption">
Scree Plot(Low Volatility)
</p>

</div>

From the above Scree Plots, one can see that during the high volatility
period, PC1 explains a greater proportion of the variation in returns.
Whereas during periods of low volatility, the significance of PC1 is
diminished, whilst PC2 and PC3 explain a greater proportion of the
variation.

*Importance of Components*

``` r
fviz_pca_var(df_PCA_Hi_Vol, col.var = "steelblue") + theme_minimal()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-61-1.png" alt="Variables-PCA (High Volatility)"  />
<p class="caption">
Variables-PCA (High Volatility)
</p>

</div>

``` r
fviz_pca_var(df_PCA_Lo_Vol, col.var = "steelblue") + theme_minimal()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-62-1.png" alt="Variables-PCA (Low Volatility)"  />
<p class="caption">
Variables-PCA (Low Volatility)
</p>

</div>

From the above figures, the quality of variables during high volatility
periods appears to be greater (further from the origin). Their
directions tend to be similar in both scenarios, however it is difficult
to tell with any certainty due to the overlap caused by the greater
number of relevant variables during low volatility periods.

*Variable Contributions*

``` r
df_PCA_Hi_Vol$var$contrib %>% kable(caption="Contribution of Variables (High Vol)",  format="html", digits=2)
```

<table>
<caption>
Contribution of Variables (High Vol)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
10.17
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
1.67
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
19.68
</td>
<td style="text-align:right;">
8.75
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.16
</td>
</tr>
<tr>
<td style="text-align:left;">
AMS
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
14.05
</td>
<td style="text-align:right;">
9.53
</td>
<td style="text-align:right;">
4.19
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
13.10
</td>
<td style="text-align:right;">
25.06
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.45
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
14.42
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
1.61
</td>
<td style="text-align:right;">
0.53
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
5.75
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
2.56
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
4.70
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
3.69
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
11.81
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
9.32
</td>
</tr>
<tr>
<td style="text-align:left;">
GFI
</td>
<td style="text-align:right;">
1.53
</td>
<td style="text-align:right;">
10.90
</td>
<td style="text-align:right;">
28.23
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left;">
GRT
</td>
<td style="text-align:right;">
2.49
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
10.17
</td>
<td style="text-align:right;">
6.78
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
7.69
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
15.49
</td>
<td style="text-align:right;">
16.02
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
6.13
</td>
<td style="text-align:right;">
4.92
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
12.29
</td>
<td style="text-align:right;">
22.83
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
16.66
</td>
<td style="text-align:right;">
0.43
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
12.42
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
1.17
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
2.20
</td>
<td style="text-align:right;">
5.13
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
2.59
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:right;">
2.64
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
15.16
</td>
<td style="text-align:right;">
5.81
</td>
</tr>
<tr>
<td style="text-align:left;">
RMH
</td>
<td style="text-align:right;">
7.57
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
11.84
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
12.22
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
6.13
</td>
</tr>
<tr>
<td style="text-align:left;">
SHP
</td>
<td style="text-align:right;">
3.69
</td>
<td style="text-align:right;">
7.06
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
1.72
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
7.64
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
13.31
</td>
<td style="text-align:right;">
2.04
</td>
<td style="text-align:right;">
9.24
</td>
<td style="text-align:right;">
0.90
</td>
</tr>
</tbody>
</table>

``` r
df_PCA_Lo_Vol$var$contrib %>% kable(caption="Contribution of Variables (Low Vol)",  format="html", digits=2)
```

<table>
<caption>
Contribution of Variables (Low Vol)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
5.88
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
5.81
</td>
<td style="text-align:right;">
2.01
</td>
<td style="text-align:right;">
6.43
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
1.96
</td>
</tr>
<tr>
<td style="text-align:left;">
AMS
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
7.68
</td>
<td style="text-align:right;">
7.65
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
5.90
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
26.15
</td>
<td style="text-align:right;">
1.41
</td>
</tr>
<tr>
<td style="text-align:left;">
APN
</td>
<td style="text-align:right;">
3.95
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
1.47
</td>
</tr>
<tr>
<td style="text-align:left;">
ARI
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
5.03
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
1.05
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
8.44
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
1.41
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
4.33
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.40
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
3.81
</td>
<td style="text-align:right;">
3.22
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
EXX
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
3.24
</td>
<td style="text-align:right;">
5.77
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
2.10
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
7.63
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
4.01
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.21
</td>
</tr>
<tr>
<td style="text-align:left;">
GFI
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
7.20
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
24.73
</td>
<td style="text-align:right;">
2.54
</td>
</tr>
<tr>
<td style="text-align:left;">
GRT
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
2.11
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
2.83
</td>
</tr>
<tr>
<td style="text-align:left;">
IMP
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
7.63
</td>
<td style="text-align:right;">
6.98
</td>
<td style="text-align:right;">
0.67
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
14.06
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
18.86
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
16.13
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
17.95
</td>
</tr>
<tr>
<td style="text-align:left;">
ITU
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
2.12
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
KIO
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
1.96
</td>
<td style="text-align:right;">
6.68
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
3.32
</td>
</tr>
<tr>
<td style="text-align:left;">
MND
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
13.06
</td>
<td style="text-align:right;">
8.01
</td>
<td style="text-align:right;">
5.07
</td>
<td style="text-align:right;">
14.63
</td>
</tr>
<tr>
<td style="text-align:left;">
MNP
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
15.46
</td>
<td style="text-align:right;">
8.15
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
12.62
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
7.35
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
1.79
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
2.75
</td>
</tr>
<tr>
<td style="text-align:left;">
OML
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
4.70
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
3.39
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
7.52
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left;">
RMH
</td>
<td style="text-align:right;">
8.84
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
SAB
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
13.03
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.63
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
7.74
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.17
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
SHP
</td>
<td style="text-align:right;">
2.79
</td>
<td style="text-align:right;">
1.23
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
2.59
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.85
</td>
</tr>
<tr>
<td style="text-align:left;">
SNH
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.56
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
0.47
</td>
</tr>
<tr>
<td style="text-align:left;">
TBS
</td>
<td style="text-align:right;">
4.57
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
2.73
</td>
</tr>
<tr>
<td style="text-align:left;">
TRU
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
1.59
</td>
</tr>
<tr>
<td style="text-align:left;">
VOD
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
1.62
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
</tbody>
</table>

*Variable Quality*

``` r
df_PCA_Hi_Vol$var$cos2 %>% kable(caption="Cos2: Quality of Representation (High Vol)",  format="html", digits=2)
```

<table>
<caption>
Cos2: Quality of Representation (High Vol)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
AMS
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.11
</td>
</tr>
<tr>
<td style="text-align:left;">
GFI
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
GRT
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.20
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.28
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left;">
RMH
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.15
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left;">
SHP
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
</tbody>
</table>

``` r
df_PCA_Lo_Vol$var$cos2 %>% kable(caption="Cos2: Quality of Representation (Low Vol)",  format="html", digits=2)
```

<table>
<caption>
Cos2: Quality of Representation (Low Vol)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dim.1
</th>
<th style="text-align:right;">
Dim.2
</th>
<th style="text-align:right;">
Dim.3
</th>
<th style="text-align:right;">
Dim.4
</th>
<th style="text-align:right;">
Dim.5
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABG
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
AGL
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
AMS
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
ANG
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
APN
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
ARI
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
BHP
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
BVT
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
CFR
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
EXX
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
FSR
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
GFI
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
GRT
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
IMP
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
INL
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
INP
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.28
</td>
</tr>
<tr>
<td style="text-align:left;">
ITU
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
KIO
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
MND
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left;">
MNP
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
NED
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
NPN
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
OML
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left;">
REM
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
RMH
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SAB
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
SBK
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SHP
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
SLM
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
SNH
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
SOL
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;">
TBS
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
TRU
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
VOD
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
</tbody>
</table>

*Total Contributions to PC1*

``` r
fviz_contrib(df_PCA_Hi_Vol, choice = "var", axes = 1)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-67-1.png" alt="Contribution of Variables to PC1 (High Volatility)"  />
<p class="caption">
Contribution of Variables to PC1 (High Volatility)
</p>

</div>

``` r
fviz_contrib(df_PCA_Lo_Vol, choice = "var", axes = 1)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-68-1.png" alt="Contribution of Variables to PC1 (Low Volatility)"  />
<p class="caption">
Contribution of Variables to PC1 (Low Volatility)
</p>

</div>

*Total Contributions to PC2*

``` r
fviz_contrib(df_PCA_Hi_Vol, choice = "var", axes = 2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-69-1.png" alt="Contribution of Variables to PC2 (High Volatility)"  />
<p class="caption">
Contribution of Variables to PC2 (High Volatility)
</p>

</div>

``` r
fviz_contrib(df_PCA_Lo_Vol, choice = "var", axes = 2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-70-1.png" alt="Contribution of Variables to PC2 (Low Volatility)"  />
<p class="caption">
Contribution of Variables to PC2 (Low Volatility)
</p>

</div>

*Total Contributions to PC1 & PC2*

``` r
fviz_contrib(df_PCA_Hi_Vol, choice = "var", axes = 1:2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-71-1.png" alt="Contribution of Variables to PC1 &amp; PC2 (High Volatility)"  />
<p class="caption">
Contribution of Variables to PC1 & PC2 (High Volatility)
</p>

</div>

``` r
fviz_contrib(df_PCA_Lo_Vol, choice = "var", axes = 1:2)
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-72-1.png" alt="Contribution of Variables to PC1 &amp; PC2 (Low Volatility)"  />
<p class="caption">
Contribution of Variables to PC1 & PC2 (Low Volatility)
</p>

</div>

# Question 4: Volatility and GARCH Estimates

``` r
pacman::p_load(MTS, robustbase, tidyverse, rugarch, lubridate, forecast, tbl2xts, PerformanceAnalytics)
```

## Introduction

This section aims to use currency data to firstly compare the volatility
of the ZAR relative to other currencies, to determine whether it has
been ‘one of the most volatile currencies’ over the past few years. And
secondly, to examine whether the ZAR benefits from a strong dollar (ie
during periods of risk-on sentiment). Intuitively, because the ZAR is
such a liquid currency, and SA has a highly developed financial system-
relative to other EME’s, but especially when compared to other African
countries- the ZAR should be one of the first EM currencies that
investors flock to when EM risk is perceived low (ie when the dollar is
strong).

## Data

``` r
cncy <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/currencies.rds")
cncy_Carry <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/cncy_Carry.rds")
cncy_value <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/cncy_value.rds")
cncyIV <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/cncyIV.rds")
bbdxy <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/data/bbdxy.rds")
```

The data sourced for this analysis exclusively relates to currencies.
cncy contains spot rates for 41 different currencies relative to the
USD. cncy_Carry and cncy_value are proxy measures, based on Deutsche
bank indexes, for the returns of carry trade and value strategies,
respectively. Currency implied volatility is captured by cncyIV, and has
values for 32 currencies. Finally, bbdxy contains data from the
Bloomberg Dollar Spot Index, which ‘tracks the performance of ten
leading global currencies’ relative to the US Dollar.

## Exploratory Analysis

``` r
# Creating a joint dataset for the variables relevant to SA

consolidated_ZAR <- left_join(cncy %>% filter(Name=="SouthAfrica_Cncy") %>% filter(date>ymd(19990207)) %>% spread(Name, Price),
                              cncyIV %>% filter(Name=="SouthAfrica_IV") %>% spread(Name, Price), by="date")
    
consolidated_ZAR <- left_join(consolidated_ZAR %>% filter(date>ymd(20000917)), # Carry returns only available from this date
                              cncy_Carry %>% spread(Name, Price), by="date") %>%
    rename("Spot"=SouthAfrica_Cncy) %>% rename("IV"=SouthAfrica_IV) %>% rename("Carry"=DBHVG10U) %>%
    left_join(., cncy_value %>% filter(date>ymd(20000917)) %>% spread(Name, Price), by="date") %>% rename("Value"=DBPPPUSF) %>%
    filter(date>ymd(20041230)) %>% left_join(., bbdxy %>% spread(Name, Price), by="date") # value returns only available from this date
```

Plotting variables to see their, historical, movements.

``` r
consolidated_ZAR %>% ggplot() + geom_line(aes(date, Spot)) + labs(x="Year", y="ZAR Spot") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-76-1.png" alt="ZAR Spot"  />
<p class="caption">
ZAR Spot
</p>

</div>

``` r
consolidated_ZAR  %>% ggplot() + geom_line(aes(date, IV)) + labs(x="Year", y="ZAR Implied Volatility") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-77-1.png" alt="ZAR Implied Volatility"  />
<p class="caption">
ZAR Implied Volatility
</p>

</div>

``` r
consolidated_ZAR %>%  ggplot() + geom_line(aes(date, Carry)) + labs(x="Year", y="Carry Trade Returns") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-78-1.png" alt="Carry Trade Returns"  />
<p class="caption">
Carry Trade Returns
</p>

</div>

``` r
consolidated_ZAR %>% ggplot() + geom_line(aes(date, Value)) + labs(x="Year", y="Value Strategy Returns") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-79-1.png" alt="Value Strategy Returns"  />
<p class="caption">
Value Strategy Returns
</p>

</div>

``` r
consolidated_ZAR %>% ggplot() + geom_line(aes(date, BBDXY)) + labs(x="Year", y="Dollar Spot Index") + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-80-1.png" alt="Bloomberg Dollar Spot Index"  />
<p class="caption">
Bloomberg Dollar Spot Index
</p>

</div>

In a (completely) informal way, comparing the above figures for the
Bloomberg Dollar Spot Index and the performance of the Rand, one can see
that these two seem to have a positive relationship; when the Dollar is
getting, the Rand benefits.

``` r
consolidated_ZAR %>% ggplot(aes(x=BBDXY, y=Spot)) + geom_point() +geom_smooth(method=lm) + theme_bw()
```

<div class="figure" style="text-align: left">

<img src="README_files/figure-markdown_github/unnamed-chunk-81-1.png" alt="Relationship between ZAR Spot and Dollar Spot Index"  />
<p class="caption">
Relationship between ZAR Spot and Dollar Spot Index
</p>

</div>

Indeed from the scatterplot above, the relationship appears to be
confirmed, with the ZAR spot and the Dollar Spot Index having a
correlation of 0.8608497 (p-value=0).

## MV-GARCH

``` r
sum(is.na(cncy$Price)) # no NA's
```

    [1] 0

``` r
cncy_xts <- cncy %>% group_by(Name) %>% mutate(scaledprice=Price-mean(Price)) %>%
  ungroup() %>%
  tbl_xts(., cols_to_xts='Price', spread_by='Name') %>%
  .[lubridate::wday(.) %in% 2:6] %>% .[, !colSums(is.na(.)) > 0] # restricting to only weekdays and eliminating columns with NA's

# MarchTest(cncy_xts, lag=10) # Conditional heteroskedasticity 

  # commented out because this test had delivered p-values=0 and significant test statistics. Once I attempted to knit the final version of this README got **error that systen is computationally singular. Rather than discard the rest of the analysis, I continued without conducting the Heteroskedasticity test to determine the appropriateness of MVGARCH models.
```

MARCH test is a MV Portmanteau test where H0: no conditional
heteroskedasticity.

All the test statistics are significant (p-value less than zero), reject
H0, therefore Multivariate GARCH model is appropriate.

``` r
cncy_VAR <- VAR(cncy_xts, p=1)
```

    Constant term: 
    Estimates:  -0.2362977 -0.2515876 -96.35898 -0.4397427 -2.673973 -0.4584698 0.05661471 -10.5872 -1.144015 -1.602613 -1.363424 -0.6360894 -3.365335 -0.3233142 0.008100806 0.4206628 -0.357759 -4.645672 -327.4024 -3.558261 -4.929655 2.029351 -1.45837 1.386916 -0.4752275 
    Std.Error:  0.477152 0.2815927 165.8737 1.590206 1.865654 0.2424447 0.1316372 9.342639 0.8053298 34.55436 0.8127656 4.892328 2.534432 0.5586498 0.8616549 0.04632835 0.2449973 4.543315 517.9795 2.672026 3.822141 9.107542 1.389578 0.03388383 0.1807594 
    AR coefficient matrix 
    AR( 1 )-matrix 
               [,1]      [,2]      [,3]      [,4]      [,5]     [,6]      [,7]
     [1,]  0.986184  0.005339  9.11e-06 -2.62e-04 -1.50e-03  0.01492  8.86e-03
     [2,]  0.004277  0.992676  4.22e-06  3.97e-04 -1.56e-03  0.01846 -6.99e-05
     [3,]  1.881272  1.454063  9.92e-01  3.49e-03 -8.29e-01  6.46942 -2.14e-01
     [4,]  0.001473  0.004959  2.74e-05  9.94e-01 -1.85e-02  0.13857  5.95e-05
     [5,]  0.010670  0.001662  1.11e-05 -2.90e-03  9.64e-01  0.26867  2.45e-02
     [6,]  0.001081  0.001956  1.79e-07 -9.44e-05  4.05e-03  0.96606  2.50e-03
     [7,] -0.000573  0.002510 -3.07e-06 -7.27e-05 -8.61e-04  0.00583  9.89e-01
     [8,]  0.147125 -0.058351 -1.24e-04 -4.62e-03  1.20e-03  0.02041  6.52e-02
     [9,] -0.002340  0.004752 -1.34e-06  6.61e-04 -3.32e-03  0.02884 -4.40e-03
    [10,]  0.818189  0.131881 -6.76e-04 -8.69e-03  3.48e-01 -1.78446 -1.37e-01
    [11,]  0.009376 -0.004461 -1.82e-06  1.21e-04  2.35e-03 -0.01894  3.89e-03
    [12,]  0.066550 -0.019294  4.41e-05 -1.02e-03 -3.47e-02  0.24148 -3.65e-03
    [13,]  0.042976 -0.015000  5.96e-05 -9.90e-04 -2.85e-02  0.31242 -5.37e-03
    [14,]  0.005161 -0.006119  7.34e-06 -1.73e-04  1.47e-04  0.01349  1.04e-02
    [15,] -0.007022 -0.002575  2.15e-05 -1.01e-03 -3.06e-03  0.03632  1.46e-02
    [16,]  0.000734 -0.000452  2.54e-07 -4.02e-05  7.93e-05 -0.00207 -1.93e-04
    [17,]  0.005228  0.000828  2.56e-06 -2.20e-04  9.39e-04 -0.00275 -5.76e-04
    [18,]  0.025400 -0.008545  4.71e-05  6.54e-04 -2.71e-02  0.25519 -2.56e-02
    [19,] -1.272388  4.183353 -3.64e-03 -3.37e-01 -2.19e-01  3.36132 -9.02e+00
    [20,]  0.057464 -0.009604  1.74e-05 -2.84e-03 -2.98e-02  0.25951  3.10e-02
    [21,]  0.053502 -0.040832  1.07e-05 -1.39e-03 -2.69e-02  0.16192  7.92e-02
    [22,]  0.266044 -0.158326  2.25e-05  8.78e-03 -3.10e-02  0.56088  7.86e-02
    [23,] -0.001687 -0.004575  1.69e-05  5.24e-05 -3.13e-04 -0.00231  7.23e-03
    [24,]  0.000355 -0.000217  1.34e-07 -1.70e-04  2.35e-04 -0.00307 -4.00e-04
    [25,]  0.003590  0.000442  1.73e-06 -2.00e-04 -4.74e-04  0.00727 -1.60e-04
               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
     [1,]  5.89e-05 -1.22e-03  2.18e-05  0.001875 -1.12e-04 -8.06e-04  4.31e-03
     [2,]  5.61e-05 -1.16e-03  1.43e-05  0.001448 -4.95e-05 -4.95e-04 -1.23e-03
     [3,]  1.53e-02 -4.09e-01  7.77e-03  0.626126 -1.16e-01 -3.79e-01 -1.74e-01
     [4,] -8.11e-05  4.57e-03 -2.84e-05  0.002781 -5.84e-04  1.52e-03 -9.03e-03
     [5,]  1.06e-04 -2.81e-03 -8.35e-05  0.003918  1.26e-04  1.61e-04 -4.02e-03
     [6,]  5.96e-06  1.50e-04 -1.10e-05 -0.000144  1.22e-04 -4.00e-06 -5.22e-04
     [7,] -5.83e-05 -1.02e-04  7.43e-06  0.000655 -3.43e-05 -1.58e-05  7.93e-04
     [8,]  9.95e-01  3.65e-02  5.73e-04 -0.004008  1.05e-03  9.88e-03 -5.49e-02
     [9,]  1.90e-04  9.93e-01 -7.03e-07 -0.003156  4.63e-04 -2.02e-03 -2.42e-04
    [10,] -1.20e-03 -7.30e-02  9.94e-01 -0.035172  1.10e-02 -5.71e-02 -4.13e-01
    [11,] -1.67e-05  1.04e-03  9.82e-05  0.991329  9.12e-04  8.89e-04 -7.30e-03
    [12,] -5.74e-04 -5.75e-03  2.56e-04  0.040178  9.91e-01  9.05e-03 -3.04e-02
    [13,]  5.92e-04 -1.30e-02  1.66e-05  0.021051  8.16e-04  9.83e-01 -1.25e-02
    [14,] -3.18e-05 -6.12e-04 -3.80e-06  0.002592  1.20e-05 -5.79e-04  9.93e-01
    [15,]  1.63e-04 -1.45e-03 -4.71e-05 -0.000411  4.42e-04 -1.51e-03  2.44e-03
    [16,]  4.99e-06  1.16e-04 -1.75e-06 -0.000218 -1.43e-05  2.47e-04 -6.30e-04
    [17,] -9.19e-05  3.35e-04  2.28e-05  0.001064 -4.42e-06 -6.53e-05 -1.06e-03
    [18,]  6.66e-04 -2.88e-02  3.67e-04  0.030028 -1.97e-03 -4.05e-03  1.33e-02
    [19,] -7.52e-02 -2.15e+00 -1.14e-02 -1.848208  3.88e-01 -5.22e-01  2.41e+00
    [20,] -5.73e-05 -1.45e-03 -6.92e-05  0.010198 -3.74e-04 -4.72e-04 -1.44e-02
    [21,] -9.40e-04  6.12e-03  4.91e-04 -0.016926  3.19e-03 -3.04e-03 -2.34e-02
    [22,]  2.29e-04  2.36e-03  3.67e-04  0.002151  7.29e-03 -1.70e-02 -1.23e-01
    [23,] -3.23e-05 -1.44e-03  1.15e-04 -0.001649  1.29e-03 -1.83e-03  2.33e-03
    [24,]  3.73e-06 -4.26e-05 -7.18e-06  0.000738 -1.56e-04  7.46e-05 -7.27e-05
    [25,] -2.41e-05 -5.10e-04 -1.72e-05  0.000660  1.16e-04 -3.54e-04 -1.19e-03
              [,15]    [,16]     [,17]     [,18]     [,19]     [,20]     [,21]
     [1,] -5.45e-04   0.1421 -0.003799 -1.12e-04 -6.70e-07  1.65e-03 -3.35e-04
     [2,] -9.08e-05   0.0587 -0.000936 -3.11e-05  2.12e-06  3.09e-04 -2.52e-04
     [3,]  6.19e-01  49.7267 -1.411632  1.56e-02  3.99e-04  3.60e-01 -6.54e-02
     [4,] -3.16e-03   0.0169 -0.007202 -8.42e-04 -3.01e-06  1.87e-03  1.45e-04
     [5,] -1.54e-03   0.3758  0.003750 -1.45e-04  2.76e-07 -1.68e-04 -1.11e-03
     [6,] -2.35e-04   0.0347 -0.003288 -3.47e-05 -1.61e-07  4.07e-04 -1.43e-04
     [7,]  6.72e-04  -0.0196 -0.002547 -1.98e-05 -1.13e-06  5.17e-05  2.05e-06
     [8,]  8.46e-03   0.3721 -0.178803  8.36e-03  9.33e-05 -4.35e-03 -9.59e-03
     [9,]  8.52e-04   0.1536  0.011277 -9.49e-06  2.47e-06  2.33e-03 -9.06e-05
    [10,]  8.44e-03   1.9391 -0.171847  6.65e-03 -2.34e-04 -3.04e-02  1.04e-02
    [11,]  1.90e-04  -0.0146  0.001527 -8.06e-05  1.02e-05  1.29e-03 -8.18e-04
    [12,]  1.33e-02   2.2860 -0.133559 -2.07e-03  4.92e-06 -7.02e-03  1.94e-03
    [13,] -2.19e-03   0.7728 -0.000397 -1.89e-03  7.86e-06  3.42e-03 -1.80e-03
    [14,] -9.37e-04   0.1457 -0.003478 -1.37e-05  4.57e-06  8.30e-04 -5.62e-04
    [15,]  9.96e-01   0.0698  0.001689 -3.50e-05 -1.52e-06  1.72e-03  3.14e-05
    [16,] -2.87e-05   0.8504  0.000484  6.48e-06  5.01e-07  7.13e-06 -2.89e-05
    [17,]  3.98e-04   0.0596  0.988829 -2.33e-04  1.34e-07 -2.83e-04 -8.32e-05
    [18,]  1.53e-02   2.2945 -0.115006  9.91e-01 -2.46e-05  6.30e-03  1.25e-03
    [19,]  1.17e-01 105.7590 -6.412034  3.00e-02  9.88e-01  6.67e-01  3.60e-01
    [20,] -4.65e-04   0.3664 -0.031997 -6.90e-04 -8.70e-07  9.92e-01 -9.43e-04
    [21,] -9.68e-04   0.2126 -0.014366  3.09e-03  3.06e-05  1.07e-02  9.89e-01
    [22,]  8.84e-03  -0.8554  0.101147 -5.06e-04  1.88e-04  2.07e-03 -1.55e-02
    [23,] -2.06e-04   0.2091 -0.001732 -4.88e-04  5.70e-06  3.99e-03 -1.90e-03
    [24,]  1.35e-04   0.0299  0.000415 -1.22e-06 -3.65e-07 -6.84e-05  6.52e-06
    [25,] -7.40e-05   0.0214 -0.002088  1.82e-05  4.14e-07  1.66e-04 -9.76e-05
              [,22]     [,23]    [,24]     [,25]
     [1,]  8.83e-05 -2.99e-04  -0.0961 -0.015756
     [2,] -1.09e-04 -4.63e-04   0.0109 -0.007574
     [3,] -3.20e-02  1.46e-01 -22.7910 -4.877092
     [4,] -8.31e-05  1.64e-04   0.1082  0.000915
     [5,]  7.13e-06 -1.26e-03   0.3105 -0.040002
     [6,]  1.14e-04 -1.63e-04   0.0859 -0.005181
     [7,]  1.11e-05  1.52e-04   0.0281 -0.000523
     [8,]  1.11e-03  4.57e-03   2.4759 -0.014989
     [9,]  1.23e-04 -1.02e-03   0.1646 -0.023898
    [10,]  1.16e-02  2.63e-02  -1.0859 -0.361442
    [11,]  2.03e-04 -6.99e-04   0.3815 -0.018346
    [12,] -1.38e-03  1.43e-03  -2.1249  0.030803
    [13,] -1.40e-03 -3.07e-03   0.1562 -0.068130
    [14,] -2.56e-05 -4.79e-04  -0.0772 -0.015203
    [15,]  1.50e-04 -5.89e-04  -0.1029 -0.017760
    [16,]  9.27e-07 -5.68e-05   0.0386  0.000263
    [17,]  2.46e-05  1.18e-04   0.0406 -0.002310
    [18,] -7.94e-04 -1.15e-03  -0.9934 -0.113053
    [19,]  4.57e-01  2.37e-01   1.9889 -2.987457
    [20,] -3.90e-05 -7.36e-04   0.5511 -0.004015
    [21,]  2.80e-03 -6.82e-03   1.0194 -0.054645
    [22,]  9.91e-01 -1.44e-02   0.2346 -0.372535
    [23,]  3.01e-04  9.99e-01   0.1775 -0.024349
    [24,] -1.84e-05  3.22e-05   0.5929  0.002004
    [25,] -2.87e-05 -1.62e-04   0.1105  0.990086
    standard error 
              [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
     [1,] 0.003705 0.003034 4.76e-06 3.20e-04 0.002598  0.01995 0.005590 6.21e-05
     [2,] 0.002187 0.001790 2.81e-06 1.89e-04 0.001533  0.01177 0.003299 3.67e-05
     [3,] 1.288038 1.054560 1.66e-03 1.11e-01 0.902996  6.93368 1.943420 2.16e-02
     [4,] 0.012348 0.010110 1.59e-05 1.07e-03 0.008657  0.06647 0.018631 2.07e-04
     [5,] 0.014487 0.011861 1.86e-05 1.25e-03 0.010156  0.07799 0.021859 2.43e-04
     [6,] 0.001883 0.001541 2.42e-06 1.62e-04 0.001320  0.01013 0.002841 3.16e-05
     [7,] 0.001022 0.000837 1.31e-06 8.82e-05 0.000717  0.00550 0.001542 1.71e-05
     [8,] 0.072547 0.059397 9.32e-05 6.26e-03 0.050860  0.39053 0.109461 1.22e-03
     [9,] 0.006254 0.005120 8.04e-06 5.39e-04 0.004384  0.03366 0.009435 1.05e-04
    [10,] 0.268321 0.219683 3.45e-04 2.31e-02 0.188110  1.44441 0.404848 4.50e-03
    [11,] 0.006311 0.005167 8.11e-06 5.44e-04 0.004425  0.03397 0.009523 1.06e-04
    [12,] 0.037990 0.031104 4.88e-05 3.28e-03 0.026633  0.20450 0.057320 6.37e-04
    [13,] 0.019680 0.016113 2.53e-05 1.70e-03 0.013797  0.10594 0.029694 3.30e-04
    [14,] 0.004338 0.003552 5.58e-06 3.74e-04 0.003041  0.02335 0.006545 7.27e-05
    [15,] 0.006691 0.005478 8.60e-06 5.77e-04 0.004691  0.03602 0.010095 1.12e-04
    [16,] 0.000360 0.000295 4.62e-07 3.10e-05 0.000252  0.00194 0.000543 6.03e-06
    [17,] 0.001902 0.001558 2.45e-06 1.64e-04 0.001334  0.01024 0.002870 3.19e-05
    [18,] 0.035280 0.028885 4.53e-05 3.04e-03 0.024733  0.18991 0.053231 5.91e-04
    [19,] 4.022202 3.293112 5.17e-03 3.47e-01 2.819817 21.65205 6.068787 6.74e-02
    [20,] 0.020749 0.016988 2.67e-05 1.79e-03 0.014546  0.11169 0.031306 3.48e-04
    [21,] 0.029680 0.024300 3.81e-05 2.56e-03 0.020807  0.15977 0.044781 4.97e-04
    [22,] 0.070722 0.057902 9.09e-05 6.10e-03 0.049580  0.38070 0.106706 1.19e-03
    [23,] 0.010790 0.008834 1.39e-05 9.31e-04 0.007565  0.05809 0.016281 1.81e-04
    [24,] 0.000263 0.000215 3.38e-07 2.27e-05 0.000184  0.00142 0.000397 4.41e-06
    [25,] 0.001404 0.001149 1.80e-06 1.21e-04 0.000984  0.00756 0.002118 2.35e-05
              [,9]    [,10]    [,11]    [,12]    [,13]    [,14]    [,15]    [,16]
     [1,] 8.37e-04 1.89e-05 1.04e-03 1.70e-04 6.33e-04 0.001983 5.98e-04  0.06000
     [2,] 4.94e-04 1.11e-05 6.14e-04 1.00e-04 3.73e-04 0.001170 3.53e-04  0.03541
     [3,] 2.91e-01 6.56e-03 3.61e-01 5.90e-02 2.20e-01 0.689388 2.08e-01 20.85926
     [4,] 2.79e-03 6.29e-05 3.46e-03 5.66e-04 2.11e-03 0.006609 1.99e-03  0.19997
     [5,] 3.27e-03 7.38e-05 4.07e-03 6.64e-04 2.47e-03 0.007754 2.34e-03  0.23461
     [6,] 4.25e-04 9.59e-06 5.28e-04 8.62e-05 3.22e-04 0.001008 3.04e-04  0.03049
     [7,] 2.31e-04 5.20e-06 2.87e-04 4.68e-05 1.75e-04 0.000547 1.65e-04  0.01655
     [8,] 1.64e-02 3.69e-04 2.04e-02 3.32e-03 1.24e-02 0.038829 1.17e-02  1.17487
     [9,] 1.41e-03 3.18e-05 1.75e-03 2.86e-04 1.07e-03 0.003347 1.01e-03  0.10127
    [10,] 6.06e-02 1.37e-03 7.53e-02 1.23e-02 4.58e-02 0.143611 4.33e-02  4.34535
    [11,] 1.43e-03 3.21e-05 1.77e-03 2.89e-04 1.08e-03 0.003378 1.02e-03  0.10221
    [12,] 8.58e-03 1.93e-04 1.07e-02 1.74e-03 6.49e-03 0.020333 6.13e-03  0.61523
    [13,] 4.45e-03 1.00e-04 5.52e-03 9.01e-04 3.36e-03 0.010533 3.17e-03  0.31871
    [14,] 9.80e-04 2.21e-05 1.22e-03 1.99e-04 7.41e-04 0.002322 7.00e-04  0.07025
    [15,] 1.51e-03 3.41e-05 1.88e-03 3.06e-04 1.14e-03 0.003581 1.08e-03  0.10836
    [16,] 8.13e-05 1.83e-06 1.01e-04 1.65e-05 6.14e-05 0.000193 5.80e-05  0.00583
    [17,] 4.30e-04 9.69e-06 5.34e-04 8.71e-05 3.25e-04 0.001018 3.07e-04  0.03081
    [18,] 7.97e-03 1.80e-04 9.90e-03 1.62e-03 6.03e-03 0.018882 5.69e-03  0.57134
    [19,] 9.09e-01 2.05e-02 1.13e+00 1.84e-01 6.87e-01 2.152775 6.49e-01 65.13794
    [20,] 4.69e-03 1.06e-04 5.82e-03 9.50e-04 3.54e-03 0.011105 3.35e-03  0.33602
    [21,] 6.71e-03 1.51e-04 8.33e-03 1.36e-03 5.07e-03 0.015885 4.79e-03  0.48065
    [22,] 1.60e-02 3.60e-04 1.98e-02 3.24e-03 1.21e-02 0.037852 1.14e-02  1.14531
    [23,] 2.44e-03 5.49e-05 3.03e-03 4.94e-04 1.84e-03 0.005775 1.74e-03  0.17474
    [24,] 5.94e-05 1.34e-06 7.38e-05 1.21e-05 4.49e-05 0.000141 4.24e-05  0.00426
    [25,] 3.17e-04 7.15e-06 3.94e-04 6.43e-05 2.40e-04 0.000751 2.26e-04  0.02273
             [,17]    [,18]    [,19]    [,20]    [,21]    [,22]    [,23]    [,24]
     [1,] 0.003803 1.79e-04 1.89e-06 5.22e-04 1.92e-04 9.69e-05 2.62e-04 1.25e-01
     [2,] 0.002244 1.06e-04 1.11e-06 3.08e-04 1.13e-04 5.72e-05 1.54e-04 7.38e-02
     [3,] 1.322094 6.24e-02 6.56e-04 1.82e-01 6.67e-02 3.37e-02 9.09e-02 4.35e+01
     [4,] 0.012675 5.98e-04 6.29e-06 1.74e-03 6.40e-04 3.23e-04 8.72e-04 4.17e-01
     [5,] 0.014870 7.02e-04 7.38e-06 2.04e-03 7.51e-04 3.79e-04 1.02e-03 4.89e-01
     [6,] 0.001932 9.12e-05 9.59e-07 2.65e-04 9.75e-05 4.93e-05 1.33e-04 6.35e-02
     [7,] 0.001049 4.95e-05 5.20e-07 1.44e-04 5.30e-05 2.67e-05 7.22e-05 3.45e-02
     [8,] 0.074465 3.51e-03 3.69e-05 1.02e-02 3.76e-03 1.90e-03 5.12e-03 2.45e+00
     [9,] 0.006419 3.03e-04 3.18e-06 8.81e-04 3.24e-04 1.64e-04 4.41e-04 2.11e-01
    [10,] 0.275415 1.30e-02 1.37e-04 3.78e-02 1.39e-02 7.02e-03 1.89e-02 9.05e+00
    [11,] 0.006478 3.06e-04 3.21e-06 8.89e-04 3.27e-04 1.65e-04 4.46e-04 2.13e-01
    [12,] 0.038994 1.84e-03 1.93e-05 5.35e-03 1.97e-03 9.94e-04 2.68e-03 1.28e+00
    [13,] 0.020201 9.53e-04 1.00e-05 2.77e-03 1.02e-03 5.15e-04 1.39e-03 6.64e-01
    [14,] 0.004453 2.10e-04 2.21e-06 6.11e-04 2.25e-04 1.14e-04 3.06e-04 1.46e-01
    [15,] 0.006868 3.24e-04 3.41e-06 9.43e-04 3.47e-04 1.75e-04 4.72e-04 2.26e-01
    [16,] 0.000369 1.74e-05 1.83e-07 5.07e-05 1.86e-05 9.41e-06 2.54e-05 1.21e-02
    [17,] 0.001953 9.21e-05 9.69e-07 2.68e-04 9.86e-05 4.98e-05 1.34e-04 6.42e-02
    [18,] 0.036212 1.71e-03 1.80e-05 4.97e-03 1.83e-03 9.23e-04 2.49e-03 1.19e+00
    [19,] 4.128550 1.95e-01 2.05e-03 5.67e-01 2.08e-01 1.05e-01 2.84e-01 1.36e+02
    [20,] 0.021297 1.00e-03 1.06e-05 2.92e-03 1.08e-03 5.43e-04 1.46e-03 7.00e-01
    [21,] 0.030464 1.44e-03 1.51e-05 4.18e-03 1.54e-03 7.77e-04 2.10e-03 1.00e+00
    [22,] 0.072592 3.42e-03 3.60e-05 9.97e-03 3.66e-03 1.85e-03 4.99e-03 2.39e+00
    [23,] 0.011076 5.23e-04 5.49e-06 1.52e-03 5.59e-04 2.82e-04 7.62e-04 3.64e-01
    [24,] 0.000270 1.27e-05 1.34e-07 3.71e-05 1.36e-05 6.88e-06 1.86e-05 8.88e-03
    [25,] 0.001441 6.80e-05 7.15e-07 1.98e-04 7.27e-05 3.67e-05 9.91e-05 4.74e-02
             [,25]
     [1,] 0.004772
     [2,] 0.002816
     [3,] 1.658886
     [4,] 0.015903
     [5,] 0.018658
     [6,] 0.002425
     [7,] 0.001316
     [8,] 0.093435
     [9,] 0.008054
    [10,] 0.345575
    [11,] 0.008128
    [12,] 0.048928
    [13,] 0.025347
    [14,] 0.005587
    [15,] 0.008617
    [16,] 0.000463
    [17,] 0.002450
    [18,] 0.045437
    [19,] 5.180262
    [20,] 0.026723
    [21,] 0.038225
    [22,] 0.091084
    [23,] 0.013897
    [24,] 0.000339
    [25,] 0.001808
      
    Residuals cov-mtx: 
                       Australia_Cncy_Inv   Canada_Cncy    Chile_Cncy   China_Cncy
    Australia_Cncy_Inv       9.992088e-05  3.242465e-05  9.641597e-03 1.243278e-05
    Canada_Cncy              3.242465e-05  3.480051e-05  5.248247e-03 5.918543e-06
    Chile_Cncy               9.641597e-03  5.248247e-03  1.207529e+01 5.127820e-03
    China_Cncy               1.243278e-05  5.918543e-06  5.127820e-03 1.109812e-03
    Denmark_Cncy             1.503689e-04  6.952788e-05  1.717654e-02 4.080200e-05
    EU_Cncy_Inv              2.049307e-05  9.467488e-06  2.322929e-03 4.688425e-06
    HongKong_Cncy            2.455838e-06  1.248928e-06  6.863854e-04 3.023118e-06
    India_Cncy               3.146094e-04  1.664549e-04  1.166174e-01 2.675715e-04
    Israel_Cncy              3.596811e-05  2.194314e-05  1.040549e-02 1.533999e-05
    Japan_Cncy               5.234446e-04  3.721150e-05 -3.610268e-02 1.706529e-04
    Malaysia_Cncy            2.529135e-05  1.058145e-05  8.094808e-03 2.362789e-05
    Mexico_Cncy              3.390943e-04  2.390890e-04  1.131043e-01 1.051029e-04
    Norway_Cncy              2.439212e-04  1.306838e-04  4.368397e-02 8.651971e-05
    NZ_Cncy_Inv              9.179865e-05  3.347364e-05  9.587728e-03 1.134303e-05
    Romania_Cncy             6.853709e-05  3.720515e-05  1.244043e-02 1.902463e-05
    Saudi_Cncy              -1.876737e-07 -7.098133e-08 -1.076380e-05 9.950951e-07
    Singapore_Cncy           2.107408e-05  1.043971e-05  3.919123e-03 5.695989e-06
    SouthAfrica_Cncy         4.010488e-04  2.360815e-04  1.085382e-01 1.472848e-04
    SouthKorea_Cncy          1.241466e-02  5.540168e-03  3.236594e+00 8.745412e-03
    Sweden_Cncy              2.609811e-04  1.314213e-04  3.755875e-02 7.292963e-05
    Taiwan_Cncy              9.353292e-05  4.520048e-05  3.301493e-02 9.881157e-05
    Thailand_Cncy            2.014120e-04  8.084091e-05  6.025307e-02 1.564986e-04
    Turkey_Cncy              5.449056e-05  3.344299e-05  2.040097e-02 3.322758e-05
    UAE_Cncy                 5.951380e-08 -4.916695e-08  4.804346e-06 4.875559e-08
    UK_Cncy_Inv              1.445350e-05  7.285165e-06  1.974455e-03 5.488225e-06
                        Denmark_Cncy   EU_Cncy_Inv HongKong_Cncy    India_Cncy
    Australia_Cncy_Inv  1.503689e-04  2.049307e-05  2.455838e-06  3.146094e-04
    Canada_Cncy         6.952788e-05  9.467488e-06  1.248928e-06  1.664549e-04
    Chile_Cncy          1.717654e-02  2.322929e-03  6.863854e-04  1.166174e-01
    China_Cncy          4.080200e-05  4.688425e-06  3.023118e-06  2.675715e-04
    Denmark_Cncy        1.527584e-03  1.829868e-04  7.299979e-06  7.226727e-04
    EU_Cncy_Inv         1.829868e-04  2.579694e-05  9.055026e-07  9.390160e-05
    HongKong_Cncy       7.299979e-06  9.055026e-07  7.605011e-06  4.216722e-05
    India_Cncy          7.226727e-04  9.390160e-05  4.216722e-05  3.830734e-02
    Israel_Cncy         1.466858e-04  2.003048e-05  3.471371e-06  4.799809e-04
    Japan_Cncy          1.000095e-02  1.181603e-03  6.590397e-05 -2.081833e-03
    Malaysia_Cncy       5.179504e-05  7.023834e-06  3.788109e-06  6.983745e-04
    Mexico_Cncy         5.911818e-04  7.748806e-05  1.776646e-05  2.925638e-03
    Norway_Cncy         1.526285e-03  1.869695e-04  1.157561e-05  1.555210e-03
    NZ_Cncy_Inv         1.854854e-04  2.482561e-05  3.125365e-06  3.383407e-04
    Romania_Cncy        3.678801e-04  4.974315e-05  3.544063e-06  5.528348e-04
    Saudi_Cncy         -1.533674e-07 -1.761741e-08  8.246901e-08  4.518507e-06
    Singapore_Cncy      7.644601e-05  1.023559e-05  2.099126e-06  1.785023e-04
    SouthAfrica_Cncy    1.008160e-03  1.365253e-04  2.030393e-05  3.209283e-03
    SouthKorea_Cncy     1.332163e-02  1.835170e-03  1.785940e-03  3.183197e-01
    Sweden_Cncy         1.685635e-03  2.053930e-04  1.157286e-05  1.447822e-03
    Taiwan_Cncy         2.369616e-04  3.420518e-05  2.003821e-05  3.263511e-03
    Thailand_Cncy       6.458812e-04  8.976868e-05  2.348223e-05  4.107034e-03
    Turkey_Cncy         1.367313e-04  1.844072e-05  2.789292e-06  7.162400e-04
    UAE_Cncy           -1.539897e-07 -2.290980e-08 -1.431489e-08  1.932192e-07
    UK_Cncy_Inv         8.794498e-05  1.074384e-05  6.176038e-07  9.502195e-05
                         Israel_Cncy    Japan_Cncy Malaysia_Cncy   Mexico_Cncy
    Australia_Cncy_Inv  3.596811e-05  5.234446e-04  2.529135e-05  3.390943e-04
    Canada_Cncy         2.194314e-05  3.721150e-05  1.058145e-05  2.390890e-04
    Chile_Cncy          1.040549e-02 -3.610268e-02  8.094808e-03  1.131043e-01
    China_Cncy          1.533999e-05  1.706529e-04  2.362789e-05  1.051029e-04
    Denmark_Cncy        1.466858e-04  1.000095e-02  5.179504e-05  5.911818e-04
    EU_Cncy_Inv         2.003048e-05  1.181603e-03  7.023834e-06  7.748806e-05
    HongKong_Cncy       3.471371e-06  6.590397e-05  3.788109e-06  1.776646e-05
    India_Cncy          4.799809e-04 -2.081833e-03  6.983745e-04  2.925638e-03
    Israel_Cncy         2.846363e-04  4.842462e-04  2.937455e-05  3.382870e-04
    Japan_Cncy          4.842462e-04  5.240209e-01  6.397017e-04 -3.415681e-03
    Malaysia_Cncy       2.937455e-05  6.397017e-04  2.899169e-04  1.526418e-04
    Mexico_Cncy         3.382870e-04 -3.415681e-03  1.526418e-04  1.050446e-02
    Norway_Cncy         2.244149e-04  8.887017e-03  1.112962e-04  1.785056e-03
    NZ_Cncy_Inv         4.123990e-05  1.090347e-03  2.859950e-05  3.473059e-04
    Romania_Cncy        8.704432e-05  1.650484e-03  2.663979e-05  4.630669e-04
    Saudi_Cncy         -2.352380e-08 -1.560478e-06  3.429626e-07 -2.109527e-07
    Singapore_Cncy      1.910142e-05  1.123707e-03  3.692679e-05  1.416533e-04
    SouthAfrica_Cncy    3.677286e-04  1.217491e-03  1.929819e-04  5.094070e-03
    SouthKorea_Cncy     1.571754e-02  1.515119e-01  4.451083e-02  7.952530e-02
    Sweden_Cncy         2.439048e-04  9.609864e-03  9.117457e-05  1.419974e-03
    Taiwan_Cncy         1.438460e-04  2.754620e-03  4.095846e-04  3.912535e-04
    Thailand_Cncy       1.383773e-04  1.586477e-02  1.505169e-03  1.033641e-03
    Turkey_Cncy         6.867851e-05 -2.223968e-04  2.579840e-05  8.063663e-04
    UAE_Cncy           -1.782783e-07  2.313524e-07  1.819706e-07 -7.052396e-07
    UK_Cncy_Inv         1.300790e-05  6.139220e-04  6.162126e-06  8.653318e-05
                         Norway_Cncy   NZ_Cncy_Inv  Romania_Cncy    Saudi_Cncy
    Australia_Cncy_Inv  2.439212e-04  9.179865e-05  6.853709e-05 -1.876737e-07
    Canada_Cncy         1.306838e-04  3.347364e-05  3.720515e-05 -7.098133e-08
    Chile_Cncy          4.368397e-02  9.587728e-03  1.244043e-02 -1.076380e-05
    China_Cncy          8.651971e-05  1.134303e-05  1.902463e-05  9.950951e-07
    Denmark_Cncy        1.526285e-03  1.854854e-04  3.678801e-04 -1.533674e-07
    EU_Cncy_Inv         1.869695e-04  2.482561e-05  4.974315e-05 -1.761741e-08
    HongKong_Cncy       1.157561e-05  3.125365e-06  3.544063e-06  8.246901e-08
    India_Cncy          1.555210e-03  3.383407e-04  5.528348e-04  4.518507e-06
    Israel_Cncy         2.244149e-04  4.123990e-05  8.704432e-05 -2.352380e-08
    Japan_Cncy          8.887017e-03  1.090347e-03  1.650484e-03 -1.560478e-06
    Malaysia_Cncy       1.112962e-04  2.859950e-05  2.663979e-05  3.429626e-07
    Mexico_Cncy         1.785056e-03  3.473059e-04  4.630669e-04 -2.109527e-07
    Norway_Cncy         2.819058e-03  2.786186e-04  4.415644e-04 -3.915972e-07
    NZ_Cncy_Inv         2.786186e-04  1.369689e-04  7.597122e-05 -2.851884e-07
    Romania_Cncy        4.415644e-04  7.597122e-05  3.258438e-04 -2.269136e-07
    Saudi_Cncy         -3.915972e-07 -2.851884e-07 -2.269136e-07  9.419685e-07
    Singapore_Cncy      1.072600e-04  2.512233e-05  3.439422e-05  4.481081e-08
    SouthAfrica_Cncy    2.050531e-03  4.198064e-04  5.448759e-04 -1.022527e-06
    SouthKorea_Cncy     3.734436e-02  1.259041e-02  1.394723e-02  4.721777e-04
    Sweden_Cncy         2.206149e-03  2.975144e-04  5.102148e-04  2.146577e-07
    Taiwan_Cncy         4.628530e-04  1.034396e-04  1.357583e-04  6.189502e-07
    Thailand_Cncy       9.569104e-04  2.652920e-04  2.574033e-04  4.374053e-07
    Turkey_Cncy         2.930762e-04  5.840746e-05  9.405278e-05 -5.944594e-08
    UAE_Cncy           -2.275023e-07  1.143232e-07  2.841041e-08  3.750599e-08
    UK_Cncy_Inv         1.119150e-04  1.708195e-05  2.677188e-05  4.051942e-08
                       Singapore_Cncy SouthAfrica_Cncy SouthKorea_Cncy  Sweden_Cncy
    Australia_Cncy_Inv   2.107408e-05     4.010488e-04    1.241466e-02 2.609811e-04
    Canada_Cncy          1.043971e-05     2.360815e-04    5.540168e-03 1.314213e-04
    Chile_Cncy           3.919123e-03     1.085382e-01    3.236594e+00 3.755875e-02
    China_Cncy           5.695989e-06     1.472848e-04    8.745412e-03 7.292963e-05
    Denmark_Cncy         7.644601e-05     1.008160e-03    1.332163e-02 1.685635e-03
    EU_Cncy_Inv          1.023559e-05     1.365253e-04    1.835170e-03 2.053930e-04
    HongKong_Cncy        2.099126e-06     2.030393e-05    1.785940e-03 1.157286e-05
    India_Cncy           1.785023e-04     3.209283e-03    3.183197e-01 1.447822e-03
    Israel_Cncy          1.910142e-05     3.677286e-04    1.571754e-02 2.439048e-04
    Japan_Cncy           1.123707e-03     1.217491e-03    1.515119e-01 9.609864e-03
    Malaysia_Cncy        3.692679e-05     1.929819e-04    4.451083e-02 9.117457e-05
    Mexico_Cncy          1.416533e-04     5.094070e-03    7.952530e-02 1.419974e-03
    Norway_Cncy          1.072600e-04     2.050531e-03    3.734436e-02 2.206149e-03
    NZ_Cncy_Inv          2.512233e-05     4.198064e-04    1.259041e-02 2.975144e-04
    Romania_Cncy         3.439422e-05     5.448759e-04    1.394723e-02 5.102148e-04
    Saudi_Cncy           4.481081e-08    -1.022527e-06    4.721777e-04 2.146577e-07
    Singapore_Cncy       2.634301e-05     1.676355e-04    9.053588e-03 1.129350e-04
    SouthAfrica_Cncy     1.676355e-04     9.059170e-03    6.941238e-02 1.898736e-03
    SouthKorea_Cncy      9.053588e-03     6.941238e-02    1.177519e+02 3.954836e-02
    Sweden_Cncy          1.129350e-04     1.898736e-03    3.954836e-02 3.133460e-03
    Taiwan_Cncy          9.732777e-05     7.157943e-04    2.428579e-01 4.306788e-04
    Thailand_Cncy        3.690775e-04     1.315242e-03    2.762032e-01 1.026809e-03
    Turkey_Cncy          2.292548e-05     9.169876e-04    1.315843e-02 2.698171e-04
    UAE_Cncy            -9.350417e-09    -5.915032e-07   -2.945930e-05 6.204574e-07
    UK_Cncy_Inv          6.451531e-06     1.042604e-04    2.968876e-03 1.149630e-04
                         Taiwan_Cncy Thailand_Cncy   Turkey_Cncy      UAE_Cncy
    Australia_Cncy_Inv  9.353292e-05  2.014120e-04  5.449056e-05  5.951380e-08
    Canada_Cncy         4.520048e-05  8.084091e-05  3.344299e-05 -4.916695e-08
    Chile_Cncy          3.301493e-02  6.025307e-02  2.040097e-02  4.804346e-06
    China_Cncy          9.881157e-05  1.564986e-04  3.322758e-05  4.875559e-08
    Denmark_Cncy        2.369616e-04  6.458812e-04  1.367313e-04 -1.539897e-07
    EU_Cncy_Inv         3.420518e-05  8.976868e-05  1.844072e-05 -2.290980e-08
    HongKong_Cncy       2.003821e-05  2.348223e-05  2.789292e-06 -1.431489e-08
    India_Cncy          3.263511e-03  4.107034e-03  7.162400e-04  1.932192e-07
    Israel_Cncy         1.438460e-04  1.383773e-04  6.867851e-05 -1.782783e-07
    Japan_Cncy          2.754620e-03  1.586477e-02 -2.223968e-04  2.313524e-07
    Malaysia_Cncy       4.095846e-04  1.505169e-03  2.579840e-05  1.819706e-07
    Mexico_Cncy         3.912535e-04  1.033641e-03  8.063663e-04 -7.052396e-07
    Norway_Cncy         4.628530e-04  9.569104e-04  2.930762e-04 -2.275023e-07
    NZ_Cncy_Inv         1.034396e-04  2.652920e-04  5.840746e-05  1.143232e-07
    Romania_Cncy        1.357583e-04  2.574033e-04  9.405278e-05  2.841041e-08
    Saudi_Cncy          6.189502e-07  4.374053e-07 -5.944594e-08  3.750599e-08
    Singapore_Cncy      9.732777e-05  3.690775e-04  2.292548e-05 -9.350417e-09
    SouthAfrica_Cncy    7.157943e-04  1.315242e-03  9.169876e-04 -5.915032e-07
    SouthKorea_Cncy     2.428579e-01  2.762032e-01  1.315843e-02 -2.945930e-05
    Sweden_Cncy         4.306788e-04  1.026809e-03  2.698171e-04  6.204574e-07
    Taiwan_Cncy         6.411449e-03  3.971044e-03  1.304128e-04 -4.080975e-07
    Thailand_Cncy       3.971044e-03  3.640367e-02  2.242679e-04 -3.069163e-07
    Turkey_Cncy         1.304128e-04  2.242679e-04  8.474396e-04  8.326303e-08
    UAE_Cncy           -4.080975e-07 -3.069163e-07  8.326303e-08  5.038809e-07
    UK_Cncy_Inv         2.876491e-05  4.892327e-05  1.372704e-05  5.531578e-08
                        UK_Cncy_Inv
    Australia_Cncy_Inv 1.445350e-05
    Canada_Cncy        7.285165e-06
    Chile_Cncy         1.974455e-03
    China_Cncy         5.488225e-06
    Denmark_Cncy       8.794498e-05
    EU_Cncy_Inv        1.074384e-05
    HongKong_Cncy      6.176038e-07
    India_Cncy         9.502195e-05
    Israel_Cncy        1.300790e-05
    Japan_Cncy         6.139220e-04
    Malaysia_Cncy      6.162126e-06
    Mexico_Cncy        8.653318e-05
    Norway_Cncy        1.119150e-04
    NZ_Cncy_Inv        1.708195e-05
    Romania_Cncy       2.677188e-05
    Saudi_Cncy         4.051942e-08
    Singapore_Cncy     6.451531e-06
    SouthAfrica_Cncy   1.042604e-04
    SouthKorea_Cncy    2.968876e-03
    Sweden_Cncy        1.149630e-04
    Taiwan_Cncy        2.876491e-05
    Thailand_Cncy      4.892327e-05
    Turkey_Cncy        1.372704e-05
    UAE_Cncy           5.531578e-08
    UK_Cncy_Inv        1.433985e-05
      
    det(SSE) =  3.795651e-79 
    AIC =  -180.4199 
    BIC =  -179.8912 
    HQ  =  -180.2392 

``` r
res <- cncy_VAR$residuals

# MarchTest(res)
  # See comment in chunk above.
```

The null hypothesis is again rejected, confirming that Multivariate
Volatility models are appropriate. In the following lines of code, a DCC
model will be fit.

``` r
DCCPre <- dccPre(cncy_xts, include.mean = F, p = 0)
```

    Component:  1 
    Estimates:  0.001403 0.97054 0.02872 
    se.coef  :  0.075802 1.057601 1.058181 
    t-value  :  0.018507 0.917681 0.027141 
    Component:  2 
    Estimates:  0.001101 0.967509 0.0318 
    se.coef  :  0.083384 1.504199 1.505386 
    t-value  :  0.013206 0.643206 0.021124 
    Component:  3 
    Estimates:  117.385 0.999737 0 
    se.coef  :  7268.661 0.525059 0.525213 
    t-value  :  0.016149 1.904049 0 
    Component:  4 
    Estimates:  0.025172 0.989193 0.010337 
    se.coef  :  2.15304 0.965894 0.965984 
    t-value  :  0.011691 1.024122 0.010701 
    Component:  5 
    Estimates:  0.040584 0.982887 0.016131 
    se.coef  :  2.489747 1.237288 1.237898 
    t-value  :  0.016301 0.794388 0.013031 
    Component:  6 
    Estimates:  0.00067 0.997684 0.001411 
    se.coef  :  0.044788 1.269355 1.269867 
    t-value  :  0.01495 0.785977 0.001111 
    Component:  7 
    Estimates:  8.8e-05 0.30896 0.691038 
    se.coef  :  NaN 0.602242 0.602251 
    t-value  :  NaN 0.513017 1.147425 
    Component:  8 
    Estimates:  0.594478 0.998358 0.00154 
    se.coef  :  25.21862 0.187444 0.186806 
    t-value  :  0.023573 5.326171 0.008246 
    Component:  9 
    Estimates:  0.001544 0.999941 0 
    se.coef  :  0.355122 0.528202 0.527936 
    t-value  :  0.004349 1.893104 0 
    Component:  10 
    Estimates:  11.43638 0.995453 0.003661 
    se.coef  :  591.5289 1.112319 1.112868 
    t-value  :  0.019334 0.894935 0.00329 
    Component:  11 
    Estimates:  0.002596 0.999856 0 
    se.coef  :  0.444299 1.262728 1.262837 
    t-value  :  0.005844 0.791822 0 
    Component:  12 
    Estimates:  0.007164 1 0.000331 
    se.coef  :  0.427802 0.072796 0.070586 
    t-value  :  0.016745 13.737 0.004694 
    Component:  13 
    Estimates:  0.050012 0.985081 0.013998 
    se.coef  :  2.347013 1.039189 1.040152 
    t-value  :  0.021309 0.947932 0.013458 
    Component:  14 
    Estimates:  0.002282 0.986017 0.013057 
    se.coef  :  0.111772 1.070725 1.071485 
    t-value  :  0.020415 0.920888 0.012186 
    Component:  15 
    Estimates:  2e-06 0.993446 0 
    se.coef  :  8.4e-05 0.016747 0.000618 
    t-value  :  0.02622 59.32128 1.6e-05 
    Component:  16 
    Estimates:  0 0.309454 0.690544 
    se.coef  :  0.000904 0.607252 0.607275 
    t-value  :  0.000467 0.509597 1.13712 
    Component:  17 
    Estimates:  0.000658 0.993373 0.006343 
    se.coef  :  0.144094 1.924604 1.924265 
    t-value  :  0.004566 0.516144 0.003296 
    Component:  18 
    Estimates:  0.003478 0.999321 0.001057 
    se.coef  :  0.392077 0.126789 0.125361 
    t-value  :  0.008871 7.881762 0.00843 
    Component:  19 
    Estimates:  15.13452 1 0.00019 
    se.coef  :  38278.19 0.710057 0.709226 
    t-value  :  0.000395 1.408338 0.000268 
    Component:  20 
    Estimates:  0.067608 0.978712 0.020202 
    se.coef  :  2.744662 0.992157 0.993301 
    t-value  :  0.024632 0.986449 0.020338 
    Component:  21 
    Estimates:  7e-06 1 2.2e-05 
    se.coef  :  75.42313 2.205088 2.203854 
    t-value  :  0 0.453497 1e-05 
    Component:  22 
    Estimates:  0.483473 0.982635 0.016978 
    se.coef  :  43.1008 1.226509 1.226925 
    t-value  :  0.011217 0.801164 0.013838 
    Component:  23 
    Estimates:  4e-06 0.985668 0 
    se.coef  :  NaN NaN 0.000719 
    t-value  :  NaN NaN 1.4e-05 
    Component:  24 
    Estimates:  0 0.309462 0.690536 
    se.coef  :  NaN 0.076191 0.071426 
    t-value  :  NaN 4.061662 9.66792 
    Component:  25 
    Estimates:  0.000288 0.999374 0 
    se.coef  :  0.025078 1.281852 1.282007 
    t-value  :  0.011473 0.779633 0 

``` r
Vol <- DCCPre$marVol
colnames(Vol) <- colnames(cncy_xts)

Vol <- data.frame( cbind( date = index(cncy_xts), Vol)) %>% # Adding a date column
  mutate(date = as.Date(date)) %>%  tbl_df()

TidyVol <- Vol %>% gather(Cncy, Sigma, -date)

ggplot(TidyVol) + geom_line(aes(x = date, y = Sigma, colour = Cncy)) + theme_bw()
```

<img src="README_files/figure-markdown_github/unnamed-chunk-84-1.png" style="display: block; margin: auto auto auto 0;" />

From the above figure, one can see that the ZAR has indeed been one of
the most volatile currencies over the past few decades.
