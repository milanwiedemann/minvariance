
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minvariance

<!-- badges: start -->

<!-- badges: end -->

The goal of `minvariance` is to help me understand longitudinal
measurement invariance. I’m trying to see which parts of this method can
be automated. Currently variable names need to be in a very specific
format; `measure_timepoint_itemnumber`. The number of time points and
items also need to be specified in separate arguments. **If you have any
questions, ideas, or comments please get in touch.**

## Installation

You can install the released version of `minvariance` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("milanwiedemann/minvariance")
```

``` r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.2          ✓ purrr   0.3.4     
#> ✓ tibble  3.0.3.9000     ✓ dplyr   1.0.0     
#> ✓ tidyr   1.1.0          ✓ stringr 1.4.0     
#> ✓ readr   1.3.1          ✓ forcats 0.5.0
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(minvariance)
# Specify list of variables 
timepoints <- list(t1 = c("i1_t1", "i2_t1", "i3_t1"), t2 = c("i1_t2", "i1_t2", "i1_t2"))

# Generate lavaan syntax
long_minvariance_syntax(var_list = timepoints, model = "configural") %>% 
  cat()
#> Configural Invariance Model (Pattern Invariance)
#> #### CONFIGURAL INVARIANCE MODEL ####
#> # Specify Latent Factors ----
#> eta1 =~ NA * i1_t1 + lambda1 * i1_t1 + i2_t1 + i3_t1
#> eta2 =~ NA * i1_t2 + lambda1 * i1_t2 + i1_t2 + i1_t2
#> # Specify Latent Variable Means ----
#> eta1 ~ 0 * 1 
#> eta2 ~ 1 
#> # Specify Latent Variable Variances ----
#> eta1 ~~ 1 * eta1
#> eta2 ~~ eta2
#> # Specify Latent Variable Covariances ----
#> eta1 ~~ eta2
#> # Specify Observed Variable Intercepts ----
#> i1_t1 ~ tau1 * 1 
#> i2_t1 ~ 1 
#> i3_t1 ~ 1 
#> i1_t2 ~ tau1 * 1 
#> i1_t2 ~ 1 
#> i1_t2 ~ 1 
#> # Specify Unique Variances ----
#> i1_t1 ~~ i1_t1 
#> i2_t1 ~~ i2_t1 
#> i3_t1 ~~ i3_t1 
#> i1_t2 ~~ i1_t2 
#> i1_t2 ~~ i1_t2 
#> i1_t2 ~~ i1_t2 
#> # Specify Unique Covariances ----
#> i1_t1 ~~ i1_t2
#> i2_t1 ~~ i1_t2
#> i3_t1 ~~ i1_t2
```

Here’s another example with different variable names

``` r
library(tidyverse)

timepoints_abc <- list(t1 = c("a1", "b1", "c1"), t2 = c("a2", "b2", "c2"))

# Generate lavaan syntax
long_minvariance_syntax(var_list = timepoints_abc, model = "strong") %>% 
  cat()
#> Strong Invariance Model (Scalar Invariance, Intercept Invariance)
#> #### STRONG INVARIANCE MODEL ####
#> # Specify Latent Factors ----
#> eta1 =~ NA * a1 + lambda1 * a1 + lambda2 * b1 + lambda3 * c1
#> eta2 =~ NA * a2 + lambda1 * a2 + lambda2 * b2 + lambda3 * c2
#> # Specify Latent Variable Means ----
#> eta1 ~ 0 * 1 
#> eta2 ~ 1 
#> # Specify Latent Variable Variances ----
#> eta1 ~~ 1 * eta1
#> eta2 ~~ eta2
#> # Specify Latent Variable Covariances ----
#> eta1 ~~ eta2
#> # Specify Observed Variable Intercepts ----
#> a1 ~ tau1 * 1 
#> b1 ~ tau2 * 1 
#> c1 ~ tau3 * 1 
#> a2 ~ tau1 * 1 
#> b2 ~ tau2 * 1 
#> c2 ~ tau3 * 1 
#> # Specify Unique Variances ----
#> a1 ~~ a1 
#> b1 ~~ b1 
#> c1 ~~ c1 
#> a2 ~~ a2 
#> b2 ~~ b2 
#> c2 ~~ c2 
#> # Specify Unique Covariances ----
#> a1 ~~ a2
#> b1 ~~ b2
#> c1 ~~ c2
```

## Resources and related work

R functions to test for longitudinal factorial/measurement invariance

  - [Longitudinal Factor Analysis - Measurement Invariance by Nilam
    Ram](https://quantdev.ssri.psu.edu/tutorials/intro-basics-longitudinal-measurement-invariance)
  - [Measurement Matters Resource List by Eiko Fried and Jessica
    Flake](https://docs.google.com/document/d/11jyoXtO0m2lUywpC04KjLvI5QcBUY4YtwEvw6cg2cMs)
  - [Fried et al (2016). Measuring depression over time . . . Or not?
    Lack of unidimensionality and longitudinal measurement invariance in
    four common rating scales of depression. Psychological
    Assessment.](https://doi.org/10.1037/pas0000275) and [Supplemental
    Materials](http://supp.apa.org/psycarticles/supplemental/pas0000275/supplementary_materials.zip)
    with Mplus files
  - [Grimm (2017) - lavaan Code for Chapter 14 - Modeling Change with
    Latent Variables Measured by Continuous Indicators by Julie
    Wood](https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-14-modeling-change-latent-variables-measured-continuous-indicators)
  - [MIE - Measurement invariance explorer (R package) by Maksim
    Rudnev](https://github.com/MaksimRudnev/MIE.package)
  - [“A Tutorial in Longitudinal Measurement Invariance Using Lavaan” by
    Robin Curtis, Sean Mackinnon, Roisin
    O’Connor](https://psyarxiv.com/tkzrb/)
  - [measEq.syntax
    {semTools}](https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/measEq.syntax)
  - [On dimensionality, measurement invariance and suitabilityof sum
    scores for the PHQ-9 and the GAD-7](https://osf.io/arufy/) with
    Mplus Code using mplusautomation
  - [lavaan
    group](https://groups.google.com/forum/#!topic/lavaan/nfdatPgLLhc)
