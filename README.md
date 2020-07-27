
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
