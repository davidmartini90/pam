
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pam

The goal of the pam package is to provide a set of functions to easily
process monitoring-pam data, in order to obtain parameters such as:

  - Photochemical quenching (PQ)

  - Maximum PQ

  - Non-photochemical quenching (NPQ)

  - Sustained component of NPQ (NPQs)

  - Reversible component of NPQ (NPQr)

  - Yield of NPQ

  - Yield of NPQs

  - Yield of NPQr

  - Yield of photochemistry

  - Yield of fluorescence and basal thermal energy dissipation

  - Yield of fluorescence

  - Relative light saturation of photosynthesis

  - maximum PSII value

  - Reference maximal fluorescence

  - Reference minimal fluorescence

  - Maximum PSII value at night

  - Night minimal fluorescence

  - Night maximal fluorescence

### How to install:

``` r
# update.packages()

if(!(require("devtools"))){install.packages("devtools")}
library(devtools)

install_github("davidmartini90/pam")
```

### How to use:

``` r
library(pam)
library(dplyr)

example_data

# adding acquisition and reference parameters and plotting Fmr and For
example_data %>%
  add_acquisition(
    start_of_night = "20:40",
    end_of_night = "05:00",
    date_format = "mdy"
  ) %>%
  add_reference() %>%
  plot_reference_parameters()

# adding acquisition, reference parameters and 
example_data %>%
  add_acquisition(
    start_of_night = "20:40",
    end_of_night = "05:00",
    date_format = "mdy"
  ) %>%
  add_reference() %>%
  add_pam_parameters() %>% 
  glimpse()
```

### How to read the functions documentation:

``` r
library(pam)

?add_acquisition
?add_reference
?plot_reference_parameters
?add_pam_parameters
```

contact: <dmartini@bgc-jena.mpg.de>
