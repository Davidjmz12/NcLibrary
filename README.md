<!-- README.md is generated from README.Rmd. Please edit that file -->

# NcLibrary - Meteorological Data Management R-Library <img src="man/figures/logo.png" align="right" style="padding: 10px"  width="300"/>

## Overview

NcLibrary is an R-library developed to simplify the retrieval and
manipulation of meteorological data from various databases, including
the ERA5 general database and observed values from ECAD. This library
aims to enhance the compatibility, accessibility, and usability of
meteorological data within statistical research and analysis.

## Features

- User-friendly interface for effortless data retrieval from ERA5 and
  ECAD databases.
- Functions for extracting, processing, and merging meteorological data.
- Support for converting between different coordinate representations.
- Comprehensive documentation for each function.

## Installation

To install the NcLibrary R-package, you can use the `devtools` package:

``` r
devtools::install_github("Davidjmz12/NcLibrary")
```

## Usage

Once the library is installed, you can load it using the library()
function:

``` r
library(NcLibrary)
```

## Setting Up the Environment

This section is meant for those interested in creating requests to ERA5
databases. If this is not applicable to your needs, feel free to skip
this section.

Firstly we must register on the [Climate Data
Store](https://cds.climate.copernicus.eu/#!/home) to obtain an API key.

Upon successful registration, proceed by following the steps outlined in
the *Install the CDS API key* section of the [official
guide](https://cds.climate.copernicus.eu/api-how-to). In case the URL or
key of the guide file fails to appear, consider refreshing the page
while ensuring that you are logged in.

Finally, we have to set the *R* environment. As the execution of Python
code is essential, the R library *reticulate* is employed. An
independent *conda* environment with the libraries needed will be
created after using the following function. Please note that if this
configuration has been completed previously, it may be skipped.

``` r
set_reticulate()
```

If there is not any conda interpreter in your computer you could use,
for example, the function `install_miniconda()`.

If the *reticulate* environment is no longer needed, it can be deleted
using the following function:

``` r
close_reticulate()
```

## Example

Here’s a simple example of how to use the library to retrieve ERA5 data:

``` r
library(NcLibrary)

# Create a NetCDF file using the user interface
create_nc_file_app()


# Create a configuration for the request:
configuration <- list(dim=list(
                                latitude=list(type="lat", coord_360=T, name="latitude", format=0),
                                longitude=list(type="lon", coord_360=T, name="longitude", format=0)
                              ),
                       time=list(extended=F,time_div=T,name="time"),
                       sep="_",
                       nc_file="path/to/your/file.nc")

# Read the NetCDF file and convert it to a data frame
data <- read_nc_file(configuration)
```

## Documentation

For more detailed information on each function and its parameters,
Please take a look at the [library
documentation](https://davidjmz12.github.io/NcLibrary/).

## Methodology

The development of NcLibrary followed a well-defined methodology,
including phases such as problem identification, research and data
collection, development of core functions, user interface design,
testing and validation, library integration and documentation,
performance evaluation, and package deployment. For a detailed breakdown
of the methodology, please refer to the [internship
report](https://davidjmz12.github.io/NcLibrary/Article.pdf).

## Issues and Contributions

If you happen to have any issues or suggestions for improvements, please
feel free to [open an
issue](https://github.com/Davidjmz12/NcLibrary/issues). Contributions
are also welcome through [pull
requests](https://github.com/Davidjmz12/NcLibrary/pulls).

## License

This project is licensed under the [GNU3 license](LICENSE).
