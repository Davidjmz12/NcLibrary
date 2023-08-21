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
- Ability to subset data based on specific cities.
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

## Example

Here’s a simple example of how to use the library to retrieve ERA5 data:

``` r
library(NcLibrary)

# Create a NetCDF file using the user interface
create_nc_file_app()

# Read the NetCDF file and convert it to a data frame
data <- read_nc_file("path/to/your/file.nc")

# Subset data for a specific city
subset_data <- get_subset_city(data, city_name = "Your City Name")
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
report](docs/Article.pdf).

## Issues and Contributions

If you happen to have any issues or suggestions for improvements, please
feel free to [open an
issue](https://github.com/Davidjmz12/NcLibrary/issues). Contributions
are also welcome through [pull
requests](https://github.com/Davidjmz12/NcLibrary/pulls).

## License

This project is licensed under the [GNU3 license](LICENSE).
