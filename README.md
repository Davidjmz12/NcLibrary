<!-- README.md is generated from README.Rmd. Please edit that file -->

# NcLibrary - Meteorological Data Management R-Library <img src="man/figures/logo.png" align="right" style="padding: 10px"/>

## Overview

NcLibrary is an R-library developed to simplify the retrieval and
manipulation of meteorological data from various databases, including
the ERA5 general database and observed values from ECAD. This library
aims to enhance compatibility, accessibility, and usability of
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

# Read the NetCDF file and convert to a data frame
data <- read_nc_file("path/to/your/file.nc")

# Subset data for a specific city
subset_data <- get_subset_city(data, city_name = "Your City Name")
```

## Documentation

For more detailed information on each function and its parameters,
please refer to the library documentation.

## Issues and Contributions

If you encounter any issues or have suggestions for improvements, please
feel free to open an issue. Contributions are also welcome through pull
requests.

## License

This project is licensed under the GNU3 license.
