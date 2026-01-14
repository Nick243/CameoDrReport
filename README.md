
# CameoDrReport

CameoDrReport contains a single function to generate the internal CAMEO Diet Recall Report from the provided enrollment and dietary recall tracker files.

## Installation

You can install the development version of CameoDrReport from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Nick243/CameoDrReport")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CameoDrReport)

make_dr_report(cdart_path = "/users/olljt2/desktop/cdart_report.xlsx", 
               redcap_path = "/users/olljt2/desktop/CAMEOBionutritionDie_DATA_YMD.csv", 
               out_xlsx = "/users/olljt2/desktop/Dietary Recall Tracking Report YMD.xlsx")

```

