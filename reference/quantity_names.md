# Names of the quantities the checks know

The shared vocabulary of
[`check_reported()`](https://dm-k1.github.io/reprokit/reference/check_reported.md):
the names under which reported numbers are passed in, and the names
handlers use in their relations.

## Usage

``` r
quantity_names()
```

## Value

A character vector.

## Examples

``` r
quantity_names()
#>  [1] "n"                 "m"                 "sd"               
#>  [4] "r"                 "t"                 "f"                
#>  [7] "chisq"             "df"                "df1"              
#> [10] "df2"               "p"                 "eta2"             
#> [13] "ci_lower"          "ci_upper"          "d"                
#> [16] "n_total"           "v"                 "p0"               
#> [19] "b"                 "se"                "beta"             
#> [22] "r2"                "p_model"           "r_all"            
#> [25] "xz"                "indirect"          "indirect_ci_lower"
#> [28] "indirect_ci_upper" "adj_r2"            "omega2"           
#> [31] "partial_eta2"      "partial_omega2"   
```
