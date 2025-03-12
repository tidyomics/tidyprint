tidyprint
================

<!-- badges: start -->

[![Lifecycle:experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

**tidyprint** is an R package that provides a centralised tidy display
strategy for biological data (e.g. SummarizedExperiment), and
centralised messaging styles for the `tidyomics` packages. To facilitate
the discussion about data display, we compare here four data diplay:

1.  **SummarizedExperiment**: Standard R/SummarizedExperiment printing
    style.

2.  **tidyprint_1**: Newly designed tibble abstraction, combines styles
    from **tidySummarizedExperiment** and **plyxp**

3.  **tidySummarizedExperiment**: Tidyverse-friendly tibble abstraction.

4.  **plyxp**: Top/bottom row truncation for large datasets.

Depending on your workflow and desired console output, `tidyprint` makes
it easy to switch between these printing styles.

------------------------------------------------------------------------

## 1. Installation

You need the `remotes` package to install from GitHub. If you don’t have
it, install via:

``` r
install.packages("remotes")
```

Then install **tidyprint** from GitHub:

``` r
remotes::install_github("tidyomics/tidyprint")
```

------------------------------------------------------------------------

## 2. Demo

Below is an example demonstrating how to use **tidyprint** with a sample
`SummarizedExperiment` object.

### 2.1 Load Required Packages

``` r

library(dplyr)
library(tidyr)

# Now load tidyprint

library(tidyprint)

# Example SummarizedExperiment data from the airway package

data(se_airway)
se_airway
#> class: SummarizedExperiment 
#> dim: 38694 8 
#> metadata(0):
#> assays(1): counts
#> rownames(38694): ENSG00000000003 ENSG00000000005 ... ENSG00000283120
#>   ENSG00000283123
#> rowData names(0):
#> colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
#> colData names(3): dex celltype geo_id
```

------------------------------------------------------------------------

### 2.2 **SummarizedExperiment** (Default)

The standard SummarizedExperiment display:

``` r

# The default style:
se_airway %>% print(design = "SummarizedExperiment")
#> class: SummarizedExperiment 
#> dim: 38694 8 
#> metadata(0):
#> assays(1): counts
#> rownames(38694): ENSG00000000003 ENSG00000000005 ... ENSG00000283120
#>   ENSG00000283123
#> rowData names(0):
#> colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
#> colData names(3): dex celltype geo_id
```

### 2.3 **tidyprint_1**

For a more compact view (top and bottom rows), similar to a
plyxp/tidyverse style with tidySummarizedExperiment header:

``` r

se_airway %>% print(design = "tidyprint_1")
#> # A SummarizedExperiment-tibble abstraction:
#> # Features=38694 | Samples=8 | Assays=counts
#>        .features       .samples   `|` counts `|` `|` dex     celltype geo_id    
#>        <chr>           <chr>      <|> <chr>  <|> <|> <chr>   <chr>    <chr>     
#> 1      ENSG00000000003 SRR1039508  |  723     |   |  control N61311   GSM1275862
#> 2      ENSG00000000005 SRR1039508  |  0       |   |  control N61311   GSM1275862
#> 3      ENSG00000000419 SRR1039508  |  467     |   |  control N61311   GSM1275862
#> 4      ENSG00000000457 SRR1039508  |  347     |   |  control N61311   GSM1275862
#> 5      ENSG00000000460 SRR1039508  |  96      |   |  control N61311   GSM1275862
#>        --------------- ---------- --  ---    --  --  ------- -------  ----------
#> 309548 ENSG00000283115 SRR1039521  |  0       |   |  treated N061011  GSM1275875
#> 309549 ENSG00000283116 SRR1039521  |  0       |   |  treated N061011  GSM1275875
#> 309550 ENSG00000283119 SRR1039521  |  0       |   |  treated N061011  GSM1275875
#> 309551 ENSG00000283120 SRR1039521  |  0       |   |  treated N061011  GSM1275875
#> 309552 ENSG00000283123 SRR1039521  |  0       |   |  treated N061011  GSM1275875
```

### 2.4 **tidySummarizedExperiment**

Use the “tidySummarizedExperiment” design to view your data in a
**tidy-friendly tibble** format:

``` r

# Tidy SummarizedExperiment print:

se_airway %>% print(design = "tidySummarizedExperiment")
#> # A SummarizedExperiment-tibble abstraction: 309,552 × 6
#> # Features=38694 | Samples=8 | Assays=counts
#>    .feature        .sample    counts dex     celltype geo_id    
#>    <chr>           <chr>       <dbl> <chr>   <chr>    <chr>     
#>  1 ENSG00000000003 SRR1039508    723 control N61311   GSM1275862
#>  2 ENSG00000000005 SRR1039508      0 control N61311   GSM1275862
#>  3 ENSG00000000419 SRR1039508    467 control N61311   GSM1275862
#>  4 ENSG00000000457 SRR1039508    347 control N61311   GSM1275862
#>  5 ENSG00000000460 SRR1039508     96 control N61311   GSM1275862
#>  6 ENSG00000000938 SRR1039508      0 control N61311   GSM1275862
#>  7 ENSG00000000971 SRR1039508   3413 control N61311   GSM1275862
#>  8 ENSG00000001036 SRR1039508   2328 control N61311   GSM1275862
#>  9 ENSG00000001084 SRR1039508    670 control N61311   GSM1275862
#> 10 ENSG00000001167 SRR1039508    426 control N61311   GSM1275862
#> # ℹ 40 more rows
```

### 2.5 **plyxp**

For a more compact view (top and bottom rows), similar to a
plyxp/tidyverse style:

``` r

se_airway %>% print(design = "plyxp")
#> # A tibble: 10 × 9
#>    .features       .samples   `|` counts `|` `|` dex     celltype geo_id    
#>    <chr>           <chr>      <|>  <dbl> <|> <|> <chr>   <chr>    <chr>     
#>  1 ENSG00000000003 SRR1039508  |     723  |   |  control N61311   GSM1275862
#>  2 ENSG00000000005 SRR1039508  |       0  |   |  control N61311   GSM1275862
#>  3 ENSG00000000419 SRR1039508  |     467  |   |  control N61311   GSM1275862
#>  4 ENSG00000000457 SRR1039508  |     347  |   |  control N61311   GSM1275862
#>  5 ENSG00000000460 SRR1039508  |      96  |   |  control N61311   GSM1275862
#>  6 ENSG00000283115 SRR1039521  |       0  |   |  treated N061011  GSM1275875
#>  7 ENSG00000283116 SRR1039521  |       0  |   |  treated N061011  GSM1275875
#>  8 ENSG00000283119 SRR1039521  |       0  |   |  treated N061011  GSM1275875
#>  9 ENSG00000283120 SRR1039521  |       0  |   |  treated N061011  GSM1275875
#> 10 ENSG00000283123 SRR1039521  |       0  |   |  treated N061011  GSM1275875
```

# 3. Messaging function

We integrated a messaging function providing standardized, visually
appealing messages for packages within the tidyomics ecosystem. It
automatically detects the calling package to provide contextualized
messaging, such as “tidyprint says” or “tidybulk says”, enhancing
consistency and readability across projects.

To use the `tidy_message` function:

``` r

tidyprint::tidy_message('message to print')
#> ℹ Console says: message to print
```

You can specify the type of message as

- info (default)

- success

- warning

- danger

``` r

test_tidy_message()
#> ℹ tidyprint says: This is an informational message send within tidyprint package.
#> ✔ tidyprint says: Operation completed successfully!
#> ! tidyprint says: Potential issue detected.
#> ✖ tidyprint says: Operation failed.
```

The above code demonstrates calling `tidy_message` within a package
function, showing the name of package.

## Session info

``` r

sessionInfo()
#> R version 4.4.1 (2024-06-14)
#> Platform: x86_64-apple-darwin20
#> Running under: macOS Sonoma 14.6.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: Australia/Adelaide
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] tidyprint_0.0.1 tidyr_1.3.1     dplyr_1.1.4    
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.4                  sass_0.4.9                 
#>  [3] generics_0.1.3              SparseArray_1.5.44         
#>  [5] stringi_1.8.4               lattice_0.22-6             
#>  [7] digest_0.6.37               magrittr_2.0.3             
#>  [9] evaluate_1.0.3              grid_4.4.1                 
#> [11] fastmap_1.2.0               rprojroot_2.0.4            
#> [13] jsonlite_1.8.9              Matrix_1.7-0               
#> [15] GenomeInfoDb_1.41.2         httr_1.4.7                 
#> [17] fansi_1.0.6                 purrr_1.0.4                
#> [19] UCSC.utils_1.1.0            jquerylib_0.1.4            
#> [21] abind_1.4-8                 cli_3.6.4                  
#> [23] rlang_1.1.5                 crayon_1.5.3               
#> [25] XVector_0.45.0              Biobase_2.65.1             
#> [27] withr_3.0.2                 cachem_1.1.0               
#> [29] DelayedArray_0.31.14        yaml_2.3.10                
#> [31] S4Arrays_1.5.11             tools_4.4.1                
#> [33] GenomeInfoDbData_1.2.13     SummarizedExperiment_1.35.4
#> [35] BiocGenerics_0.51.3         vctrs_0.6.5                
#> [37] R6_2.6.1                    matrixStats_1.5.0          
#> [39] stats4_4.4.1                lifecycle_1.0.4            
#> [41] stringr_1.5.1               zlibbioc_1.51.1            
#> [43] S4Vectors_0.43.2            IRanges_2.39.2             
#> [45] pkgconfig_2.0.3             pillar_1.10.1              
#> [47] bslib_0.9.0                 glue_1.8.0                 
#> [49] xfun_0.50                   tibble_3.2.1               
#> [51] GenomicRanges_1.57.2        tidyselect_1.2.1           
#> [53] rstudioapi_0.16.0           MatrixGenerics_1.17.0      
#> [55] knitr_1.49                  htmltools_0.5.8.1          
#> [57] rmarkdown_2.29              compiler_4.4.1
```
