tidyprint
================

<!-- badges: start -->

[![Lifecycle:experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Introduction

**tidyprint** is an R package that provides a centralised tidy display
strategy for biological data (e.g. SummarizedExperiment) and centralised
messaging styles for the `tidyomics` packages. This package addresses a
critical need in the Bioconductor ecosystem for improved data
visualization and user experience when working with genomic data.

**tidyprint** fills this important gap by:

1.  **Enhancing Data Discoverability**: Large genomic datasets often
    contain millions of features and thousands of samples, making it
    difficult to quickly understand data structure and content.
    tidyprint provides intuitive, compact visualizations that help
    researchers immediately grasp their data’s dimensions and key
    characteristics.

2.  **Improving Workflow Integration**: The package seamlessly
    integrates with existing Bioconductor infrastructure while providing
    output formats that are familiar to users of modern R data science
    tools, particularly the tidyverse ecosystem.

3.  **Supporting Reproducible Research**: By standardizing how
    SummarizedExperiment objects are displayed across different analysis
    contexts, tidyprint promotes consistency in scientific communication
    and documentation.

4.  **Addressing Scalability Challenges**: As genomic datasets continue
    to grow in size and complexity, traditional display methods become
    inadequate. tidyprint’s adaptive display strategies ensure that
    users can effectively explore data regardless of scale.

5.  **Fostering Community Standards**: The package establishes
    conventions for data display that can be adopted across the
    Bioconductor ecosystem, promoting consistency and reducing the
    learning curve for new users.

### Package Overview

**tidyprint** provides an improved display for SummarizedExperiment
objects with a tidy tibble-style format that makes genomic data more
accessible and easier to explore.

------------------------------------------------------------------------

## Installation

You need the `remotes` package to install from GitHub. If you don’t have
it, install it via:

``` r
install.packages("remotes")
```

Then install **tidyprint** from GitHub:

``` r
remotes::install_github("tidyomics/tidyprint")
```

When available in Bioconductor, install with:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("tidyprint")
```

------------------------------------------------------------------------

## Demo

Below is an example demonstrating how to use **tidyprint** with a sample
`SummarizedExperiment` object.

### Load Required Packages

``` r
library(dplyr)
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.5.1
library(airway)
#> Warning: package 'SummarizedExperiment' was built under R version 4.5.1
#> Warning: package 'MatrixGenerics' was built under R version 4.5.1
#> Warning: package 'GenomicRanges' was built under R version 4.5.2
#> Warning: package 'BiocGenerics' was built under R version 4.5.1
#> Warning: package 'S4Vectors' was built under R version 4.5.1
#> Warning: package 'IRanges' was built under R version 4.5.1
#> Warning: package 'Seqinfo' was built under R version 4.5.1
#> Warning: package 'Biobase' was built under R version 4.5.1
data(airway)
```

### **SummarizedExperiment**

The standard SummarizedExperiment display:

``` r
airway 
#> class: RangedSummarizedExperiment 
#> dim: 63677 8 
#> metadata(1): ''
#> assays(1): counts
#> rownames(63677): ENSG00000000003 ENSG00000000005 ... ENSG00000273492
#>   ENSG00000273493
#> rowData names(10): gene_id gene_name ... seq_coord_system symbol
#> colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
#> colData names(9): SampleName cell ... Sample BioSample
```

------------------------------------------------------------------------

## Opting In and Out of Tidy Print

By default, **tidyprint** does not change the standard
SummarizedExperiment print format. You can opt in to use the tidy print
format, and opt out at any time. The setting can be configured for the
current R session only, or saved to persist across sessions.

``` r
library(tidyprint)
```

``` r
airway
#> ! tidyprint says: R option 'tidyprint.use_tidy_print' (FALSE) overrides cache value (TRUE). Use tidy_print_off(remember = TRUE) to update cache.
#> class: RangedSummarizedExperiment 
#> dim: 63677 8 
#> metadata(1): ''
#> assays(1): counts
#> rownames(63677): ENSG00000000003 ENSG00000000005 ... ENSG00000273492
#>   ENSG00000273493
#> rowData names(10): gene_id gene_name ... seq_coord_system symbol
#> colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
#> colData names(9): SampleName cell ... Sample BioSample
```

### Opt In to Tidy Print

To enable the tidy print format, use `tidy_print_on()`. By default, this
only affects the current R session:

``` r
# Enable tidy print for current session only
tidy_print_on()
#> ℹ tidyprint says: Tidy print enabled for this session only. Use tidy_print_on(remember = TRUE) to save this setting for future sessions.
airway  
#> # A SummarizedExperiment-tibble abstraction: Features=63677 | Samples=8 | 
#> #   Assays=counts
#> #                                 |----------------- COVARIATES ---------------|
#>        .feature  .sample | counts | SampleName cell  dex   albut Run   avgLength
#>        <chr>     <chr>   | <chr>  | <fct>      <fct> <fct> <fct> <fct> <chr>    
#> 1      ENSG0000… SRR103… | 679    | GSM1275862 N613… untrt untrt SRR1… 126      
#> 2      ENSG0000… SRR103… | 0      | GSM1275862 N613… untrt untrt SRR1… 126      
#> 3      ENSG0000… SRR103… | 467    | GSM1275862 N613… untrt untrt SRR1… 126      
#> 4      ENSG0000… SRR103… | 260    | GSM1275862 N613… untrt untrt SRR1… 126      
#> 5      ENSG0000… SRR103… | 60     | GSM1275862 N613… untrt untrt SRR1… 126      
#>        --------  ------- - ------ - ---------- ----  ---   ----- ---   ---------
#> 509412 ENSG0000… SRR103… | 0      | GSM1275875 N061… trt   untrt SRR1… 98       
#> 509413 ENSG0000… SRR103… | 0      | GSM1275875 N061… trt   untrt SRR1… 98       
#> 509414 ENSG0000… SRR103… | 0      | GSM1275875 N061… trt   untrt SRR1… 98       
#> 509415 ENSG0000… SRR103… | 0      | GSM1275875 N061… trt   untrt SRR1… 98       
#> 509416 ENSG0000… SRR103… | 0      | GSM1275875 N061… trt   untrt SRR1… 98       
#> # ℹ 14 more variables: Experiment <fct>, Sample <fct>, BioSample <fct>,
#> #   `|` <|>, gene_id <chr>, gene_name <chr>, entrezid <chr>,
#> #   gene_biotype <chr>, gene_seq_start <chr>, gene_seq_end <chr>,
#> #   seq_name <chr>, seq_strand <chr>, seq_coord_system <chr>, symbol <chr>
```

If you want the setting to persist across R sessions, use
`remember = TRUE`:

``` r
# Enable tidy print and remember the setting
tidy_print_on(remember = TRUE)
airway  # Will display with tidy format in all future sessions
```

When `remember = TRUE`, the setting is saved to a cache file in your R
configuration directory, so it will automatically be enabled when you
start new R sessions.

### Opt Out of Tidy Print

To return to the standard SummarizedExperiment print format, use
`tidy_print_off()`. By default, this only affects the current session:

``` r
# Disable tidy print for current session only
tidy_print_off()
airway  # Will display with standard format
```

To permanently disable tidy print and clear any saved preference:

``` r
# Disable tidy print and remember the setting
tidy_print_off(remember = TRUE)
airway  # Will display with standard format in all future sessions
```

## Messaging function

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
demo_tidy_message()
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
#> R version 4.5.0 (2025-04-11)
#> Platform: x86_64-apple-darwin20
#> Running under: macOS Sonoma 14.6.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: Australia/Adelaide
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] tidyprint_0.99.8            airway_1.30.0              
#>  [3] SummarizedExperiment_1.40.0 Biobase_2.70.0             
#>  [5] GenomicRanges_1.62.1        Seqinfo_1.0.0              
#>  [7] IRanges_2.44.0              S4Vectors_0.48.0           
#>  [9] BiocGenerics_0.56.0         generics_0.1.4             
#> [11] MatrixGenerics_1.22.0       matrixStats_1.5.0          
#> [13] tidyr_1.3.2                 dplyr_1.1.4                
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.6          SparseArray_1.10.8  stringi_1.8.7      
#>  [4] lattice_0.22-7      digest_0.6.39       magrittr_2.0.4     
#>  [7] evaluate_1.0.5      grid_4.5.0          fastmap_1.2.0      
#> [10] rprojroot_2.1.1     Matrix_1.7-4        purrr_1.2.0        
#> [13] fansi_1.0.7         abind_1.4-8         cli_3.6.5          
#> [16] rlang_1.1.6         XVector_0.50.0      withr_3.0.2        
#> [19] DelayedArray_0.36.0 yaml_2.3.12         otel_0.2.0         
#> [22] S4Arrays_1.10.1     tools_4.5.0         vctrs_0.6.5        
#> [25] R6_2.6.1            lifecycle_1.0.4     stringr_1.6.0      
#> [28] pkgconfig_2.0.3     pillar_1.11.1       glue_1.8.0         
#> [31] xfun_0.55           tibble_3.3.0        tidyselect_1.2.1   
#> [34] rstudioapi_0.17.1   knitr_1.51          htmltools_0.5.9    
#> [37] rmarkdown_2.30      compiler_4.5.0
```
