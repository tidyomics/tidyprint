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

**tidyprint** provides an improved display for SummarizedExperiment objects with a tidy tibble-style format that makes genomic data more accessible and easier to explore.

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
library(airway)
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

### **tidyprint**

Now we load tidyprint for a tidy data display

``` r
library(tidyprint)
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
#> R version 4.4.3 (2025-02-28)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.1 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Etc/UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] tidyprint_0.99.2            airway_1.26.0              
#>  [3] SummarizedExperiment_1.36.0 Biobase_2.66.0             
#>  [5] GenomicRanges_1.58.0        GenomeInfoDb_1.42.3        
#>  [7] IRanges_2.40.1              S4Vectors_0.44.0           
#>  [9] BiocGenerics_0.52.0         MatrixGenerics_1.18.1      
#> [11] matrixStats_1.5.0           tidyr_1.3.1                
#> [13] dplyr_1.1.4                
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.6              generics_0.1.4          SparseArray_1.6.2      
#>  [4] stringi_1.8.7           lattice_0.22-6          digest_0.6.37          
#>  [7] magrittr_2.0.4          evaluate_1.0.5          grid_4.4.3             
#> [10] fastmap_1.2.0           rprojroot_2.1.0         jsonlite_2.0.0         
#> [13] Matrix_1.7-2            httr_1.4.7              fansi_1.0.6            
#> [16] purrr_1.1.0             UCSC.utils_1.2.0        abind_1.4-8            
#> [19] cli_3.6.5               rlang_1.1.6             crayon_1.5.3           
#> [22] XVector_0.46.0          withr_3.0.2             DelayedArray_0.32.0    
#> [25] yaml_2.3.10             S4Arrays_1.6.0          tools_4.4.3            
#> [28] GenomeInfoDbData_1.2.13 vctrs_0.6.5             R6_2.6.1               
#> [31] lifecycle_1.0.4         stringr_1.5.2           zlibbioc_1.52.0        
#> [34] pkgconfig_2.0.3         pillar_1.11.1           glue_1.8.0             
#> [37] xfun_0.53               tibble_3.3.0            tidyselect_1.2.1       
#> [40] rstudioapi_0.17.1       knitr_1.50              htmltools_0.5.8.1      
#> [43] rmarkdown_2.30          compiler_4.4.3
```
