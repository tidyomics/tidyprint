tidyprint
================

<!-- badges: start -->

[![Lifecycle:experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

**tidyprint** is an R package that provides a centralised tidy display
strategy for biological data (e.g. SummarizedExperiment) and centralised
messaging styles for the `tidyomics` packages. To facilitate the
discussion about data display, we compare here four data displays:

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
it, install it via:

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

------------------------------------------------------------------------

**For comparative purposes we display the alternative visualisations we
are trying to harmonise (now depreciated)**

### 2.4 **tidySummarizedExperiment**

Use the “tidySummarizedExperiment” design to view your data in a
**tidy-friendly tibble** format:

``` r

# Tidy SummarizedExperiment print:

airway %>% print(design = "tidySummarizedExperiment")
#> # A SummarizedExperiment-tibble abstraction: 509,416 × 23
#> # Features=63677 | Samples=8 | Assays=counts
#>    .feature        .sample   counts SampleName cell  dex   albut Run   avgLength
#>    <chr>           <chr>      <int> <fct>      <fct> <fct> <fct> <fct>     <int>
#>  1 ENSG00000000003 SRR10395…    679 GSM1275862 N613… untrt untrt SRR1…       126
#>  2 ENSG00000000005 SRR10395…      0 GSM1275862 N613… untrt untrt SRR1…       126
#>  3 ENSG00000000419 SRR10395…    467 GSM1275862 N613… untrt untrt SRR1…       126
#>  4 ENSG00000000457 SRR10395…    260 GSM1275862 N613… untrt untrt SRR1…       126
#>  5 ENSG00000000460 SRR10395…     60 GSM1275862 N613… untrt untrt SRR1…       126
#>  6 ENSG00000000938 SRR10395…      0 GSM1275862 N613… untrt untrt SRR1…       126
#>  7 ENSG00000000971 SRR10395…   3251 GSM1275862 N613… untrt untrt SRR1…       126
#>  8 ENSG00000001036 SRR10395…   1433 GSM1275862 N613… untrt untrt SRR1…       126
#>  9 ENSG00000001084 SRR10395…    519 GSM1275862 N613… untrt untrt SRR1…       126
#> 10 ENSG00000001167 SRR10395…    394 GSM1275862 N613… untrt untrt SRR1…       126
#> # ℹ 40 more rows
#> # ℹ 14 more variables: Experiment <fct>, Sample <fct>, BioSample <fct>,
#> #   gene_id <chr>, gene_name <chr>, entrezid <int>, gene_biotype <chr>,
#> #   gene_seq_start <int>, gene_seq_end <int>, seq_name <chr>, seq_strand <int>,
#> #   seq_coord_system <int>, symbol <chr>, GRangesList <list>
```

### 2.5 **plyxp**

For a more compact view (top and bottom rows), similar to a
plyxp/tidyverse style:

``` r

airway %>% print(design = "plyxp")
#> # A tibble: 10 × 25
#>    .features .samples `|` counts `|` gene_id     gene_name entrezid gene_biotype
#>    <chr>     <chr>    <|>  <int> <|> <chr>       <chr>        <int> <chr>       
#>  1 ENSG0000… SRR1039… |      679 |   ENSG000000… TSPAN6          NA protein_cod…
#>  2 ENSG0000… SRR1039… |        0 |   ENSG000000… TNMD            NA protein_cod…
#>  3 ENSG0000… SRR1039… |      467 |   ENSG000000… DPM1            NA protein_cod…
#>  4 ENSG0000… SRR1039… |      260 |   ENSG000000… SCYL3           NA protein_cod…
#>  5 ENSG0000… SRR1039… |       60 |   ENSG000000… C1orf112        NA protein_cod…
#>  6 ENSG0000… SRR1039… |        0 |   ENSG000002… RP11-180…       NA antisense   
#>  7 ENSG0000… SRR1039… |        0 |   ENSG000002… TSEN34          NA protein_cod…
#>  8 ENSG0000… SRR1039… |        0 |   ENSG000002… RP11-138…       NA lincRNA     
#>  9 ENSG0000… SRR1039… |        0 |   ENSG000002… AP000230…       NA lincRNA     
#> 10 ENSG0000… SRR1039… |        0 |   ENSG000002… RP11-80H…       NA lincRNA     
#> # ℹ 16 more variables: gene_seq_start <int>, gene_seq_end <int>,
#> #   seq_name <chr>, seq_strand <int>, seq_coord_system <int>, symbol <chr>,
#> #   `|` <|>, SampleName <fct>, cell <fct>, dex <fct>, albut <fct>, Run <fct>,
#> #   avgLength <int>, Experiment <fct>, Sample <fct>, BioSample <fct>
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

-   info (default)

-   success

-   warning

-   danger

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
#> R version 4.4.0 (2024-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Red Hat Enterprise Linux 9.4 (Plow)
#> 
#> Matrix products: default
#> BLAS/LAPACK: FlexiBLAS OPENBLAS;  LAPACK version 3.10.1
#> 
#> locale:
#>  [1] LC_CTYPE=en_AU.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_AU.UTF-8        LC_COLLATE=en_AU.UTF-8    
#>  [5] LC_MONETARY=en_AU.UTF-8    LC_MESSAGES=en_AU.UTF-8   
#>  [7] LC_PAPER=en_AU.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_AU.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Australia/Melbourne
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] tidyprint_0.0.1             airway_1.26.0              
#>  [3] SummarizedExperiment_1.36.0 Biobase_2.66.0             
#>  [5] GenomicRanges_1.58.0        GenomeInfoDb_1.42.3        
#>  [7] IRanges_2.40.1              S4Vectors_0.44.0           
#>  [9] BiocGenerics_0.52.0         MatrixGenerics_1.18.1      
#> [11] matrixStats_1.5.0           tidyr_1.3.1                
#> [13] dplyr_1.1.4                
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.6              generics_0.1.4          SparseArray_1.6.2      
#>  [4] stringi_1.8.7           lattice_0.22-7          digest_0.6.37          
#>  [7] magrittr_2.0.3          evaluate_1.0.4          grid_4.4.0             
#> [10] fastmap_1.2.0           rprojroot_2.1.0         jsonlite_2.0.0         
#> [13] Matrix_1.7-3            httr_1.4.7              fansi_1.0.6            
#> [16] purrr_1.1.0             UCSC.utils_1.2.0        abind_1.4-8            
#> [19] cli_3.6.5               rlang_1.1.6             crayon_1.5.3           
#> [22] XVector_0.46.0          withr_3.0.2             DelayedArray_0.32.0    
#> [25] yaml_2.3.10             S4Arrays_1.6.0          tools_4.4.0            
#> [28] GenomeInfoDbData_1.2.13 vctrs_0.6.5             R6_2.6.1               
#> [31] lifecycle_1.0.4         stringr_1.5.1           zlibbioc_1.52.0        
#> [34] pkgconfig_2.0.3         pillar_1.11.0           glue_1.8.0             
#> [37] xfun_0.52               tibble_3.3.0            tidyselect_1.2.1       
#> [40] rstudioapi_0.17.1       knitr_1.50              htmltools_0.5.8.1      
#> [43] rmarkdown_2.29          compiler_4.4.0
```
