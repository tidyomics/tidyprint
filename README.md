# tidyprint



**tidyprint** is an R package that provides multiple printing styles for `SummarizedExperiment` objects. You can choose among:



1. **SummarizedExperiment** (default): Standard R/SummarizedExperiment printing style.

2. **tidySummarizedExperiment**: Tidyverse-friendly tibble abstraction.

3. **plyxp**: Top/bottom row truncation for large datasets.



Depending on your workflow and desired console output, `tidyprint` makes it easy to switch between these printing styles.



---



## 1. Installation



You need the `` package to install from GitHub. If you don’t have it, install via:



```r

install.packages("devtools")

```



Then install **tidyprint** from GitHub:



```r

devtools::install_github("tidyomics/tidyprint")

```



---



## 2. Demo



Below is an example demonstrating how to use **tidyprint** with a sample `SummarizedExperiment` object.



### 2.1 Load Required Packages



```r

# Load SummarizedExperiment (install if needed)

if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {

  install.packages("BiocManager")

  BiocManager::install("SummarizedExperiment")

}



library(SummarizedExperiment)

library(tidyverse)

library(magrittr)



# Example SummarizedExperiment from the package's built-in example

example("SummarizedExperiment", package = "SummarizedExperiment", ask = FALSE)



# Now load tidyprint

library(tidyprint)



```



---



### 2.2 **SummarizedExperiment** (Default)



By default, `print()` on a `SummarizedExperiment` displays the standard SummarizedExperiment info:



```r

# The default style:

se0 %>% print()



# or equivalently:

se0 %>% print(design = 1)

se0 %>% print(design = "SummarizedExperiment")

```



**Example Output**:



```

class: SummarizedExperiment

dim: 200 6

metadata(0):

assays(1): counts

rownames: NULL

rowData names(0):

colnames(6): A B ... E F

```



---



### 2.3 **tidySummarizedExperiment**



Use the “tidySummarizedExperiment” design to view your data in a **tidy-friendly tibble** format:



```r

# Tidy SummarizedExperiment print:

se0 %>% print(design = 2)

# or

se0 %>% print(design = "tidySummarizedExperiment")

```



**Example Output**:



```

# A SummarizedExperiment-tibble abstraction: 1,200 × 4

# Features=200 | Samples=6 | Assays=counts

   .feature .sample counts Treatment

   <chr>    <chr>    <dbl> <chr>    

 1 1        A         9.21 ChIP     

 2 2        A         7.56 ChIP     

 3 3        A         8.27 ChIP     

 4 4        A         9.73 ChIP     

 5 5        A         8.61 ChIP     

 6 6        A         9.64 ChIP     

 7 7        A         8.59 ChIP     

 8 8        A         8.62 ChIP     

 9 9        A         6.11 ChIP     

10 10       A         9.38 ChIP     

# ℹ 40 more rows

# ℹ Use `print(n = ...)` to see more rows

colData names(1): Treatment

```



---



### 2.4 **plyxp**



For a more compact view (top and bottom rows), similar to a plyxp/tidyverse style:



```r

se0 %>% print(design = 3)

# or

se0 %>% print(design = "plyxp")

```



**Example Output**:



```

# A tibble: 10 × 7

   .features .samples ``     counts ``     ``     Treatment

       <int> <chr>    <sep!>  <dbl> <sep!> <sep!> <chr>    

 1         1 A        |        9.21 |      |      ChIP     

 2         2 A        |        7.56 |      |      ChIP     

 3         3 A        |        8.27 |      |      ChIP     

 4         4 A        |        9.73 |      |      ChIP     

 5         5 A        |        8.61 |      |      ChIP     

 6       196 F        |        9.65 |      |      Input    

 7       197 F        |        8.29 |      |      Input    

 8       198 F        |        9.78 |      |      Input    

 9       199 F        |        9.02 |      |      Input    

10       200 F        |        9.01 |      |      Input    

```



You can also limit the number of displayed rows by setting `n_print` (or a similar argument in your code):



```r

se0 %>% print(design = "plyxp", n_print = 5)

```



**Example Output**:



```

# A tibble: 5 × 7

  .features .samples ``     counts ``     ``     Treatment

      <int> <chr>    <sep!>  <dbl> <sep!> <sep!> <chr>    

1         1 A        |        9.21 |      |      ChIP     

2         2 A        |        7.56 |      |      ChIP     

3         3 A        |        8.27 |      |      ChIP     

4       199 F        |        9.02 |      |      Input    

5       200 F        |        9.01 |      |      Input    

```



---

### 2.5 **alternative_1**



For a more compact view (top and bottom rows), similar to a plyxp/tidyverse style with tidySummarizedExperiment header and customised row_id:



```r

se0 %>% print(design = 4)

# or

se0 %>% print(design = "alternative_1")

```



**Example Output**:



```

# A SummarizedExperiment-tibble abstraction: 1,200 × 7
# Features=200 | Samples=6 | Assays=counts
     .features .samples `|` counts `|` `|` Treatment
         <int> <chr>    <|>  <dbl> <|> <|> <chr>    
   1         1 A        |     7.62 |   |   ChIP     
   2         2 A        |     9.58 |   |   ChIP     
   3         3 A        |     8.54 |   |   ChIP     
   4         4 A        |     9.32 |   |   ChIP     
   5         5 A        |     8.37 |   |   ChIP     
1196       196 F        |     9.13 |   |   Input    
1197       197 F        |     9.67 |   |   Input    
1198       198 F        |     7.85 |   |   Input    
1199       199 F        |     9.09 |   |   Input    
1200       200 F        |     9.67 |   |   Input     

```



You can also limit the number of displayed rows by setting `n_print` (or a similar argument in your code):



```r

se0 %>% print(design = "alternative_1", n_print = 5)

```



**Example Output**:



```

# A SummarizedExperiment-tibble abstraction: 1,200 × 7
# Features=200 | Samples=6 | Assays=counts
     .features .samples `|` counts `|` `|` Treatment
         <int> <chr>    <|>  <dbl> <|> <|> <chr>    
   1         1 A        |     7.62 |   |   ChIP     
   2         2 A        |     9.58 |   |   ChIP     
   3         3 A        |     8.54 |   |   ChIP     
1199       199 F        |     9.09 |   |   Input    
1200       200 F        |     9.67 |   |   Input  


```



---
