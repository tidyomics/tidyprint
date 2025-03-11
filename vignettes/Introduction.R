params <-
list(demo_metadata = TRUE)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)
root_dir <- knitr::opts_knit$get("root.dir")
if (!is.null(root_dir)){
    # This hack fixes the relative image paths. 
    # See https://github.com/rstudio/rmarkdown/issues/2473
    knitr::opts_knit$set(
        output.dir = root_dir
    )
}
proj_root <- rprojroot::find_package_root_file() |> normalizePath() 
# Utility function for figures to force them to have the correct path
find_figure <- function(names){
    rprojroot::find_package_root_file() |>
        file.path("man", "figures", names) 
}

## ----eval=FALSE---------------------------------------------------------------
# install.packages("devtools")

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("tidyomics/tidyprint")

