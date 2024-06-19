# README

Analysis code for [Measuring the Contribution of Voting Blocs to Election Outcomes](https://osf.io/preprints/socarxiv/c9fkg).

The GitHub repository does not include any data files. Please download the analysis data sets [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3LWMM9). You'll find the raw data files [here](https://www.dropbox.com/scl/fo/5rh657wqziobgl0baqnm9/h?dl=0&rlkey=046s9n53v5iwk4jbrvo7f1lqp).

Files in `code/preprocessing/` prepare the raw data for analysis, writing clean files to `data/`. Each `code/vb_*.Rmd` file runs a voting bloc analysis, with results in the corresponding HTML file. The file `code/race_sens.Rmd` examines sensitivity to the choice of survey, and outputs the plots and LaTeX-formatted tables that appear in the paper. The script `code/numbers-in-main-text.R` returns the remaining estimates in the paper.


The bulk of the paper's analyses relies on its accompanying `blocs` package. The package is currently available on CRAN, or you can download the development version from GitHub.
```r
install.packages("blocs")

# dev version
devtools::install_github("coletl/blocs")
```

We ran all preprocessing and analysis code using R (version 4.1.2, 64-bit Linux) on Stanford's high-performance computing cluster. Please adjust the file paths and number of CPU cores according to your system. Wherever possible, we used relative file paths so that you should be able to run most scripts without any edits. Just be sure to open `trump_blocs.Rproj` in RStudio, or otherwise set this repository as your working directory.
