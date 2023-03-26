# README

Analysis code for https://osf.io/preprints/socarxiv/c9fkg.

The GitHub repository does not include any data files. Raw and preprocessed data are available on the [Harvard Dataverse](https://doi.org/10.7910/DVN/2ZV5X2). Unfortunately, due to Dataverse restrictions on file sizes, we are unable to post the raw or cleaned Census data. If you require these files, please send a message to coletl@stanford.edu, and we will transfer them to you. You may also download raw survey data directly from the U.S. Census, CPS, ANES, and CCES websites.

Files in `code/preprocessing/` prepare the raw data for analysis, writing clean files to `data/`. Each `code/vb_*.Rmd` file runs a voting bloc analysis with results in the corresponding HTML file. The file `code/race_sens.Rmd` examines sensitivity to the choice of surveym, and outputs the LaTeX-formatted tables that appear in the paper.

Please be sure to install the package dependecies before running any scripts in this repositiory.

```r
# install.packages("renv")
renv::restore()
```

The bulk of the paper's analyses rely on its accompanying `blocs` package. The package is currently available on CRAN, or you can download the development version from GitHub.
```r
install.packages("blocs")

# dev version
devtools::install_github("coletl/blocs")
```

We ran all preprocessing and analysis code on Stanford's high-performance computing cluster. Please adjust the file paths and number of CPU cores according to your system. Wherever possible, we used relative file paths so that you should be able to run most scripts without any edits. Just be sure to open `trump_blocs.Rproj` in RStudio, or otherwise set this repository as your working directory.
