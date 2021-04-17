# How to Analyze Data

This is the accompanying repository for a series of tutorials I am developing on How to Analyze Data.

## Exploratory Analysis with the 2020 ANES Pre-election Survey

The rendered report in html format is [available here](https://czep.net/pub/anes2020-pre.html).

I have selected the 2020 ANES dataset as the main example dataset for use in my tutorials.  The ANES is a very high quality survey dataset that is freely available upon registering at the [ANES home page]().  There is an exceptional wealth of variables included in the study and it involves highly topical subject matter that is substantively very interesting.

While familiarizing myself with the ANES data, I prepared a set of initial crosstabs along with some notes to set the stage for how I would like to use the ANES dataset as a way of teaching the process of data analysis.  Since this is a complex survey on a broad range of topics that may be interesting to a lot of people, I wanted to share the work I've done in hopes that it will be useful for anyone approaching this survey for the first time.

Working with survey data in R requires some care because variance estimates must take into account the fact that the survey design is not a simple random sample.  Fortunately, the work of [Thomas Lumley](https://r-survey.r-forge.r-project.org/survey/) provides an excellent toolkit for properly specifying complex survey designs to ensure that variance is calculated correctly when computing confidence intervals, t-tests, chi-squared tests, and regressions.


## To reproduce

The report is written in RMarkdown with several supporting packages.  Start by cloning this repository and downloading the 2020 ANES dataset.  ANES data is publically available upon free registration at the [ANES project home page](https://electionstudies.org/).  Download the Stata file (.dta) and unzip.  The haven package in R is used to read Stata data files.  This preserves useful metadata such as the value lables.  These labels are stored as class `labelled` in R and there are a few differences from base R's factors.  Generally, they're very user friendly, but often you have to be careful when recoding because the labels can sometimes get dropped.

In the code, I've separated out two supporting files: one for recodes and one for the functions that are used mainly in tabulating.  The style of programming that the tidyverse enables can be quite elegant and with just a little bit more modularity this could easily be turned into a general purpose tabulation package.  In any case, if you are interested in adapting any of this code for your own use---with or without ANES data---I think this code is a good start.

To run the RMarkdown report, first install knitr as well as all of the packages listed in the "Environment" section if you do not already have them.  To get them all, run this in an R session:

    install.packages(c("knitr", tidyverse", "haven", "labelled", "kableExtra", "survey", "srvyr", "scales"))

You may also need a stand-alone install of [Pandoc](https://pandoc.org/).

You could knit the file from within RStudio, but in order to guarantee a clean session I typically call R from a shell like this:

    R -e "rmarkdown::render('anes2020-pre.Rmd')"

Most of the code chunks have caching turned on, so this will create a couple of directories to store the cached results.  I do recommend keeping this on because a first run of this report will take upwards of half an hour.  Yay interpreted languages!

## License

Any of the code I use to generate the report is free to use under the terms of the MIT License.  However, please do not reproduce the text of the report (my introduction as well as notes I've written about the tabs).

