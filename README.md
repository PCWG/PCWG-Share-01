#Introduction
This is the repository for an R package that provides code to aggregate responses to the Power Curve Working Group "PCWG_Share_01" exercise. More details of the exercise can be found at http://www.pcwg.org and http://www.pcwg.org/PCWG-Share-01/PCWG-Share-01-Definition-Document.pdf.

#Requirements
1. A working installation of R. This can be obtained from e.g. http://r-project.org.
2. A GUI for R. R-studio is strongly recommended. See http://rstudio.com.
3. Two or more data files from the Power Curve Working Group Tool, prepared for PCWG_Share_01

#Releases
You can find the most recent _stable_ version of this code in the [releases](../../releases) part of this repository. These different versions are designed to work with different versions of the PCWG Share Tool. You can download the source there.

# How To Use This Code
1. Create a directory somewhere and place the contents of the zip file into that directory (let's call this `$root`). *It is recommended that there be nothing else in the directory*.
2. Load the PWCG tool data files into a 'data' directory in `$root`. You can add directories for different sites, etc.
3. Open `PCWG_share_01_main.rmd` in Rstudio. 
  1. Modify `project.root` to be `$root`.
  2. Decide if you intend the results to be public. If you do, set `data.public` to `TRUE`. This will prevent any details of the individual data sets being shown. If `data.public` is `FALSE`, all results will be printed.
  3. Decide if you want to reanalyze raw data and set `data.analyze.raw`. This should be `TRUE` the first time you run the code. Afterwards you can set this to `FALSE` to just generate plots.
4. Run `PCWG_share_01_main.rmd` in Rstudio. Either:
  1. hit `chunks -> run all` to run all of the R code and not generate any output PDF or HTML files.
  2. hit `knit HTML` to generate the html results file; this will create an HTML file but will not generate any data in memory
  3. hit `knit PDF` to generate the html results file; this will create an HTML file but will not generate any data in memory

#Documentation
Documentation is provided in several forms:
* within `PCWG_share_01_main.rmd`
* within `PCWG_share_01_main.pdf`, which will also contain the results when run using `knitr`.
* within `PCWG_share_01_main.html`, which will also contain the results when run using `knitr`.

#Reporting Bugs and Requesting Improvements
Please use the [issue tracker](../../issues) to report problems or request improvements.

#Wiki
Please use [the wiki](../../wiki) in this repository as you feel fit.

#Comments
This software is provided as is, with no guarantees.

#Recent changes
See /releases.

#Code Maintainers
* [Andy Clifton](mailto:andrew.clifton@nrel.gov) (National Renewable Energy Laboratory)

#Project Contributors
* Peter Stuart, RES
