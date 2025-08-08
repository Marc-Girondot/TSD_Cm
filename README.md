# Workflow for Green Sea Turtle temperature-dependent sex determination analysis

**Resilience of Green Sea Turtles to a Feminizing Climate: A Validated Model of Sex Ratio Under Temperature-Dependent Sex Determination**

The zip folder must be opened in Rstudio and the code for tsd analysis at constant temperature (tsd pattern) is in file "TSD Cm final RE.r" while the code the sex ratio analysis at fluctuation temperatures is in "CM.Rmd" file.

# ACCESS INFORMATION

## 1. Licenses

CC0 1.0 Universal (CC0 1.0)

## 2. Data derived from other sources

Some data comes from ROSIE database:

Krueger, C. J., & Janzen, F. J. (2022). ROSIE, a database of reptilian offspring sex ratios and sex-determining mechanisms, beginning with Testudines. Scientific Data, 9(1), 22. https://doi.org/10.1038/s41597-021-01108-1 

The ROSIE database is embeded in R package embryogrowth:

Girondot, M. (2025). embryogrowth: Tools to analyze the thermal reaction norm of embryo growth. (Version >10.2) The Comprehensive R Archive Network. https://CRAN.R-project.org/package=embryogrowth

## 3. Recommended citation for this data/code archive

Will be provided when the paper will be published

# DATA & CODE FILE OVERVIEW

This data repository consist of 36 data files, 2 code scripts, and this README document.

## Data files and variables

1. template.docx
A template used for knitr
2. Cm.Rproj
The project to be opened in Rstudio
3. figs
The folder to store the pictures generated during knitr
4. dataIn/
The folder with raw data for analyzed nests
5. dataOut/
The folder with all results recorded. All the results of the analyses are stored in dataOut folder.

## Code scripts and workflow

The code is in two parts. 

The code file "TSD Cm final RE.r" permits to estimate the thermal reaction norm of sex ratio. It generates the two files "flexit_CM.Rdata" and "tsd_flexit.Rdata" located in dataOut/tsd/

The code in "Cm.Rmd" permits to estimate the model to convert a timeseries of temperatures into a sex ratio. The CM.Rmd can be knitred. All results are stored in dataOut/Gompertz/ and dataOut/ because some code are running for several days. 

# SOFTWARE VERSIONS

The last version of embryogrowth and HelpersMG packages must always be used. The version available in CRAN is generally not the last one. These functions can be used to ensure to use the last version.

install.packages("embryogrowth")

install.packages("HelpersMG")

install.packages("http://marc.girondot.free.fr/CRAN/embryogrowth.tar.gz", repos=NULL, type="source")

install.packages("http://marc.girondot.free.fr/CRAN/HelpersMG.tar.gz", repos=NULL, type="source")

# REFERENCES

Girondot, M. (2025). embryogrowth: Tools to analyze the thermal reaction norm of embryo growth. (Version 10.3) The Comprehensive R Archive Network. https://CRAN.R-project.org/package=embryogrowth

Girondot, M. (2025). HelpersMG: Tools for Environmental Analyses, Ecotoxicology and Various R Functions. (Version 6.6) The Comprehensive R Archive Network. https://CRAN.R-project.org/package=HelpersMG

