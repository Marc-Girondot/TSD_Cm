# Workflow for Green Sea Turtles temperature-dependent sex determination 
Code to analyze Temperature-Dependent Sex Determination in green turtles, *Chelonia mydas*.

Code used in the submitted publication: 

**Resilience of Green Sea Turtles to a Feminizing Climate: A Validated Model of Sex Ratio Under Temperature-Dependent Sex Determination**

The zip folder must be opened in Rstudio and the code for tsd analysis at constant temperature (tsd pattern) is in file "TSD Cm final RE.r" while the code the sex ratio analysis at fluctuation temperatures is in "CM.Rmd" file.

All the results of the analyses are stored in dataOut folder.

The CM.Rmd can be knitred.

The last version of embryogrowth and HelpersMG packages must always be used. The version available in CRAN is generally not the last one. These functions can be used to ensure to use the last version.

install.packages("embryogrowth")

install.packages("HelpersMG")

install.packages("http://marc.girondot.free.fr/CRAN/embryogrowth.tar.gz", repos=NULL, type="source")

install.packages("http://marc.girondot.free.fr/CRAN/HelpersMG.tar.gz", repos=NULL, type="source")

