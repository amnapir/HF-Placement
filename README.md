# HF-Placement
The repository contains scripts and data for replication and evaluation of the methodology used in the manuscript. Unfortunately, due to non-disclosure agreement, the authors are not allowed to release the empirical data as-is. bUT WE HAVE SHARED ALLTH MAIN DATA USED FOR STATICTIAL ANALYSIS, ONLY REMOVED THE DATA FROM TEXT FIELDS SUCH ROLE DESCRIPTION AND OTHER COMMENTS SECTION.BECAUSE IN MANY OFTHE FIELDS  WHERE IT WAS POSSIBLE TO FIND THE IDENTIFY OF PARTICPANTS.

We provide a data set, which an interested reader could make use of to follow our analysis as presented in the replication package.
[data/] contains data.rds, which is a synthetic data set generated using the empirical data set. In synthDataGen.R one can see how we have generated this data set. This means that one can follow the same steps as in our replication package, but instead use the synthetic data set by simply loading it in R using

In data/ all data for the analysis can be found in MULTIPLE formats including csv, r, and sav format. In the model described in this repliucation we used sav file. 

In [docs/] conatinsthe replication package. one can find the R Markdown file (index.Rmd) which, when executed in RStudio compiles to index.html, which can be viewed online. (word of warning, we use `Stan`, `brms`, etc. so there's a lot to install and configure so one can  use `Docker`)

 [data/plots/]  contains plots as found in the manuscript (in addition to some that have not been included in order to save space).

Finally, one can always look at the replication package directly [online]().

In case of questions please contact [Amna Pir Muhammad ](mailto:amnap@chalmers.se?subject=[GitHub]%20Feature%20Selection).
