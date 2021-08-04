# BayesianEstimation
Bayesian Time-Series Estimation Package idea using R6 Classes 

## Installation and Usage

If you want to install the package you have to make sure you have devtools and Rtools installed.
You can get Rtools here https://cran.r-project.org/bin/windows/Rtools/ - I used Rtools 41 for the installation, but higher versions should work too.

To get devtools you can just use the line of code below:
```{r ,echo=T,eval=F}
install.packages("devtools")
```

If you followed the steps, to get displayR from my GitHub just use:

```{r ,echo=T,eval=F}
devtools::install_github("SValv/BayesianEstimation")
```
There is a high probability that RStudio wants to update packages that got pre-installed from github. Just download all to ensure functionality.

To load and use it in your environment use:

```{r ,echo=T,eval=F}
library(displayR)
```

