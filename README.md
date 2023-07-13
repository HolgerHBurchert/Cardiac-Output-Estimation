# Cardiac-Output-Estimation

This R project repository is associated with the manuscript "Revisiting cardiac output estimated noninvasively from oxygen uptake during exercise: An exploratory hypothesis generating replication study".
The repository contains the data and the analyses code to facilitate repetition, replication and reproduction of the analyses published in the manuscript.
To cite the code, use: [![DOI](https://zenodo.org/badge/644910171.svg)](https://zenodo.org/badge/latestdoi/644910171)

## Repository Content:
- The R file containing all the analyses code, called "Analyses_Code".

* The original data on which this replication study is based:
  * "Data_Stringer_AJP_1997" published by Stringer, W. W., Hansen, J. E., & Wasserman, K. (1997). Cardiac output estimated noninvasively from oxygen uptake during exercise. Journal of Applied Physiology, 82(3), 908-912. https://doi.org/10.1152/jappl.1997.82.3.908

  * "Data_Astrand_AJP_1964" published by Åstrand, P. O., Cuddy, T. E., Saltin, B., & Stenberg, J. (1964). Cardiac output during submaximal and maximal work. Journal of Applied Physiology, 19(2), 268-274. https://doi.org/10.1152/jappl.1964.19.2.268

- Four PDF files of the Figures presented in the associated manuscript. Figures 2, 3, 4 and Figure_supplement are created by running the R code, Figure 5 was plotted in GraphPad PRISM.

- The LICENSE file providing the terms of use.
- The R project file named "Cardiac-Output-Estimation.Rproj". 
- the .gitignore file

## Dependencies
The R code requires that the following packages are installed: ggplot2, ggpubr, rootSolve, cowplot, car and AICcmodavg. These packages are available at https://cran.r-project.org/ and
can be installed using the following code:

```{r}
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("rootSolve")
install.packages("cowplot")
install.packages("car")
install.packages("AICcmodavg")
```
