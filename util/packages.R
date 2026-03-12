required_packages <- c(
  "tidyverse",
  "patchwork",
  "fuzzyjoin",
  "lmerTest",
  "sjPlot",
  "HLMdiag",
  "glmmTMB",
  "broom.mixed",
  "plotly",
  "doMC",
  "MASS",
  "knitr",
  "kableExtra",
  "emmeans",
  "pbkrtest",
  "effsize",
  "hms",
  "data.table",
  "reshape2"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) {
  install.packages(new_packages)
}

if (!("cosinoRmixedeffects" %in% installed.packages()[, "Package"])) {
  if (!("remotes" %in% installed.packages()[, "Package"])) {
    install.packages("remotes")
  }
  remotes::install_github("maytesuarezfarinas/cosinoRmixedeffects")
}

if (!("limma" %in% installed.packages()[, "Package"])) {
  if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("limma")
}

library(tidyverse) # Includes readr, lubridate, tibble, purrr, tidyr, and more
library(patchwork)
library(sjPlot)
library(fuzzyjoin)
library(lmerTest) # Automatically loads lme4
library(HLMdiag)
library(glmmTMB)
library(broom.mixed)
library(data.table)
library(plotly)
library(splines)
library(emmeans)
library(reshape2)
library(limma)
library(cosinoRmixedeffects)
library(knitr)
library(kableExtra)
library(pbkrtest)
library(effsize)
library(hms)