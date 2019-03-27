library(tidyverse)

dat = read.csv("/Users/Tom/Documents/EcologicalModelingCourse/_A_Master_Lab_Exercises/Multi-level models NO2/IndexExample.csv")

#Make index for site (each unique)
dat$Site.index <- as.numeric(as.factor(dat$Site))

# or using tidyverse

dat = dat %>% mutate(Site.index = as.numeric(as.factor(Site)))

#Make index for strata and  for sites within strata
dat = dat %>% mutate(Stratum.index = as.numeric(as.factor(Stratum)))
 dat = dat %>% group_by(Stratum) %>%
     mutate(Site.within.strat.index = 1:n()) %>%
     ungroup
