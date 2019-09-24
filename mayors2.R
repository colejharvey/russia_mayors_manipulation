###Next thing to consider here is adding socioeconomic controls into the bottom-level
###model that generates the coefficients
###Also include number of districts within each city as a covariate

library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(lme4)

###Treatment group

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2004 <- read_excel("2004 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2004 %>% filter(regionid==99)  ##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2004 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}

 ###Generating new variables
data.container <- data.container %>% rename(voter.list = "Number of voters included in the list",
                                            invalid = "Number of invalid ballots",
                                            valid = "Number of valid ballots",
                                            mobile = "Number of ballots in mobile ballot boxes",
                                            absentee.voters = "Number of voters voting by absentee",
                                            absentee.received = "Number of absentee ballots received",
                                            putin = "Putin")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)



 ###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                   data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>%
    mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$regionid <- NA

j <- 1
for(j in 1:nrow(coefs.tg.pretreat)){
  territory.name <- coefs.tg.pretreat$territory.id[j]
  data.container <- data.container %>% mutate(match.tf = (is.na(str_extract(data.container$territory.id, territory.name))==FALSE))
  region.subset <- data.container %>% filter(match.tf == TRUE)
  regionid.temp <- region.subset$regionid[1]
  coefs.tg.pretreat$regionid[j] <- regionid.temp 
}


write.csv(coefs.tg.pretreat, "coefs treatment group 2004 pre-treatment.csv")

###Testing that those regions with multiple targets work
###
#sub.iteration <- sub2004 %>% filter(regionid == 75) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
#match.targets <- as.character(match.set$match.target[37])
#str_view(sub.iteration$territory, match.targets, match=TRUE)
#districts <- unique(sub.iteration$territory) #For troubleshooting

#sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
#sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
#unique(sub.iteration$territory)

###
###2012 election for treatment group
###

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2012 <- read_excel("russia 2012 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2012 %>% filter(regionid==99)  ##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2012 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom2, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}

###Generating new variables
data.container <- data.container %>% rename(voter.list = "number of voters on list",
                                            mobile = "number in mobile ballot box",
                                            absentee.voters = "number of absentee voters",
                                            absentee.received = "absentee")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)

###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.postreat <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>%
  mutate(treatment.group = 1, post.treatment = 1)

coefs.tg.postreat$regionid <- NA

j <- 1
for(j in 1:nrow(coefs.tg.postreat)){
  territory.name <- coefs.tg.postreat$territory.id[j]
  data.container <- data.container %>% mutate(match.tf = (is.na(str_extract(data.container$territory.id, territory.name))==FALSE))
  region.subset <- data.container %>% filter(match.tf == TRUE)
  regionid.temp <- region.subset$regionid[1]
  coefs.tg.postreat$regionid[j] <- regionid.temp 
}


write.csv(coefs.tg.postreat, "coefs treatment group 2012 post-treatment.csv")



###
###Control group
###

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2004 <- read_excel("2004 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2004 %>% filter(regionid==99)  ##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2004 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}


###Generating new variables
data.container <- data.container %>% rename(voter.list = "Number of voters included in the list",
                                            invalid = "Number of invalid ballots",
                                            valid = "Number of valid ballots",
                                            mobile = "Number of ballots in mobile ballot boxes",
                                            absentee.voters = "Number of voters voting by absentee",
                                            absentee.received = "Number of absentee ballots received",
                                            putin = "Putin")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)



###Generating nonstandard vote coefficients

model.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$regionid <- NA

j <- 1
for(j in 1:nrow(coefs.cg.pretreat)){
  territory.name <- coefs.cg.pretreat$territory.id[j]
  data.container <- data.container %>% mutate(match.tf = (is.na(str_extract(data.container$territory.id, territory.name))==FALSE))
  region.subset <- data.container %>% filter(match.tf == TRUE)
  regionid.temp <- region.subset$regionid[1]
  coefs.cg.pretreat$regionid[j] <- regionid.temp 
}


write.csv(coefs.cg.pretreat, "coefs control group 2004 pre-treatment.csv")



###
###2012 election for treatment group
###
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2012 <- read_excel("russia 2012 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2012 %>% filter(regionid==99)  ##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2012 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom2, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}


###Generating new variables
data.container <- data.container %>% rename(voter.list = "number of voters on list",
                                            mobile = "number in mobile ballot box",
                                            absentee.voters = "number of absentee voters",
                                            absentee.received = "absentee")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)

###Generating nonstandard vote coefficients

model.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.postreat <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>%
  mutate(treatment.group = 0, post.treatment = 1)

coefs.cg.postreat$regionid <- NA

j <- 1
for(j in 1:nrow(coefs.cg.postreat)){
  territory.name <- coefs.cg.postreat$territory.id[j]
  data.container <- data.container %>% mutate(match.tf = (is.na(str_extract(data.container$territory.id, territory.name))==FALSE))
  region.subset <- data.container %>% filter(match.tf == TRUE)
  regionid.temp <- region.subset$regionid[1]
  coefs.cg.postreat$regionid[j] <- regionid.temp 
}


write.csv(coefs.cg.postreat, "coefs control group 2012 post-treatment.csv")

###
###Combining the coef datasets
###
coefs.cg.pretreat <- read.csv("coefs control group 2004 pre-treatment.csv")
coefs.cg.postreat <- read.csv("coefs control group 2012 post-treatment.csv")
coefs.tg.pretreat <- read.csv("coefs treatment group 2004 pre-treatment.csv")
coefs.tg.postreat <- read.csv("coefs treatment group 2012 post-treatment.csv")

coefs.all <- rbind(coefs.cg.pretreat, coefs.cg.postreat, coefs.tg.pretreat,
                   coefs.tg.postreat)
write.csv(coefs.all, "all coefs by territory with covariates.csv")

###Quick test models
coefs.all <- read.csv("all coefs by territory with covariates.csv") #This version has the covariates

didmodel <- lm(pct.nonstandard ~ treatment.group + post.treatment +
                 treatment.group*post.treatment, data=coefs.all)
summary(didmodel)



