library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(foreign)

###Reading in Reuter et al data
reuter.data <- read.dta("reuter_et_al_data.dta")

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

n.precincts <- matrix(NA, nrow = nrow(match.set), ncol = 1)
i <- 1
for(i in 1:nrow(match.set)){
  sub.iteration <- sub2004 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  n.precincts[i,1] <- nrow(sub.iteration) 
}

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

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 1)
data.container <- data.container %>% mutate(post.treatment = 0)

write.csv(data.container, "treatment group 2004 pre-treatment.csv")

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

n.precincts <- matrix(NA, nrow = nrow(match.set), ncol = 1)
i <- 1
for(i in 1:nrow(match.set)){
  sub.iteration <- sub2012 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  n.precincts[i,1] <- nrow(sub.iteration) 
}


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

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 1)
data.container <- data.container %>% mutate(post.treatment = 1)

write.csv(data.container, "treatment group 2012 post-treatment.csv")


###Combining the datasets
###
data2004 <- as_tibble(read.csv("treatment group 2004 pre-treatment.csv"))
data2012 <- as_tibble(read.csv("treatment group 2012 post-treatment.csv"))

data2004.small <- select(data2004, regionid, territory, territory.id, treatment.group, post.treatment, Number.of.voters.included.in.the.list,
                   Number.of.invalid.ballots, Number.of.valid.ballots, Number.of.ballots.in.mobile.ballot.boxes,
                   Number.of.voters.voting.by.absentee, Number.of.absentee.ballots.received,
                   Putin)

data2004.small <- data2004.small %>% rename(voter.list = Number.of.voters.included.in.the.list,
                          invalid = Number.of.invalid.ballots,
                          valid = Number.of.valid.ballots,
                          mobile = Number.of.ballots.in.mobile.ballot.boxes,
                          absentee.voters = Number.of.voters.voting.by.absentee,
                          absentee.received = Number.of.absentee.ballots.received,
                          putin = Putin)

data2012.small <- select(data2012, regionid, territory, territory.id, treatment.group, post.treatment,
                         number.of.voters.on.list, invalid, valid, number.in.mobile.ballot.box,
                         number.of.absentee.voters, absentee, putin)

data2012.small <- data2012.small %>% rename(voter.list = number.of.voters.on.list,
                                            mobile = number.in.mobile.ballot.box,
                                            absentee.voters = number.of.absentee.voters,
                                            absentee.received = absentee)

data.treatmentgroup <- rbind(data2004.small, data2012.small)

data.treatmentgroup <- data.treatmentgroup %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.treatmentgroup <- data.treatmentgroup %>% mutate(putin.abshare = putin/voter.list)


write.csv(data.treatmentgroup, "treatment group.csv")

###Quick test model
library(lme4)
model.test <- lmer(putin.abshare~pct.nonstandard + post.treatment + pct.nonstandard*post.treatment +
                     (1 + pct.nonstandard | territory.id),
                 data=data.treatmentgroup, REML=FALSE)
summary(model.test)
     #Model works--basic results:
     #Percent non-standard benefits Putin absolute vote-share prior to treatment (.36)
     #Putin absolute vote-share is lower after treatment 
     #Treatment increases the relationship between Putin absolute share and nonstandard share by almost 30% (.09)

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

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 0)
data.container <- data.container %>% mutate(post.treatment = 0)

write.csv(data.container, "control group 2004 pre-treatment.csv")

###Testing that those regions with multiple targets work
###
#sub.iteration <- sub2004 %>% filter(regionid == 88) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
#match.targets <- as.character(match.set$match.target[42])
#str_view(sub.iteration$territory, match.targets, match=TRUE)
#districts <- unique(sub.iteration$territory) #For troubleshooting

#sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
#sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
#unique(sub.iteration$territory)

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

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 0)
data.container <- data.container %>% mutate(post.treatment = 1)

write.csv(data.container, "control group 2012 post-treatment.csv")


###Combining the datasets
###
data2004 <- as_tibble(read.csv("control group 2004 pre-treatment.csv"))
data2012 <- as_tibble(read.csv("control group 2012 post-treatment.csv"))

data2004.small <- select(data2004, regionid, territory, territory.id, treatment.group, post.treatment, Number.of.voters.included.in.the.list,
                         Number.of.invalid.ballots, Number.of.valid.ballots, Number.of.ballots.in.mobile.ballot.boxes,
                         Number.of.voters.voting.by.absentee, Number.of.absentee.ballots.received,
                         Putin)

data2004.small <- data2004.small %>% rename(voter.list = Number.of.voters.included.in.the.list,
                                            invalid = Number.of.invalid.ballots,
                                            valid = Number.of.valid.ballots,
                                            mobile = Number.of.ballots.in.mobile.ballot.boxes,
                                            absentee.voters = Number.of.voters.voting.by.absentee,
                                            absentee.received = Number.of.absentee.ballots.received,
                                            putin = Putin)

data2012.small <- select(data2012, regionid, territory, territory.id, treatment.group, post.treatment,
                         number.of.voters.on.list, invalid, valid, number.in.mobile.ballot.box,
                         number.of.absentee.voters, absentee, putin)

data2012.small <- data2012.small %>% rename(voter.list = number.of.voters.on.list,
                                            mobile = number.in.mobile.ballot.box,
                                            absentee.voters = number.of.absentee.voters,
                                            absentee.received = absentee)

data.controlgroup <- rbind(data2004.small, data2012.small)

data.controlgroup <- data.controlgroup %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.controlgroup <- data.controlgroup %>% mutate(putin.abshare = putin/voter.list)


write.csv(data.controlgroup, "control group.csv")

###Combining treatment and control datasets
control.data <- read.csv("control group.csv")
treatment.data <- read.csv("treatment group.csv")

mayors.data <- rbind(control.data, treatment.data)

write.csv(mayors.data, "all local results.csv")

###Getting nonstandard.coefs for each election

sub2004 <- mayors.data %>% filter(post.treatment == 0)
library(lme4)
model.test <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                   data=sub2004, REML=FALSE)
coefs.test <- coef(model.test)
coefs.territory0 <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>% mutate(
     post.treatment = 0)
coefs.territory0 <- mutate(coefs.territory0,
                           regionid = NA)

j <- 1
for(j in 1:nrow(coefs.territory0)){
  sub.territory.04 <- subset(sub2004, sub2004$territory.id == coefs.territory0$territory.id[j])
  coefs.territory0$regionid[j] <- sub.territory.04$regionid[1]
}

write.csv(coefs.territory0, "pre-treatment coefs by territory.csv")

 ###

sub2012 <- mayors.data %>% filter(post.treatment.y == 1)
j <- 1
for(i in 1:length(unique(sub2004$territory.id))){
  sub.territory.12 <- subset
}

library(lme4)
model.test <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory.id),
                   data=sub2012, REML=FALSE)
coefs.test <- coef(model.test)
coefs.territory1 <- as_tibble(coefs.test$territory.id, rownames = 'territory.id') %>% mutate(
  post.treatment = 1)

coefs.territory1 <- mutate(coefs.territory1,
                           regionid = NA)

j <- 1
for(j in 1:nrow(coefs.territory1)){
  sub.territory.12 <- subset(sub2012, sub2012$territory.id == coefs.territory1$territory.id[j])
  coefs.territory1$regionid[j] <- sub.territory.12$regionid[1]
}

write.csv(coefs.territory1, "post-treatment coefs by territory.csv")

coefs.all <- rbind(coefs.territory0, coefs.territory1)
coefs.all <- coefs.all %>% mutate(treatment.group = 0) 

write.csv(coefs.all, "all coefs by territory.csv")

###Merging coefs by territory with precinct-level dataset
mayors.data <- read.csv("all local results.csv")

mayors.data <- mayors.data %>% left_join(coefs.all, by = "territory.id")

###Quick test models
coefs.all <- read.csv("all coefs by territory with covariates.csv") #This version has the covariates

didmodel <- lm(pct.nonstandard ~ treatment.group + post.treatment +
                 treatment.group*post.treatment, data=coefs.all)
summary(didmodel)



