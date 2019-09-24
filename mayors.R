library(readxl)
library(tidyverse)
library(stringi)
library(stringr)

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
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 0)
data.container <- data.container %>% mutate(treatment.treated = 0)

write.csv(data.container, "control group 2004 pre-treatment.csv")

###Testing that those regions with multiple targets work
###
sub.iteration <- sub2004 %>% filter(regionid == 88) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
match.targets <- as.character(match.set$match.target[42])
#str_view(sub.iteration$territory, match.targets, match=TRUE)
districts <- unique(sub.iteration$territory) #For troubleshooting

sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
unique(sub.iteration$territory)

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
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}

#districts <- unique(sub.iteration$territory) #For troubleshooting

data.container <- as_tibble(data.container)
data.container <- data.container %>% mutate(treatment.group = 0)
data.container <- data.container %>% mutate(treatment.treated = 1)

write.csv(data.container, "control group 2012 post-treatment.csv")


###Combining the datasets
###
data2004 <- as_tibble(read.csv("control group 2004 pre-treatment.csv"))
data2012 <- as_tibble(read.csv("control group 2012 post-treatment.csv"))

data2004.small <- select(data2004, regionid, territory, treatment.group, treatment.treated, Number.of.voters.included.in.the.list,
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

data2012.small <- select(data2012, regionid, territory, treatment.group, treatment.treated,
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

###Quick test model
mayors.data <- read.csv("all local results.csv")
mayors.data <- mayors.data %>% select(-X, -X.Intercept., - nonstandard.coef)
library(lme4)
model.test <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | territory),
                 data=mayors.data, REML=FALSE)
summary(model.test)
coefs.test <- coef(model.test)
coefs.territory <- as_tibble(coefs.test$territory, rownames = 'territory')

mayors.data <- coefs.territory %>% select(1:3) %>% right_join(mayors.data, by = "territory")
mayors.data <- rename(mayors.data, nonstandard.coef = pct.nonstandard.y,
                    pct.nonstandard = pct.nonstandard.x)

write.csv(mayors.data, "all local results.csv")

###Test DiD

didmodel <- lm(nonstandard.coef ~ treatment.group + treatment.treated +
                 treatment.group*treatment.treated, data=mayors.data)
summary(didmodel)