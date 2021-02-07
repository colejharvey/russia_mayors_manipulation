##Missing cities coefficients

library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(foreign)

###Reading in Reuter et al data
reuter.data <- read.csv("reuter_et_al_data_v3.csv")

###Treatment group

ids <- read.csv("russia region ids.csv")
match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2004 <- read_excel("2004 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2004 %>% filter(regionid==99)  ##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2004 %>% filter(regionid == match.set$regionid.ch[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
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

write.csv(data.container, "treatment group 2004 missing.csv")

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
match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
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

write.csv(data.container, "treatment group 2012 missing.csv")


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





#####
library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(lme4)

###
###Treatment group
###

###2003

ids <- read.csv("russia region ids.csv")
match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2003 <- read_excel("2003 duma election.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2003 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2003 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
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
                                            absentee.received = "Number of absentee ballots received")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)



ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(united.russia, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(united.russia, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)

###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(ur.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE)
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2003
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")


write.csv(coefs.tg.pretreat, "missing coefs treatment group 2003 pre-treatment w city_id.csv")



###2004



sub2004 <- read_excel("2004 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2004 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2004 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
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
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)



ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(putin, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(putin, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)


###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2004
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(putin.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")

write.csv(coefs.tg.pretreat, "missing coefs treatment group 2004 pre-treatment w city_id.csv")


###2007



match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2007 <- read_excel("2007 duma election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2007 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2007 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
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
                                            absentee.received = "Number of absentee ballots received")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)


ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(united.russia, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(united.russia, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)

###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(ur.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2007
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")


write.csv(coefs.tg.pretreat, "missing coefs treatment group 2007 w city_id.csv")


###2008

match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2008 <- read_excel("2008 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2008 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2008 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom1, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
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
                                            absentee.received = "Number of absentee ballots received")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = Medvedev/voter.list)
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)


ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(Medvedev, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(Medvedev, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)


###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(ur.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2008
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")

write.csv(coefs.tg.pretreat, "missing coefs treatment group 2008 w city_id.csv")



###2011

match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

sub2011 <- read_excel("2011 duma election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2011 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2011 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom3, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
  if ((nrow(sub.iteration) == 0)){
    print("Error, no observations")
    break
  }
  data.container <- rbind(data.container, sub.iteration)
}

##The problem is in the 2011 spreadsheet--some districts are coded in kom2, others in kom3. Will need a fix.

###Generating new variables
data.container <- data.container %>% rename(#voter.list = "Number of voters included in the list",
  invalid = "invalid.ballots",
  valid = "valid.ballots",
  mobile = "mobile.ballots",
  absentee.voters = "absentee.voters",
  absentee.received = "absentee.received")


data.container <- data.container %>% mutate(pct.nonstandard = (absentee.voters + mobile)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)


ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(united.russia, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(united.russia, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)


###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(ur.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2011
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")

write.csv(coefs.tg.pretreat, "missing coefs treatment group 2011 w city_id.csv")





####Treatment group 2012
match.set <- read.csv("missing matches.csv")

treatment.ids <- c(9, 34, 68, 33, 77, 41)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")
sub2012 <- read_excel("russia 2012 presidential election full.xlsx") %>% semi_join(treatment.ids)

treatment.ids <- arrange(treatment.ids, regionid)

data.container <- sub2012 %>% filter(regionid==99) %>%
  mutate(city_id = NA)##99 doesn't exit, so creates an empty tibble with correct columns

i<-1

for(i in 1:nrow(match.set)){
  sub.iteration <- sub2012 %>% filter(regionid == match.set$regionid[i]) %>% mutate(territory = stri_trans_general(kom2, 'latin'))
  match.targets <- as.character(match.set$match.target[i])
  city_id.temp <- match.set$city_id[i]
  sub.iteration <- sub.iteration %>% mutate(match.tf = (is.na(str_extract(sub.iteration$territory, match.targets))==FALSE))
  sub.iteration <- sub.iteration %>% filter(match.tf == TRUE)
  sub.iteration <- sub.iteration %>% mutate(match.target = as.character(match.set$match.target[i]))
  territory.list <- str_split_fixed(sub.iteration$territory, boundary("word"), n =2)  #Creates a fixed-column-number dataframe
  sub.iteration <- sub.iteration %>% mutate(territory.id = territory.list[,1])
  sub.iteration <- sub.iteration %>% mutate(city_id = city_id.temp)
  
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
data.container <- data.container %>% mutate(total.votes = valid + invalid)
data.container <- data.container %>% mutate(turnout = total.votes / voter.list)


ur.totals <- data.container  %>% group_by(city_id) %>% summarize(sum(putin, na.rm=TRUE))
vote.totals <- data.container %>% group_by(city_id) %>% summarize(sum(total.votes, na.rm=TRUE))
ur.results <- merge(ur.totals, vote.totals, by = "city_id")
ur.results <- ur.results %>% rename(ur.totals = 'sum(putin, na.rm = TRUE)')
ur.results <- ur.results %>% rename(vote.totals = 'sum(total.votes, na.rm = TRUE)')

ur.results <- ur.results %>% mutate(ur.voteshare = ur.totals / vote.totals)

###Generating nonstandard vote coefficients

library(lme4)
model.ns.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2012
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(putin.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")

write.csv(coefs.tg.pretreat, "missing coefs treatment group 2012 post-treatment w city_id.csv")


missing.all <- read.csv("missing treatment coefs.csv")
coefs.all <- missing.all %>% mutate(city_id_year = paste(city_id, year, sep = "_"))

reuter.data <- read.csv("reuter_et_al_data_v3.csv")

full.data <- coefs.all %>% merge(reuter.data, by="city_id_year")

write.csv(full.data, "missing coefs with covariates.csv")
