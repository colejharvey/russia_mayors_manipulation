###This version uses city_id from Reuter et al as the key


###Next thing to consider here is adding socioeconomic controls into the bottom-level
###model that generates the coefficients
###Also include number of districts within each city as a covariate

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
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                       data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2003  #68 coefs
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                       data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.tg.pretreat <- merge(coefs.tg.pretreat, ur.results, by = "city_id")
coefs.tg.pretreat <- merge(coefs.tg.pretreat, coefs.ur, by = "city_id")


write.csv(coefs.tg.pretreat, "coefs treatment group 2003 pre-treatment w city_id.csv")



  ###2004

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(putin.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2004 #68 obs


write.csv(coefs.tg.pretreat, "coefs treatment group 2004 pre-treatment w city_id.csv")


  ###2007

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2007 #68 obs

write.csv(coefs.tg.pretreat, "coefs treatment group 2007 w city_id.csv")


  ###2008

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2008 #68 obs

write.csv(coefs.tg.pretreat, "coefs treatment group 2008 w city_id.csv")



  ###2011

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2011 #68 obs


write.csv(coefs.tg.pretreat, "coefs treatment group 2011 w city_id.csv")

##Grabbing a subset for illustration
data.container.sub <- data.container %>% filter(city_id == 111)
write.csv(data.container.sub, "penza2011.csv")

data.container.sub <- data.container %>% filter(city_id == 94)
write.csv(data.container.sub, "novomoskovsk2011.csv")

##Grabbing a subset for illustration of turnout coefficient
data.container.sub <- data.container %>% filter(city_id == 27)
write.csv(data.container.sub, "Volgodonsk2011.csv")

data.container.sub <- data.container %>% filter(city_id == 23)
write.csv(data.container.sub, "VelikijNovgorod2011.csv")



####Treatment group 2012
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8, 43)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

##Absentee
model.absentee.coefs <- lmer(putin.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2012  #68 obs


write.csv(coefs.tg.pretreat, "coefs treatment group 2012 post-treatment w city_id.csv")



###
###Control group
###


   ###2003
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2003
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2003 #106 obs


write.csv(coefs.cg.pretreat, "coefs control group 2003 pre-treatment w city_id.csv")



   ###2004


ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
treatment.ids <- tibble(unique(treatment.ids))
treatment.ids <- rename(treatment.ids, regionid = "unique(treatment.ids)")

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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

model.ns.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2004
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(putin.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")

##Absentee
model.absentee.coefs <- lmer(putin.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2004 #106 obs



write.csv(coefs.cg.pretreat, "coefs control group 2004 pre-treatment w city_id.csv")



  ###2007

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2007
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2007 #106 obs


write.csv(coefs.cg.pretreat, "coefs control group 2007 w city_id.csv")


  ###2008
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")
match.set[58,3] <- "Âroslavskaâ"  #Necessary change to match this city
match.set[60,3] <- "Âroslavskaâ"  #Necessary change to match this city

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2008
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2008  #106, fixed

write.csv(coefs.cg.pretreat, "coefs control group 2008 w city_id.csv")



  ###2011

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2011
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(ur.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")

##Absentee
model.absentee.coefs <- lmer(ur.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2011 #106


write.csv(coefs.cg.pretreat, "coefs control group 2011 w city_id.csv")

###Subsets for illustration

data.container.sub <- data.container %>% filter(city_id == 6) #Armavir, no relationship
write.csv(data.container.sub, "armavir2011.csv")
data.container.sub <- data.container %>% filter(city_id == 38) #Elec, for comparison
write.csv(data.container.sub, "elec2011.csv")


###
###2012 election for control group
###
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88, 85, 12, 80)
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
data.container <- data.container %>% mutate(pct.absentee = (absentee.voters)/voter.list)

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

model.ns.coefs <- lmer(putin.abshare~pct.nonstandard  + (1 + pct.nonstandard | city_id),
                       data=data.container, REML=FALSE, 
                       control = lmerControl(optimizer ="Nelder_Mead"))
coefs.test <- coef(model.ns.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2012
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

model.coefs <- lmer(putin.abshare~turnout  + (1 + turnout | city_id),
                    data=data.container, REML=FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
coefs.ur <- coef(model.coefs)

coefs.ur <- as_tibble(coefs.ur$city_id, rownames = 'city_id') 


coefs.cg.pretreat <- merge(coefs.cg.pretreat, ur.results, by = "city_id")
coefs.cg.pretreat <- merge(coefs.cg.pretreat, coefs.ur, by = "city_id")


##Absentee
model.absentee.coefs <- lmer(putin.abshare~pct.absentee  + (1 + pct.absentee | city_id),
                             data=data.container, REML=FALSE)
coefs.abs <- coef(model.absentee.coefs)
coefs.abs <- as_tibble(coefs.abs$city_id, rownames = 'city_id')

coefs.abs$year <- 2012 #106


write.csv(coefs.cg.pretreat, "coefs control group 2012 post-treatment w city_id.csv")

###Combining the coef data
  ###Combining done manually in Excel


coefs2003t <- read.csv("coefs treatment group 2003 pre-treatment w city_id.csv")
coefs2004t <- read.csv("coefs treatment group 2004 pre-treatment w city_id.csv")
coefs2007t <- read.csv("coefs treatment group 2007 w city_id.csv")
coefs2008t <- read.csv("coefs treatment group 2008 w city_id.csv")
coefs2011t <- read.csv("coefs treatment group 2011 w city_id.csv")
coefs2012t <- read.csv("coefs treatment group 2012 post-treatment w city_id.csv")

coefs2003c <- read.csv("coefs control group 2003 pre-treatment w city_id.csv")
coefs2004c <- read.csv("coefs control group 2004 pre-treatment w city_id.csv")
coefs2007c <- read.csv("coefs control group 2007 w city_id.csv")
coefs2008c <- read.csv("coefs control group 2008 w city_id.csv")
coefs2011c <- read.csv("coefs control group 2011 w city_id.csv")
coefs2012c <- read.csv("coefs control group 2012 post-treatment w city_id.csv")


coefs.all <- rbind(coefs2003t, coefs2004t, coefs2007t, coefs2008t, coefs2011t, coefs2012t,
                   coefs2003c, coefs2004c, coefs2007c, coefs2008c, coefs2011c, coefs2012c)
coefs.all <- coefs.all %>% mutate(city_id_year = paste(city_id, year, sep = "_"))
write.csv(coefs.all, "all ur coefs with cityid.csv")

###Quick test models
coefs.all <- read.csv("all ur coefs with cityid.csv") 

didmodel <- lm(pct.nonstandard ~ treatment.group + post.treatment +
                 treatment.group*post.treatment, data=coefs.all)
summary(didmodel)

m.2way.fe <- lm(pct.nonstandard ~ factor(city_id) + factor(year) + treatment.group, data=coefs.all)
summary(m.2way.fe)

####Getting Reuter et al data
library(foreign)

###Reading in Reuter et al data
reuter.data <- as_tibble(read.dta("reuter_et_al_data.dta")) 

data.container <- reuter.data %>% filter(regionid==99) %>%  mutate(cancel.year = NA)
for (i in unique(reuter.data$city_id)){
  city.sub <- subset(reuter.data, reuter.data$city_id == i)
  cancel.year.row <- subset(city.sub, city.sub$cancel == 1)
  cancel.year <- cancel.year.row$year
  if (length(cancel.year) == 0) {
    city.sub <- city.sub %>% mutate(cancel.year = NA)
    data.container <- rbind(data.container, city.sub)}
  else {
  city.sub <- city.sub %>% mutate(cancel.year = max(cancel.year))
  data.container <- rbind(data.container, city.sub)
  }
}
  
reuter.data <- data.container  

write.csv(reuter.data, "reuter_et_al_data_v2.csv")

###Combining reuter data with coefs
coefs.all <- read.csv("all ur coefs with cityid.csv") 

coefs.all <- coefs.all %>% mutate(intercept.ns = round(X.Intercept..x, digits = 4))
coefs.all <- coefs.all %>% mutate(nonstandard.coef = round(pct.nonstandard, digits = 4))
coefs.all <- coefs.all %>% mutate(intercept.turnout = round(X.Intercept..y, digits = 4))
coefs.all <- coefs.all %>% dplyr::select(-post.treatment, -X.Intercept..x, -X.Intercept..y, -pct.nonstandard)

coefs.all <- coefs.all %>% mutate(city_id_year = paste(city_id, year, sep="_"))

reuter.data <- read.csv("reuter_et_al_data_v2.csv")

full.data <- coefs.all %>% merge(reuter.data, by="city_id_year")

###Adding regionid.ch
regionids <- read_xlsx("match-targets-combined.xlsx")
regionids <- regionids %>% dplyr::select(regionid.ch, city_id)
regionids$city_id <- factor(regionids$city_id)

full.data <- full.data %>% rename(city_id = city_id.x)
full.data$city_id <- factor(full.data$city_id)
full.data <- full.data %>% merge(regionids, by = "city_id")

full.data <- full.data %>% distinct(city_id_year, .keep_all = T)  #Remove duplicates from merge

###Determining treatment status by year
full.data <- full.data %>% mutate(treat.group.07 = ifelse(is.na(cancel.year) == TRUE, 0, 
                                                          ifelse(cancel.year < 2007, 1, 0)))  #TG if elections canceled in 04/05/06
full.data <- full.data %>% mutate(treat.group.08 = ifelse(is.na(cancel.year) == TRUE, 0, 
                                                          ifelse(cancel.year == 2007, 1, 0))) #TG if elections canceled in 04/05/06/07

full.data <- full.data %>% mutate(treat.group.11 = ifelse(is.na(cancel.year) == TRUE, 0, 
                                                          ifelse(cancel.year > 2007 & cancel.year < 2011, 1, 0)))  #TG if canceled in 08/09/10

full.data <- full.data %>% mutate(treat.group.12 = ifelse(is.na(cancel.year) == TRUE, 0, 
                                                          ifelse(cancel.year == 2011, 1, 0)))  #TG if canceled in 08/09/10


full.data <- full.data %>% mutate(posttreat.07 = ifelse(year.x >= 2007, 1, 0))
full.data <- full.data %>% mutate(posttreat.08 = ifelse(year.x >= 2008, 1, 0))
full.data <- full.data %>% mutate(posttreat.11 = ifelse(year.x >= 2011, 1, 0))
full.data <- full.data %>% mutate(posttreat.12 = ifelse(year.x >= 2012, 1, 0))

full.data <- full.data %>% mutate(presidential = ifelse(year.x == 2004 | year.x == 2008 | year.x == 2012, 1, 0))

###Combining full data with variables from diss project
regional.data <- read_xlsx("russia full ts data from diss.xlsx")

regional.data <- regional.data %>% dplyr::select(urgov, comp.scale2, comp.scale03_18, regionid, year, xconst.lag, pensioners.per.1000, ethnic.categorical,
                                                 putin.app.3mo, govemploy.by.pop, urgov)
regional.data <- regional.data %>% rename(year.x = year, regionid.ch = regionid)

full.data <- full.data %>% merge(regional.data, by = c("regionid.ch", "year.x"))

###Determining whether old mayor retained after cancelation
data.container <- full.data %>% filter(regionid==99) %>%  mutate(mayor.cancel.year = NA)
for (i in unique(full.data$city_id)){
  city.sub <- subset(full.data, full.data$city_id == i)
  cancel.year.row <- subset(city.sub, city.sub$cancel == 1)
  mayor.cancel.year <- cancel.year.row$mayorid
  if (length(mayor.cancel.year) == 0) {
    city.sub <- city.sub %>% mutate(mayor.cancel.year = 0)
    data.container <- rbind(data.container, city.sub)}
  else {
    city.sub <- city.sub %>% mutate(mayor.cancel.year = mayor.cancel.year)
    data.container <- rbind(data.container, city.sub)
  }
}

full.data <- data.container

full.data <- full.data %>% mutate(elections.kept = ifelse(is.na(cancel.year) == TRUE, 1, 0))
full.data <- full.data %>% mutate(elections.canceled = ifelse(is.na(cancel.year) == FALSE, 1, 0))

full.data <- full.data %>% mutate(treat.appoint.newmayor = ifelse(is.na(cancel.year) == FALSE & 
                                                                    year.x > cancel.year &
                                                                    mayorid != mayor.cancel.year, 1, 0))
full.data <- full.data %>% mutate(treat.appoint.oldmayor = ifelse(is.na(cancel.year) == FALSE & 
                                                                    year.x > cancel.year &
                                                                   mayorid == mayor.cancel.year, 1, 0))
sum(full.data$treat.appoint.oldmayor, na.rm=TRUE)  #One year where the old mayor is retained


###Getting mayor tenure in years
mayors.start <- full.data %>% group_by(mayorid) %>% summarise(min(year.x))
mayors.start <- mayors.start %>% rename(mayor.start.year = `min(year.x)`)
full.data <- merge(full.data, mayors.start, by="mayorid")

full.data <- full.data %>% mutate(mayor.tenure = year.x - mayor.start.year)

#Presidential
full.data <- full.data %>% mutate(presidential = ifelse(year.x == 2004 | year.x == 2008 | year.x == 2012, 1, 0))


write.csv(full.data, "coefs with city covariates.csv")
###Quick test model of full data, using only 2004 and 2012
full.data <- read.csv("coefs with city covariates.csv")



full.data <- full.data %>% filter(cancel.year > 2004 | is.na(cancel.year) == TRUE) #Comment this out to include obs treated in 2004

full.data.sub <- subset(full.data, full.data$year.x == 2004 | full.data$year.x == 2012)
full.data.sub <- full.data.sub %>% mutate(post.treatment = ifelse(year.x == 2012, 1, 0))
didmodel <- lm(nonstandard.coef ~ treatment.group + post.treatment +
                 treatment.group*post.treatment + mayor.tenure +
                 UR_majority + lnAvgSalary + unemployrate +  dem + pctRussian2002_new + education, data=full.data.sub)
summary(didmodel)  #Success! But this result is very fragile / model dependent

m.2way.fe <- lm(nonstandard.coef ~ factor(city_id) + post.treatment + treatment.group:post.treatment, data=full.data.sub)
summary(m.2way.fe)

###Year-by-year models

  ###2007 models
full.data.sub07 <- subset(full.data, full.data$year.x <= 2007)
full.data.sub07 <- full.data.sub07 %>% mutate(post.treatment = ifelse(year.x == 2007, 1, 0))


didmodel07 <- lm(nonstandard.coef ~ treat.group.07 + post.treatment +
                   treat.group.07*post.treatment +  ethnic.categorical + urgov  + mayor.tenure +
                   lnAvgSalary + unemployrate + dem +  education + presidential, data=full.data.sub07)
summary(didmodel07) #Positive effect at .1 sig


  ###2008 model

full.data.sub08 <- subset(full.data, full.data$year.x <= 2008)
full.data.sub08 <- full.data.sub08 %>% mutate(post.treatment = ifelse(year.x == 2008, 1, 0))

didmodel08 <- lm(nonstandard.coef ~ treat.group.08 + post.treatment +
                   treat.group.08*post.treatment +  ethnic.categorical + urgov + mayor.tenure +
                    lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.sub08)
summary(didmodel08) #No effect


###2011 model

full.data.sub11 <- subset(full.data, full.data$year.x <= 2011)
full.data.sub11 <- subset(full.data.sub11, full.data.sub11$treat.group.07 == 0)
full.data.sub11 <- subset(full.data.sub11, full.data.sub11$treat.group.08 == 0)

full.data.sub11 <- full.data.sub11 %>% mutate(post.treatment = ifelse(year.x == 2011, 1, 0))

didmodel11 <- lm(nonstandard.coef ~ treat.group.11 + post.treatment +
                   treat.group.11*post.treatment +  ethnic.categorical + urgov + mayor.tenure +
                    lnAvgSalary + unemployrate +   education + presidential, data=full.data.sub11)
summary(didmodel11) #No effect


###2012 model

full.data.sub12 <- subset(full.data, full.data$year.x <= 2012)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.07 == 0)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.08 == 0)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.11 == 0)


full.data.sub12 <- full.data.sub12 %>% mutate(post.treatment = ifelse(year.x == 2012, 1, 0))

didmodel12 <- lm(nonstandard.coef ~ treat.group.12 + post.treatment +
                   treat.group.12*post.treatment +  ethnic.categorical + urgov + mayor.tenure +
                    lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.sub12)
summary(didmodel12) #No effect

###Next step could be 03/04 vs. 07/08
###Then 07/08 vs. 11/12

full.data.0308 <- subset(full.data, full.data$year.x <= 2008)
full.data.0308 <- full.data.0308 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2004)
full.data.0308 <- full.data.0308 %>% mutate(treat.group.0308 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year < 2008, 1, 0)))
full.data.0308 <- full.data.0308 %>% mutate(post.treat.0308 = ifelse(year.x == 2007 | year.x == 2008, 1, 0))


didmodel0308 <- lm(nonstandard.coef ~ treat.group.0308 + post.treat.0308 +
                     treat.group.0308*post.treat.0308 +  ethnic.categorical + urgov + margin + mayor.tenure +
                   lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0308)
summary(didmodel0308) #Success! Treatment group produces more manipulation than control in post-treatment period

qplot(jitter(full.data.0308$year.x), full.data.0308$nonstandard.coef, aes(color = factor(full.data.0308$treat.group.0308)))

###07/08 vs 11/12

full.data.0712 <- subset(full.data, full.data$year.x >= 2007)
full.data.0712 <- full.data.0712 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2008)
full.data.0712 <- full.data.0712 %>% mutate(treat.group.0712 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year <= 2010, 1, 0)))
full.data.0712 <- full.data.0712 %>% mutate(post.treat.0712 = ifelse(year.x == 2011 | year.x == 2012, 1, 0))


didmodel0712 <- lm(nonstandard.coef ~ treat.group.0712 + post.treat.0712 +
                     treat.group.0712*post.treat.0712 +  ethnic.categorical + urgov  + margin  + mayor.tenure +
                     lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0712)
summary(didmodel0712) #No effect


###03/04 vs. 11/12

full.data.0312 <- subset(full.data, full.data$year.x <= 2004 | full.data$year.x > 2008)
full.data.0312 <- full.data.0312 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2008)
full.data.0312 <- full.data.0312 %>% mutate(treat.group.0312 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year > 2008 & cancel.year <= 2010, 1, 0)))
full.data.0312 <- full.data.0312 %>% mutate(post.treat.0312 = ifelse(year.x == 2011 | year.x == 2012, 1, 0))


didmodel0312 <- lm(nonstandard.coef ~ treat.group.0312 + post.treat.0312 +
                     treat.group.0312*post.treat.0312 +  ethnic.categorical + urgov + margin + mayor.tenure +
                     lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0312)
summary(didmodel0312) #No effect


###A next step will be to consider how the later wave of treatments might differ from the first wave
###It might also be worth investigating whether the delivery of manipulation itself might be beneficial for preventing the canceling of elections

full.data %>% group_by(cancel.year) %>% summarize(mean(UR_majority, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(comp.scale2, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(dem, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(lnAvgSalary, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(margin, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(civsoc91, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(regelect_margin, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(URshare, na.rm=TRUE))
full.data %>% group_by(cancel.year) %>% summarize(mean(mayor.tenure, na.rm=TRUE))
   ggplot(data=na.omit(full.data[,c("mayor.tenure","cancel.year")]), aes(mayor.tenure, colour = factor(cancel.year))) +
    geom_freqpoly(binwidth = 1)

   ggplot(data=na.omit(full.data[,c("mayor.tenure","elections.canceled")]), aes(mayor.tenure, colour = factor(elections.canceled))) +
     geom_freqpoly(binwidth = 1)
#Major differences aren't apparent, but the groups do appear more balanced in 0308 vs 0712,
##and there are about 2.5 times as many treatment obs in 0308 vs 0712

###CBPS
library(CBPS)
   ###2003-2008
   full.data.0308 <- full.data.0308 %>% mutate(cbps.treat.0708 = ifelse(treat.group.0308 == 1 & year.x >= 2007, 1, 0))
   myvars <- c("city_id", "nonstandard.coef",  "cbps.treat.0708",
                "ethnic.categorical",  "urgov", "margin", "mayor.tenure",
                 "lnAvgSalary",  "unemployrate",  "dem", "regelect_margin",  "education", "presidential")
   
   dataset.matching <- full.data.0308[myvars]
   dataset.matching.complete <- na.omit(dataset.matching)
   
   cbps.out.0308 <- CBPS(cbps.treat.0708 ~ margin + dem + regelect_margin +
                      mayor.tenure, data=dataset.matching.complete)
   summary(cbps.out.0308)
   oversight.bal.firstround <- balance(cbps.out.0308)
   
   didmodel0308 <- lm(nonstandard.coef ~ cbps.treat.0708 + ethnic.categorical + urgov + margin + mayor.tenure +
                        lnAvgSalary + unemployrate +  dem +  education + presidential, 
                      weights=cbps.out.0308$weights,
                      data = cbps.out.0308$data)
   summary(didmodel0308)
   
   library(lmtest)  #Getting clustered sEs 
   library(sandwich)
   coeftest(didmodel0308, vcov = vcovCL(didmodel0308, type="HC1", cluster = cbps.out.0308$data$city_id))
       ##vcovCL is for clustered SEs, type is correct for lm models
   
   ###2007-2012
   full.data.0712 <- full.data.0712 %>% mutate(cbps.treat.1112 = ifelse(treat.group.0712 == 1 & year.x >= 2011, 1, 0))
   myvars <- c("city_id", "nonstandard.coef",  "cbps.treat.1112",
               "ethnic.categorical",  "urgov", "margin", "mayor.tenure",
               "lnAvgSalary",  "unemployrate",  "dem", "regelect_margin",  "education", "presidential")
   
   
   dataset.matching <- full.data.0712[myvars]
   dataset.matching.complete <- na.omit(dataset.matching)
   
   cbps.out.0712 <- CBPS(cbps.treat.1112 ~ margin + dem + regelect_margin +
                      mayor.tenure, data=dataset.matching.complete)
   summary(cbps.out.0712)
   oversight.bal.secondround <- balance(cbps.out.0712)


   didmodel0712 <- lm(nonstandard.coef ~ cbps.treat.1112 + ethnic.categorical + urgov + margin + mayor.tenure +
                        lnAvgSalary + unemployrate +  dem +  education + presidential, 
                      weights=cbps.out.0712$weights,
                      data = cbps.out.0712$data)
   summary(didmodel0712)
   
   coeftest(didmodel0712, vcov = vcovCL(didmodel0712, type="HC1", cluster = cbps.out.0712$data$city_id))
   

   ###CBPS method shows positive significant results for treatment in both periods
