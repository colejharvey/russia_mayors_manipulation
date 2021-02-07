#Overall turnout coefficients
#No effects found for any of the below models


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
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = unity/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2003
#match.set$city_id <- factor(match.set$city_id)
#coefs.tg.pretreat <- left_join(coefs.tg.pretreat, match.set, by = "city_id")

write.csv(coefs.tg.pretreat, "coefs treatment group 2003 pre-treatment w city_id overall.csv")



###2004

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(putin.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 0)

coefs.tg.pretreat$year <- 2004

write.csv(coefs.tg.pretreat, "coefs treatment group 2004 pre-treatment w city_id overall.csv")


###2007

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid))/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1)

coefs.tg.pretreat$year <- 2007

write.csv(coefs.tg.pretreat, "coefs treatment group 2007 w city_id overall.csv")


###2008

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = Medvedev/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1)

coefs.tg.pretreat$year <- 2008

write.csv(coefs.tg.pretreat, "coefs treatment group 2008 w city_id overall.csv")



###2011

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1)

coefs.tg.pretreat$year <- 2011

write.csv(coefs.tg.pretreat, "coefs treatment group 2011 w city_id overall.csv")





####Treatment group 2012
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets.xlsx")

treatment.ids <- c(27, 36, 2, 6, 3, 16, 81, 21, 77, 67, 76, 80, 50, 15, 52, 55, 18, 19, 23, 33, 25,
                   28, 29, 60, 81, 80, 36, 87, 84, 74, 67, 19, 40, 81, 42, 20, 69, 45, 63, 
                   76, 67, 73, 64, 71, 75, 74, 72, 16, 45, 8)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)

###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(putin.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.tg.postreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 1, post.treatment = 1)

coefs.tg.postreat$year <- 2012

write.csv(coefs.tg.postreat, "coefs treatment group 2012 post-treatment w city_id overall.csv")



###
###Control group
###


###2003
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = unity/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2003

write.csv(coefs.cg.pretreat, "coefs control group 2003 pre-treatment w city_id overall.csv")



###2004


ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)



###Generating nonstandard vote coefficients

model.coefs <- lmer(putin.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 0)

coefs.cg.pretreat$year <- 2004

write.csv(coefs.cg.pretreat, "coefs control group 2004 pre-treatment w city_id overall.csv")



###2007

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0)

coefs.cg.pretreat$year <- 2007

write.csv(coefs.cg.pretreat, "coefs control group 2007 w city_id overall.csv")


###2008
ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")
match.set[60,3] <- "roslav"  #Necessary change to match this city

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = Medvedev/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0)

coefs.cg.pretreat$year <- 2008

write.csv(coefs.cg.pretreat, "coefs control group 2008 w city_id overall.csv")



###2011

ids <- read.csv("russia region ids.csv")
match.set <- read_excel("match-targets-control.xlsx")

treatment.ids <- c(46, 47, 4, 5, 6, 49, 81, 50, 16, 14, 53, 22, 79, 61, 26, 27,
                   29, 31, 32, 33, 35, 36, 37, 38, 39, 40, 43, 44, 62, 57, 66,
                   64, 69, 67, 71, 73, 77, 8, 9, 10, 11, 87, 88)
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(ur.abshare = united.russia/voter.list)



###Generating nonstandard vote coefficients

library(lme4)
model.coefs <- lmer(ur.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.pretreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0)

coefs.cg.pretreat$year <- 2011

write.csv(coefs.cg.pretreat, "coefs control group 2011 w city_id overall.csv")


###
###2012 election for control group
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


data.container <- data.container %>% mutate(pct.turnout = (valid + invalid)/voter.list)
data.container <- data.container %>% mutate(putin.abshare = putin/voter.list)

###Generating nonstandard vote coefficients

model.coefs <- lmer(putin.abshare~pct.turnout  + (1 + pct.turnout | city_id),
                    data=data.container, REML=FALSE)
coefs.test <- coef(model.coefs)
coefs.cg.postreat <- as_tibble(coefs.test$city_id, rownames = 'city_id') %>%
  mutate(treatment.group = 0, post.treatment = 1)

coefs.cg.postreat$year <- 2012



write.csv(coefs.cg.postreat, "coefs control group 2012 post-treatment w city_id overall.csv")


###Quick test models
coefs.all <- read_xlsx("turnout coefs combined.xlsx") #This version has the covariates

didmodel <- lm(pct.turnout ~ treatment.group + post.treatment +
                 treatment.group*post.treatment, data=coefs.all)
summary(didmodel)

m.2way.fe <- lm(pct.turnout ~ factor(city_id) + factor(year) + treatment.group, data=coefs.all)
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
coefs.all <- read_xlsx("turnout coefs combined.xlsx") #This version has the covariates

reuter.data <- read.csv("reuter_et_al_data_v2.csv")
coefs.all <- coefs.all %>% select(-post.treatment)

full.data <- coefs.all %>% merge(reuter.data, by="city_id_year")

###Adding regionid.ch
regionids <- read_xlsx("match-targets-combined.xlsx")
regionids <- regionids %>% select(regionid.ch, city_id)
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

regional.data <- regional.data %>% select(urgov, comp.scale2, comp.scale03_18, regionid, year, ethnic.categorical)
regional.data <- regional.data %>% rename(year.x = year, regionid.ch = regionid)

full.data <- full.data %>% merge(regional.data, by = c("regionid.ch", "year.x"))

###

write.csv(full.data, "overall coefs with city covariates.csv")
###Quick test model of full data, using only 2004 and 2012
full.data <- read.csv("overall coefs with city covariates.csv")



full.data <- full.data %>% filter(cancel.year > 2004 | is.na(cancel.year) == TRUE) #Comment this out to include obs treated in 2004

full.data.sub <- subset(full.data, full.data$year.x == 2004 | full.data$year.x == 2012)
full.data.sub <- full.data.sub %>% mutate(post.treatment = ifelse(year.x == 2012, 1, 0))
didmodel <- lm(pct.turnout ~ treatment.group + post.treatment +
                 treatment.group*post.treatment + 
                 UR_majority  + lnAvgSalary + unemployrate +  dem + pctRussian2002_new + education, data=full.data.sub)
summary(didmodel)  

m.2way.fe <- lm(pct.turnout ~ factor(city_id) + post.treatment + treatment.group:post.treatment, data=full.data.sub)
summary(m.2way.fe)

###Year-by-year models

###2007 models
full.data.sub07 <- subset(full.data, full.data$year.x <= 2007)
full.data.sub07 <- full.data.sub07 %>% mutate(post.treatment = ifelse(year.x == 2007, 1, 0))


didmodel07 <- lm(pct.turnout ~ treat.group.07 + post.treatment +
                   treat.group.07*post.treatment + comp.scale2 + ethnic.categorical + urgov +
                   lnAvgSalary + unemployrate + dem + pctRussian2002_new + education + presidential, data=full.data.sub07)
summary(didmodel07) #No effect


###2008 model

full.data.sub08 <- subset(full.data, full.data$year.x <= 2008)
full.data.sub08 <- full.data.sub08 %>% mutate(post.treatment = ifelse(year.x == 2008, 1, 0))

didmodel08 <- lm(pct.turnout ~ treat.group.08 + post.treatment +
                   treat.group.08*post.treatment + comp.scale2 + ethnic.categorical + urgov +
                   lnAvgSalary + unemployrate +  dem + pctRussian2002_new + education + presidential, data=full.data.sub08)
summary(didmodel08) #No effect


###2011 model

full.data.sub11 <- subset(full.data, full.data$year.x <= 2011)
full.data.sub11 <- subset(full.data.sub11, full.data.sub11$treat.group.07 == 0)
full.data.sub11 <- subset(full.data.sub11, full.data.sub11$treat.group.08 == 0)

full.data.sub11 <- full.data.sub11 %>% mutate(post.treatment = ifelse(year.x == 2011, 1, 0))

didmodel11 <- lm(pct.turnout ~ treat.group.11 + post.treatment +
                   treat.group.11*post.treatment + comp.scale2 + ethnic.categorical + urgov + UR_majority +
                   lnAvgSalary + unemployrate +   education + presidential, data=full.data.sub11)
summary(didmodel11) #No effect


###2012 model

full.data.sub12 <- subset(full.data, full.data$year.x <= 2012)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.07 == 0)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.08 == 0)
full.data.sub12 <- subset(full.data.sub12, full.data.sub12$treat.group.11 == 0)


full.data.sub12 <- full.data.sub12 %>% mutate(post.treatment = ifelse(year.x == 2012, 1, 0))

didmodel12 <- lm(pct.turnout ~ treat.group.12 + post.treatment +
                   treat.group.12*post.treatment + comp.scale2 + ethnic.categorical + urgov +
                   lnAvgSalary + unemployrate +  dem + pctRussian2002_new + education + presidential, data=full.data.sub12)
summary(didmodel12) #

###Next step could be 03/04 vs. 07/08
###Then 07/08 vs. 11/12

full.data.0308 <- subset(full.data, full.data$year.x <= 2008)
full.data.0308 <- full.data.0308 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2004)
full.data.0308 <- full.data.0308 %>% mutate(treat.group.0308 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year < 2008, 1, 0)))
full.data.0308 <- full.data.0308 %>% mutate(post.treat.0308 = ifelse(year.x == 2007 | year.x == 2008, 1, 0))


didmodel0308 <- lm(pct.turnout ~ treat.group.0308 + post.treat.0308 +
                     treat.group.0308*post.treat.0308 + comp.scale2 + ethnic.categorical + urgov + margin +
                     lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0308)
summary(didmodel0308) 

qplot(jitter(full.data.0308$year.x), full.data.0308$pct.turnout, aes(color = factor(full.data.0308$treat.group.0308)))

###07/08 vs 11/12

full.data.0712 <- subset(full.data, full.data$year.x >= 2007)
full.data.0712 <- full.data.0712 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2008)
full.data.0712 <- full.data.0712 %>% mutate(treat.group.0712 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year < 2010, 1, 0)))
full.data.0712 <- full.data.0712 %>% mutate(post.treat.0712 = ifelse(year.x == 2011 | year.x == 2012, 1, 0))


didmodel0712 <- lm(pct.turnout ~ treat.group.0712 + post.treat.0712 +
                     treat.group.0712*post.treat.0712 + comp.scale2 + ethnic.categorical + urgov  + UR_majority + margin  +
                     lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0712)
summary(didmodel0712) #No effect


###03/04 vs. 11/12

full.data.0312 <- subset(full.data, full.data$year.x <= 2004 | full.data$year.x > 2008)
full.data.0312 <- full.data.0312 %>% filter(is.na(cancel.year) == TRUE | cancel.year > 2008)
full.data.0312 <- full.data.0312 %>% mutate(treat.group.0312 = ifelse(is.na(cancel.year == TRUE), 0, 
                                                                      ifelse(cancel.year > 2008 & cancel.year < 2010, 1, 0)))
full.data.0312 <- full.data.0312 %>% mutate(post.treat.0312 = ifelse(year.x == 2011 | year.x == 2012, 1, 0))


didmodel0312 <- lm(pct.turnout ~ treat.group.0312 + post.treat.0312 +
                     treat.group.0312*post.treat.0312 + comp.scale2 + ethnic.categorical + urgov +
                     lnAvgSalary + unemployrate +  dem +  education + presidential, data=full.data.0312)
summary(didmodel0312) #No effect



