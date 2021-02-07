###Combining reuter et al data with election manipulation coefficients and other election vars
library(tidyverse)
library(foreign)
library(sjPlot)

###Reading in Reuter et al data and full.data
#reuter.data <- read.dta("reuter_et_al_data.dta")
reuter.data <- read.csv("reuter_et_al_data_v2.csv")

reuter.data <- reuter.data %>% arrange(city_id, year)
full.data <- read.csv("cleaned data full all years.csv")


###Setting up lagged presidential election variables
reuter.data <- reuter.data %>% mutate(lagged.urvoteshare.pres = NA)
reuter.data <- reuter.data %>% mutate(lagged.turnout.coef.pres = NA)
reuter.data <- reuter.data %>% mutate(lagged.nonstandard.coef.pres = NA)



for(i in 1:nrow(reuter.data)){
  city.temp <- reuter.data[i,] %>% dplyr::select(city_id) %>% as.numeric()
  year.temp <- reuter.data[i,] %>% dplyr::select(year) %>% as.numeric()
  full.sub <- full.data %>% filter(city_id == city.temp)
  
  if(year.temp <= 2003){
    reuter.data[i,50] <- NA
    reuter.data[i,51] <- NA
    reuter.data[i,52] <- NA
  }
  
  if(year.temp >= 2004 & year.temp <= 2007) {
    reuter.data[i,50] <-  full.sub %>% filter(year.x == 2004) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,51] <- full.sub %>% filter(year.x == 2004) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,52] <- full.sub %>% filter(year.x == 2004) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
  }
  
  if(year.temp >= 2008 & year.temp <= 2011) {
    reuter.data[i,50] <-  full.sub %>% filter(year.x == 2008) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,51] <- full.sub %>% filter(year.x == 2008) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,52] <- full.sub %>% filter(year.x == 2008) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
    
  }
  
  if (year.temp == 2012){
    reuter.data[i,50] <-  full.sub %>% filter(year.x == 2012) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,51] <- full.sub %>% filter(year.x == 2012) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,52] <- full.sub %>% filter(year.x == 2012) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
    
  }
}
 

###Setting up lagged legislative election variables
reuter.data <- reuter.data %>% mutate(lagged.urvoteshare.parl = NA)
reuter.data <- reuter.data %>% mutate(lagged.turnout.coef.parl = NA)
reuter.data <- reuter.data %>% mutate(lagged.nonstandard.coef.parl = NA)



for(i in 1:nrow(reuter.data)){
  city.temp <- reuter.data[i,] %>% dplyr::select(city_id) %>% as.numeric()
  year.temp <- reuter.data[i,] %>% dplyr::select(year) %>% as.numeric()
  full.sub <- full.data %>% filter(city_id == city.temp)
  
  if(year.temp <= 2003){
    reuter.data[i,53] <- NA
    reuter.data[i,54] <- NA
    reuter.data[i,55] <- NA
  }
  
  if(year.temp >= 2004 & year.temp <= 2007) {
    reuter.data[i,53] <-  full.sub %>% filter(year.x == 2003) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,54] <- full.sub %>% filter(year.x == 2003) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,55] <- full.sub %>% filter(year.x == 2003) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
  }
  
  if(year.temp >= 2008 & year.temp <= 2011) {
    reuter.data[i,53] <-  full.sub %>% filter(year.x == 2007) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,54] <- full.sub %>% filter(year.x == 2007) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,55] <- full.sub %>% filter(year.x == 2007) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
    
  }
  
  if (year.temp == 2012){
    reuter.data[i,53] <-  full.sub %>% filter(year.x == 2011) %>% dplyr::select(ur.voteshare) %>% as.numeric()  #Voteshare
    reuter.data[i,54] <- full.sub %>% filter(year.x == 2011) %>% dplyr::select(turnout.coef) %>% as.numeric()  #Turnout coef
    reuter.data[i,55] <- full.sub %>% filter(year.x == 2011) %>% dplyr::select(nonstandard.coef) %>% as.numeric()  #Nonstandard coef
    
  }
}


write.csv(reuter.data, "reuter_et_al_data_v3.csv")


###Quick models

reuter.data <- read.csv("reuter_et_al_data_v3.csv")
reuter.data <- reuter.data %>% mutate(treated0708 = ifelse(is.na(cancel.year)==T, 0, ifelse(cancel.year <= 2007, 1, 0)))
reuter.data <- reuter.data %>% mutate(divided.govt = opp_mayor1 * UR_majority)
reuter.data <- reuter.data %>% mutate(years.to.election = ifelse(year == 2003 | year == 2007 | year == 2011, 0, 
                                                                 ifelse(year == 2000 | year == 2004 | year == 2008 | year == 2012, 3,
                                                                        ifelse(year == 2001 | year == 2005 | year == 2009, 2,
                                                                               ifelse(year == 2002 | year == 2006 | year == 2010, 1, NA)))))
reuter.data <- reuter.data %>% mutate(mayor.ur = ifelse(is.na(opp_mayor1) == T | opp_mayor1 == 1, 0, 1))
reuter.data <- reuter.data %>% mutate(mayor.opp = ifelse(is.na(opp_mayor1) == T | opp_mayor1 == 0, 0, 1))
reuter.data <- reuter.data %>% mutate(mayor.ind = ifelse(is.na(opp_mayor1) == T, 1, 0))


model1 <- glm(cancel ~ margin + naive_counter + naive_counter2 + naive_counter3  + opp_mayor1*margin*lagged.turnout.coef.pres +
                twoyrdum2 +
              twoyrdum3 +
              twoyrdum4 +
              twoyrdum5 +
              twoyrdum6 +
              twoyrdum7,
              data = reuter.data)
summary(model1)

plot_model(model1, type = "pred", terms = c("lagged.turnout.coef.pres", "opp_mayor1", "margin [.0002, .385, .95]"))


model1.2 <- glm(cancel ~margin + naive_counter + naive_counter2 + naive_counter3  + opp_mayor1*margin*lagged.urvoteshare.pres +
                  twoyrdum2 +
                  twoyrdum3 +
                  twoyrdum4 +
                  twoyrdum5 +
                  twoyrdum6 +
                  twoyrdum7,
                data = reuter.data)
summary(model1.2)
plot_model(model1.2, type = "pred", terms = c("lagged.urvoteshare.pres", "opp_mayor1", "margin [.0002, .385, .95]"))




model2.2 <- glm(cancel ~ margin + + UR_majority + poppopprop + lnAvgSalary + civsoc91 + opp_mayor1*margin*lagged.turnout.coef.pres +
                  naive_counter + naive_counter2 + naive_counter3 + 
                  twoyrdum2 +
                  twoyrdum3 +
                  twoyrdum4 +
                  twoyrdum5 +
                  twoyrdum6 +
                  twoyrdum7,
                data = reuter.data)
summary(model2.2)

plot_model(model2.2, type = "pred", terms = c("lagged.turnout.coef.pres", "opp_mayor1", "margin [.0002, .385, .95]"))


model.3 <- glm(cancel ~ margin + regelect_margin + UR_majority + poppopprop + lnAvgSalary + civsoc91 +
                 opp_mayor1*margin*lagged.turnout.coef.pres +
                 naive_counter + naive_counter2 + naive_counter3 + 
                 twoyrdum2 +
                 twoyrdum3 +
                 twoyrdum4 +
                 twoyrdum5 +
                 twoyrdum6 +
                 twoyrdum7,
               data = reuter.data)
summary(model.3)
plot_model(model.3, type = "pred", terms = c("lagged.turnout.coef.pres", "opp_mayor1", "margin [.0002, .385, .95]"))


model.4 <- glm(cancel ~ margin + regelect_margin + regmarginXmargin +
                 opp_mayor1*margin*lagged.turnout.coef.pres +
                 naive_counter + naive_counter2 + naive_counter3 + 
                 twoyrdum2 +
                 twoyrdum3 +
                 twoyrdum4 +
                 twoyrdum5 +
                 twoyrdum6 +
                 twoyrdum7,
               data = reuter.data)
summary(model.4)
plot_model(model.4, type = "pred", terms = c("lagged.turnout.coef.pres", "opp_mayor1", "margin [.0002, .385, .95]"))


model.5 <- glm(cancel ~ margin + regelect_margin + regmarginXmargin + UR_majority + poppopprop + lnAvgSalary + civsoc91 +
                 opp_mayor1*margin*lagged.turnout.coef.pres +
                 naive_counter + naive_counter2 + naive_counter3 + 
                 twoyrdum2 +
                 twoyrdum3 +
                 twoyrdum4 +
                 twoyrdum5 +
                 twoyrdum6 +
                 twoyrdum7,
               data = reuter.data)
summary(model.5)
plot_model(model.5, type = "pred", terms = c("lagged.turnout.coef.pres", "opp_mayor1", "margin [.0002, .385, .95]"))


###Using mfx
library(mfx)

model.0 <- logitmfx(formula = cancel ~ margin, data = reuter.data %>% filter(year >= 2003), atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model.0$fit)
length(unique(model.0$fit$data$city_id))

model.1 <- logitmfx(formula = cancel ~ margin  + naive_counter + naive_counter2 + naive_counter3  + opp_mayor1*margin*lagged.turnout.coef.pres +
                         twoyrdum2 +
                         twoyrdum3 +
                         twoyrdum4 +
                         twoyrdum5 +
                         twoyrdum6 +
                         twoyrdum7, data = reuter.data, atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model.1$fit)

plot_model(model.1$fit, type = "pred", terms = c("lagged.turnout.coef.pres [all]", "opp_mayor1", "margin [.16, .385, .61]"))


model2.2 <- logitmfx(data = reuter.data, formula = cancel ~ margin  + UR_majority + poppopprop + lnAvgSalary + civsoc91 + mayor.ind*margin*lagged.turnout.coef.pres + mayor.ur*margin*lagged.turnout.coef.pres +
                  naive_counter + naive_counter2 + naive_counter3 + 
                  
                  twoyrdum3 +
                  twoyrdum4 +
                  twoyrdum5 +
                  twoyrdum6 
                  , atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model2.2$fit)

plot_model(model2.2$fit, type = "pred", terms = c("lagged.turnout.coef.pres [all]", "mayor.ur", "margin [.16, .385, .61]"))


model.3 <- logitmfx(data = reuter.data, formula = cancel ~ margin + regelect_margin + UR_majority + poppopprop + lnAvgSalary + civsoc91 +
                 opp_mayor1*margin*lagged.turnout.coef.pres +
                 naive_counter + naive_counter2 + naive_counter3 + 
                 twoyrdum2 +
                 twoyrdum3 +
                 twoyrdum4 +
                 twoyrdum5 +
                 twoyrdum6 +
                 twoyrdum7,
               atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model.3)
plot_model(model.3$fit, type = "pred", terms = c("lagged.turnout.coef.pres [all]", "opp_mayor1", "margin [.16, .385, .61]"))

model.4 <- logitmfx(data = reuter.data, formula =cancel ~ margin + regelect_margin + regmarginXmargin +
                 opp_mayor1*margin*lagged.turnout.coef.pres +
                 naive_counter + naive_counter2 + naive_counter3 + 
                 twoyrdum2 +
                 twoyrdum3 +
                 twoyrdum4 +
                 twoyrdum5 +
                 twoyrdum6 +
                 twoyrdum7,
                 atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model.4)
plot_model(model.4$fit, type = "pred", terms = c("lagged.turnout.coef.pres [all]", "opp_mayor1", "margin [.16, .385, .61]"))


model.5 <- logitmfx(data = reuter.data, formula =cancel ~ margin + regelect_margin + regmarginXmargin +
                      UR_majority + poppopprop + lnAvgSalary + civsoc91 +
                      opp_mayor1*margin*lagged.turnout.coef.pres +
                      naive_counter + naive_counter2 + naive_counter3 + 
                      twoyrdum2 +
                      twoyrdum3 +
                      twoyrdum4 +
                      twoyrdum5 +
                      twoyrdum6 +
                      twoyrdum7,
                    atmean = TRUE, robust = F, clustervar1 = "city_id")
summary(model.5$fit)
plot_model(model.5$fit, type = "pred", terms = c("lagged.turnout.coef.pres [all]", "opp_mayor1", "margin [.16, .385, .61]"))

