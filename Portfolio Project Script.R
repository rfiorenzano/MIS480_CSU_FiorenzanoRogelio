rm(list=ls())
library(tibbletime)
library(base)
library(caret)
library(crunch)
library(datasets)
library(dplyr)
library(ggplot2)
library(stats)


# Clear console screen CTRL+L #
# MIS480 Portfolio Project
# Module 8

Sys.Date()
setwd("~/CSUGLOBAL/MIS480/MOD08")
getwd()
mis480covid <- read.csv("COVID-19_Reported_Patients_Hospital_Capacity.csv")
# head(mis480covid,n=10)
# Clear console screen CTRL+L #

# COVID-19 Deaths by State
State <- mis480covid$state
ObsDate <- as.Date(mis480covid$date)
MortCov <- mis480covid$deaths_covid

# COVID-19 Hospital Staffing Shortage - Linear Regression
StaffShort <- mis480covid$critical_staffing_shortage_today_yes
HosPatTranCov <- mis480covid$hospital_onset_covid
HosTranCov <- mis480covid$hospital_onset_covid_coverage
HosBedsOcc <- mis480covid$inpatient_beds
HosBedsOccCov <- mis480covid$inpatient_beds_used_covid
HosPatSusCov <- mis480covid$total_adult_patients_hospitalized_confirmed_and_suspected_covid
HosRepSusCov <- mis480covid$total_adult_patients_hospitalized_confirmed_and_suspected_covid_coverage
HosPatCov <- mis480covid$total_adult_patients_hospitalized_confirmed_covid
HosRepCov <- mis480covid$total_adult_patients_hospitalized_confirmed_covid_coverage
HosPedPatSusCov <- mis480covid$total_pediatric_patients_hospitalized_confirmed_and_suspected_covid
HosRepPedSusCov <- mis480covid$total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_coverage
HosPedPatCov <- mis480covid$total_pediatric_patients_hospitalized_confirmed_covid
HosRepPedCov <- mis480covid$total_pediatric_patients_hospitalized_confirmed_covid_coverage


# COVID-19 Hospital Staffing Anticipating Shortage - Linear Regression
AntStaffShort <- mis480covid$critical_staffing_shortage_anticipated_within_week_yes
PDayCovAdm <- mis480covid$previous_day_admission_adult_covid_confirmed
HosRepPDayCov <- mis480covid$previous_day_admission_adult_covid_confirmed_coverage
PDaySusCovAdm <- mis480covid$previous_day_admission_adult_covid_suspected
HosRepSusPDayCov <- mis480covid$previous_day_admission_adult_covid_suspected_coverage
PDayCovPedAdm <- mis480covid$previous_day_admission_pediatric_covid_confirmed
HosRepPDayPedCov <- mis480covid$previous_day_admission_pediatric_covid_confirmed_coverage
PDaySusCovPedAdm <- mis480covid$previous_day_admission_pediatric_covid_suspected
HosRepSusPDayPedCov <- mis480covid$previous_day_admission_pediatric_covid_suspected_coverage

# COVID-19 Mortality Dataframe
dfCovMort <- data.frame(
  State,
  ObsDate,
  StaffShort,
  AntStaffShort,
  MortCov
)


# COVID-19 Staff Shortage Dataframe
dfStaffShort <- data.frame(
  State,
  ObsDate,
  StaffShort,
  HosPatTranCov,
  HosTranCov,
  HosBedsOcc,
  HosBedsOccCov,
  HosPatSusCov,
  HosRepSusCov,
  HosPatCov,
  HosRepCov,
  HosPedPatSusCov,
  HosRepPedSusCov,
  HosPedPatCov,
  HosRepPedCov
)


# COVID-19 Staff Shortage Anticipation Dataframe
dfStaffShortAnt <- data.frame(
  State,
  ObsDate,
  AntStaffShort,
  PDayCovAdm,
  HosRepPDayCov,
  PDaySusCovAdm,
  HosRepSusPDayCov,
  PDayCovPedAdm,
  HosRepPDayPedCov,
  PDaySusCovPedAdm,
  HosRepSusPDayPedCov
)

# Dataframes Filtered for State Deaths (State Abbreviation) & Dates
ST <- "NY"
Y20 <- (~ '2020')                       # R:293, E:293
Y21 <- (~ '2021')                       # R:181, E:181
YPV <- ('2020-12-01' ~ '2021-12-31')    # R:212, E:212
FDATE <- YPV

# Filtered Mortality Dataframes
STDeaths <- dfCovMort[dfCovMort$State==ST,]
STStaffShort <- dfStaffShort[dfStaffShort$State==ST,]
STStaffShortAnt <- dfStaffShortAnt[dfStaffShortAnt$State==ST,]


# Tibbled Dataframes
DeathsTbl <- as_tbl_time(STDeaths, index = ObsDate)
StaffShortTbl <- as_tbl_time(STStaffShort, index = ObsDate)
StaffShortAntTbl <- as_tbl_time(STStaffShortAnt, index = ObsDate)


# Final Dataframes Configuration
Deaths <- filter_time(DeathsTbl, FDATE)
xm <- Deaths$ObsDate
ym <- Deaths$MortCov
sm <- Deaths$StaffShort
am <- Deaths$AntStaffShort

Shorts <- filter_time(StaffShortTbl, FDATE)
xs <- Shorts$ObsDate
ys <- Shorts$StaffShort

Antici <- filter_time(StaffShortAntTbl, FDATE)
xa <- Antici$ObsDate
ya <- Antici$AntStaffShort

# Linear Regression Models
deathslm <- lm(ym~xm,Deaths)
shortslm <- lm(ys~xs,Shorts)
anticilm <- lm(ya~xa,Antici)


# Multiple Linear Regression Models
DeathsModel <- lm(ym ~ sm + am, data = Deaths)

ShortsModel <- lm(ys ~ Shorts$HosPatTranCov + 
                    Shorts$HosTranCov + 
                    Shorts$HosBedsOcc + 
                    Shorts$HosBedsOccCov + 
                    Shorts$HosPatSusCov + 
                    Shorts$HosRepSusCov + 
                    Shorts$HosPatCov + 
                    Shorts$HosRepCov + 
                    Shorts$HosPedPatSusCov + 
                    Shorts$HosRepPedSusCov + 
                    Shorts$HosPedPatCov + 
                    Shorts$HosRepPedCov,
                  data = Shorts)

AnticiModel <- lm(ya ~ Antici$PDayCovAdm + 
                    Antici$HosRepPDayCov + 
                    Antici$PDaySusCovAdm + 
                    Antici$HosRepSusPDayCov + 
                    Antici$PDayCovPedAdm + 
                    Antici$HosRepPDayPedCov + 
                    Antici$PDaySusCovPedAdm + 
                    Antici$HosRepSusPDayPedCov,
                  data = Antici)
# model review
summary(DeathsModel)
summary(ShortsModel)
summary(AnticiModel)
#plot(DeathsModel)





# Plot Configuration
title_deaths = "COVID-19 Deaths Report"
title_shorts = "Hospitals Reporting Critical Staff Shortage"
title_antici = "Hospitals Anticipating Critical Staff Shortage"
titlesub = paste("Results for the state of", ST, sep = " ", collapse = NULL)


plot(xm,ym,type = "l", col="blue", 
     xlab="", ylab="Deaths",
     main = title_deaths,
     sub = titlesub)
abline(deathslm, col="red",lwd=3)

plot(xs,ys,type = "l", col="blue", 
     xlab="", ylab="Short Staffed Hospitals",
     main = title_shorts,
     sub = titlesub)
abline(shortslm, col="red",lwd=3)

plot(xa,ya,type = "l", col="blue", 
     xlab="", ylab="Hospitals",
     main = title_antici,
     sub = titlesub)
abline(anticilm, col="red",lwd=3)

