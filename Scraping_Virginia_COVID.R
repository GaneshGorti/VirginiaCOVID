#1st June 2020 
#Phased Reopening and COVID-19 Incidence: Evidence from Virginia

#Downloading data 

setwd("D:\\Work\\CU Boulder\\Summer 20\\Virginia_COVID19\\")
out_file <- "Processed_data\\"
pop_data <- "Data\\Census_data\\VAPopulationEstimates_2019-07_UVACooperCenter.xls"

library(dplyr)
library(RCurl)
library(foreign)
library(lubridate)
library(readxl)

URL <- "https://www.vdh.virginia.gov/content/uploads/sites/182/2020/05/VDH-COVID-19-PublicUseDataset-Cases.csv"
myfile <- readLines(URL)
head(myfile)
casedata <- read.csv(URL, header = TRUE, sep = ",", quote = "\"'")

sysdate <- format(Sys.Date(),"%m%d%y")
download.file("https://www.vdh.virginia.gov/content/uploads/sites/182/2020/05/VDH-COVID-19-PublicUseDataset-Cases.csv",destfile=paste("Data\\COVID_data_raw\\",sysdate,".csv"),method="libcurl")

casedata <- as.data.frame(casedata)

casedata$FIPS <- as.numeric(as.character(casedata$FIPS))
casedata <- casedata %>%
  mutate(Report.Date = mdy(Report.Date)) #converting factor to mdy type
casedata <- casedata %>% mutate_if(is.factor, as.character) 

sumtotalcases <- casedata %>%  
  group_by(Report.Date) %>% 
  summarise(Frequency = sum(Total.Cases))
tail(sumtotalcheck,10) #totalcases as of 1st June 2020 are 45398. This matches with the total number of cases from Johns Hopkins. Hence, the cases are not daily new cases but total cases.
sumtotaldeaths <- casedata %>%  
  group_by(Report.Date) %>% 
  summarise(Frequency = sum(Deaths))
tail(sumtotaldeaths,10) #checking for total deaths too. 1392 total deaths as of 1st June. Johns Hopkins reporting similar total deaths.

casedata <- casedata[order(as.Date(casedata$Report.Date, format="%Y/%m/%d")),] #sorting by date
casedata <- casedata[order(casedata$FIPS),] #sorting by FIPS

casedata <- casedata %>%
  group_by(Locality) %>%
  mutate(New.Cases = Total.Cases - lag(Total.Cases)) #lagging total cases for change in cases 

casedata <- casedata %>%
  group_by(Locality) %>%
  mutate(New.Deaths = Deaths - lag(Deaths)) #lagging deaths for change in deaths 

casedata <- casedata %>%
  group_by(Locality) %>%
  mutate(New.Hospitalizations = Hospitalizations - lag(Hospitalizations)) #lagging hospitalization for change in hospitalization

pop_data <- read_excel(paste(pop_data)) #population data from: https://demographics.coopercenter.org/index.php/virginia-population-estimates
names(pop_data)[1] <- "FIPS"
names(pop_data)[4] <- "popestimate2019"
pop_data$FIPS <- as.numeric(pop_data$FIPS)
pop_data <- pop_data[,c("FIPS","popestimate2019")]

pop_data$FIPS <- pop_data$FIPS + 51000 #converting FIPS to match the COVID dataset FIPS

casedata.merged <- merge(pop_data, casedata,  by=c("FIPS"), all=FALSE) #merging population data with casedata

casedata.merged <- casedata.merged %>%
  mutate(Adj.Total.Cases = (Total.Cases/popestimate2019)*100000) #adjusting total cases to per 100000

casedata.merged <- casedata.merged %>%
  mutate(Adj.Hospitalizations = (Hospitalizations/popestimate2019)*100000)

casedata.merged <- casedata.merged %>%
  mutate(Adj.Deaths = (Deaths/popestimate2019)*100000)

casedata.merged <- casedata.merged %>%
  mutate(Adj.New.Cases = (New.Cases/popestimate2019)*100000)

casedata.merged <- casedata.merged %>%
  mutate(Adj.New.Hospitalizations = (New.Hospitalizations/popestimate2019)*100000)

casedata.merged <- casedata.merged %>%
  mutate(Adj.New.Deaths = (New.Deaths/popestimate2019)*100000)

systime <- gsub(x=Sys.time(),pattern = ":", replacement="-")
save(casedata.merged, file=paste(out_file, "processed_virginia_covid_", as.character(systime), ".RData"))