# Creates dataset "COVID-19 i verdens land"
# Outputs "covidverden.csv"

# Preamble
rm(list=ls())
setwd("/Users/oysteinsolvang/Dropbox/Akademia/02-Undervisning/COVID-datasett/03-Landdata")


# Libraries
library(tidyverse)
library(stargazer)
library(wbstats)
library(utils) # for ECDC data
library(readxl) # excel file import


## ISO-3166 country code list
isolist <- read.csv2("iso-3166.csv")
isolist <- isolist[,c(1,3,4,5)]
colnames(isolist) <- c("land","kortkode","kode","tallkode")


## European Center for Disease Control and Prevention
#ecdcdata <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
d <- ecdcdata
# Select columns of interest, renaming
d <- d[,c(1,5:6,9)]
colnames(d) <- c("dato","tilfeller","dode","kode")
# Coercing dato as date type
d$dato <- as.Date(d$dato, "%d/%m/%y")
# Ordering data chronologically
d <- d[order(d$kode, as.Date(d$dato, format="%Y/%m/%d")),]
# Making variables 'fatale' and 'tilfeller' cross-sectional
d <- as.data.frame(d %>% group_by(kode) %>% mutate(sum_cases = cumsum(tilfeller)))
d$tilfeller <- d$sum_cases
d <- as.data.frame(d %>% group_by(kode) %>% mutate(sum_deaths = cumsum(dode)))
d$dode <- d$sum_deaths
d <- d[,-c(5:6)]
# Select date of interest
d <- d %>% filter(dato=="2020-07-15")
# Remove variable 'dato'
d <- d[,c(2:4)]
# Rename data set
data <- d


## Verdensbanken
# wbdata <- wb(indicator = c("SP.POP.TOTL","SP.URB.TOTL.IN.ZS","SP.POP.65UP.TO.ZS","NY.GDP.PCAP.CD","AG.LND.TOTL.K2","ST.INT.ARVL"),startdate=2018,enddate=2018, return_wide=T)
# Select variables of interest
d <- wbdata
d <- d[,c(1,4:10)]
# Set variable names
colnames(d) <- c("kode","navn","areal","BNPpc","over64","folketall","urban","turisme")
# Merge
data <- merge(data,d,by="kode")

# Variable: temperatur
rawdata <- read_excel("climate_change_download_0.xls")
d <- as.data.frame(rawdata)
# Rename variables
colnames(d) <- c("kode","navn","serie","serie-navn","?","??",1990:2010,"temp")
# Select relevant variable
d <- d[d$serie=="EN.CLC.MMDT.C",]
d <- d[,c(1,28)]
# Replace miscellanous values for missing data with 'NA'
d$temp <- ifelse(d$temp=="..",NA,d$temp)
d$temp <- ifelse(d$temp=="n/a",NA,d$temp)
# Split max and min values to separate variables
d <- d %>% separate(temp, c("low", "high"),sep="/")
# Calculate averages
d$high <- as.numeric(d$high)
d$low <- as.numeric(d$low)
d$temperatur <- d$high + d$low
d$temperatur <- d$temperatur / 2
# Remove excess variables
d <- d[,c(1,4)]
# merge
data <- left_join(data,d,by="kode")


## Our world in data
# Variable: respons
d <- read.csv("covid-stringency-index.csv")
# Selects only observation on Jun 2
d <- d %>% filter(Date=="Jun 2, 2020")
# Select only relevant variables, set variable names
d <- d[,c(2,4)]
colnames(d) <- c("kode","respons")
# Merge
data <- left_join(data,d,by="kode")


## Freedom house
# Variable: FHIFIW
d <- read.csv("fhi.csv")
# Inlcudes only countries
d <- d %>% filter(d$"C.T"=="c")
# Includes only last observation
d <- d %>% filter(d$Edition==2020)
# Includes only variables of interest, renames
d <- as.data.frame(d)
d <- d[,45:46]
colnames(d) <- c("FHIFIW","kode")
# Merge
data <- left_join(data,d,by="kode")


## EU
# Variable: eumedlem
# Import self-made csv-file
d <- read.csv("eumedlem.csv")
# Omit country names
d <- d[,2:3]
# Merge
data <- left_join(data,d,by="kode")
# Coerce NA's to 0
data$eumedlem[is.na(data$eumedlem)] <- 0


## OECD
# Variable: oecdmedlem
# Import self-made csv-file
d <- read.csv("oecdmedlem.csv")
# Omit country names
d <- d[,2:3]
# Merge
data <- left_join(data,d,by="kode")
# Coerce NA's to 0
data$oecdmedlem[is.na(data$oecdmedlem)] <- 0


## FN
# Variable: immigranter
rawdata <- read.csv2("migrants.csv",skip=14)
# Remove unused variables
d <- rawdata[,c(2,4,12)]
# Remove excess rows
d <- d[-c(1:24,45,55,61,79,80,88,107:108,114,124:125,133,145:146,173,182,197:198,201,207,215,225:227,238,252,269,279),]
# Remove incorrect rownumbers
rownames(d) <- NULL
# Give colnames
colnames(d) <- c("land","tallkode","immigranter")
# Coerce immigrants to numeric
d$immigranter <- as.numeric(sub(",", ".", d$immigranter, fixed = TRUE))
# Merge with ISO-3166-1 list
d <- left_join(d,isolist,by="tallkode")
d <- d[,c(3,6)]
# Merge with data
data <- left_join(data,d,by="kode")


## Esping-Andersen
# Variable: velferdsstatstypologi
velferdtypologi <- read.csv("welfarestatetypology.csv")
# Rename variables
colnames(velferdtypologi) <- c("kode","velferdsstatstypologi")
# Merge
data <- left_join(data,velferdtypologi,by="kode")


## Scruggs et al
# Variable: sykepenger
d <- read.csv("cwed.csv")
# Keep relevant variables, rename
d <- d[,c(2,6)]
colnames(d) <- c("kode","sykepenger")
# Merge
data <- left_join(data,d,by="kode")


## World Value Survey

wvs_rawdata <- readRDS("WVS_Cross-National_Wave_7_R_v1_1.rds")
# rename
d <- wvs_rawdata
# Select variables of interest
d <- d[,c(4,8,87,101)]
# Coerce to data frame
d <- as.data.frame(d)
# Rename variables
colnames(d) <- c("kode","intervju","t1","t2")
# NA omit
d <- na.omit(d)
# Calculate averages
d <- as.data.frame(d %>% group_by(kode) %>% mutate(tg = mean(t1)))
d <- as.data.frame(d %>% group_by(kode) %>% mutate(tp = mean(t2)))
# Delete all but one observation per country
d <- d %>% group_by(kode) %>% slice(1)
# Select variables of interest, rename
d <- d[,c(1,5:6)]
colnames(d) <- c("kode","tillitgenerelt","tillitmyndigheter")
# Merge
data <- left_join(data,d,by="kode")


## CIA
# Varialbe: kvinneligleder
rawdata <- read.csv2("statsledere.csv")
# Select variables of interest
d <- rawdata[,2:3]
# Rename variable
colnames(d) <- c("kode","kvinneligleder")
# Recode missing values
d$kvinneligleder <- ifelse(d$kvinneligleder=="na",NA,d$kvinneligleder)
# Coerce kvinneleder to integer
d$kvinneligleder <- as.integer(d$kvinneligleder)
# Merge
data <- left_join(data,d,by="kode")


## Klein et al
# Varialbe: dri
d <- read.csv("resistanceindex.csv")
# Rename variables
colnames(d) <- c("kode","DRI")
# Merge
data <- left_join(data,d,by="kode")


## WHO
# Variable: DDD1000
d <- read.csv("antibiotics_data.csv")
# Select variables of interest
d <- d[,c(2,4)]
# Rename variables
colnames(d) <- c("kode","DDD1000")
# Merge
data <- left_join(data,d,by="kode")


## ECDC og Verdensbanken
# Variable 'tilfeller1000'
data$tilfeller1000 <- data$tilfeller / data$folketall
data$tilfeller1000 <- data$tilfeller1000 * 1000



## Statistisk sentralbyrå
Sys.setlocale(locale="no_NO")
rawdata <- read.csv2("norskenavn.csv")
d <- rawdata
d <- d[,c(1,4)]
colnames(d) <- c("kortkode","norsknavn")
d <- merge(d,isolist,by="kortkode")
d <- d[,c(2,4)]
# Merge
data <- merge(data,d,by="kode")
# Fjern engelske navn
data <- data[,-c(4)]
# Rename 'norsknavn'
variabelnavn <- colnames(data)
variabelnavn[24] <- c("navn")
colnames(data) <- variabelnavn


## FN
# Ny geografisk kategorisering
# Variable: verdensdel -> to replace "verdensdel"

verdensdel <- read.csv("UNSD-Methodology.csv")
d <- verdensdel[,c(4,6,12)]
colnames(d) <- c("verdensdel","region","kode")
d$verdensdel <- factor(d$verdensdel, levels=unique(d$verdensdel), labels=c("Afrika","Amerika",NA,"Asia","Europa","Oseania"))
d$region <- factor(d$region, levels=unique(d$region),labels=c("Nord-Afrika","Afrika sør for Sahara", "Latin-Amerika og Karibia",NA,"Nord-Amerika","Sentral-Asia","Øst-Asia","Sørøst-Asia","Sør-Asia","Vest-Asia","Øst-Europa","Nord-Europa","Sør-Europa","Vest-Europa","Australia og New Zealand","Melanesia","Mikronesia","Polynesia"))

# Merge
data <- merge(data,d,by="kode")


## REORDER DATA FRAME
data <- data[,c(1,24,25,26,2,23,3,11,4,7,5,6,8,9,10,15,12,20,13,14,18,19,17,16,22,21)]


# TO SAVE SESSION PROGRESS
#write.csv2(data,"data-temp.csv")

# TO RESUME SESSION
#data <- read.csv("data-temp.csv")

# TO SAVE DATA SET
write.csv(data,"covidverden.csv", row.names=FALSE)

# SAVE DATA SET AS TAB SEP TXT FILE
#write.table(data,file="covidverden_tab.txt",sep="\t",fileEncoding="UTF-8")




## Write tables to codebook ##

# ECDC
df  <-  data %>% select("dode")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'dode'")), "dode")
df  <-  data %>% select("tilfeller")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tilfeller'")), "tilfeller")

# WB
df  <-  data %>% select("areal")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'areal'")), "areal")
df  <-  data %>% select("BNPpc")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'BNPpc'")), "BNPpc")
df  <-  data %>% select("over64")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'over64'")), "over64")
df  <-  data %>% select("folketall")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'folketall'")), "folketall")
df  <-  data %>% select("urban")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'urban'")), "urban")
df <- data %>% select("turisme")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'turisme'")), "turisme")
df  <-  data %>% select("temperatur")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'temperatur'")), "temperatur")

# Our World in Data
df  <-  data %>% select("respons")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'respons'")), "respons")

# Freedom House
df  <-  data %>% select("FHIFIW")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'FHIFIW'")), "FHIFIW")

# EU
df  <-  data %>% select("eumedlem")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'eumedlem'")), "eumedlem")

# OECD
df  <-  data %>% select("oecdmedlem")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'oecdmedlem'")), "oecdmedlem")

# FN
df  <-  data %>% select("immigranter")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'immigranter'")), "immigranter")
df  <-  data %>% select("verdensdel")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'verdensdel'")), "verdensdel")
df  <-  data %>% select("region")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'region'")), "region")

# Esping-Andersen
df  <-  data %>% select("velferdsstatstypologi")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'velferdsstatstypologi'")), "velferdsstatstypologi")

# Scruggs et al
df  <-  data %>% select("sykepenger")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'sykepenger'")), "sykepenger")

# World Value Survey
df  <-  data %>% select("tillitgenerelt")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tillitgenerelt'")), "tillitgenerelt")
df  <-  data %>% select("tillitmyndigheter")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tillitmyndigheter'")), "tillitmyndigheter")

# ECDC og verdensbanken
df  <-  data %>% select("tilfeller1000")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tilfeller1000'")), "tilfeller1000")

# CIA
df  <-  data %>% select("kvinneligleder")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'kvinneligleder'")), "kvinneligleder")

# Klein et al
df  <-  data %>% select("DRI")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'DRI'")), "DRI")

# WHO
df  <-  data %>% select("DDD1000")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'DDD1000'")), "DDD1000")


