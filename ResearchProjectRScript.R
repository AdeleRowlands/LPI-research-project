###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments

#clear environment
rm(list=ls())

#install packages - unhash if ever unistalled
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
#install.packages("rredlist")
#devtools::install_github("ropenscilabs/rredlist")

#load packages
library(plyr)
library(dplyr)
require(stringr)
require(naniar)
require(devtools)
require(rlpi)
require(rredlist)
require(tidyverse)
require(car)

#check working director
getwd()
dir()

###IMPORT REDLIST DATA###

#store key as enviornment variable
##later convert this code to a .Renviron text file (File/New File/Text File) so you dont accidently push it to github
#Sys.setenv(IUCN_REDLIST_KEY = "xxx")
#Sys.getenv("IUCN_REDLIST_KEY")

#Can either import from red list package - takes time, or import from working dirtory if it has already been saved there
#unhash as appropriate

##rl species as a data frame
#RLdata <- as.data.frame(rl_sp(all = TRUE))

#out <- rl_sp(all = TRUE)
#vapply(out, "[[", 1, "count")
#RL <- do.call(rbind, lapply(out, "[[", "result"))

#write.csv(RL, "RedListDataframe.csv")

###import red list dataframe from working directory###
dir()
RL <- read.csv("RedListDataframe.csv", header = TRUE)

##tidy RLdata

#select useful columns
RLdata <- select(RL, scientific_name, class_name, category)
colnames(RLdata) <- c("Scientific_Name", "Class", "IUCN_Category")

#replace _ with space in red list data to ensure all scientific names written the same way
RLdata$Scientific_Name <- sub(" ", "_", RLdata$Scientific_Name)

#replace class to lower case
RLdata$Class <- tolower(RLdata$Class)

#filter to just vertibrate species
RLdata <- filter(RLdata, Class %in% c("actinopterygii", "amphibia", "cephalaspidomorphi", "chondrichthyes", "mammalia", "reptilia", "sarcopterygii", "aves", "elasmobranchii", "holocephali", "myxini", "sarcopterygii"))

#change class to a factor
str(RLdata)
RLdata$Class <- as.factor(RLdata$Class)
str(RLdata)

#set up a present in RL collum with TRUE
RLdata$PresentInRL <- TRUE

###IMPORT LPI DATA###

#import csv file from directory
dir()
LPIdata <- read.csv("LPI_LPR2016data_public.csv", header = TRUE)

##tidy LPIdata
#select useful columns
LPIdata <- select(LPIdata, Binomial, Class, Common_name)
colnames(LPIdata) <- c("Scientific_Name", "Class", "Common_Name")

#replace class to lower case
LPIdata$Class <- tolower(LPIdata$Class)

#remove repeats in LPI (maybe don't do this later as each repeat is for a different location but just for now)
LPIdata <- distinct(LPIdata)

#change variables to characters and factors
str(LPIdata)
LPIdata$Scientific_Name <- as.character(LPIdata$Scientific_Name)
LPIdata$Common_Name <- as.character(LPIdata$Common_Name)
LPIdata$Class <- as.factor(LPIdata$Class)

#set up a present in LPI collumn as TRUE
LPIdata$PresentInLPI <- TRUE

#create new dataframe called AllData by merging LPI data and RL data
AllData <- merge(LPIdata, RLdata, all = TRUE, by = c("Scientific_Name", "Class"))
AllData <- arrange(AllData, Scientific_Name)

#convert Present from 'TRUE, NA' to 'TRUE, FALSE'
AllData$PresentInLPI <- ifelse(is.na(AllData$PresentInLPI), FALSE, TRUE)
AllData$PresentInRL <- ifelse (is.na(AllData$PresentInRL), FALSE, TRUE)

#check the table
with(AllData, table(PresentInLPI, PresentInRL))

# DO - One thing here is you have 626 LPI species that aren't in the RL. That is odd.
# I've checked a couple and this looks like taxonomic things. E.g. Alcelaphus_caama (LPI) ==
# Alcelaphus_buselaphus ssp. caama (RL) and Alces_americanus is actually the American
# subspecies of Alces alces. You might need to go through these by hand

###Create Summary Table###
#remove plry for group_by to work properly
detach(package:plyr)

# SumTable with counts 
SumTableCounts <- AllData%>%group_by(Class)%>%summarise(number_in_LPI = sum(PresentInLPI), number_in_RL = sum(PresentInRL), number_in_both = sum(PresentInRL&PresentInLPI))

#add a total row
SumTableCounts2 <- AllData%>%summarise(number_in_LPI = sum(PresentInLPI), number_in_RL  = sum(PresentInRL), number_in_both = sum(PresentInRL&PresentInLPI))
SumTableCounts2$Class <- "Total"
SumTableCounts <- rbind(SumTableCounts, SumTableCounts2)

#save for counts
#write.csv(SumTableCounts, "SumTableCounts.csv")

#Sum Table with percentage 
SumTablePercent <- AllData%>%group_by(Class)%>%summarise(PercentageLPI = sum((PresentInLPI)/n()*100), PercentageRL = sum((PresentInRL)/n()*100), PercentageBoth = sum((PresentInRL&PresentInLPI)/n()*100))

#add a total row
SumTablePercent2 <- AllData%>%summarise(PercentageLPI = sum((PresentInLPI)/n()*100), PercentageRL = sum((PresentInRL)/n()*100), PercentageBoth = sum((PresentInRL&PresentInLPI)/n()*100))
SumTablePercent2$Class <- "Total"
SumTablePercent <- rbind(SumTablePercent, SumTablePercent2)

#round to 2 decimal places
SumTablePercent$PercentageLPI <- format(round(SumTablePercent$PercentageLPI, 2), nsmall = 2)
SumTablePercent$PercentageRL <- format(round(SumTablePercent$PercentageRL, 2), nsmall = 2)
SumTablePercent$PercentageBoth <- format(round(SumTablePercent$PercentageBoth, 2), nsmall = 2)

#save for percentages
#write.csv(SumTablePercent, "SumTablePercent.csv")

#reload plry
library(plyr)

###INCLUDE IUCN ASSESSMENT CRITERIA###

#access criteria through rl_search function as below
#save an example dataframe to use col names from later
Puffin <- as.data.frame(rl_search('Fratercula arctica'))

#reformat RLdata to suit rl_search
#RLdata$Scientific_Name <- sub("_", " ", RLdata$Scientific_Name)

##subsetting for speed/bc laptop couldnt do the whole dataset at once - kept crashing - have hased out once dataframe saved, in order to not run each time

#list all classes to subset into 
classes <- unique(RLdata$Class) 
classes

#subset
#RLdataChond <- filter(RLdata, Class == "chondrichthyes")
#RLdataMam <- filter(RLdata, Class == "mammalia")
#RLdataAct <- filter(RLdata, Class == "actinopterygii")
#RLdataAmph <- filter(RLdata, Class == "amphibia")
#RLdataRep <- filter(RLdata, Class == "reptilia")
#RLdataCeph <- filter(RLdata, Class == "cephalaspidomorphi")
#RLdataSarco <- filter(RLdata, Class == "sarcopterygii")
#RLdataMyx <- filter(RLdata, Class == "myxini")
#RLdataAve <- filter(RLdata, Class == "aves")

#check subsets
#nrow(RLdataAve) + nrow(RLdataMyx) + nrow(RLdataSarco) + nrow(RLdataCeph) + nrow(RLdataRep) + nrow(RLdataAmph) + nrow(RLdataAct) + nrow(RLdataMam) + nrow(RLdataChond) == nrow(RLdata)

##Extract data from rl_search for each class into a new dataframe


#create new dataframe to fill with rl_search output for CHOND
#outputChond <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputChond) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataChond$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputChond <- rbind.fill(outputChond, rl_result)
#}

#remove first row
#outputChond = outputChond[-1,]

#save output for Chond
#write.csv(outputChond, "outputChond.csv")

#import Chond data
#outputChond <- read.csv("outputChond.csv", header = TRUE, stringsAsFactors=FALSE)


##MAMMALS
#subset mammals further
#RLdataMam1 <- filter(RLdataMam, IUCN_Category %in% "LC")
#RLdataMam2 <- filter(RLdataMam, IUCN_Category %in% c("DD", "LR/cd", "LR/nt"))
#RLdataMam3 <- filter(RLdataMam, IUCN_Category %in% c("CR", "VU", "EN", "EX", "NT", "EW"))

#check mammal subsets
#nrow(RLdataMam1) + nrow(RLdataMam2) + nrow(RLdataMam3) == nrow(RLdataMam)


#create new dataframe to fill with rl_search output for MAM1 
#outputMam1 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputMam1) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataMam1$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputMam1 <- rbind.fill(outputMam1, rl_result)
#}

#remove first row
#outputMam1 = outputMam1[-1,]

#save output for Mam1
#write.csv(outputMam1, "outputMam1.csv")

#import Mam1 data
#outputMam1 <- read.csv("outputMam1.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for MAM2 
#outputMam2 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputMam2) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataMam2$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputMam2 <- rbind.fill(outputMam2, rl_result)
#}

#remove first row
#outputMam2 = outputMam2[-1,]

#save output for Mam2
#write.csv(outputMam2, "outputMam2.csv")

#import Mam2 data
#outputMam2 <- read.csv("outputMam2.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for MAM3
#outputMam3 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputMam3) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataMam3$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputMam3 <- rbind.fill(outputMam3, rl_result)
#}

#remove first row
#outputMam3 = outputMam3[-1,]

#save output for Mam2
#write.csv(outputMam3, "outputMam3.csv")

#import Mam3 data
#outputMam3 <- read.csv("outputMam3.csv", header = TRUE, stringsAsFactors=FALSE)


##ACT
#subset ACT further
#RLdataAct1 <- filter(RLdataAct, IUCN_Category %in% "LC")
#RLdataAct2 <- filter(RLdataAct, IUCN_Category %in% c("DD", "LR/cd", "LR/nt", "LR/lc"))
#RLdataAct3 <- filter(RLdataAct, IUCN_Category %in% c("CR", "VU", "EN", "EX", "NT", "EW"))

#check for extra categories
#RLdataActbla <- filter(RLdataAct, !(IUCN_Category %in% c("CR", "VU", "EN", "EX", "NT", "EW", "LC", "DD")))

#check ACT subsets
#nrow(RLdataAct1) + nrow(RLdataAct2) + nrow(RLdataAct3) == nrow(RLdataAct)


###ACT1
#split ACT1 further
#split(RLdataAct1, , drop = FALSE)
#Act1ad <- RLdataAct1[grep('[ABCD]', RLdataAct1$Scientific_Name), ]
#Act1ej <- RLdataAct1[grep('[EFGHIJ]', RLdataAct1$Scientific_Name), ]
#Act1ko <- RLdataAct1[grep('[KLMNO]', RLdataAct1$Scientific_Name), ]
#Act1pt <- RLdataAct1[grep('[PQRST]', RLdataAct1$Scientific_Name), ]
#Act1uz <- RLdataAct1[grep('[UVWXYZ]', RLdataAct1$Scientific_Name), ]


#create new dataframe to fill with rl_search output for ACT1ad
#outputAct1ad <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct1ad) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Act1ad$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct1ad <- rbind.fill(outputAct1ad, rl_result)
#}

#remove first row
#outputAct1ad = outputAct1ad[-1,]

#save output for Act1ad
#write.csv(outputAct1ad, "outputAct1ad.csv")

#import data for Act1ad
#outputAct1ad <- read.csv("outputAct1ad.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for ACT1ej
#outputAct1ej <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct1ej) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Act1ej$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct1ej <- rbind.fill(outputAct1ej, rl_result)
#}

#remove first row
#outputAct1ej = outputAct1ej[-1,]

#save output for Act1ej
#write.csv(outputAct1ej, "outputAct1ej.csv")

#import data for Act1eg
#outputAct1ej <- read.csv("outputAct1ej.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for ACT1ko
#outputAct1ko <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct1ko) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Act1ko$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct1ko <- rbind.fill(outputAct1ko, rl_result)
#}

#remove first row
#outputAct1ko = outputAct1ko[-1,]

#save output Act1ko
#write.csv(outputAct1ko, "outputAct1ko.csv")

#import outputAct1ko
#outputAct1ko <- read.csv("outputAct1ko.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for ACT1pt
#outputAct1pt <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct1pt) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Act1pt$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct1pt <- rbind.fill(outputAct1pt, rl_result)
#}

#remove first row
#outputAct1pt = outputAct1pt[-1,]

#save output for Act1pt
#write.csv(outputAct1pt, "outputAct1pt.csv")

#import outputAct1pt
#outputAct1pt <- read.csv("outputAct1pt.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for ACT1uz
#outputAct1uz <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct1uz) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Act1uz$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct1uz <- rbind.fill(outputAct1uz, rl_result)
#}

#remove first row
#outputAct1uz = outputAct1uz[-1,]

#save output for Act1uz
#write.csv(outputAct1uz, "outputAct1uz.csv")

#import outputAct1uz
#outputAct1uz <- read.csv("outputAct1uz.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for ACT2
#outputAct2 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct2) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataAct2$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct2 <- rbind.fill(outputAct2, rl_result)
#}

#remove first row
#outputAct2 = outputAct2[-1,]

#save output for Act2
#write.csv(outputAct2, "outputAct2.csv")

#import Act2 data
#outputAct2 <- read.csv("outputAct2.csv", header = TRUE, stringsAsFactors=FALSE)


##BUG FIXING ACT3 - KEEPS STOPPING ON 739 SO TRYING WITHOUT NUMBER 740 - hasn't worked
#RLdataAct3739 <- RLdataAct3[-739, ]

#create new dataframe to fill with rl_search output for ACT3
#outputAct3 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAct3) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataAct3739$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAct3 <- rbind.fill(outputAct3, rl_result)
#}

#remove first row
#outputAct3 = outputAct3[-1,]

#save output for Act3
#write.csv(outputAct3, "outputAct3.csv")

#import outputAct3
#outputAct3 <- read.csv("outputAct3.csv", header = TRUE, stringsAsFactors=FALSE)

##AMPH
#Subset Amph
#Amph1 <- RLdataAmph[grep('[ABCDEFGH]', RLdataAmph$Scientific_Name), ]
#Amph2 <- RLdataAmph[grep('[IJKLMNOPQ]', RLdataAmph$Scientific_Name), ]
#Amph3 <- RLdataAmph[grep('[RSTUVWXYZ]', RLdataAmph$Scientific_Name), ]

#check Amph subsets
#nrow(Amph1) + nrow(Amph2) + nrow(Amph3) == nrow(RLdataAmph)


#create new dataframe to fill with rl_search output for AMPH1
#outputAmph1 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAmph1) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Amph1$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAmph1 <- rbind.fill(outputAmph1, rl_result)
#}

#remove first row
#outputAmph1 = outputAmph1[-1,]

#save output for Amph1
#write.csv(outputAmph1, "outputAmph1.csv")

#import Amph1 data 
#outputAmph1 <- read.csv("outputAmph1.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AMPH2
#outputAmph2 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAmph2) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Amph2$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAmph2 <- rbind.fill(outputAmph2, rl_result)
#}

#remove first row
#outputAmph2 = outputAmph2[-1,]

#save output for Amph2
#write.csv(outputAmph2, "outputAmph2.csv")

#import Amph2 data 
#outputAmph2 <- read.csv("outputAmph2.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AMPH3
#outputAmph3 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAmph3) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Amph3$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAmph3 <- rbind.fill(outputAmph3, rl_result)
#}

#remove first row
#outputAmph3 = outputAmph3[-1,]

#save output for Amph3
#write.csv(outputAmph3, "outputAmph3.csv")

#import Amph3 data 
#outputAmph3 <- read.csv("outputAmph3.csv", header = TRUE, stringsAsFactors=FALSE)

###REP
#subset rep 
#Rep1 <- RLdataRep[grep('[ABCDEF]', RLdataRep$Scientific_Name), ]
#Rep2 <- RLdataRep[grep('[GHIJKLMNO]', RLdataRep$Scientific_Name), ]
#Rep3 <- RLdataRep[grep('[PQRSTUVWXYZ]', RLdataRep$Scientific_Name), ]

#check subsets
#nrow(Rep1) + nrow(Rep2) + nrow(Rep3) == nrow(RLdataRep)

##BUG FIX REP1
#create new dataframe to fill with rl_search output for REP1
#outputRep1 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputRep1) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Rep1$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputRep1 <- rbind.fill(outputRep1, rl_result)
#}

#remove first row
#outputRep1 = outputRep1[-1,]

#save output for Rep1
#write.csv(outputRep1, "outputRep1.csv")

#import data for Rep 1
#outputRep1 <- read.csv("outputRep1.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for REP2
#outputRep2 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputRep2) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Rep2$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputRep2 <- rbind.fill(outputRep2, rl_result)
#}

#remove first row
#outputRep2 = outputRep2[-1,]

#save output for Rep2
#write.csv(outputRep2, "outputRep2.csv")

#import data for Rep 2
#outputRep2 <- read.csv("outputRep2.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for REP3
#outputRep3 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputRep3) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Rep3$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputRep3 <- rbind.fill(outputRep3, rl_result)
#}

#remove first row
#outputRep3 = outputRep3[-1,]

#save output for Rep3
#write.csv(outputRep3, "outputRep3.csv")

#import data for Rep 3
#outputRep3 <- read.csv("outputRep3.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for CEPH
#outputCeph <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputCeph) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataCeph$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputCeph <- rbind.fill(outputCeph, rl_result)
#}

#remove first row
#outputCeph = outputCeph[-1,]

#save output for Ceph
#write.csv(outputCeph, "outputCeph.csv")

#import Ceph data
#outputCeph <- read.csv("outputCeph.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for SARCO
#outputSarco <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputSarco) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataSarco$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputSarco <- rbind.fill(outputSarco, rl_result)
#}

#remove first row
#outputSarco = outputSarco[-1,]

#save output for Sarco
#write.csv(outputSarco, "outputSarco.csv")

#import Sarco data
#outputSarco <- read.csv("outputSarco.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for MYX
#outputMyx <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputMyx) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataMyx$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputMyx <- rbind.fill(outputMyx, rl_result)
#}

#remove first row
#outputMyx = outputMyx[-1,]

#save output for Myx
#write.csv(outputMyx, "outputMyx.csv")

#import Myx data
#outputMyx <- read.csv("outputMyx.csv", header = TRUE, stringsAsFactors=FALSE)


##AVES
#subset AVES
#RLdataAve1 <- filter(RLdataAve, IUCN_Category %in% "LC")
#RLdataAve2 <- filter(RLdataAve, IUCN_Category %in% c("DD", "LR/cd", "LR/nt", "LR/lc", "CR", "VU"))
#RLdataAve3 <- filter(RLdataAve, IUCN_Category %in% c("EN", "EX", "NT", "EW"))

#check AVES subsets
#nrow(RLdataAve1) + nrow(RLdataAve2) + nrow(RLdataAve3) == nrow(RLdataAve)

#check for extra categories
#RLdataActCheck <- filter(RLdataAct, !(IUCN_Category %in% c("CR", "VU", "EN", "EX", "NT", "EW", "LC", "DD", "LR/cd", "LR/nt", "LR/lc")))


##split AVE1 into 3
#Ave1af <- RLdataAve1[grep('[ABCDEF]', RLdataAve1$Scientific_Name), ]
#Ave1go <- RLdataAve1[grep('[GHIJKLMNO]', RLdataAve1$Scientific_Name), ]
#Ave1pz <- RLdataAve1[grep('[PQRSTUVWXYZ]', RLdataAve1$Scientific_Name), ]


#create new dataframe to fill with rl_search output for AVE1af
#outputAve1af <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAve1af) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Ave1af$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAve1af <- rbind.fill(outputAve1af, rl_result)
#}

#remove first row
#outputAve1af = outputAve1af[-1,]

#save Ave1af output
#write.csv(outputAve1af, "outputAve1af.csv")

#import Ave1af output
#outputAve1af <- read.csv("outputAve1af.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AVE1go
#outputAve1go <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAve1go) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Ave1go$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAve1go <- rbind.fill(outputAve1go, rl_result)
#}

#remove first row
#outputAve1go = outputAve1go[-1,]

#save Ave1go output
#write.csv(outputAve1go, "outputAve1go.csv")

#import Ave1af output
#outputAve1go <- read.csv("outputAve1go.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AVE1pz
#outputAve1pz <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAve1pz) <- colnames(Puffin)

#loop to append data
#for ( i in unique(Ave1pz$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAve1pz <- rbind.fill(outputAve1pz, rl_result)
#}

#remove first row
#outputAve1pz = outputAve1pz[-1,]

#save Ave1pz output
#write.csv(outputAve1pz, "outputAve1pz.csv")

#import Ave1pz output
#outputAve1pz <- read.csv("outputAve1pz.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AVE2
#outputAve2 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAve2) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataAve2$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAve2 <- rbind.fill(outputAve2, rl_result)
#}

#remove first row
#outputAve2 = outputAve2[-1,]

#save output for Ave2
#write.csv(outputAve2, "outputAve2.csv")

#import outputAve2
#outputAve2 <- read.csv("outputAve2.csv", header = TRUE, stringsAsFactors=FALSE)


#create new dataframe to fill with rl_search output for AVE3
#outputAve3 <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
#colnames(outputAve3) <- colnames(Puffin)

#loop to append data
#for ( i in unique(RLdataAve3$Scientific_Name)) {
#  rl_result <- as.data.frame(rl_search(i))
#  outputAve3 <- rbind.fill(outputAve3, rl_result)
#}

#remove first row
#outputAve3 = outputAve3[-1,]

#save output for Ave3
#write.csv(outputAve3, "outputAve3.csv")

#import outputAve3
#outputAve3 <- read.csv("outputAve3.csv", header = TRUE, stringsAsFactors=FALSE)

## END Of DATA EXTRACTION ##

##rbind subsetted dataframe together again
#RLCriteria <- rbind.fill(outputAct1ad, outputAct1ej, outputAct1ko, outputAct1pt, outputAct1uz, outputAct2, outputAct3, outputAmph1, outputAmph2, outputAmph3, outputAve1af, outputAve1go, outputAve1pz, outputAve2, outputAve3, outputCeph, outputChond, outputMam1, outputMam2, outputMam3, outputMyx, outputRep1, outputRep2, outputRep3, outputSarco)

#arrange alphabetically
#RLCriteria <- arrange(RLCriteria, result.scientific_name)

#format RLCriteria to have Scientific_Name correct
#RLCriteria$result.scientific_name <- sub(" ", "_", RLCriteria$result.scientific_name)

##reformat RLdata to have Scientific_Name correct again
#RLdata$Scientific_Name <- sub(" ", "_", RLdata$Scientific_Name)

#save rl_search output as csv 
#write.csv(RLCriteria, "RLCriteria.csv")

#import RL criteria (all previous hashed out code has been to crearte RLCriteria)
RLCriteria <- read.csv("RLCriteria.csv", header = TRUE)

#subset to only scienfitic name, criteria and common name columns
RLCriteria <- select(RLCriteria, result.scientific_name, result.criteria, result.main_common_name)
#give columns names
colnames(RLCriteria) <- c("Scientific_Name", "IUCN_Criteria", "Common_Name")

#merge with AllData to produce full dataset 
DataWithCriteria <- merge(RLCriteria, AllData, all = TRUE, by ="Scientific_Name")

#check structure of full dataset, DataWithCriteria
str(DataWithCriteria)
DataWithCriteria$Common_Name.y <- as.factor(DataWithCriteria$Common_Name.y)
str(DataWithCriteria)

#merge common names together 
DataWithCriteria <- DataWithCriteria%>%mutate(Common_Name = coalesce(Common_Name.x, Common_Name.y))
DataWithCriteria <- select(DataWithCriteria, -Common_Name.x, -Common_Name.y)

#save as csv
write.csv(DataWithCriteria, "DataWithCriteria.csv")

#merge with RLData to use for summary tables and estimates of population trends
RLWithCriteria <- merge(RLCriteria, RLdata, all = TRUE, by ="Scientific_Name")

#save as csv
write.csv(RLWithCriteria, "RLWithCriteria.csv")

##Add TRUE FALSE collumns for  A, B, C etc.for summary table###

#explore different criteria
Criteria <- unique(RLWithCriteria$IUCN_Criteria) 
Criteria
length(Criteria)

#dataset to only species which have a criteria (most likely only threatened species)
RLOnlyCriteria <- subset(RLWithCriteria, (!is.na(RLWithCriteria$IUCN_Criteria)))

#identify those categorised by each criterion
criterionA <- RLOnlyCriteria[grep('[A]', RLOnlyCriteria$IUCN_Criteria), ]
criterionA$CriteriaA <- "TRUE"

criterionB <- RLOnlyCriteria[grep('[B]', RLOnlyCriteria$IUCN_Criteria), ]
criterionB$CriteriaB <- "TRUE"

criterionC <- RLOnlyCriteria[grep('[C]', RLOnlyCriteria$IUCN_Criteria), ]
criterionC$CriteriaC <- "TRUE"

criterionD <- RLOnlyCriteria[grep('[D]', RLOnlyCriteria$IUCN_Criteria), ]
criterionD$CriteriaD <- "TRUE"

criterionE <- RLOnlyCriteria[grep('[E]', RLOnlyCriteria$IUCN_Criteria), ]
criterionE$CriteriaE <- "TRUE"

#join together to produce full dataset 
criterionAB <- merge(criterionA, criterionB, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Common_Name", "Class", "IUCN_Category", "PresentInRL"))
criterionABC <- merge(criterionAB, criterionC, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name"))
criterionABCD <- merge(criterionABC, criterionD, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name"))
RLOnlyCriteria <- merge(criterionABCD, criterionE, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name"))

#Add falses ('TRUE, FALSE' used for ease of creating summary tables)
RLOnlyCriteria$CriteriaA <- ifelse(is.na(RLOnlyCriteria$CriteriaA), FALSE, TRUE)
RLOnlyCriteria$CriteriaB <- ifelse(is.na(RLOnlyCriteria$CriteriaB), FALSE, TRUE)
RLOnlyCriteria$CriteriaC <- ifelse(is.na(RLOnlyCriteria$CriteriaC), FALSE, TRUE)
RLOnlyCriteria$CriteriaD <- ifelse(is.na(RLOnlyCriteria$CriteriaD), FALSE, TRUE)
RLOnlyCriteria$CriteriaE <- ifelse(is.na(RLOnlyCriteria$CriteriaE), FALSE, TRUE)

#save RLOnlyCriteria as a csv
write.csv(RLOnlyCriteria, "RLOnlyCriteria.csv")

###Create Summary Tables###

#check structure
str(RLOnlyCriteria)

##table of percentages of species categorised by each criteria
CriteriaSumTableA <- summarise(RLOnlyCriteria, PercentA = (sum(CriteriaA)/nrow(RLOnlyCriteria)*100))
CriteriaSumTableB <- summarise(RLOnlyCriteria, PercentB = (sum(CriteriaB)/nrow(RLOnlyCriteria)*100))
CriteriaSumTableC <- summarise(RLOnlyCriteria, PercentC = (sum(CriteriaC)/nrow(RLOnlyCriteria)*100))
CriteriaSumTableD <- summarise(RLOnlyCriteria, PercentD = (sum(CriteriaD)/nrow(RLOnlyCriteria)*100))
CriteriaSumTableE <- summarise(RLOnlyCriteria, PercentE = (sum(CriteriaE)/nrow(RLOnlyCriteria)*100))

#bind together
CriteriaSumTable <- cbind(CriteriaSumTableA, CriteriaSumTableB, CriteriaSumTableC, CriteriaSumTableD, CriteriaSumTableE)

#round to 2 decimal places
CriteriaSumTable <- format(round(CriteriaSumTable, 2), nsmall = 2)

#save percent summary table
write.csv(CriteriaSumTable, "CriteriaSumTablePercent.csv")

##table of counts of species categorised by each criteria
CritSumTableCounts <- summarise(RLOnlyCriteria, counts_A = sum(CriteriaA), counts_B = sum(CriteriaB), counts_C = sum(CriteriaC), counts_D = sum(CriteriaD), counts_E = sum(CriteriaE))

#save counts summary table
write.csv(CritSumTableCounts, "CriteriaSumTableCounts.csv")


###RATE OF CHANGE PER YEAR FOR EACH SPECIES###

###start next stage by clearing environment 
rm(list=ls())

#import all useful datasets
RLOnlyCriteria <- read.csv("RLOnlyCriteria.csv", header = TRUE)
DataWithCriteria <- read.csv("DataWithCriteria.csv", header = TRUE)
SearchResults <- read.csv("RLCriteria.csv", header = TRUE)

#create new dataset with only species in RL categorised by criterion A 
RLCritA <- RLOnlyCriteria[grep('[A]', RLOnlyCriteria$IUCN_Criteria), ]

#create column called %Decrease
RLCritA$PercentDecrease <- "NA"

#for CR specier fill PercentDecrease with 9%
RLCritACR <- filter(RLCritA, IUCN_Category %in% "CR")
RLCritACR$PercentDecrease <- 9

#for EN specier fill PercentDecrease with 6.5%
RLCritAEN <- filter(RLCritA, IUCN_Category %in% "EN")
RLCritAEN$PercentDecrease <- 6.5

#for VU specier fill PercentDecrease with 4%
RLCritAVU <- filter(RLCritA, IUCN_Category %in% "VU")
RLCritAVU$PercentDecrease <- 4

#rebind together
RLCritA2 <- rbind(RLCritACR, RLCritAEN, RLCritAVU)

#arrange alphabetically by scientific name
RLCritA2 <- arrange(RLCritA2, Scientific_Name)

#save RL species with their population trend
write.csv(RLCritA2, "RLWithTrend.csv")

#check all species included
nrow(RLCritACR) + nrow(RLCritAEN) + nrow(RLCritAVU) == nrow(RLCritA)

#check which species not included - check for bugs too: e.g. some species listed more than once - belive this is because different populations of some species (e.g. turtles) are in the RL, some non-threatened species have criterion (maybe because other populalations that are threated have criteria)
Check <- filter(RLCritA, !(IUCN_Category %in% c("CR", "VU", "EN")))


###Information extraction for results###

#number of species with IUCN criteria
length(DataWithCriteria$IUCN_Criteria) - sum(is.na(DataWithCriteria$IUCN_Criteria))

#number of species classified under Crit A and not in the LPI
CritA_LP_RL <- DataWithCriteria[grep('[A]', DataWithCriteria$IUCN_Criteria), ]
sum(CritA_LP_RL$PresentInRL) - sum(CritA_LP_RL$PresentInLPI)

#mean rate of decrease per annum for threatedn IUCN vertebrates 
mean(RLCritA2$PercentDecrease)



