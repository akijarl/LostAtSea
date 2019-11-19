###CONSERVATION BIOLOGY REVIEW PAPER ANALYSIS 
###Code to standardize the location (country) effort and need data ####
### based on manually searched papers (effort) and relative IUCN threat status (need)
rm(list = ls()) #remove past stored objects

####Load packages/ libraries ####
library(scales)

####Set original parameters and directories ####
dateNum = as.character(Sys.Date())

dataDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Location/LocationData/"

####Open the location effort and need data ####
locEffortNeedDF <- read.csv(file = paste0(dataDir, "ConReview_CountryEffortNeed_Combined_ByYearJournalAndAltogether_2019-09-20.csv"), stringsAsFactors = F) #215332 obs of 34 variables

####Explore data ####
str(locEffortNeedDF)
unique(locEffortNeedDF$Year)
unique(locEffortNeedDF$Journal)
# [1] "BiodiversityandConservation" "AllJournals"                 "BiologicalConservation"     
# [4] "ConservationBiology"         "ConservationLetters"         NA  

##Look at the NA values and remove if invalid
test <- subset(locEffortNeedDF, is.na(Journal))
locEffortNeedDF <- subset(locEffortNeedDF, !is.na(Journal))

####Rescale each need metric (prop spp country no DD for everything but DD) for each year, Journal, and IUCN Red List Category #### 
uniqueYears <- unique(locEffortNeedDF$Year)

#Add new column for "PropSppCountry_Rescaled"
locEffortNeedDF$PropSppCountry_Rescaled <- NA

uniqueYear <- "AllYears"
for (uniqueYear in uniqueYears) {
  message("Starting year ", uniqueYear)
  
  #Get just that year
  yearDF <- subset(locEffortNeedDF, Year == uniqueYear & !is.na(PropSppCountryNoDD))
  
  #Get a list of all journals in that year
  uniqueJournals <- unique(yearDF$Journal)
  
  uniqueJournal <- "AllJournals"
  for(uniqueJournal in uniqueJournals) {
    message("Starting journal ", uniqueJournal)
    
    #Get just that journal
    journalDF <- subset(yearDF, Journal == uniqueJournal & !is.na(PropSppCountryNoDD))
    
    #Get a list of all red list categories with that year and journal
    uniqueRlCats <- unique(journalDF$RedListCategory) 
    
    uniqueRlCat <- "All Threatened"
    for (uniqueRlCat in uniqueRlCats) {
      message("Starting category ", uniqueRlCat)
      
      #Get just that category
      rlDF <- subset(journalDF, RedListCategory == uniqueRlCat & !is.na(PropSppCountryNoDD))
      
      if(uniqueRlCat == "Data Deficient") {
        needCol <- rlDF$PropSppCountryWithDD
      } else {
        needCol <- rlDF$PropSppCountryNoDD
      }
      
      message("Old proportion range = ", min(needCol), " to ", max(needCol))
      
      #Rescale the data for that category, journal, and year
      rlDF$PropSppCountry_Rescaled <- rescale(x = needCol, to = c(0,1))
      
      i = 1
      for (i in 1:nrow(rlDF)) {
        
        locEffortNeedDF$PropSppCountry_Rescaled[locEffortNeedDF$Year == uniqueYear & locEffortNeedDF$Journal == uniqueJournal & locEffortNeedDF$RedListCategory == uniqueRlCat & locEffortNeedDF$Alpha.3.code == rlDF$Alpha.3.code[i]] <- rlDF$PropSppCountry_Rescaled[i]
        
      }
      
      message("New proportion range = ", min(rlDF$PropSppCountry_Rescaled), " to ", max(rlDF$PropSppCountry_Rescaled))
    }
  }
}

####Rescale each proportion of papers for each year, journal, and red list category #### 
uniqueYears <- unique(locEffortNeedDF$Year)

#Add new column for "PropSppCountryNoDD_Standardized"
locEffortNeedDF$PropEffort_Rescaled  <- NA

uniqueYear <- uniqueYears[1]
for (uniqueYear in uniqueYears) {
  message("Starting year ", uniqueYear)
  
  #Get just that year
  yearDF <- subset(locEffortNeedDF, Year == uniqueYear & !is.na(PropEffort))
  
  #Get a list of all journals in that year
  uniqueJournals <- unique(yearDF$Journal)
  
  uniqueJournal <- uniqueJournals[1]
  for(uniqueJournal in uniqueJournals) {
    message("Starting journal ", uniqueJournal)
    
    #Get just that journal
    journalDF <- subset(yearDF, Journal == uniqueJournal & !is.na(PropEffort))
    
    #Get a list of all red list categories with that year and journal
    uniqueRlCats <- unique(journalDF$RedListCategory) 
    
    uniqueRlCat <- uniqueRlCats[1]
    for (uniqueRlCat in uniqueRlCats) {
      message("Starting category ", uniqueRlCat)
      
      #Get just that category
      rlDF <- subset(journalDF, RedListCategory == uniqueRlCat & !is.na(PropEffort))
      
      #Rescale from 0 to 1
      rlDF$PropEffort_Rescaled <- rescale(x = rlDF$PropEffort, to = c(0,1))
      
      message("Old proportion range = ", min(rlDF$PropEffort), " to ", max(rlDF$PropEffort))
      
      i = 1
      for (i in 1:nrow(rlDF)) {
        
        locEffortNeedDF$PropEffort_Rescaled[locEffortNeedDF$Year == uniqueYear & locEffortNeedDF$Journal == uniqueJournal & locEffortNeedDF$RedListCategory == uniqueRlCat & locEffortNeedDF$Alpha.3.code == rlDF$Alpha.3.code[i]] <- rlDF$PropEffort_Rescaled[i]
        
      }
      
      message("New proportion range = ", min(rlDF$PropEffort_Rescaled), " to ", max(rlDF$PropEffort_Rescaled))
    }
  }
}

##Create a new column that assigns just the most recent "need" data to all years
#Assign the latest iucn data to each country for each iucn category
locEffortNeedDF$RecentNeed <- NA
locEffortNeedDF$RecentNeed_Rescaled <- NA

uniqueCountry = unique(locEffortNeedDF$Alpha.3.code)[1]
for(uniqueCountry in unique(locEffortNeedDF$Alpha.3.code)) {
  message("Starting country ", uniqueCountry)
  
  #subset just for that country
  countryDF <- subset(locEffortNeedDF, Alpha.3.code == uniqueCountry)
  
  uniqueCat = unique(countryDF$RedListCategory)[1]
  for(uniqueCat in unique(countryDF$RedListCategory)) {
    message("Starting category ", uniqueCat)
    
    #Subset for just that category
    catDF <- subset(countryDF, RedListCategory == uniqueCat) 
    
    #Get the need (original and rescaled) just for the "All Journals" and "AllYears" 
    recentDF <- subset(catDF, Journal == "AllJournals" & Year == "AllYears")
    
    if(uniqueCat == "Data Deficient") {
      needOrig <- recentDF$PropSppCountryWithDD
    } else {
      needOrig <- recentDF$PropSppCountryNoDD
    }

    needRescaled <- recentDF$PropSppCountry_Rescaled
    
    #Assign the recent data to all of the rest of the years and journals
    catDF$RecentNeed <- needOrig
    catDF$RecentNeed_Rescaled <- needRescaled
    
    locEffortNeedDF$RecentNeed[locEffortNeedDF$Alpha.3.code == uniqueCountry & locEffortNeedDF$RedListCategory == uniqueCat] <- needOrig
    locEffortNeedDF$RecentNeed_Rescaled[locEffortNeedDF$Alpha.3.code == uniqueCountry & locEffortNeedDF$RedListCategory == uniqueCat] <- needRescaled
  }
}

####Create a new column that subtracts the rescaled need from the rescaled effort
locEffortNeedDF$RescaledEffortMinusNeed <- locEffortNeedDF$PropEffort_Rescaled - locEffortNeedDF$RecentNeed_Rescaled

range(locEffortNeedDF$RescaledEffortMinusNeed, na.rm = T)

####Calculate whether the rescaled effort minus need values for each country are significantly different from zero and identify significant slopes####
### for each country, iucn category, and journal, remember to remove the "AllYears" year and assign P value to new column for all rows within subset

#Create new empty columns for the new data
locEffortNeedDF$PvalueEffortMinNeedZero <- NA
locEffortNeedDF$SigPosEffortSlope <- NA 
locEffortNeedDF$SigNegEffortSlope <- NA 


uniqueCountry = unique(locEffortNeedDF$Alpha.3.code)[1]
for (uniqueCountry in unique(locEffortNeedDF$Alpha.3.code)) {
  message("Starting country ", uniqueCountry)
  #Subset for just that country
  countryDF <- subset(locEffortNeedDF, Alpha.3.code == uniqueCountry & !is.na(RescaledEffortMinusNeed))
  
  uniqueCat = unique(countryDF$RedListCategory)[1]
  for (uniqueCat in unique(countryDF$RedListCategory)) {
    message("Starting category ", uniqueCat)
    #Subset for that category
    catDF <- subset(countryDF, RedListCategory == uniqueCat)
    
    uniqueJournal = unique(catDF$Journal)[2]
    for (uniqueJournal in unique(catDF$Journal)) {
      message("Starting journal ", uniqueJournal)
      #subset for journal
      journalDF <- subset(catDF, Journal == uniqueJournal & Year != "AllYears")
      
      if (abs(max(journalDF$RescaledEffortMinusNeed) - min(journalDF$RescaledEffortMinusNeed)) > 0) {
        #run t-test
        pvalue <- t.test(journalDF$RescaledEffortMinusNeed, mu = 0, alternative = "two.sided")$p.value
        
        locEffortNeedDF$PvalueEffortMinNeedZero[locEffortNeedDF$Alpha.3.code == uniqueCountry & locEffortNeedDF$RedListCategory == uniqueCat & locEffortNeedDF$Journal == uniqueJournal] <- pvalue
        
        message("P value is ", pvalue)
      
      } else {
        message("P value is NA")
      }
      
      #Find out if the slopes are significantly different and, if so, assign to one of the new columns
      allYearDF <- subset(catDF, Journal == uniqueJournal & Year == "AllYears")
      
      if(uniqueCat == "Data Deficient") {
        slopeEstCol = "SlopeEst_2000to2015_PropSppCountryWithDD"
        slopeSEcol = "SlopeSE_2000to2015_PropSppCountryWithDD"
      } else {
        slopeEstCol = "SlopeEst_2000to2015_PropSppCountryNoDD"
        slopeSEcol = "SlopeSE_2000to2015_PropSppCountryNoDD"
      } 
      
      if(abs(allYearDF[,slopeEstCol]) - allYearDF[,slopeSEcol] > 0) {
        message("Significant slope")
        if(allYearDF[,slopeEstCol] > 0) {
          locEffortNeedDF$SigPosEffortSlope[locEffortNeedDF$Alpha.3.code == uniqueCountry & locEffortNeedDF$RedListCategory == uniqueCat & locEffortNeedDF$Journal == uniqueJournal] <- allYearDF[,slopeEstCol]
        } else {
          locEffortNeedDF$SigNegEffortSlope[locEffortNeedDF$Alpha.3.code == uniqueCountry & locEffortNeedDF$RedListCategory == uniqueCat & locEffortNeedDF$Journal == uniqueJournal] <- allYearDF[,slopeEstCol]
        }
      }
    }
  }
}

sum(!is.na(locEffortNeedDF$SigPosEffortSlope))
sum(!is.na(locEffortNeedDF$SigNegEffortSlope))

###Assign significant positive and significant negative
locEffortNeedDF$SigEffortMinMean <- NA
locEffortNeedDF$SigEffortMinMean[locEffortNeedDF$PvalueEffortMinNeedZero < 0.05 & locEffortNeedDF$RescaledEffortMinusNeed > 0] <- "Effort > Need"
locEffortNeedDF$SigEffortMinMean[locEffortNeedDF$PvalueEffortMinNeedZero < 0.05 & locEffortNeedDF$RescaledEffortMinusNeed < 0] <- "Effort < Need"

####Save the new file with the standardized countries ####
write.csv(x = locEffortNeedDF, file = paste0(dataDir, "ConReview_CountryEffortNeed_ByYearJournalRlCat_Rescaled_", dateNum, ".csv"), row.names = F)
