####CONSERVATION BIOLOGY REVIEW PAPER ANALYSIS 
###Code to plot conservation effort and need by broad taxonomic category (animal, plant, fungi) based on manual data
rm(list = ls()) #remove past stored objects
options(scipen = 999) #turn off scientific notation

####Load packages/ libraries ####
library(ggplot2) #plotting
#library(ggpmisc)
library(betareg) #for beta regression
#library(sjPlot)
#library(sjlabelled)
#library(sjmisc)
library(scales) #for rescaling data
library(plyr) #for summarizing data

####Set original parameters ####effortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot
dateNum = as.character(Sys.Date())

####Set folder locations ####
taxonDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyData/"
iucnDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/IUCNdata/"
plotDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyPlots/"
analysisDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyAnalysis/"
  
####Open the kingdom effort and need data####
taxonEffortDF <- read.csv(file = paste0(taxonDir, "ConBioRev_TaxonEffortBroad_ByYearJournalAndAltogether_2019-12-17.csv"), stringsAsFactors = F) #988 of 7 obs
taxonNeedDF <- read.csv(file = paste0(iucnDir, "ConReview_KingdomNeed_ByYearIUCNcat_2019-10-03.csv"), stringsAsFactors = F) #988 of 7 obs

####1. Effort over time - analysis and plots ####
###Analyze with beta regression
head(taxonEffortDF)

###Combine the "Other" and "Not applicable" categories
unique(taxonEffortDF$TaxonName)
# [1] "Animal"         "Plant"          "Not applicable" "Multiple"      
# [5] "Fungi"          "Microbe"        "Other"  

uniqueYears <- unique(taxonEffortDF$Year)
uniqueYear = uniqueYears[1]
for (uniqueYear in uniqueYears) {
  yearDF <- subset(taxonEffortDF, Year == uniqueYear)
  uniqueJournals <- unique(yearDF$Journal)
  
  uniqueJournal = uniqueJournals[1]
  for (uniqueJournal in uniqueJournals) {
    journalTaxonDF <- subset(yearDF, Journal == uniqueJournal & TaxonName %in% c("Not applicable", "Other"))
    
    #Add row to the taxonEffortDF wth totals for other and NA taxon categories
    taxonEffortDF[nrow(taxonEffortDF) + 1,] = NA
    taxonEffortDF$TaxonName[nrow(taxonEffortDF)] <- "Other_NA"
    taxonEffortDF$Year[nrow(taxonEffortDF)] = uniqueYear
    taxonEffortDF$Journal[nrow(taxonEffortDF)] = uniqueJournal
    taxonEffortDF$NumPapers[nrow(taxonEffortDF)] = sum(journalTaxonDF$NumPapers, na.rm = T)
    taxonEffortDF$AllPapers[nrow(taxonEffortDF)] = sum(journalTaxonDF$AllPapers, na.rm = T)
    taxonEffortDF$PropEffortAllTaxa[nrow(taxonEffortDF)] = sum(journalTaxonDF$PropEffortAllTaxa, na.rm = T)
    taxonEffortDF$PropEffortSpecificTaxa[nrow(taxonEffortDF)] = NA
    
  }
}

##Remove any of the rows with NA and other separate
taxonEffortDF <- subset(taxonEffortDF, !TaxonName %in% c("Other", "Not applicable"))

unique(taxonEffortDF$TaxonName)
#[1] "Animal"   "Plant"    "Multiple" "Fungi"    "Microbe"  "Other_NA"

#Change the order of the taxon categories so it starts with the main categories
taxonEffortDF$TaxonName <- factor(taxonEffortDF$TaxonName, levels = c("Animal", "Plant", "Fungi", "Microbe", "Multiple", "Other_NA"))

#Make dataframe without "All Years" for analysis
sepYearEffortDF <- subset(taxonEffortDF, Year != "AllYears")
sepYearEffortDF$Year <- as.integer(sepYearEffortDF$Year)

###Run beta regression analysis for each taxon with varying slopes and intercepts for each journal and each taxon
#Re-assign any zeros in the proportional data to 0.001 and 1's to 0.999 because those are not allowed in the beta regression
betaDF <- sepYearEffortDF
sum(betaDF$AllPapers == 0) #0
sum(betaDF$PropEffortAllTaxa == 0) #106
sum(betaDF$PropEffortAllTaxa == 1) #0
betaDF$PropEffortAllTaxa[betaDF$PropEffortAllTaxa == 0] <- 0.0001

###Create a new column transforming the year to make the intercept useful
##  by subtracting the first year
betaDF$YearZero <- betaDF$Year - min(betaDF$Year)

##Create an empty dataframe to add results of the beta regression analysis
betaResultsDF <- data.frame(TaxonName = character(),
                            Coefficient = character(),
                            Estimate = numeric(),
                            StdError = numeric(),
                            Zvalue = numeric(),
                            Pvalue = numeric(), 
                            PrecisionEst = numeric(),
                            PrecisionStdErr = numeric(),
                            PrecisionZval = numeric(),
                            PrecisionPval = numeric(), stringsAsFactors = F) 

uniqueTaxa = unique(betaDF$TaxonName)
uniqueTaxon = uniqueTaxa[6]
for (uniqueTaxon in uniqueTaxa) {
  taxonDF <- subset(betaDF, TaxonName == uniqueTaxon)
  taxonBetaRegModel <- betareg(PropEffortAllTaxa ~ YearZero * Journal, data = taxonDF)
  taxonBetaRegDF <- as.data.frame(summary(taxonBetaRegModel)$coef)
  taxonBetaRegDF$TaxonName = uniqueTaxon #Add the taxon name
  taxonBetaRegDF$Coefficient <- row.names(taxonBetaRegDF) #Change the row names to the Coefficient column
  rownames(taxonBetaRegDF) <- c() #remove row names 
  
  #rename columns to match the empty dataframe above
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "mean.Estimate"] <- "Estimate"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "mean.Std..Error"] <- "StdError"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "mean.z.value"] <- "Zvalue"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "mean.Pr...z.."] <- "Pvalue"
  
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "precision.Estimate"] <- "PrecisionEst"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "precision.Std..Error"] <- "PrecisionStdErr"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "precision.z.value"] <- "PrecisionZval"
  colnames(taxonBetaRegDF)[colnames(taxonBetaRegDF) == "precision.Pr...z.."] <- "PrecisionPval"
  
  #Merge (rbind) with the empty dataframe
  betaResultsDF <- rbind(betaResultsDF, taxonBetaRegDF)
  
  #Plot the effect sizes for that taxon
  taxonEffectPlot <- plot_model(taxonBetaRegModel, transform = NULL, show.values = TRUE, value.offset = .3, rm.terms = "(phi)", 
                                vline.color = "black", 
                                axis.title = "Estimate (log-odds)",
                                title = paste0("Beta regression effect sizes predicting conservation research effort - ", uniqueTaxon, " taxon"))
  
  ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortEffectSizes_", uniqueTaxon, "_", dateNum, ".jpg"), plot = taxonEffectPlot, width = 10)
  
}

#Change the order of the columns to start with the Taxon then Coefficient
betaResultsDF <- betaResultsDF[c("TaxonName", "Coefficient", "Estimate", "StdError", "Zvalue", "Pvalue", "PrecisionEst", "PrecisionStdErr", "PrecisionZval", "PrecisionPval")]

test <- subset(betaResultsDF, Pvalue < 0.05) #The only significant results are with "Multiple" category

##Save the analysis results for beta regression
write.csv(x = betaResultsDF, file = paste0(analysisDir, "ConsBioRev_BetaRegRes_TaxonEffortBroad_", dateNum, ".csv"), row.names = F)

###Plot the effect sizes for the full regression
allBetaReg <- betareg(PropEffortAllTaxa ~ YearZero + TaxonName * Journal, data = betaDF)

allGLM <- glm(PropEffortAllTaxa ~ YearZero * TaxonName * Journal, data = betaDF)

fullEffectPlot <- plot_model(allBetaReg, tranform = NULL, show.values = TRUE, value.offset = .3, rm.terms = "(phi)", 
                             vline.color = "black",
                             axis.title = "Estimate (log-odds)",
                             title = "Beta regression effect sizes predicting conservation research effort by taxon") + theme(panel.grid = element_blank())

ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortFullEffectSizes_AllTaxon_", dateNum, ".jpg"), plot = fullEffectPlot, width = 10, height = 10)

###Plot the effect sizes again but only for the year effect
####REDO THIS NEXT SECTION BUT USING THE COEFFICIENT ESTIMATES FROM THE SAVED CSV ####
yearAllJournalsTermsToInclude <- c("YearZero", "YearZero:TaxonNamePlant", "YearZero:TaxonNameFungi", "YearZero:TaxonNameMicrobe", "YearZero:TaxonNameMultiple", "YearZero:TaxonNameOther_NA")

yearEffectPlotAllJournals <- plot_model(allBetaReg,
                                        tranform = NULL, 
                                        show.values = TRUE, 
                                        value.offset = .3, 
                                        terms = yearAllJournalsTermsToInclude, 
                                        vline.color = "black",
                                        axis.title = "Estimate (log-odds)",
                                        title = "Beta regression effect sizes - conservation research effort by taxon (all journals year effect)"
                                        ) + theme(panel.grid = element_blank())


###Plot the effort over time (by journal and by taxon)
#Define color-blind friendly palettes
cbp1 <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
cbp2 <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

#Plot the effort over time for each broad taxonomic category (colors) with journals as facets
taxonEffortOverTimePlot_FacetByJournal <- ggplot(betaDF, aes(x = Year, y = PropEffortAllTaxa, group = TaxonName, color = TaxonName)) + 
  geom_point() + geom_line() + 
  scale_colour_manual(values = cbp1) +
  facet_wrap(~Journal, ncol = 3, scales = "free_x") +
  stat_smooth(method = "betareg", se = F, formula = y~x, fullrange = F, linetype = "longdash") + 
  labs(title = "Conservation research effort over time by broad taxonomic category",
       subtitle = "Proportion of papers from 2000-2015",
       x = "Year", 
       y = "Research effort (proportion of papers)") +
  theme(panel.grid = element_blank())
  

ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffort_ByJournalFacet_", dateNum, ".jpg"), plot = taxonEffortOverTimePlot_FacetByJournal, width = 10, height = 7)

#Plot the effort over time for each broad taxonomic factor (as facets) with journals as colors
taxonEffortOverTimePlot_FacetByTaxon <- ggplot(betaDF, aes(x = Year, y = PropEffortAllTaxa, color = Journal, group = Journal)) + 
  geom_line() + geom_point() + 
  scale_colour_manual(values = cbp1) +
  facet_wrap(~TaxonName, ncol = 3, scales = "free") +
  stat_smooth(method = "betareg", se = F, formula = y~x, fullrange = F, linetype = "longdash") +
  labs(title = "Conservation research effort over time by broad taxonomic category",
       subtitle = "Proportion of papers from 2000-2015",
       x = "Year", 
       y = "Research effort (proportion of papers)") +
  theme(panel.grid = element_blank())

ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortFacet_ByJournalColor_", dateNum, ".jpg"), plot = taxonEffortOverTimePlot_FacetByTaxon, width = 10, height = 7)

#Plot the total effort over time for each taxonomic category (colors) with journals as different symbols
plotDF <- subset(betaDF, Journal == "AllJournals")
taxonEffortOverTimePlot_AllJournals <- ggplot(plotDF, aes(x = Year, y = PropEffortAllTaxa, group = TaxonName, color = TaxonName)) +
  geom_point() + geom_line() + 
  scale_colour_manual(values = cbp1) +
  stat_smooth(method = "betareg", se = F, formula = y~x, fullrange = F, linetype = "longdash") + 
  labs(title = "Conservation research effort over time by broad taxonomic category",
       subtitle = "Proportion of papers from 2000-2015",
       x = "Year", 
       y = "Research effort (proportion of papers)") +
  theme(panel.grid = element_blank())

ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortOverTime_AllJournals_", dateNum, ".jpg"), plot = taxonEffortOverTimePlot_AllJournals, width = 10, height = 7)

####Summarize the data to get the medians and interquartile ranges for each taxonomic group for all journals, and each journal separately
taxonEffortSummDF <- ddply(subset(taxonEffortDF, Year != "AllYears"), c("TaxonName", "Journal"), summarise,
                           N = length(PropEffortAllTaxa),
                           MeanPropEffortAllTaxa = mean(PropEffortAllTaxa),
                           SdPropEffortAllTaxa = sd(PropEffortAllTaxa),
                           SePropEffortAllTaxa = SdPropEffortAllTaxa / sqrt(N),
                           MedianPropEffortAllTaxa = median(PropEffortAllTaxa),
                           IqrPropEffortAllTaxa = IQR(PropEffortAllTaxa),
                           MaxInnerQuantPropEffortAllTaxa = quantile(PropEffortAllTaxa, 0.75),
                           MinInnerQuantPropEffortAllTaxa = quantile(PropEffortAllTaxa, 0.25),
                           MaxPropEffortAllTaxa = max(PropEffortAllTaxa),
                           MinPropEffortAllTaxa = min(PropEffortAllTaxa)
)

####Summarize again but removing "All Journals" and treating journals as reps
taxonEffortSummJournalRepsDF <- ddply(subset(taxonEffortDF, Year != "AllYears" & Journal != "AllJournals"), c("TaxonName"), summarise,
                                      N = length(PropEffortAllTaxa),
                                      MeanPropEffortAllTaxa = mean(PropEffortAllTaxa),
                                      SdPropEffortAllTaxa = sd(PropEffortAllTaxa),
                                      SePropEffortAllTaxa = SdPropEffortAllTaxa / sqrt(N),
                                      MedianPropEffortAllTaxa = median(PropEffortAllTaxa),
                                      IqrPropEffortAllTaxa = IQR(PropEffortAllTaxa),
                                      MaxInnerQuantPropEffortAllTaxa = quantile(PropEffortAllTaxa, 0.75),
                                      MinInnerQuantPropEffortAllTaxa = quantile(PropEffortAllTaxa, 0.25),
                                      MaxPropEffortAllTaxa = max(PropEffortAllTaxa),
                                      MinPropEffortAllTaxa = min(PropEffortAllTaxa)
)

####2. Need (perceived) over time - analysis and plots ####
###Analyze with beta regression
head(taxonNeedDF)

unique(taxonNeedDF$KingdomName)

###Run beta regression analysis over time for all taxon (Year * KingdomName) 
##  and for each RL category (except "All Categories")
##  also do for each of PropSppKingdomNoDD or with DD if DD and PropSppAllTaxaByCat
##  and do this for multiple year ranges (all, all three kingdom, all four kingdom, and manual range)

####FIX THIS FROM NOW FORWARD TO MAKE CHANGES TO NEED DATA AND STANDARDIZE SO THE SUM OF PROPORTIONS EQUALS ONE ####
#Create a dataset without "All Categories" 
taxonNeedDF <- subset(taxonNeedDF, RedListCategory != "All Categories")

#Combine all of the "Low risk" categories
unique(taxonNeedDF$RedListCategory)

uniqueYears <- unique(taxonNeedDF$YearUpTo)
uniqueYear = uniqueYears[1]
for (uniqueYear in uniqueYears) {
  yearDF <- subset(taxonNeedDF, YearUpTo == uniqueYear & RedListCategory %in% c("Lower Risk/conservation dependent", "Lower Risk/least concern", "Lower Risk/near threatened"))
  
  if(nrow(yearDF) > 0) {
    
    uniqueKingdoms <- unique(yearDF$KingdomName)
    uniqueKingdom = uniqueKingdoms[1]
    for(uniqueKingdom in uniqueKingdoms) {
      kingdomDF <- subset(yearDF, KingdomName == uniqueKingdom) 
      
      #Add row to the taxonNeedDF wth totals for all the lower risk red list categories
      taxonNeedDF[nrow(taxonNeedDF) + 1,] = NA
      taxonNeedDF$KingdomName[nrow(taxonNeedDF)] <- uniqueKingdom
      taxonNeedDF$YearUpTo[nrow(taxonNeedDF)] <- uniqueYear
      taxonNeedDF$RedListCategory[nrow(taxonNeedDF)] <- "Lower Risk"
      taxonNeedDF$NumSpp[nrow(taxonNeedDF)] <- sum(kingdomDF$NumSpp)
      taxonNeedDF$NumAssessWithinKingdom[nrow(taxonNeedDF)] <- mean(kingdomDF$NumAssessWithinKingdom)
      taxonNeedDF$NumAssessWithinRLcat[nrow(taxonNeedDF)] <- sum(kingdomDF$NumAssessWithinRLcat)
      taxonNeedDF$PropSppKingdomNoDD[nrow(taxonNeedDF)] <- sum(kingdomDF$PropSppKingdomNoDD)
      taxonNeedDF$PropSppKingdomWithDD[nrow(taxonNeedDF)] <- sum(kingdomDF$PropSppKingdomWithDD)
      taxonNeedDF$PropSppAllTaxaByCat[nrow(taxonNeedDF)] <- taxonNeedDF$NumSpp[nrow(taxonNeedDF)]/taxonNeedDF$NumAssessWithinRLcat[nrow(taxonNeedDF)]
      
    }
  }
}

##Remove any of the rows with separate Lower risk categories
taxonNeedDF <- subset(taxonNeedDF, !RedListCategory %in% c("Lower Risk/conservation dependent", "Lower Risk/least concern", "Lower Risk/near threatened"))

unique(taxonNeedDF$RedListCategory)
# [1] "Near Threatened"       "Vulnerable"            "Endangered"            "Least Concern"         "Data Deficient"       
# [6] "Extinct"               "Extinct in the Wild"   "Critically Endangered" "All Threatened"        "Lower Risk"  

#Change the order of the red list categories so it starts with the least concern and works up
taxonNeedDF$RedListCategory <- factor(taxonNeedDF$RedListCategory, levels = c("Data Deficient",
                                                                    "Least Concern", 
                                                                    "Lower Risk", 
                                                                    "Near Threatened",
                                                                    "All Threatened",
                                                                    "Vulnerable",
                                                                    "Endangered",
                                                                    "Critically Endangered",
                                                                    "Extinct in the Wild",
                                                                    "Extinct"))

#Remove any rows where there have been no assessments either within the kingdom or within RL category as these should be NA
taxonNeedDF <- subset(taxonNeedDF, NumAssessWithinKingdom != 0 & NumAssessWithinRLcat != 0)


#Change order of kingdoms to "Animal", "Plant", "Fungi"
taxonNeedDF$KingdomName <- factor(taxonNeedDF$KingdomName, levels = c("ANIMALIA", "PLANTAE", "FUNGI"))

#Remove the "CHROMISTA" category before running beta regression --> too little data for beta regression
betaDF <- subset(taxonNeedDF, KingdomName != "CHROMISTA")

#Re-assign any zeros in the proportional data to 0.001 and 1's to 0.999 because those are not allowed in the beta regression
sum(betaDF$PropSppKingdomNoDD == 0) #93
sum(betaDF$PropSppKingdomNoDD == 1) #11
sum(betaDF$PropSppKingdomWithDD == 0) #93
sum(betaDF$PropSppKingdomWithDD == 1) #11
sum(betaDF$PropSppAllTaxaByCat == 0) #93
sum(betaDF$PropSppAllTaxaByCat == 1) #8

#Change all zeros to 0.0001 and all 1's to 0.9999 to work with the beta regressions
betaDF[betaDF == 0] <- 0.0001
betaDF[betaDF == 1] <- 0.9999

##Create an empty dataframe to add results of the beta regression analysis
betaResultsDF <- data.frame(Kingdom = character(),
                            RedListCat = character(),
                            Response = character(),
                            YearRange = character(),
                            Coefficient = character(),
                            Estimate = numeric(),
                            StdError = numeric(),
                            Zvalue = numeric(),
                            Pvalue = numeric(), 
                            PrecisionEst = numeric(),
                            PrecisionStdErr = numeric(),
                            PrecisionZval = numeric(),
                            PrecisionPval = numeric(), stringsAsFactors = F) 

#Remove the "CHROMISTA" category before running beta regression --> too little data for beta regression
betaDF <- subset(taxonNeedDF, KingdomName != "CHROMISTA")

responseVars <- c("PropSppKingdomNoDD", "PropSppAllTaxaByCat") #name the two main reponse vars (although the first will be swapped for "PropSppKingdomWithDD" for the DD category in the loop)

yearRanges <- c("All (1996 to 2019)", "AllThreeKingdom (2003 to 2019)", "Manual (2000 to 2015)")

uniqueRLcats <- unique(betaDF$RedListCategory)
uniqueRLcat <- uniqueRLcats[1] #for testing
for (uniqueRLcat in uniqueRLcats) {
  cat(paste0("Started RL cat ", uniqueRLcat), '\n')
  rlCatDF <- subset(betaDF, RedListCategory == uniqueRLcat)
  
  responseVar = responseVars[1] #for testing
  for(responseVar in responseVars) {
    cat(paste0("Started response ", responseVar, " in ", uniqueRLcat), '\n')
    if(uniqueRLcat == "Data Deficient" & responseVar == "PropSppKingdomNoDD") {
      responseVar <- "PropSppKingdomWithDD"
    }
    
    yearRange = yearRanges[1]
    for(yearRange in yearRanges) {
      cat(paste0("Started year range ", yearRange, " in ", responseVar, " - ", uniqueRLcat), '\n')
      
      #Get only the first part of the year range (for naming files)
      yearRangeShort <- sub("(\\w+).*", "\\1", yearRange)
      
      #Get the min and max year to extract
      yearRangeYears <- regmatches(yearRange, gregexpr("(?<=\\().*?(?=\\))", yearRange, perl=T))[[1]]
      minYear <- as.integer(sub("(\\w+).*", "\\1", yearRangeYears))
      maxYear <- as.integer(sub('^.* (\\w+)$', '\\1', yearRangeYears))
      
      #Get the data for just that year range
      yearRangeDF <- subset(rlCatDF, YearUpTo %in% minYear:maxYear)
      
      #Create a new column in the dataframe with transformed year (to make intercepts useful)
      yearRangeDF$YearZero <- yearRangeDF$YearUpTo - max(yearRangeDF$YearUpTo) #shift the intercept to the most recent data
      
      #Run an analysis of trends over time for each kingdom separately
      uniqueKingdoms <- unique(yearRangeDF$KingdomName)
      uniqueKingdom <- uniqueKingdoms[3]
      for (uniqueKingdom in uniqueKingdoms) {
        cat(paste0("Started  kingdom ", uniqueKingdom, " in ", yearRange, " - ", responseVar, " - ", uniqueRLcat), '\n')  
        
        kingdomDF <- subset(yearRangeDF, KingdomName == uniqueKingdom)
        
        #Assign all of the model variables
        betaResultsDF[nrow(betaResultsDF) + 1,] = NA
        betaResultsDF$Kingdom[nrow(betaResultsDF)] <- uniqueKingdom
        betaResultsDF$RedListCat[nrow(betaResultsDF)] <- uniqueRLcat
        betaResultsDF$Response[nrow(betaResultsDF)] <- responseVar
        betaResultsDF$YearRange[nrow(betaResultsDF)] <- yearRange
        betaResultsDF$Coefficient[nrow(betaResultsDF)] <- "Year"
        
        #Run a beta regresstion with year
        betaRegFormula <- paste0(responseVar, " ~ YearZero")
        
        if(class(try(betareg(formula = betaRegFormula, data = kingdomDF))) != "try-error") {
          kingBetaRegModel <- betareg(formula = betaRegFormula, data = kingdomDF)
          
          #Add just the information for year (as the intercept doesn't mean much), and use NAs if there are errors returned
          betaResultsDF$Estimate[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "mean.Estimate"]
          betaResultsDF$StdError[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "mean.Std..Error"]
          betaResultsDF$Zvalue[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "mean.z.value"]
          betaResultsDF$Pvalue[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "mean.Pr...z.."]
          betaResultsDF$PrecisionEst[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "precision.Estimate"]
          betaResultsDF$PrecisionStdErr[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "precision.Std..Error"]
          betaResultsDF$PrecisionZval[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "precision.z.value"]
          betaResultsDF$PrecisionPval[nrow(betaResultsDF)] <- as.data.frame(summary(kingBetaRegModel)$coef)["YearZero", "precision.Pr...z.."]
          
        }
      }
      
      ####MAYBE FIX THIS LATER IF NEEDED ####
      #Run the beta regression for this RL category, response, and year range 
      # betaRegFormula <- paste0(responseVar, " ~ YearZero * KingdomName")
      # respBetaRegModel <- betareg(formula = betaRegFormula, data = yearRangeDF)
      # respBetaRegDF <- as.data.frame(summary(respBetaRegModel)$coef)
      # respBetaRegDF$RedListCat <- uniqueRLcat
      # respBetaRegDF$Response <- responseVar
      # respBetaRegDF$YearRange <- yearRange
      # 
      # respBetaRegDF$Coefficient <- row.names(respBetaRegDF) #Change the row names to the Coefficient column
      # rownames(respBetaRegDF) <- c() #remove row names 
      # 
      # #rename columns to match the empty dataframe above
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "mean.Estimate"] <- "Estimate"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "mean.Std..Error"] <- "StdError"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "mean.z.value"] <- "Zvalue"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "mean.Pr...z.."] <- "Pvalue"
      # 
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "precision.Estimate"] <- "PrecisionEst"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "precision.Std..Error"] <- "PrecisionStdErr"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "precision.z.value"] <- "PrecisionZval"
      # colnames(respBetaRegDF)[colnames(respBetaRegDF) == "precision.Pr...z.."] <- "PrecisionPval"
      # 
      # #Merge (rbind) with the empty dataframe
      # betaResultsDF <- rbind(betaResultsDF, respBetaRegDF)
      
      # #Plot the effect sizes for that taxon
      # respEffectPlot <- plot_model(respBetaRegModel, transform = NULL, show.values = TRUE, value.offset = .3, rm.terms = "(phi)", 
      #                               vline.color = "black",
      #                               axis.title = "Estimate (log-odds)",
      #                               title = paste0("Beta regression effect sizes for conservation need - ", uniqueRLcat, ", ", yearRangeShort, " years")) + theme(panel.grid = element_blank())
      # 
      #Remove the spaces in the RL categories for naming the files
      uniqueRLcatShort <- gsub(pattern = " ", replacement = "", x = uniqueRLcat)
      # 
      # ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonNeedEffectSizes_", uniqueRLcatShort, "_", responseVar, "_", yearRangeShort, "Years_", dateNum, ".jpg"), plot = respEffectPlot, width = 10)
      # 
      
      ###Plot the trends over time for that red list category, responseVar, and year range
      if(responseVar %in% c("PropSppKingdomNoDD", "PropSppKingdomWithDD")) {
        subTitle <- paste0("Proportion of species within taxon that are ", uniqueRLcat)
        yAxisTitle <- paste0("Relative conservation need (proportion of species within taxon)")
      } else if (responseVar == "PropSppAllTaxaByCat") {
        subTitle <- paste0("Proportion of ", uniqueRLcat, " species that are within taxon")
        yAxisTitle <- paste0("Overall conservation need (proportion of species within Red List category)")
      } else {
        subTitle <- "Error"
        yAxisTitle <- "Error"
      }
      
      #Set the response variable category to the one of interest
      yearRangeDF$Response <- yearRangeDF[,responseVar]
      
      yearRangeNeedOverTimePlot <- ggplot(yearRangeDF, aes(x = YearUpTo, y = Response, group = KingdomName, color = KingdomName)) +
        geom_point() + geom_line() + 
        scale_colour_manual(values = cbp1) +
        stat_smooth(method = "betareg", se = F, formula = y~x, fullrange = F, linetype = "longdash") + 
        labs(title = "Conservation need over time by broad taxonomic category",
             subtitle = subTitle,
             x = "Year", 
             y = yAxisTitle) +
        theme(panel.grid = element_blank())
      
      ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonNeedOverTime_", uniqueRLcatShort, "_", responseVar, "_", yearRangeShort, "Years_", dateNum, ".jpg"), plot = yearRangeNeedOverTimePlot, width = 10, height = 7)
    }
  }
  cat(paste0("Ended RL cat ", uniqueRLcat), '\n')
}

#Find out how many of the trends are significant
test <- subset(betaResultsDF, Pvalue < 0.05 & RedListCat == "All Threatened")

##Save the analysis results for beta regression
write.csv(x = betaResultsDF, file = paste0(analysisDir, "ConsBioRev_BetaRegRes_TaxonNeedOverTimeBroad_", dateNum, ".csv"), row.names = F)

####3. Calculate effort vs. need (subtract and divide) over time, then analyze and plot
###Subset the effort data to only include the three equivalent taxa in the need dataset
taxonEffortMergeDF <- subset(taxonEffortDF, TaxonName %in% c("Animal", "Plant", "Fungi"))

###Subset the need data to only include the relevant years for the manual data, three taxa, and remove "All Categories"
taxonNeedMergeDF <- subset(taxonNeedDF, KingdomName %in% c("ANIMALIA", "PLANTAE", "FUNGI") & YearUpTo %in% c(2000:2015, 2019) & RedListCategory != "All Categories")

#Remove any where the either the number assessed within Kingdom or Number assesed within RLcat are zero
taxonNeedMergeDF <- subset(taxonNeedMergeDF, NumAssessWithinKingdom > 0 & NumAssessWithinRLcat > 0)

#Change the Kingdom names in the taxonNeed data to match the TaxonNames in the taxonEffort data
taxonNeedMergeDF$KingdomName[taxonNeedMergeDF$KingdomName == "ANIMALIA"] <- "Animal"
taxonNeedMergeDF$KingdomName[taxonNeedMergeDF$KingdomName == "PLANTAE"] <- "Plant"
taxonNeedMergeDF$KingdomName[taxonNeedMergeDF$KingdomName == "FUNGI"] <- "Fungi"
colnames(taxonNeedMergeDF)[colnames(taxonNeedMergeDF) == "KingdomName"] <- "TaxonName"

#Change the "YearUpTo" to "Year" in taxon need
colnames(taxonNeedMergeDF)[colnames(taxonNeedMergeDF) == "YearUpTo"] <- "Year"

#Change the 2019 year to "AllYears" in the need to march the effort
taxonNeedMergeDF$Year[taxonNeedMergeDF$Year == 2019] <- "AllYears"

##Fill in the missing years (2001 = 2000 & 2005 = 2004) - that would be the perceived effort in that year
#Extract the data just for 2000 and 2004
missYrsDF <- subset(taxonNeedMergeDF, Year %in% c("2000", "2004"))

#Change the 2000 to 2001 and 2004 to 2005 before adding back to the need data
missYrsDF$Year[missYrsDF$Year == "2000"] <- "2001"
missYrsDF$Year[missYrsDF$Year == "2004"] <- "2005"

#Add the missing years back to the taxon need
taxonNeedMergeDF <- rbind(taxonNeedMergeDF, missYrsDF)

###Merge the need and effort data
taxonEffortNeedDF <- merge(taxonEffortMergeDF, taxonNeedMergeDF, by = c("TaxonName", "Year"))

####Calculate need vs. effort metrics
#First change the need metrics from zero to a really small number to prevent Inf results
taxonEffortNeedDF$PropSppKingdomNoDD[taxonEffortNeedDF$PropSppKingdomNoDD == 0] <- 0.001
taxonEffortNeedDF$PropSppKingdomWithDD[taxonEffortNeedDF$PropSppKingdomWithDD == 0] <- 0.001
taxonEffortNeedDF$PropSppAllTaxaByCat[taxonEffortNeedDF$PropSppAllTaxaByCat == 0] <- 0.001

###Need - effort
##For the three taxa of interest and proportional for all spp within the RL category (e.g. proportion of all threatened species on the RL that are animals)
#Calculate and check range
taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat <- taxonEffortNeedDF$PropEffortSpecificTaxa - taxonEffortNeedDF$PropSppAllTaxaByCat 
range(taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat) #-0.8 to 0.8

#Standardize and check range
taxonEffortNeedDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat <- rescale(x = taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat)
range(taxonEffortNeedDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat) #0 to 1

##For the three taxa of interest and proportional for all spp within taxa (e.g. proportional of animals on the RL that are in a threatened category)
#First need to standardize the proportional need so that the sum of all three taxa adds to 1 (need to do separately for each journal, year, and rl category)

#Calculate and check range
taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat <- taxonEffortNeedDF$PropEffortSpecificTaxa - taxonEffortNeedDF$PropSppAllTaxaByCat 
range(taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat) #-0.8 to 0.8

#Standardize and check range
taxonEffortNeedDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat <- rescale(x = taxonEffortNeedDF$EffortMinNeed_EffortPropThreeTaxa_NeedPropRLcat)
range(taxonEffortNeedDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat) #0 to 1


###Need / Effort 
##For the three taxa of interest and proportional for all spp within the RL category
#Calculate and check range
taxonEffortNeedDF$EffortDivNeed_EffortPropThreeTaxa_NeedPropRLcat <- taxonEffortNeedDF$PropEffortSpecificTaxa / taxonEffortNeedDF$PropSppAllTaxaByCat 
range(taxonEffortNeedDF$EffortDivNeed_EffortPropThreeTaxa_NeedPropRLcat, na.rm = T) #0 to 1251

#Standardize and check range - first change from 0:1 to 0:0.5 
taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat <- taxonEffortNeedDF$EffortDivNeed_EffortPropThreeTaxa_NeedPropRLcat
taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat[taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat <= 1] <- rescale(x = taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat[taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat <= 1], to = c(0, 0.5))
taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat[taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat >= 0.5] <- rescale(x = taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat[taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat >= 0.5], to = c(0.5, 1))
range(taxonEffortNeedDF$EffortDivNeedStand_EffortPropThreeTaxa_NeedPropRLcat) #0 to 1

####Analyze the trends using beta regression
###Beta regression with the all threatened category without all years but including all journals
betaDF <- subset(taxonEffortNeedDF, RedListCategory == "All Threatened" & Year != "AllYears")
betaDF$Year <- as.integer(betaDF$Year)
betaDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat[betaDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat == 0] <- 0.001
betaDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat[betaDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat == 1] <- 0.999

effortMinNeedBetaRegModel <- betareg(formula = EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat ~ Year * TaxonName * Journal, data = betaDF)

###Plot the effect sizes from the model
fullEffectPlot <- plot_model(effortMinNeedBetaRegModel, show.values = TRUE, value.offset = .3, rm.terms = "(phi)", 
                             vline.color = "black",
                             axis.title = "Estimate (transformed)",
                             title = "Beta regression effect sizes predicting conservation effort minus need by taxon") + theme(panel.grid = element_blank())

ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortFullEffectSizes_AllTaxon_", dateNum, ".jpg"), plot = fullEffectPlot, width = 10, height = 10)



####Plot the relationships by journal as above in the effort data
#Plot the effort vs. need over time for each broad taxonomic category (colors) with journals as facets
plotDF <- subset(taxonEffortNeedDF, RedListCategory == "All Threatened" & Year != "AllYears")
plotDF$Year <- as.integer(plotDF$Year)
plotDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat[plotDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat == 0] <- 0.001
plotDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat[plotDF$EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat == 1] <- 0.999

taxonEffortMinNeedOverTimePlot_FacetByJournal <- ggplot(plotDF, aes(x = Year, y = EffortMinNeedStand_EffortPropThreeTaxa_NeedPropRLcat, group = TaxonName, color = TaxonName)) + 
  geom_point() + geom_line() + 
  scale_colour_manual(values = cbp1) +
  facet_wrap(~Journal, ncol = 3, scales = "free_x") +
  stat_smooth(method = "betareg", se = F, formula = y~x, fullrange = F, linetype = "longdash") + 
  labs(title = "Perceived conservation effort vs. need over time by broad taxonomic category",
       subtitle = "Proportion of papers (effort) and proportion of threatened species on IUCN Red List (need) from 2000-2015",
       x = "Year", 
       y = "Conservation effort - need") +
  geom_hline(yintercept = 0.5, color = "grey") +
  theme(panel.grid = element_blank())


ggsave(filename = paste0(plotDir, "ConsBioRev_TaxonEffortMinNeed_ByJournalFacet_", dateNum, ".jpg"), plot = taxonEffortMinNeedOverTimePlot_FacetByJournal, width = 10, height = 7)


####3. Add trend lines for each and perhaps statistics
####4. Add dashed line for the most current need (one horizontal line)
