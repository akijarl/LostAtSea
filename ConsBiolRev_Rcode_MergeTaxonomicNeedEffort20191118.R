####CONSERVATION BIOLOGY REVIEW PAPER ANALYSIS 
#### Code to merge conservation need and effort by broad taxonomic category (animal, plant, fungi) based on IUCN and manual paper data ####
rm(list = ls()) #remove past stored objects

####Load packages/ libraries ####
library(ggplot2)
library(ggpubr)

####Set original parameters ####effortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot
dateNum = as.character(Sys.Date())

####Set folder locations ####
taxaNeedDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/IUCNdata/"
taxaEffortDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyData/"
plotDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyPlots/"


####Open the kingdom need and broad toxonomic effort data####
taxaNeedDF <- read.csv(file = paste0(taxaNeedDir, "ConReview_KingdomNeed_ByYearIUCNcat_2019-10-02.csv"), stringsAsFactors = F) #988 obs of 9 variables
taxaEffortDF <- read.csv(file = paste0(taxaEffortDir, "ConBioRev_TaxonEffortBroad_ByYearJournalAndAltogether_2019-11-18.csv"), stringsAsFactors = F) #532 obs of 6 variables

####Explore the datasets ####
head(taxaNeedDF)
head(taxaEffortDF)

unique(taxaNeedDF$YearUpTo)
#[1] 1996 1998 2000 2002 2003 2004 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018

unique(taxaNeedDF$KingdomName)
#[1] "ANIMALIA"  "PLANTAE"   "FUNGI"     "CHROMISTA"

####Plot the effort data over time ####
effortPlotDF <- subset(taxaEffortDF, Year != "AllYears" & TaxonName %in% c("Animal", "Plant", "Fungi"))
effortPlotDF$Year <- as.integer(effortPlotDF$Year)

effortByTimeTaxon <- ggplot(effortPlotDF, aes(x = Year, y = PropEffortAllTaxa*100)) + 
  geom_line(aes(color = TaxonName)) + geom_point(aes(color = TaxonName)) +
  geom_hline(yintercept = taxaNeedDF$PropSppKingdomNoDD[taxaNeedDF$YearUpTo == 2018 & taxaNeedDF$KingdomName == "ANIMALIA" & taxaNeedDF$RedListCategory == "All Threatened"]*100, linetype="dashed", color = "red") + #Add dashed red line for "Animalia"
  geom_hline(yintercept = taxaNeedDF$PropSppKingdomNoDD[taxaNeedDF$YearUpTo == 2018 & taxaNeedDF$KingdomName == "FUNGI" & taxaNeedDF$RedListCategory == "All Threatened"]*100, linetype="dashed", color = "forest green") + #Add dashed green line for "Fungi"
  geom_hline(yintercept = taxaNeedDF$PropSppKingdomNoDD[taxaNeedDF$YearUpTo == 2018 & taxaNeedDF$KingdomName == "PLANTAE" & taxaNeedDF$RedListCategory == "All Threatened"]*100, linetype="dashed", color = "blue") + #Add dashed blue line for "Fungi"
  
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  facet_wrap(~Journal, ncol = 3) +
  #stat_smooth(aes(color = TaxonName), method = "lm", formula = y~x) +
  #stat_cor(method = "spearman", aes(color = TaxonName, label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Conservation research effort by taxon over time (2000 - 2015)",
       subtitle = "% of papers focused on taxon",
       x = "Year", 
       y = "Conservation effort (% of papers)")

ggsave(filename = paste0(plotDir, "ConsBioRev_BroadTaxonEffortOverTime_ByTaxon_JournalAsFacet", dateNum, ".jpg"), plot = effortByTimeTaxon, width = 7, height = 4)

####Calculate the relative proportion between animals, plants, and fungi for the effort database ####
###for each year and journal
#Create new column in the dataset
taxaEffortDF$PropEffortAnimalPlantFungi <- NA

taxaOfInterest <- c("Animal", "Plant", "Fungi")
uniqueYears <- unique(taxaEffortDF$Year)

uniqueYear = uniqueYears[1]
for (uniqueYear in uniqueYears) {
  message("Started year ", uniqueYear)
  #subset the dataset for that year
  yearDF <- subset(taxaEffortDF, Year == uniqueYear)
  uniqueJournals <- unique(yearDF$Journal)
  
  uniqueJournal = uniqueJournals[1]
  for (uniqueJournal in uniqueJournals) {
    message("Started journal ", uniqueJournal)
    #Subset the journal of interest
    journalDF <- subset(yearDF, Journal == uniqueJournal)
    
    #Get the total proportion for the three taxa of interest
    totalProp <- sum(journalDF$PropEffortAllTaxa[journalDF$TaxonName %in% taxaOfInterest])
    
    #Assign the new values to the original dataset
    taxon = taxaOfInterest[1]
    for (taxon in taxaOfInterest) {
      taxaEffortDF$PropEffortAnimalPlantFungi[taxaEffortDF$TaxonName == taxon & taxaEffortDF$Year == uniqueYear & taxaEffortDF$Journal == uniqueJournal] <- taxaEffortDF$PropEffortAllTaxa[taxaEffortDF$TaxonName == taxon & taxaEffortDF$Year == uniqueYear & taxaEffortDF$Journal == uniqueJournal]/totalProp
    }
    message("Finished journal ", uniqueJournal)
  }
  
}

####Add the proportional numbers for conservation need (most recent data) ####



####Calculate a mean, variation, and trend for effort, then ass the total number of papers prop effort for all taxa and specifc taxa####
###i.e. for all years, by journal and taxa
unique(taxaEffortDF$TaxonName)



####Calculate an overall need (from all years) ####
###i.e. for all years, by red list category. 


table(taxaNeedDF$KingdomName)
# ANIMALIA CHROMISTA     FUNGI   PLANTAE 
# 247       247       247       247 

table(taxaEffortDF$TaxonName)
# Animal          Fungi        Microbe       Multiple Not applicable          Other          Plant 
# 76             76             76             76             76             76             76 

