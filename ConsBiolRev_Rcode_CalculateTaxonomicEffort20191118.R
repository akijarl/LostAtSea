####CONSERVATION BIOLOGY REVIEW PAPER ANALYSIS - Taxonomy 
###Code to calculate broad taxomonic effort using the manual dataset ####
rm(list = ls()) #remove past stored objects

####Load packages/ libraries ####

####Set original parameters ####
dateNum = as.character(Sys.Date())

####Set folder locations ####
manDataDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/AllSourcesAllJournals/"
taxonDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Taxonomy/TaxonomyData/"

####Open files ####
manDF <- read.csv(file = paste0(manDataDir, "Con_Review_Manual_Datasets_All_20190717.csv"), stringsAsFactors = F) #3210 obs of 21 variables

#### Check the categories and fix ####
table(manDF$Taxon..General.)
# Animal (excluding single-celled Animals)                     Fungi (including microscopic) 
#                                     1850                                                30 
# Microbe (including Protozoa, Bacteria and Archea)          Multiple (please clarify in Note column) 
#                                       6                                               302 
# Not Applicable (NA)             Other (please clarify in Note column) 
#                 290                                                25 
# Plant (including algae) 
#                 707

#Create a new column with shorter titles
manDF$TaxonGeneralShort <- NA
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Animal (excluding single-celled Animals)"] <- "Animal"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Fungi (including microscopic)"] <- "Fungi"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Microbe (including Protozoa, Bacteria and Archea)"] <- "Microbe"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Multiple (please clarify in Note column)"] <- "Multiple"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Not Applicable (NA)"] <- "Not applicable"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Other (please clarify in Note column)"] <- "Other"
manDF$TaxonGeneralShort[manDF$Taxon..General. == "Plant (including algae)"] <- "Plant"

sum(is.na(manDF$Taxon..General.)) #0

table(manDF$TaxonGeneralShort)
# Animal          Fungi        Microbe       Multiple Not applicable          Other          Plant 
# 1850             30              6            302            290             25            707

#### Calculate the percentage of papers that focused on each taxonomic category for each year (and all years) and each journal (and all journals) ####
###Create a new dataframe for the results
taxonEffortDF <- data.frame(TaxonName = character(),
                            Year = integer(),
                            Journal = character(),
                            NumPapers = integer(),
                            PropEffortAllTaxa = numeric(),
                            PropEffortSpecificTaxa = numeric(),
                            stringsAsFactors = F)

###Get unique categories for taxa, year, and journal
uniqueTaxa <- unique(manDF$TaxonGeneralShort) 
specificTaxa <- c("Animal", "Fungi", "Microbe", "Plant")
uniqueYears <- sort(unique(manDF$Year))
uniqueJournals <- unique(manDF$Source.title)

### Run loop to fill in dataframe
uniqueTaxon <- uniqueTaxa[1] #for testing
for (uniqueTaxon in uniqueTaxa) {
  message("Started taxon ", uniqueTaxon)
  
  #Subset the data for the specified taxa and just that taxon
  specificTaxaDF <- subset(manDF, TaxonGeneralShort %in% specificTaxa)
  taxonDF <- subset(manDF, TaxonGeneralShort == uniqueTaxon)
  
  #add new line to the dataframe
  taxonEffortDF[nrow(taxonEffortDF) + 1,] <- NA
  
  #fill in fields for all years and all journals
  taxonEffortDF$TaxonName[nrow(taxonEffortDF)] <- uniqueTaxon
  taxonEffortDF$Year[nrow(taxonEffortDF)] <- "AllYears"
  taxonEffortDF$Journal[nrow(taxonEffortDF)] <- "AllJournals"
  taxonEffortDF$NumPapers[nrow(taxonEffortDF)] <- nrow(taxonDF)
  taxonEffortDF$PropEffortAllTaxa[nrow(taxonEffortDF)] <- nrow(taxonDF)/nrow(manDF)
  taxonEffortDF$PropEffortSpecificTaxa[nrow(taxonEffortDF)] <- nrow(taxonDF)/nrow(specificTaxaDF)
  
  ##Run another sub-loop for all years but each journal
  uniqueJournal <- uniqueJournals[1]
  for (uniqueJournal in uniqueJournals) {
    message("Started journal ", uniqueJournal, " for all years")
    
    #Subset all of the data by year and just the taxon for that year
    journalAllTaxaDF <- subset(manDF, Source.title == uniqueJournal)
    journalSpecificTaxaDF <- subset(journalAllTaxaDF, TaxonGeneralShort %in% specificTaxa)
    journalTaxonDF <- subset(journalAllTaxaDF, TaxonGeneralShort == uniqueTaxon)
    
    #add new line to the dataframe
    taxonEffortDF[nrow(taxonEffortDF) + 1,] <- NA
    
    #fill in fields for all years and all journals
    taxonEffortDF$TaxonName[nrow(taxonEffortDF)] <- uniqueTaxon
    taxonEffortDF$Year[nrow(taxonEffortDF)] <- "AllYears"
    taxonEffortDF$Journal[nrow(taxonEffortDF)] <- uniqueJournal
    taxonEffortDF$NumPapers[nrow(taxonEffortDF)] <- nrow(journalTaxonDF)
    taxonEffortDF$PropEffortAllTaxa[nrow(taxonEffortDF)] <- nrow(journalTaxonDF)/nrow(journalAllTaxaDF)
    taxonEffortDF$PropEffortSpecificTaxa[nrow(taxonEffortDF)] <- nrow(journalTaxonDF)/nrow(journalSpecificTaxaDF)
    
  }
  
  ##Now run sub-loop for each year but all journals
  uniqueYear <- uniqueYears[1]
  for (uniqueYear in uniqueYears) {
    message("Started year ", uniqueYear, " for all journals")
    
    #Subset all of the data by year and just the taxon for that year
    yearAllTaxaDF <- subset(manDF, Year == uniqueYear)
    yearSpecificTaxaDF <- subset(yearAllTaxaDF, TaxonGeneralShort %in% specificTaxa)
    yearTaxonDF <- subset(yearAllTaxaDF, TaxonGeneralShort == uniqueTaxon)
    
    #add new line to the dataframe
    taxonEffortDF[nrow(taxonEffortDF) + 1,] <- NA
    
    #fill in fields for all years and all journals
    taxonEffortDF$TaxonName[nrow(taxonEffortDF)] <- uniqueTaxon
    taxonEffortDF$Year[nrow(taxonEffortDF)] <- uniqueYear
    taxonEffortDF$Journal[nrow(taxonEffortDF)] <- "AllJournals"
    taxonEffortDF$NumPapers[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)
    taxonEffortDF$PropEffortAllTaxa[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)/nrow(yearAllTaxaDF)
    taxonEffortDF$PropEffortSpecificTaxa[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)/nrow(yearSpecificTaxaDF)
    
  }
  
  ##Run one last loop for each combination of journal and taxa
  uniqueJournal <- uniqueJournals[1]
  for (uniqueJournal in uniqueJournals) {
    journalDF <- subset(manDF, Source.title == uniqueJournal)
    
    uniqueJournalYears <- sort(unique(journalDF$Year))
    
    uniqueYear <- uniqueJournalYears[1]
    for (uniqueYear in uniqueJournalYears) {
      message("Started journal ", uniqueJournal," and year ", uniqueYear)
      
      #Subset all of the journal data by year and just the taxon for that year
      yearAllTaxaDF <- subset(journalDF, Year == uniqueYear)
      yearSpecificTaxaDF <- subset(yearAllTaxaDF, TaxonGeneralShort %in% specificTaxa)
      yearTaxonDF <- subset(yearAllTaxaDF, TaxonGeneralShort == uniqueTaxon)
      
      #add new line to the dataframe
      taxonEffortDF[nrow(taxonEffortDF) + 1,] <- NA
      
      #fill in fields for all years and all journals
      taxonEffortDF$TaxonName[nrow(taxonEffortDF)] <- uniqueTaxon
      taxonEffortDF$Year[nrow(taxonEffortDF)] <- uniqueYear
      taxonEffortDF$Journal[nrow(taxonEffortDF)] <- uniqueJournal
      taxonEffortDF$NumPapers[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)
      taxonEffortDF$PropEffortAllTaxa[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)/nrow(yearSpecificTaxaDF)
      taxonEffortDF$PropEffortSpecificTaxa[nrow(taxonEffortDF)] <- nrow(yearTaxonDF)/nrow(yearAllTaxaDF)
      
    }
    
  }
  
}

####Save the conservation effort by broad taxonomy data ###
write.csv(x = taxonEffortDF, file = paste0(taxonDir, "ConBioRev_TaxonEffortBroad_ByYearJournalAndAltogether_", dateNum, ".csv"), row.names = F)
