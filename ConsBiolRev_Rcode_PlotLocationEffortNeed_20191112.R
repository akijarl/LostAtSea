###CONSERVATION BIOLOGY REVIEW PAPER ANALYSIS 
###Code to plot the results of location (country-level) effort and need ####
### based on manually searched papers (effort) and relative IUCN threat status (need)
rm(list = ls()) #remove past stored objects

options(scipen=999) #Get rid of scientific notation

####Load packages/ libraries ####
library(ggplot2)
library(plyr)
library(rworldmap)
library(RColorBrewer)
library(ggpubr)
library(scales)

####Set original parameters and directories ####
dateNum = as.character(Sys.Date())

dataDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Location/LocationData/"
plotDir <- "C:/Users/jc690391/OneDrive - James Cook University/Documents/ConsBioReview/ConsBioRev_Analysis/Location/LocationPlots/"

####Open the location effort and need data ####
locEffortNeedDF <- read.csv(file = paste0(dataDir, "ConReview_CountryEffortNeed_ByYearJournalRlCat_Rescaled_2019-11-13.csv"), stringsAsFactors = F) #206492 obs of 38 variables

####Plot effort vs. location ####
str(locEffortNeedDF)
unique(locEffortNeedDF$Year)
# [1] "2000"     "2001"     "2002"     "2003"     "2004"     "2005"     "2006"     "2007"     "2008"     "2009"    
# [11] "2010"     "2011"     "2012"     "2013"     "2014"     "2015"     "AllYears"

unique(locEffortNeedDF$Journal)
# [1] "BiodiversityandConservation" "AllJournals"                 "BiologicalConservation"     
# [4] "ConservationBiology"         "ConservationLetters"         

####Create box plot of effort vs. need ####
###with the numbers for all years for the effort (as replicates)
###but only using one number for need (total)
#create dataset with just the "All Threatened" category
plotDF <- subset(locEffortNeedDF, RedListCategory == "All Threatened" & Journal == "AllJournals" & Year != "AllYears")

#Summarize the data to get means and ranges
plotSumDF <- ddply(plotDF, c("Alpha.3.code", "EffortCountryName", "Continent", "MeanHDI_2000to2015"), summarise,
                   MeanNeed = mean(RecentNeed, na.rm = T),
                   MeanNeedRescaled = mean(RecentNeed_Rescaled, na.rm = T),
                   MeanEffort = mean(PropEffort, na.rm = T),
                   MeanEffortRescaled = mean(PropEffort_Rescaled, na.rm = T),
                   MinRescaledEffort = min(PropEffort_Rescaled, na.rm = T),
                   MaxRescaledEffort = max(PropEffort_Rescaled, na.rm = T),
                   MinEffort = min(PropEffort, na.rm = T),
                   MaxEffort = max(PropEffort, na.rm = T),
                   MeanEffortMinusNeed = mean(RescaledEffortMinusNeed, na.rm = T),
                   SDrescaledEffort = sd(PropEffort_Rescaled, na.rm = T),
                   SDeffort = sd(PropEffort, na.rm = T),
                   SlopeEst = mean(SlopeEst_2000to2015_PropSppCountryNoDD, na.rm = T),
                   SigEffortMinMean = max(SigEffortMinMean, na.rm = T),
                   SigPosEffortSlope = mean(SigPosEffortSlope, na.rm = T),
                   SigNegEffortSlope = mean(SigNegEffortSlope, na.rm = T))

##Plot the relationship with error bars for effort, points colored by if effort > need or need < effort, and arrows for trends
#For all threatened species
effortVsNeedAllThreatenedCountryNoDD_ColByHDI_plot <- ggplot(plotSumDF, aes(x = MeanNeedRescaled, y = MeanEffortRescaled, color = MeanHDI_2000to2015)) + 
  geom_errorbar(aes(ymin = MinRescaledEffort, ymax = MaxRescaledEffort)) + scale_colour_gradient(low = "dark red", high = "dark green") +
  geom_abline(slope = 1, intercept = 0, colour = "blue", linetype = "dashed") +
  #geom_segment(aes(x = MeanNeedRescaled, y = MeanEffortRescaled, xend = MeanNeedRescaled, yend = MeanEffortRescaled + SigNegEffortSlope), arrow = arrow(length = unit(0.1,"cm"), type = "closed"), arrow.fill = "red") +
  #geom_segment(aes(x = MeanNeedRescaled, y = MeanEffortRescaled, xend = MeanNeedRescaled, yend = MeanEffortRescaled + SigPosEffortSlope), arrow = arrow(length = unit(0.1,"cm"), type = "closed"), arrow.fill = "green") +
  geom_point(size = 2) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  labs(title = "Conservation effort and need by country (colored by human development index)",
       subtitle = "Mean and ranges of effort values from 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (Relative proportion of threatened species within country)", 
       y = "Conservation effort (Relative proportion of papers)",
       color = "HDI")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeedAllThreatenedCountryNoDD_ColByHDI_Plot", dateNum, ".jpg"), plot = effortVsNeedAllThreatenedCountryNoDD_ColByHDI_plot, width = 7.5, height = 6.5)


effortVsNeedAllThreatenedCountryNoDD_ColByEffortMinusNeed_plot <- ggplot(plotSumDF, aes(x = MeanNeed * 100, y = MeanEffort * 100, color = MeanEffortMinusNeed)) + 
  geom_errorbar(aes(ymin = MinEffort * 100, ymax = MaxEffort * 100)) + 
  geom_point(size = 2) + scale_colour_gradient(low = "dark red", high = "dark green", limits = c(-1,1)) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  labs(title = "Conservation effort and need by country (coloured by standardized differences)",
       subtitle = "Mean and ranges of effort 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (% threatened species within country)", 
       y = "Conservation effort (% papers)",
       color = "Effort - Need")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeedAllThreatenedCountryNoDD_ColByEffortMinNeed_Plot", dateNum, ".jpg"), plot = effortVsNeedAllThreatenedCountryNoDD_ColByEffortMinusNeed_plot, width = 7.5, height = 6.5)


####Repeat these for the DD species ####
#create dataset with just the "Data Deficient" category
plotDF <- subset(locEffortNeedDF, RedListCategory == "Data Deficient" & Journal == "AllJournals" & Year != "AllYears")

#Summarize the data to get means and ranges
plotSumDF <- ddply(plotDF, c("Alpha.3.code", "EffortCountryName", "Continent", "MeanHDI_2000to2015"), summarise,
                   MeanNeed = mean(RecentNeed, na.rm = T),
                   MeanNeedRescaled = mean(RecentNeed_Rescaled, na.rm = T),
                   MeanEffort = mean(PropEffort, na.rm = T),
                   MeanEffortRescaled = mean(PropEffort_Rescaled, na.rm = T),
                   MinRescaledEffort = min(PropEffort_Rescaled, na.rm = T),
                   MaxRescaledEffort = max(PropEffort_Rescaled, na.rm = T),
                   MinEffort = min(PropEffort, na.rm = T),
                   MaxEffort = max(PropEffort, na.rm = T),
                   MeanEffortMinusNeed = mean(RescaledEffortMinusNeed, na.rm = T),
                   SDrescaledEffort = sd(PropEffort_Rescaled, na.rm = T),
                   SDeffort = sd(PropEffort, na.rm = T),
                   SlopeEst = mean(SlopeEst_2000to2015_PropSppCountryWithDD, na.rm = T),
                   SigEffortMinMean = max(SigEffortMinMean, na.rm = T),
                   SigPosEffortSlope = mean(SigPosEffortSlope, na.rm = T),
                   SigNegEffortSlope = mean(SigNegEffortSlope, na.rm = T))

effortVsNeed_DD_ColByHDI_plot <- ggplot(plotSumDF, aes(x = MeanNeedRescaled, y = MeanEffortRescaled, color = MeanHDI_2000to2015)) + 
  geom_errorbar(aes(ymin = MinRescaledEffort, ymax = MaxRescaledEffort)) + scale_colour_gradient(low = "dark red", high = "dark green") +
  geom_abline(slope = 1, intercept = 0, colour = "blue", linetype = "dashed") +
  #geom_segment(aes(x = MeanNeedRescaled, y = MeanEffortRescaled, xend = MeanNeedRescaled, yend = MeanEffortRescaled + SigNegEffortSlope), arrow = arrow(length = unit(0.1,"cm"), type = "closed"), arrow.fill = "red") +
  #geom_segment(aes(x = MeanNeedRescaled, y = MeanEffortRescaled, xend = MeanNeedRescaled, yend = MeanEffortRescaled + SigPosEffortSlope), arrow = arrow(length = unit(0.1,"cm"), type = "closed"), arrow.fill = "green") +
  geom_point(size = 2) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  labs(title = "Conservation effort and need by country (colored by human development index)",
       subtitle = "Mean and ranges of effort values from 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (Relative proportion of data deficient species within country)", 
       y = "Conservation effort (Relative proportion of papers)",
       color = "HDI")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeed_DD_ColByHDI_Plot", dateNum, ".jpg"), plot = effortVsNeed_DD_ColByHDI_plot, width = 7.5, height = 6.5)


effortVsNeed_DD_ColByEffortMinusNeed_plot <- ggplot(plotSumDF, aes(x = MeanNeed * 100, y = MeanEffort * 100, color = MeanEffortMinusNeed)) + 
  geom_errorbar(aes(ymin = MinEffort * 100, ymax = MaxEffort * 100)) + 
  geom_point(size = 2) + scale_colour_gradient(low = "dark red", high = "dark green", limits = c(-1,1)) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  labs(title = "Conservation effort and need by country (coloured by standardized differences)",
       subtitle = "Mean and ranges of effort 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (% data deficient species within country)", 
       y = "Conservation effort (% papers)",
       color = "Effort - Need")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeed_DD_ColByEffortMinNeed_Plot", dateNum, ".jpg"), plot = effortVsNeed_DD_ColByEffortMinusNeed_plot, width = 7.5, height = 6.5)




##Start with simple plot with all years and journals as panels (continent as color)
plotDF <- subset(locEffortNeedDF, Year == "AllYears" & RedListCategory == "All Threatened" & !is.na(Journal))

#Plot the need as proportion of threatened species within country (log-log scale) with spearman rank values
effortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot <- ggplot(plotDF, aes(x = PropSppCountryNoDD*100, y = PropEffort*100)) + geom_point() +
  facet_wrap(~Journal, ncol = 3) +
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Conservation effort and need for each country",
       subtitle = "% of papers (effort) and threatened species (need) from 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (% species within country that are threatened; log scale)", 
       y = "Conservation effort (% of papers; log scale)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeedAllThreatenedCountryNoDD_ByJournal_", dateNum, ".jpg"), plot = effortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot)

#Plot the same but with need as the proportion of vulnerable species globally
effortVsNeedAllThreatenedGlobalCat_AllYearsByJournalPlot <- ggplot(plotDF, aes(x = PropSppGlobalByCat*100, y = PropEffort*100)) + geom_point() +
  facet_wrap(~Journal, ncol = 3) +
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Conservation effort and need for each country",
       subtitle = "% of papers (effort) and threatened species (need) from 2000-2015 (with spearman rank correlation values)",
       x = "Conservation need (% global threatened species; log scale)", 
       y = "Conservation effort (% papers; log scale)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationEffortVsNeedAllThreatenedGlobalCat_ByJournal_", dateNum, ".jpg"), plot = effortVsNeedAllThreatenedGlobalCat_AllYearsByJournalPlot)

#### Plot the change in effort vs. change in need ####
changeEffortVsChangeNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot <- ggplot(plotDF, aes(x = SlopeEst_2000to2015_PropSppCountryNoDD*100, y = SlopeEst*100)) + geom_point() +
  facet_wrap(~Journal, ncol = 3, scales = "free") +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Change in conservation effort and need for each country (2000-2015)",
       subtitle = "Change in the % of papers (effort) and % threatened species (need) with spearman rank correlation values",
       x = "Change in conservation need (% species within country that are threatened per year)", 
       y = "Change in conservation effort (% papers per year)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationChangeEffortVsChangeNeedAllThreatenedCountryNoDD_ByJournal_", dateNum, ".jpg"), plot = changeEffortVsChangeNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot)

changeEffortVsChangeNeedAllThreatenedGlobalCat_AllYearsByJournalPlot <- ggplot(plotDF, aes(x = SlopeEst_2000to2015_PropSppGlobalByCat*100, y = SlopeEst*100)) + geom_point() +
  facet_wrap(~Journal, ncol = 3, scales = "free") +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Change in conservation effort and need for each country (2000-2015)",
       subtitle = "Change in the % of papers (effort) and % threatened species (need) with spearman rank correlation values",
       x = "Change in conservation need (% global threatened species per year)", 
       y = "Change in conservation effort (% papers per year)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationChangeEffortVsChangeNeedAllThreatenedGlobalCat_ByJournal_", dateNum, ".jpg"), plot = changeEffortVsChangeNeedAllThreatenedGlobalCat_AllYearsByJournalPlot)

####Repeat the plots of effort vs. need with ranks for all journals only
plotDF2 <- subset(plotDF, Journal == "AllJournals")
plotDF2$EffortRank <- rank(x = plotDF2$PropEffort, ties.method = "min")
plotDF2$NeedRankCountryNoDD <- rank(x = plotDF2$PropSppCountryNoDD, ties.method = "min")
plotDF2$NeedRankGlobalCat <- rank(x = plotDF2$PropSppGlobalByCat, ties.method = "min")

#Plot the need as ranked proportion of threatened species within country with spearman rank values
rankedEffortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot <- ggplot(plotDF2, aes(x = NeedRankCountryNoDD, y = EffortRank)) + geom_point() +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Conservation effort and need for each country (ranked)",
       subtitle = "Ranked % of papers (effort) and threatened species (need) from 2000-2015 (with spearman correlation values)",
       x = "Conservation need (ranked % species within country that are threatened)", 
       y = "Conservation effort (ranked % of papers)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationRankedEffortVsNeedAllThreatenedCountryNoDD_ByJournal_", dateNum, ".jpg"), plot = rankedEffortVsNeedAllThreatenedCountryNoDD_AllYearsByJournalPlot)

#Plot the same but with need as the proportion of vulnerable species globally
rankedEffortVsNeedAllThreatenedGlobalCat_AllYearsByJournalPlot <- ggplot(plotDF2, aes(x = NeedRankGlobalCat, y = EffortRank)) + geom_point() +
  stat_smooth(method = "lm", formula = y~x, fullrange = T) + coord_cartesian(clip = "off") +
  stat_cor(method = "spearman", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "Conservation effort and need for each country (ranked)",
       subtitle = "Ranked % of papers (effort) and threatened species (need) from 2000-2015 (with spearman correlation values)",
       x = "Conservation need (ranked % global threatened species)", 
       y = "Conservation effort (ranked % of papers)")

ggsave(filename = paste0(plotDir, "ConsBioRev_LocationRankedEffortVsNeedAllThreatenedGlobalCat_ByJournal_", dateNum, ".jpg"), plot = rankedEffortVsNeedAllThreatenedGlobalCat_AllYearsByJournalPlot)

####Create map of the effort and need results ####

##First for all journals, all years, and "All Threatened" red list category
mapDF <- plotDF2
  
#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(mapDF, joinCode = "NAME", nameJoinColumn = "Country")

jpeg(filename = paste0(plotDir, "ConsBioRev_LocationEffortHeatmap_", dateNum, ".jpg"))
mapCountryData(spdf, nameColumnToPlot = "PropEffort", catMethod = "logFixedWidth", 
               colourPalette = brewer.pal(n = 7, name = "RdYlGn"), mapTitle = "Conservation effort by country (2000-2015)")

dev.off()