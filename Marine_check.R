#Analysis of the marine habitat data from the manually curated data set
#
# There appears to be a slight dicrepancy between the number of article each year
#setwd("E:/Research_AJL/Conservation review/Completed/Completed_new/Checked/")
setwd("~/Documents/Conservation_review_updated/Completed/Completed_new/Checked/")

require(tidyverse)
require(readxl)
require(data.table)
require(cowplot)

#View IUCN information
MIUCN<-read.csv("Species_Country/IUCN_marine_all years.csv")
plot(as.factor(MIUCN$Year.assessed))
summary(as.factor(MIUCN$Year.assessed))

#read in all manually curated papers
file.list <- list.files(pattern='*.xlsx')
A<-lapply(file.list,read_excel)
A <- bind_rows(A, .id = "id")
A<-A[A$Reviewer!="Example",]
Res<- A %>%
  gather(variable, value, 'Research?') %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

A <- A[A$`Research?`!=Res$value[1],]

A$Habitat<-sapply(A$Habitat,function(x) gsub("Fresh water","Fresh Water",x))
A$Habitat<-sapply(A$Habitat,function(x) gsub("Fresh Water","Freshwater",x))

#Dataset A ready for analysis at this point

#Isolate habitat information
Hab <-A %>%
  gather(variable, value, Habitat) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

Hab<-Hab[order(Hab$n,decreasing=T),]
Hab

HabYear <-A %>%
  #gather(variable, value, Habitat, Year) %>%
  group_by(Year, Habitat)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))# %>%
  #mutate(value = fct_reorder(value, n))


# HabYear[HabYear$Year==2015 & HabYear$Habitat=="Terrestrial",]$n/sum(HabYear[HabYear$Year==2015,]$n)
# 
# HabYear[HabYear$Habitat==c("Marine","Marine & Fresh Water","Marine & Terrestrial"),]
# 
# a<-subset(HabYear, Habitat=="Marine")
# b<-subset(HabYear, Habitat=="Marine & Fresh Water")
# c<-subset(HabYear, Habitat=="Marine & Terrestrial")
# Mar<-rbind(a,b,c)
# 
# Mar[Mar$Year==2000,]
# Mar[Mar$Year==2015,]
# 
# Ter<-subset(HabYear, Habitat=="Terrestrial")
# 
# HabYear2 <-A %>%
#   #gather(variable, value, Habitat, Year) %>%
#   group_by(Habitat, Year)%>%
#   summarise (n = n())%>%
#   summarise (sd(n)/mean(n))

Asub<-A[,c(3,17)]
#Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Marine & Fresh Water", "Marine",x))
#Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Marine & Terrestrial", "Marine",x))
#Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Multiple.+", "Marine",x, perl=T))
Asub_mar <- Asub

HabYear_subMar <-Asub_mar %>%
  #gather(variable, value, Habitat, Year) %>%
  group_by(Year, Habitat)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

HabYear_subMar<-HabYear_sub[grep("M.+",HabYear_sub$Habitat, perl=T),]

ggplot(data=HabYear_subMar,aes(x=Year, y=freq, fill=Habitat))+
  geom_col()+
  scale_fill_discrete(name="Habitat")+
  ylab("")

HabYear_sub <-Asub %>%
  #gather(variable, value, Habitat, Year) %>%
  group_by(Year, Habitat)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data=HabYear_sub,aes(Year))+
  geom_bar()+
  scale_fill_discrete(name="Habitat")+
  ylab("")

ggplot(data=Asub,aes(Year, fill=Habitat))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Habitat")+
  ylab("")
  
ggplot(data=HabYear_sub[HabYear_sub$Habitat=="Marine",],aes(x=Year, y=freq))+
    geom_col()+
    scale_fill_discrete(name="Habitat")+
    ylab("Proportion 'Marine' per Year")


test<-data.frame(HabYear_sub[HabYear_sub$Habitat=="Marine",c(1,4)])
test<-data.frame(HabYear_sub[HabYear_sub$Habitat=="Fresh Water & Terrestrial",c(1,4)])

summary(lm(freq~Year, HabYear))

test<-data.frame(HabYear[HabYear$Habitat=="Marine",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Marine & Fresh Water",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Marine & Terrestrial",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Multiple (please clarify in Note column)",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Fresh Water",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Fresh Water & Terrestrial",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Terrestrial",])
summary(lm(freq~Year, test))
test<-data.frame(HabYear[HabYear$Habitat=="Not Applicable (NA)",])
summary(lm(freq~Year, test))

Marine<-Asub[Asub$Habitat=="Marine",]
barplot(summary(as.factor(Asub$Year)),main="Marine habitat through time")
plot(as.factor(Mar$Year),main="Marine habitat through time")



Asub<-A[,c(3,17)]
Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Marine & Fresh Water", "Multiple",x))
Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Marine & Terrestrial", "Multiple",x))
Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Fresh Water & Terrestrial", "Multiple",x))
Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Multiple (please clarify in Note column)", "Multiple",x))
#Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub("Terrestrial & Fresh Water", "Multiple",x))
#Asub$Habitat<-sapply(Asub$Habitat, function(x) gsub(" Terrestrial", "Multiple",x))


ggplot(data=Asub,aes(Year, fill=Habitat))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Habitat")+
  ylab("")

ggplot(data=Asub[Asub$Habitat=="Marine",],aes(Year, fill=Habitat))+
  geom_bar()+
  scale_fill_discrete(name="Habitat")+
  ylab("")

ggplot(data=Asub[Habitat=="Marine"],aes(x=Year, y=Habitat))+
  geom_point()

  scale_fill_discrete(name="Habitat")+
  ylab("")

HabYear2 <-Asub %>%
  #gather(variable, value, Habitat, Year) %>%
  group_by(Habitat, Year)%>%
  summarise (n = n())

#there are unequal number of articles considered each year, need to standardize those. Do that with position="fill" in order to get percentages

ggplot(data=A,aes(Year, fill=Source.title))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Journal")+
  ylab("")

ggplot(data=A,aes(Year, fill=Source.title))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Journal")+
  ylab("")


ggplot(data=A,aes(Year, fill=Habitat))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Habitat")+
  ylab("")

#Seeing a clear periodicity in the proportion of papers

ggplot(data=A[A$Habitat!="Terrestrial" & A$Habitat!="Not Applicable (NA)" & A$Habitat!="Multiple (please clarify in Note column)",],aes(Year, fill=Habitat))+
  geom_bar(position="fill")+
  scale_fill_discrete(name="Habitat")+
  ylab("")


##########################################################################################
#Read in full dataset
##########################################################################################
#setwd("E:/Research_AJL/Conservation review/Completed/Completed_new/Checked/")

#dat<-fread("Full_dataset/ConsBioReview_AllJournalsAllYearsAllSourcesWithIUCNcatsTaxonLocationKeywordSearch_2018-04-10.csv")

# Group_Y_KwMar<-dat[,c('Year','KW_Marine')]
# Group_sum<-Group_Y_KwMar[, sum(KW_Marine), by = Year]
# 
# #summary(lm(Group_sum$freq ~ Group_sum$Year*Group_sum$Habitat, data=Group_sum))
# summary(lm(Group_sum$V1 ~ Group_sum$Year, data=Group_sum))
# 
# summary(lm(HabYear_sub$freq ~ HabYear_sub$Year*HabYear_sub$Habitat, data=HabYear_sub))
#facet_wrap(~cyl)
#interaction(cyl,gear)
# 
# ggplot(data=Group_sum,aes(x=Year,y=V1))+
#   geom_col()+
#   ylab("Number of papers with the keyword marine")

# ggplot(data=Group_sum,aes(x=Year,y=freq))+
#   geom_col()
  
# Group_sum<-A %>%
#   group_by(Year, Habitat) %>%
#   summarise (n = n()) %>%
#   mutate(freq = n / sum(n))

# Group_sum<-A %>%
#   group_by(Source.title, Habitat) %>%
#   summarise (n = n()) %>%
#   mutate(freq = n / sum(n))

# Group_sum_species<-A %>%
#   group_by(Source.title, `Species focus`) %>%
#   summarise (n = n()) %>%
#   mutate(freq = n / sum(n))

# ggplot(data=A[A$Habitat!="Not Applicable (NA)" & A$Habitat!="Multiple (please clarify in Note column)",],aes(Year, fill=Habitat))+
#   geom_bar(position="fill")+
#   scale_fill_discrete(name="Habitat")+
#   ylab("")
# 
# ggplot()+
#   geom_line(data=Group_sum,aes(x=Year,y=freq,col=Habitat),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("All threatened")+
#   theme_classic()

# ggplot()+
#   geom_line(data=Group_sum[Group_sum$RedListCategory=="All Threatened",],aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("All threatened")+
#   theme_classic()

#Compare need vs effort

dat<-fread("ConReview_SystemNeed_ByYearIUCNcat_2019-10-01.csv")
dat[dat$StudySystem=="Marine, Freshwater & Terrestrial",]$StudySystem<-"Marine, Freshwater, & Terrestrial"

#thret<-dat[dat$RedListCategory=="All Threatened",]

# Group_sum<-thret %>%
#   group_by(StudySystem, YearUpTo) %>%
#   summarise (n = NumSpp) %>%
#   mutate(freq = n / sum(n))

# ggplot()+
#   geom_line(data=Group_sum,aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("All threatened")+
#   theme_classic()

# dat2<-dat[dat$RedListCategory!="All Categories",]
# 
# Group_sum<-dat2 %>%
#   group_by(StudySystem) %>%
#   #summarise (n = NumSpp) %>%
#   mutate(freq = NumSpp / sum(NumSpp))
# 
# Group_sum[Group_sum$StudySystem=="Marine, Freshwater & Terrestrial",]$StudySystem<-"Marine, Freshwater, & Terrestrial"
# 
Prop_Hab<-A[A$Habitat!="Not Applicable (NA)",]

Group_hab_sum<-Prop_Hab %>%
  group_by(Habitat,Year) %>%
  summarise (n = n()) 

Group_hab<-Group_hab_sum %>%
  group_by(Year) %>%
  mutate (freq = n/sum(n))

#colnames(Group_hab)<-

Group_hab$Habitat[Group_hab$Habitat=="Fresh Water"]<-"Freshwater"
Group_hab$Habitat[Group_hab$Habitat=="Fresh Water & Terrestrial"]<-"Freshwater & Terrestrial"
Group_hab$Habitat[Group_hab$Habitat=="Marine & Fresh Water"]<-"Marine & Freshwater"
Group_hab$Habitat[Group_hab$Habitat=="Multiple (please clarify in Note column)"]<-"Marine, Freshwater, & Terrestrial"


# Group_hab$AllThreat<-0
# 
# # for(i in levels(as.factor(Group_sum$StudySystem))){
# #   x<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="All Threatened",8]
# #   Group_hab[Group_hab$Habitat==i,]$AllThreat <- rep(as.numeric(x),nrow(Group_hab[Group_hab$Habitat==i,]))
# # }
# 
# for(i in levels(as.factor(Group_sum$StudySystem))){
#   q<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="All Threatened","NumSpp"]
#   p<-dat[dat$StudySystem==i & dat$YearUpTo==2018 & dat$RedListCategory=="All Categories","NumSpp"]
#   x<-as.numeric(q)/as.numeric(p)
#   Group_hab[Group_hab$Habitat==i,]$AllThreat <- rep(x,nrow(Group_hab[Group_hab$Habitat==i,]))
# }
#   
# Group_hab$Extinct<-0
# 
# # for(i in levels(as.factor(Group_sum$StudySystem))){
# #   x<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Extinct",8]
# #   Group_hab[Group_hab$Habitat==i,]$Extinct <- rep(as.numeric(x),nrow(Group_hab[Group_hab$Habitat==i,]))
# # }
# 
# for(i in levels(as.factor(Group_sum$StudySystem))){
#   q<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Extinct","NumSpp"]
#   p<-dat[dat$StudySystem==i & dat$YearUpTo==2018 & dat$RedListCategory=="All Categories","NumSpp"]
#   x<-as.numeric(q)/as.numeric(p)
#   Group_hab[Group_hab$Habitat==i,]$Extinct <- rep(x,nrow(Group_hab[Group_hab$Habitat==i,]))
# }
# 
# Group_hab$LC<-0
# 
# # for(i in levels(as.factor(Group_sum$StudySystem))){
# #   x<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Least Concern",8]
# #   Group_hab[Group_hab$Habitat==i,]$LC <- rep(as.numeric(x),nrow(Group_hab[Group_hab$Habitat==i,]))
# # }
# 
# for(i in levels(as.factor(Group_sum$StudySystem))){
#   q<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Least Concern","NumSpp"]
#   p<-dat[dat$StudySystem==i & dat$YearUpTo==2018 & dat$RedListCategory=="All Categories","NumSpp"]
#   x<-as.numeric(q)/as.numeric(p)
#   Group_hab[Group_hab$Habitat==i,]$LC <- rep(x,nrow(Group_hab[Group_hab$Habitat==i,]))
# }
# 
# Group_hab$DD<-0
# 
# # for(i in levels(as.factor(Group_sum$StudySystem))){
# #   x<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Data Deficient",8]
# #   Group_hab[Group_hab$Habitat==i,]$DD <- rep(as.numeric(x),nrow(Group_hab[Group_hab$Habitat==i,]))
# # }
# 
# for(i in levels(as.factor(Group_sum$StudySystem))){
#   q<-Group_sum[Group_sum$StudySystem==i & Group_sum$YearUpTo==2018 & Group_sum$RedListCategory=="Data Deficient","NumSpp"]
#   p<-dat[dat$StudySystem==i & dat$YearUpTo==2018 & dat$RedListCategory=="All Categories","NumSpp"]
#   x<-as.numeric(q)/as.numeric(p)
#   Group_hab[Group_hab$Habitat==i,]$DD <- rep(x,nrow(Group_hab[Group_hab$Habitat==i,]))
# }

ggplot(data=Group_hab)+
  geom_line(aes(x=Year,y=freq,col=Habitat),size=2)+
  ylab("Proportions of papers per year")+
  ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
  theme_classic()

# ggplot(data=Group_hab)+
#   geom_line(aes(x=Year,y=freq,col=Habitat),size=2)+
#   geom_hline(aes(yintercept=AllThreat,col=Habitat,linetype="Proportion of assessed\nspecies threatened as of 2018"),size=2)+
#   ylab("Proportions of papers per year")+
#   scale_y_continuous(sec.axis = sec_axis(~ . ,name="Proportion of species threatened"))+
#   scale_linetype_manual(name = "",values=3)+
#   ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
#   theme_classic()
#   
# ggplot(data=Group_hab)+
#   geom_line(aes(x=Year,y=freq,col=Habitat),size=2)+
#   geom_hline(aes(yintercept=Extinct*10,col=Habitat,linetype="Proportion of assessed\nspecies extinct as of 2018"),size=2)+
#   ylab("Proportions of papers per year")+
#   scale_y_continuous(sec.axis = sec_axis(~ . /10,name="Proportion of species extinct"))+
#   scale_linetype_manual(name = "",values=3)+
#   ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
#   theme_classic()
# 
# ggplot(data=Group_hab)+
#   geom_line(aes(x=Year,y=freq,col=Habitat),size=2)+
#   geom_hline(aes(yintercept=LC,col=Habitat,linetype="Proportion of assessed\nspecies of least concern as of 2018"),size=2)+
#   ylab("Proportions of papers per year")+
#   scale_y_continuous(sec.axis = sec_axis(~ . ,name="Proportion of species of least concern"))+
#   scale_linetype_manual(name = "",values=3)+
#   ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
#   theme_classic()
# 
# ggplot(data=Group_hab)+
#   geom_line(aes(x=Year,y=freq,col=Habitat),size=2)+
#   geom_hline(aes(yintercept=DD*2,col=Habitat,linetype="Proportion of assessed\nspecies data deficient as of 2018"),size=2)+
#   ylab("Proportions of papers per year")+
#   scale_y_continuous(sec.axis = sec_axis(~ . /2,name="Proportion of species data deficient"))+
#   scale_linetype_manual(name = "",values=3)+
#   ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
#   theme_classic()
# 
# 
# A1<-ggplot()+
#   geom_line(data=Group_sum[Group_sum$RedListCategory=="All Threatened",],aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("All threatened")+
#   theme_classic()
# 
# 
# B2<-ggplot()+
#   geom_line(data=Group_sum[Group_sum$RedListCategory=="Extinct",],aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Extinct")+
#   theme_classic()
# 
# 
# C3<-ggplot()+
#   geom_line(data=Group_sum[Group_sum$RedListCategory=="Least Concern",],aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Least Concern")+
#   theme_classic()
# 
# D4<-ggplot()+
#   geom_line(data=Group_sum[Group_sum$RedListCategory=="Data Deficient",],aes(x=YearUpTo,y=freq,col=StudySystem),size=2)+
#   ylab("Proportion of species in a study system\n assessed compared with 2018 total")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Data Deficient")+
#   theme_classic()
# 
# plot_grid(A1,B2,C3,D4)
# 
# 
# 
# 
# 
# A1<-ggplot()+
#   geom_line(data=dat[dat$RedListCategory=="All Threatened",],aes(x=YearUpTo,y=NumSpp,col=StudySystem),size=2)+
#   ylab("Number of species assessed")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("All threatened")+
#   theme_classic()
# 
# 
# B2<-ggplot()+
#   geom_line(data=dat[dat$RedListCategory=="Extinct",],aes(x=YearUpTo,y=NumSpp,col=StudySystem),size=2)+
#   ylab("Number of species assessed")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Extinct")+
#   theme_classic()
# 
# 
# C3<-ggplot()+
#   geom_line(data=dat[dat$RedListCategory=="Least Concern",],aes(x=YearUpTo,y=NumSpp,col=StudySystem),size=2)+
#   ylab("Number of species assessed")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Least Concern")+
#   theme_classic()
# 
# D4<-ggplot()+
#   geom_line(data=dat[dat$RedListCategory=="Data Deficient",],aes(x=YearUpTo,y=NumSpp,col=StudySystem),size=2)+
#   ylab("Number of species assessed")+
#   xlab("Year")+
#   scale_colour_discrete(name="Study system")+
#   ggtitle("Data Deficient")+
#   theme_classic()
# 
# plot_grid(A1,B2,C3,D4)
# 
# 
# #Plotting need vs effort
# ggplot(data=Group_hab)+
#   geom_boxplot(aes(x=AllThreat,y=freq,col=Habitat),size=1)+
#   geom_abline(intercept=0,slope=1)+
#   xlab("Proportion threatened")+
#   ylab("Proportion published")+
#   xlim(0,0.5)+
#   ylim(0,1)+
#   theme_classic()
# 
# ggplot(data=Group_hab)+
#   geom_boxplot(aes(x=Extinct,y=freq,col=Habitat),size=1)+
#   geom_abline(intercept=0,slope=1)+
#   xlab("Proportion extinct")+
#   ylab("Proportion published")+
#   xlim(0,0.025)+
#   ylim(0,1)+
#   theme_classic()
# 
# ggplot(data=Group_hab)+
#   geom_boxplot(aes(x=LC,y=freq,col=Habitat),size=1)+
#   geom_abline(intercept=0,slope=1)+
#   xlab("Proportion least concern")+
#   ylab("Proportion published")+
#   xlim(0.25,0.8)+
#   ylim(0,1)+
#   theme_classic()
# 
# ggplot(data=Group_hab)+
#   geom_boxplot(aes(x=DD,y=freq,col=Habitat),size=1)+
#   geom_abline(intercept=0,slope=1)+
#   xlab("Proportion data deficient")+
#   ylab("Proportion published")+
#   xlim(0,0.3)+
#   ylim(0,1)+
#   theme_classic()
# 

colnames(Group_hab)<-c("StudySystem","Year","n","freq")

Group_hab<-merge(Group_hab,dat[dat$YearUpTo==2018,],by="StudySystem")

ggplot(data=Group_hab[Group_hab$RedListCategory=="All Threatened",])+
  geom_line(aes(x=Year,y=freq,col=StudySystem),size=2)+
  geom_hline(aes(yintercept=PropSppWithinSystemNoDD,col=StudySystem,linetype="Proportion of assessed\nspecies threatened as of 2018"),size=2)+
  ylab("Proportions of papers per year")+
  scale_y_continuous(sec.axis = sec_axis(~ . ,name="Proportion of species threatened"))+
  scale_linetype_manual(name = "",values=3)+
  ggtitle("Proportion of papers published per year focusing on a habitat specific system")+
  theme_classic()

ggplot(data=Group_hab[Group_hab$RedListCategory=="All Threatened",])+
  geom_boxplot(aes(x=PropSppWithinSystemNoDD,y=freq,col=StudySystem))+
  geom_abline(intercept=0,slope=1)+
  xlab("Proportion threatened")+
  ylab("Proportion published")+
  xlim(0,0.5)+
  ylim(0,1)+
  theme_classic()

ggplot(data=Group_hab[Group_hab$RedListCategory=="All Threatened",])+
  geom_boxplot(aes(x=PropSppWithinSystemNoDD/sum(PropSppWithinSystemNoDD),y=freq,col=StudySystem))+
  geom_abline(intercept=0,slope=1)+
  xlab("Proportion threatened")+
  ylab("Proportion published")+
  #xlim(0,0.5)+
  #ylim(0,1)+
  theme_classic()
