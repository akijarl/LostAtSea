#######################
#Visualize manual data
#######################
setwd("E:/Research_AJL/Conservation review/Completed/Completed_new/Checked/")

require(tidyverse)
require(readxl)

file.list <- list.files(pattern='*.xlsx')
A<-lapply(file.list,read_excel)
A <- bind_rows(A, .id = "id")
A<-A[A$Reviewer!="Example",]

summary(as.factor(A$Source.title))


#A[A$paperID==3989,16]
#A<-read_excel("Con_Review_Dataset_F.xlsx")
#A<-A[A$Reviewer!="Example",]

Res<- A %>%
  gather(variable, value, 'Research?') %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

Res
#sum(Res$n)==200
#sum(Res$n)==201

A2 <- A[A$`Research?`!=Res$value[1],]
#A<-lapply(file.list,read_excel)
#A <- bind_rows(A, .id = "id")
#A<-A[A$Reviewer!="Example",]

C3<-as.data.frame(A2$`Country/ies (if more than 3)`)
colnames(C3)<-"Countries"
C3$Countries<-as.character(C3$Countries)
C4<-strsplit(C3$Countries,";")
MC<-max(sapply(C4,length))
R<-as.character(1:MC)

df <- A2 %>%
  separate(`Country/ies (if more than 3)`,into=R,sep=";")

df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) trimws(x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub( "`", "",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("United kingdom", "United Kingdom",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("United Kingdo,", "United Kingdom",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("United states", "United States",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Uruquay", "Uruguay",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Trinidad & Tobago", "Trinidad and Tobago",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Sao Tome & Principe", "Sao Tome and Principe",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Sao Tome and Príncipe", "Sao Tome and Principe",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Sao Tome and Príncipe", "Sao Tome and Principe",x))
df[,16:(16+(ncol(df)-ncol(A2)))]<-sapply(df[,16:(16+(ncol(df)-ncol(A2)))],function(x) gsub("Saint Vincent and Grenadines", "Saint Vincent and the Grenadines",x))


test<-c("United kingdom","United kingdom","United Kingdom")
test2<-c("United states","United states","United States")
tdf<-data.frame(test,test2)

gsub("United kingdom","United Kingdom",test)

tdf
tdf[,1]<-sapply(tdf[,1],function(x) gsub("United kingdom","United Kingdom",x))

Count<-df %>%
  gather(variable, value, Country,`Country 2 (if needed)`,`Country 3 (if needed)`,R) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

Count<-Count[!is.na(Count$value),]
Count<-Count[!nchar(Count$value)==0,]
Count<-Count[order(Count$n,decreasing=T),]
Count
head(Count,20)
tail(Count,20)

Count%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust = 1))

Count[c(1:20),]%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#for(i in file.list){
#  A<-lapply(i,read_excel)
#  A <- bind_rows(A, .id = "id")
#  A<-A[A$Reviewer!="Example",]
  
#  C3<-as.data.frame(A$`Country/ies (if more than 3)`)
#  colnames(C3)<-"Countries"
#  C3$Countries<-as.character(C3$Countries)
#  C4<-strsplit(C3$Countries,";")
#  MC<-max(sapply(C4,length))
#  R<-as.character(1:MC)
  
#  df <- A %>%
#    separate(`Country/ies (if more than 3)`,into=R,sep=";")
  
#  Count<-df %>%
#    gather(variable, value, Country,`Country 2 (if needed)`,`Country 3 (if needed)`,R) %>%
#    group_by(value)%>%
#    summarise (n = n()) %>%
#    mutate(freq = n / sum(n))
  
#  Count<-Count[!is.na(Count$value),]
#  Count<-Count[!nchar(Count$value)==0,]
#  Count<-Count[order(Count$n,decreasing=T),]
#  Count
#  head(Count,20)
#  tail(Count,20)
  
#  pic<-Count[c(1:20),]%>%
#    mutate(value = fct_reorder(value, n))%>%
#    ggplot(aes(value,n))+
#    geom_col()+
#    ggtitle(substr(i,20,20))+
#    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#  ggsave(filename=print(paste("NewFigs/",substr(i,20,20),"_countries.png",sep="")),plot=pic,device="png")
  
#}


Count1<-A %>%
  gather(variable, value, Country) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

Count1<-Count1[order(Count1$n,decreasing=T),]
Count1
head(Count1,20)
tail(Count1,20)

Count1%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

#sum(Count1$n)==200
#sum(Count1$n)==201

Count2<-A %>%
  gather(variable, value, `Country 2 (if needed)`) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

Count2<-Count2[order(Count2$n,decreasing=T),]
Count2
head(Count2,20)
tail(Count2,20)

Count2%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

sum(Count2[!is.na(Count2$value),'n'])

Count2_na<-Count2[!is.na(Count2$value),]
Count2_na%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

#sum(Count2$n)==200
#sum(Count2$n)==201

Count3<-A %>%
  gather(variable, value, `Country 3 (if needed)`) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

Count3<-Count3[order(Count3$n,decreasing=T),]
Count3
head(Count3,20)
tail(Count3,20)

Count3%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

sum(Count3[!is.na(Count3$value),'n'])

Count3_na<-Count3[!is.na(Count3$value),]
Count3_na%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

sum(Count3$n)==200
sum(Count3$n)==201

df2<-data.frame(lapply(df, trimws), stringsAsFactors = FALSE)
R2<-paste("X",R,sep="")

Count4P<-df2 %>%
  gather(variable, value, R2) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

Count4P<-Count4P[order(Count4P$n,decreasing=T),]
Count4P
head(Count4P,20)
tail(Count4P,20)

Count4P%>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))

sum(Count4P[!is.na(Count4P$value),'n'])

sum(Count4P$n)


Hab <-A %>%
  gather(variable, value, Habitat) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

Hab<-Hab[order(Hab$n,decreasing=T),]
Hab

#sum(Hab$n)==200
#sum(Hab$n)==201

Hab %>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Tax <-A %>%
  gather(variable, value, `Taxon (General)`) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

Tax<-Tax[order(Tax$n,decreasing=T),]
Tax

sum(Tax$n)==200
sum(Tax$n)==201


SpF <-A %>%
  gather(variable, value, `Species focus`) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(value = fct_reorder(value, n))

SpF

sum(SpF$n)==200
sum(SpF$n)==201

Tax %>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

TaxS<-as.data.frame(A$`Taxon (Specific)`)
colnames(TaxS)<-"Spp"
TaxS$Spp<-as.character(TaxS$Spp)
TS<-strsplit(TaxS$Spp,";")
MT<-max(sapply(TS,length))
S<-as.character(1:MT)

dft<-A %>%
  separate(`Taxon (Specific)`,into=S,sep=";")

dft_spp<-dft[!is.na(dft$`1`),]

TaxSp<-dft %>%
  gather(variable, value, S) %>%
  group_by(value)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

TaxSp<-TaxSp[!is.na(TaxSp$value),]
TaxSp<-TaxSp[order(TaxSp$n,decreasing=T),]
TaxSp

TaxSp[c(1:20),] %>%
  mutate(value = fct_reorder(value, n))%>%
  ggplot(aes(value,n))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################
# Sumary analysis
##########################################################

RevSum<-read.csv("Conrev_Summary_checked.csv")
CN<-RevSum$Category

tRevSum<-data.frame(t(RevSum),stringsAsFactors = FALSE)
colnames(tRevSum)<-CN
tRevSum<-tRevSum[-1,]
tRevSum$Set<-row.names(tRevSum)

se <- function(x) sqrt(var(x)/length(x))

mean(as.numeric(tRevSum$`Non-Research`))
mean(as.numeric(tRevSum$`Non-Research`))+se(as.numeric(tRevSum$`Non-Research`))
mean(as.numeric(tRevSum$`Non-Research`))-se(as.numeric(tRevSum$`Non-Research`))

summary(as.numeric(tRevSum$`Non-Research`))[2]
summary(as.numeric(tRevSum$`Non-Research`))[5]

NR<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Non-Research`)))+
  ylab("Non-Research")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Non-Research`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Non-Research`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Non-Research`))+se(as.numeric(tRevSum$`Non-Research`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Non-Research`))-se(as.numeric(tRevSum$`Non-Research`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Non-Research`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Non-Research`))[5]),col="orange")
#theme(axis.text.x = element_text(angle = 0, hjust = 1))

ggsave(filename="NewFigs/NR.png",plot=NR,device="png")

summary(as.numeric(tRevSum$`Country1_other`))[2]
summary(as.numeric(tRevSum$`Country1_other`))[5]

C1_O<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Country1_other`)))+
  ylab("Country1 'Other'")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_other`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Country1_other`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_other`))+se(as.numeric(tRevSum$`Country1_other`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_other`))-se(as.numeric(tRevSum$`Country1_other`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country1_other`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country1_other`))[5]),col="orange")

ggsave(filename="NewFigs/C1_O.png",plot=C1_O,device="png")

summary(as.numeric(tRevSum$`Country1_NA`))[2]
summary(as.numeric(tRevSum$`Country1_NA`))[5]

C1_NA<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Country1_NA`)))+
  ylab("Country1 'NA'")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_NA`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Country1_NA`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_NA`))+se(as.numeric(tRevSum$`Country1_NA`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country1_NA`))-se(as.numeric(tRevSum$`Country1_NA`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country1_NA`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country1_NA`))[5]),col="orange")

ggsave(filename="NewFigs/C1_NA.png",plot=C1_NA,device="png")

summary(as.numeric(tRevSum$`Country2_entry`))[2]
summary(as.numeric(tRevSum$`Country2_entry`))[5]

C2<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Country2_entry`)))+
  ylab("Country2 entries")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country2_entry`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Country2_entry`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country2_entry`))+se(as.numeric(tRevSum$`Country2_entry`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country2_entry`))-se(as.numeric(tRevSum$`Country2_entry`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country2_entry`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country2_entry`))[5]),col="orange")

ggsave(filename="NewFigs/C2.png",plot=C2,device="png")

summary(as.numeric(tRevSum$`Country3_entry`))[2]
summary(as.numeric(tRevSum$`Country3_entry`))[5]

C3<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Country3_entry`)))+
  ylab("Country3 entries")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country3_entry`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Country3_entry`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country3_entry`))+se(as.numeric(tRevSum$`Country3_entry`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Country3_entry`))-se(as.numeric(tRevSum$`Country3_entry`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country3_entry`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Country3_entry`))[5]),col="orange")

ggsave(filename="NewFigs/C3.png",plot=C3,device="png")

summary(as.numeric(tRevSum$`Countries_m3_entry`))[2]
summary(as.numeric(tRevSum$`Countries_m3_entry`))[5]

C4p<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Countries_m3_entry`)))+
  ylab("Countries 4+ entries")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Countries_m3_entry`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Countries_m3_entry`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Countries_m3_entry`))+se(as.numeric(tRevSum$`Countries_m3_entry`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Countries_m3_entry`))-se(as.numeric(tRevSum$`Countries_m3_entry`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Countries_m3_entry`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Countries_m3_entry`))[5]),col="orange")

ggsave(filename="NewFigs/C4p.png",plot=C4p,device="png")

summary(as.numeric(tRevSum$`Hab_Terrestrial`))[2]
summary(as.numeric(tRevSum$`Hab_Terrestrial`))[5]

HT<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_Terrestrial`)))+
  ylab("Terrestrial habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Terrestrial`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_Terrestrial`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Terrestrial`))+se(as.numeric(tRevSum$`Hab_Terrestrial`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Terrestrial`))-se(as.numeric(tRevSum$`Hab_Terrestrial`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Terrestrial`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Terrestrial`))[5]),col="orange")

ggsave(filename="NewFigs/HT.png",plot=HT,device="png")

summary(as.numeric(tRevSum$`Hab_Mar`))[2]
summary(as.numeric(tRevSum$`Hab_Mar`))[5]

HM<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_Mar`)))+
  ylab("Marine habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mar`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_Mar`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mar`))+se(as.numeric(tRevSum$`Hab_Mar`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mar`))-se(as.numeric(tRevSum$`Hab_Mar`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Mar`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Mar`))[5]),col="orange")

ggsave(filename="NewFigs/HM.png",plot=HM,device="png")

summary(as.numeric(tRevSum$`Hab_Fr`))[2]
summary(as.numeric(tRevSum$`Hab_Fr`))[5]

HF<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_Fr`)))+
  ylab("Fresh Water habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Fr`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_Fr`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Fr`))+se(as.numeric(tRevSum$`Hab_Fr`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Fr`))-se(as.numeric(tRevSum$`Hab_Fr`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Fr`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Fr`))[5]),col="orange")

ggsave(filename="NewFigs/HF.png",plot=HF,device="png")

summary(as.numeric(tRevSum$`Hab_MaTe`))[2]
summary(as.numeric(tRevSum$`Hab_MaTe`))[5]

HMT<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_MaTe`)))+
  ylab("Terrestrial-Marine habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaTe`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_MaTe`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaTe`))+se(as.numeric(tRevSum$`Hab_MaTe`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaTe`))-se(as.numeric(tRevSum$`Hab_MaTe`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_MaTe`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_MaTe`))[5]),col="orange")

ggsave(filename="NewFigs/HMT.png",plot=HMT,device="png")

summary(as.numeric(tRevSum$`Hab_FrTe`))[2]
summary(as.numeric(tRevSum$`Hab_FrTe`))[5]

HFT<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_FrTe`)))+
  ylab("Terrestrial-Fresh Water habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_FrTe`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_FrTe`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_FrTe`))+se(as.numeric(tRevSum$`Hab_FrTe`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_FrTe`))-se(as.numeric(tRevSum$`Hab_FrTe`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_FrTe`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_FrTe`))[5]),col="orange")

ggsave(filename="NewFigs/HFT.png",plot=HFT,device="png")

summary(as.numeric(tRevSum$`Hab_MaFr`))[2]
summary(as.numeric(tRevSum$`Hab_MaFr`))[5]

HMF<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_MaFr`)))+
  ylab("Marine-Fresh Water habitat")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaFr`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_MaFr`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaFr`))+se(as.numeric(tRevSum$`Hab_MaFr`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_MaFr`))-se(as.numeric(tRevSum$`Hab_MaFr`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_MaFr`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_MaFr`))[5]),col="orange")

ggsave(filename="NewFigs/HMF.png",plot=HMF,device="png")

summary(as.numeric(tRevSum$`Hab_Mu`))[2]
summary(as.numeric(tRevSum$`Hab_Mu`))[5]

HMu<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_Mu`)))+
  ylab("Multiple habitats")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mu`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_Mu`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mu`))+se(as.numeric(tRevSum$`Hab_Mu`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_Mu`))-se(as.numeric(tRevSum$`Hab_Mu`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Mu`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_Mu`))[5]),col="orange")

ggsave(filename="NewFigs/HMu.png",plot=HMu,device="png")

summary(as.numeric(tRevSum$`Hab_NA`))[2]
summary(as.numeric(tRevSum$`Hab_NA`))[5]

HNA<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Hab_NA`)))+
  ylab("'NA' habitats")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_NA`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Hab_NA`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_NA`))+se(as.numeric(tRevSum$`Hab_NA`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Hab_NA`))-se(as.numeric(tRevSum$`Hab_NA`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_NA`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Hab_NA`))[5]),col="orange")

ggsave(filename="NewFigs/HNA.png",plot=HNA,device="png")

summary(as.numeric(tRevSum$`Tax_An`))[2]
summary(as.numeric(tRevSum$`Tax_An`))[5]
summary(as.numeric(tRevSum$`Tax_Pl`))[2]
summary(as.numeric(tRevSum$`Tax_Pl`))[5]
summary(as.numeric(tRevSum$`Tax_Fu`))[2]
summary(as.numeric(tRevSum$`Tax_Fu`))[5]
summary(as.numeric(tRevSum$`Tax_Mi`))[2]
summary(as.numeric(tRevSum$`Tax_Mi`))[5]
summary(as.numeric(tRevSum$`Tax_Mu`))[2]
summary(as.numeric(tRevSum$`Tax_Mu`))[5]
summary(as.numeric(tRevSum$`Tax_Ot`))[2]
summary(as.numeric(tRevSum$`Tax_Ot`))[5]
summary(as.numeric(tRevSum$`Tax_NA`))[2]
summary(as.numeric(tRevSum$`Tax_NA`))[5]

TA<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_An`)))+
  ylab("Animal taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_An`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_An`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_An`))+se(as.numeric(tRevSum$`Tax_An`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_An`))-se(as.numeric(tRevSum$`Tax_An`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_An`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_An`))[5]),col="orange")

ggsave(filename="NewFigs/TA.png",plot=TA,device="png")

TP<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_Pl`)))+
  ylab("Plant taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Pl`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_Pl`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Pl`))+se(as.numeric(tRevSum$`Tax_Pl`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Pl`))-se(as.numeric(tRevSum$`Tax_Pl`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Pl`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Pl`))[5]),col="orange")

ggsave(filename="NewFigs/TP.png",plot=TP,device="png")

TF<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_Fu`)))+
  ylab("Fungi taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Fu`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_Fu`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Fu`))+se(as.numeric(tRevSum$`Tax_Fu`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Fu`))-se(as.numeric(tRevSum$`Tax_Fu`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Fu`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Fu`))[5]),col="orange")

ggsave(filename="NewFigs/TF.png",plot=TF,device="png")

TMi<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_Mi`)))+
  ylab("Microbial taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mi`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_Mi`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mi`))+se(as.numeric(tRevSum$`Tax_Mi`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mi`))-se(as.numeric(tRevSum$`Tax_Mi`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Mi`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Mi`))[5]),col="orange")

ggsave(filename="NewFigs/TMi.png",plot=TMi,device="png")

TMu<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_Mu`)))+
  ylab("Multiple taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mu`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_Mu`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mu`))+se(as.numeric(tRevSum$`Tax_Mu`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Mu`))-se(as.numeric(tRevSum$`Tax_Mu`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Mu`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Mu`))[5]),col="orange")

ggsave(filename="NewFigs/TMu.png",plot=TMu,device="png")

TOt<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_Ot`)))+
  ylab("'Other' taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Ot`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_Ot`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Ot`))+se(as.numeric(tRevSum$`Tax_Ot`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_Ot`))-se(as.numeric(tRevSum$`Tax_Ot`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Ot`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_Ot`))[5]),col="orange")

ggsave(filename="NewFigs/TOt.png",plot=TOt,device="png")

TNA<-ggplot(data=tRevSum)+
  geom_col(aes(Set,as.numeric(tRevSum$`Tax_NA`)))+
  ylab("'NA' taxon")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_NA`))),col="red")+
  geom_hline(aes(yintercept=median(as.numeric(tRevSum$`Tax_NA`))),col="blue")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_NA`))+se(as.numeric(tRevSum$`Tax_NA`))),col="green")+
  geom_hline(aes(yintercept=mean(as.numeric(tRevSum$`Tax_NA`))-se(as.numeric(tRevSum$`Tax_NA`))),col="green")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_NA`))[2]),col="orange")+
  geom_hline(aes(yintercept=summary(as.numeric(tRevSum$`Tax_NA`))[5]),col="orange")

ggsave(filename="NewFigs/TNA.png",plot=TNA,device="png")
