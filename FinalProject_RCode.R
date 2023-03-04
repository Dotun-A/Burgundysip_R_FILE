#Upload the dataset
BurgundyDB<-read.csv("BurgundySip.csv",na.strings = c("N.V.","NA"," ",""));
BurgundyDB;

install.packages("ggcorrplot");
library(ggplot2);
library(ggcorrplot);
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

library(dplyr);
library(PerformanceAnalytics);

#check the structure of the dataset
str(BurgundyDB);

#variable summary
summary(BurgundyDB);

#Treat variables RSG, AL, DN, deleting the blank spaces and treat the NA's
BurgundyDB$RSG<-gsub(" ","",BurgundyDB$RSG);
BurgundyDB$AL<-gsub(" ","",BurgundyDB$AL);
BurgundyDB$DN<-gsub(" ","",BurgundyDB$DN);

BurgundyDB$RSG<-gsub("NA",NA,BurgundyDB$RSG);
BurgundyDB$AL<-gsub("NA",NA,BurgundyDB$AL);
BurgundyDB$DN<-gsub("NA",NA,BurgundyDB$DN);

#convert the variables to numeric
BurgundyDB$RSG<-as.numeric(BurgundyDB$RSG);
BurgundyDB$AL<-as.numeric(BurgundyDB$AL);
BurgundyDB$DN<-as.numeric(BurgundyDB$DN);

#factorize
BurgundyDB[,c("NAME","WINE","REG","TP")] <- lapply(BurgundyDB[,c("NAME","WINE","REG","TP")], factor);
summary(BurgundyDB);
?lapply
#variable age
BurgundyDB$AGE<-2022-BurgundyDB$YR;

#initial visualizationS
str(BurgundyDB);

#boxplot for numerical variables
boxplot(BurgundyDB[,c("YR","RT","NUMR","PR","BD","ACD","RSG","AL","DN","AGE")]);
boxplot(BurgundyDB$RT,main="RT (Rating)");
boxplot(BurgundyDB$YR,main="YR (Year)");
boxplot(BurgundyDB$NUMR,main="NUMR (Number of ratings)");
boxplot(BurgundyDB$PR,main="PR (Price)");
boxplot(BurgundyDB$BD,main="BD (Body Score)");
boxplot(BurgundyDB$ACD,main="ACD");
boxplot(BurgundyDB$RSG,main="RSG");
boxplot(BurgundyDB$AL,main="AL");
boxplot(BurgundyDB$DN,main="RSG");
boxplot(BurgundyDB$AGE,main="AGE");

#plot categorial variables
plot(BurgundyDB$WINE);
plot(BurgundyDB$REG);
plot(BurgundyDB$TP);

#Treating duplicates
duplicates<-duplicated(BurgundyDB);
BurgundyDB[duplicates,];

#checking the variable SN for duplicates
duplicated(BurgundyDB$SN);

#create 3 dataframes with the mean of the variables RSG,AL and DN for each SN
MeanRSG<-aggregate(RSG~SN,BurgundyDB,mean);MeanRSG;
colnames(MeanRSG)<-c("SN","Mean_RSG");
MeanAL<-aggregate(AL~SN,BurgundyDB,mean);MeanAL;
colnames(MeanAL)<-c("SN","Mean_AL");
MeanDN<-aggregate(DN~SN,BurgundyDB,mean);MeanDN;
colnames(MeanDN)<-c("SN","Mean_DN");

#merge the 3 previuos data frames with the original one by SN keeping all the columns from the left (original data frame)
BurgundyDB2<-merge(BurgundyDB,MeanRSG,by='SN',x.all=T);BurgundyDB2;
BurgundyDB2<-merge(BurgundyDB2,MeanAL,by='SN',x.all=T);BurgundyDB2;
BurgundyDB2<-merge(BurgundyDB2,MeanDN,by='SN',x.all=T);BurgundyDB2;

#subsetting the new datadrame and keeping the new variables mean columns for RSG, AL, and DN.
BurgundyDB2<-BurgundyDB2[,c("SN","NAME","WINE","REG","TP","YR","RT","NUMR","PR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")];

#delete the duplicates values and generate a new data frame BurgundyDB3
duplicates2<-duplicated(BurgundyDB2);
BurgundyDB3<-BurgundyDB2[!duplicates2,];

#Treating missing values
summary(BurgundyDB3);

#Missing values

calculateMissProb <- function(x){
  return(sum(is.na(x))/length(x)*100);
}

apply(BurgundyDB3,2,calculateMissProb);

#Check covariance of variables

var(BurgundyDB2[,c("RT","NUMR","PR","YR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")],na.rm = T);
heatmap(var(BurgundyDB2[,c("RT","NUMR","PR","YR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")],na.rm = T));

#auxiliary data frame to check correlation without NA's
BDaux<-na.omit(BurgundyDB2);
cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")]);
heatmap(cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")]));

#check an treat variable NAME
#validate NA values
BurgundyDB3[is.na(BurgundyDB3$NAME),];

#Create a data frame to identify the Names with the highest frequency and keep the top 10
NameSummary<-data.frame(table(BurgundyDB3$NAME));
colnames(NameSummary)<-c("NAME","Freq");
NameSummary<-NameSummary[order(NameSummary$Freq, decreasing = TRUE),][c(1:10),1];NameSummary;
droplevels(NameSummary);

#replace the names that are not in the top 10 by the category "Other" including the NA
OutName<-which(!(BurgundyDB3$NAME %in% NameSummary));OutName;
BurgundyDB3$NAME<-factor(BurgundyDB3$NAME,levels=c(levels(BurgundyDB3$NAME),"Other"));
BurgundyDB3[OutName,"NAME"]="Other";

BurgundyDB3[is.na(BurgundyDB3$NAME),];

#check an treat variable PR

BurgundyDB3[is.na(BurgundyDB3$PR),];
summary(BurgundyDB3);

aggregate(BurgundyDB3$PR~NAME,BurgundyDB3,mean);

#Check variable PR for the top 5 of WINE
summary(BurgundyDB3);
summary(BurgundyDB3[BurgundyDB3$WINE==c("Altos de Losada","Rioja Graciano","Mirto","El Viejo","Reserva"),]$PR);
boxplot(BurgundyDB3[BurgundyDB3$WINE==c("Altos de Losada","Rioja Graciano","Mirto","El Viejo","Reserva"),]$PR);
aggregate(PR~WINE,BurgundyDB3[BurgundyDB3$WINE==c("Altos de Losada","Rioja Graciano","Mirto","El Viejo","Reserva"),],mean);

#Check variable PR for the top 5 of REG
summary(BurgundyDB3);
summary(BurgundyDB3[BurgundyDB3$REG==c("Rioja","Ribera del Duero","Priorato","Bierzo","Toro"),]$PR);
boxplot(BurgundyDB3[BurgundyDB3$REG==c("Rioja","Ribera del Duero","Priorato","Bierzo","Toro"),]$PR);
aggregate(PR~REG,BurgundyDB3[BurgundyDB3$REG==c("Rioja","Ribera del Duero","Priorato","Bierzo","Toro"),],mean);

#Check variable PR for the top 5 of TP
summary(BurgundyDB3);
summary(BurgundyDB3[BurgundyDB3$TP==c("Rioja Red","Ribera Del Duero Red","Priorat Red","Red","Mencia"),]$PR);
boxplot(BurgundyDB3[BurgundyDB3$TP==c("Rioja Red","Ribera Del Duero Red","Priorat Red","Red","Mencia"),]$PR);
aggregate(PR~TP,BurgundyDB3[BurgundyDB3$TP==c("Rioja Red","Ribera Del Duero Red","Priorat Red","Red","Mencia"),],mean);

#Check variable PR with numerical variables
cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","AGE","Mean_RSG","Mean_AL","Mean_DN")]);

#replace missing values on PR by categorial variables
PR_mean<-data.frame(aggregate(PR~NAME+WINE+REG+TP,BurgundyDB3,mean));
PR_mean;

#Check variable BD for NAs
A <- is.na(BurgundyDB3$BD)
B <- BurgundyDB3[A,]
Le <- BurgundyDB3$BD < 3
f<-  !is.na(BurgundyDB3[Le,])

#Check variable BD for NAs



