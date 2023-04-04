library(dplyr);
library(PerformanceAnalytics);
library(ggplot2);
library(reshape2);
library(scatterplot3d);

#Upload the dataset
BurgundyDB<-read.csv("BurgundySip.csv",na.strings = c("N.V.","NA"," ",""));
BurgundyDB;

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

#initial visualizations

#boxplot for numerical variables
boxplot(BurgundyDB[,c("YR","RT","NUMR","PR","BD","ACD","RSG","AL","DN")]);

#plot categorial variables
plot(BurgundyDB$WINE);
plot(BurgundyDB$REG);
plot(BurgundyDB$TP);

##########################################TREATING DUPLICATES
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
rm(MeanRSG,MeanAL,MeanDN);

#subsetting the new datadrame and keeping the new variables mean columns for RSG, AL, and DN.
BurgundyDB2<-BurgundyDB2[,c("SN","NAME","WINE","REG","TP","YR","RT","NUMR","PR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")];

#delete the duplicates values and generate a new data frame BurgundyDB3
duplicates2<-duplicated(BurgundyDB2);
BurgundyDB3<-BurgundyDB2[!duplicates2,];

################################TREATING MISSING VALUES
summary(BurgundyDB3);

#function for calculate missing probability

calculateMissProb <- function(x){
  return(sum(is.na(x))/length(x)*100);
}

apply(BurgundyDB3,2,calculateMissProb);

#Check covariance of variables

var(BurgundyDB2[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")],na.rm = T);
heatmap(var(BurgundyDB2[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")],na.rm = T));

#auxiliary data frame to check correlation without NA's
BDaux<-na.omit(BurgundyDB2);
cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]);
heatmap(cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]));

###########check an treat variable TP and treat the missing values

#Verify if the NA's of the TP variable has a relation with the region, name, winery and year
BDaux<-BurgundyDB3[is.na(BurgundyDB3$TP),];BDaux;
TPSummary<-data.frame(table(BDaux$NAME));TPSummary;
TPSummary<-data.frame(table(BDaux$WINE));TPSummary;
TPSummary<-data.frame(table(BDaux$REG));TPSummary;
TPSummary<-data.frame(table(BDaux$YR));TPSummary;

BurgundyDB3[is.na(BurgundyDB3$TP),];   
TPSummary<-data.frame(table(BurgundyDB3$TP)); 
colnames(TPSummary)<-c("TP","Freq"); 
TPSummary<-TPSummary[order(TPSummary$Freq, decreasing = TRUE),][c(1:10),1];TPSummary; 
droplevels(TPSummary);  

#replace the TP that are not in the top 10 by the category "Other" including the NA 
OutTP<-which(!(BurgundyDB3$TP %in% TPSummary));OutTP; 
BurgundyDB3$TP<-factor(BurgundyDB3$TP,levels=c(levels(BurgundyDB3$TP),c("Other"))); 
BurgundyDB3[OutTP,"TP"]="Other";

###########check an treat variable NAME and treat the missing values
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

###########check an treat variable WINE and treat the missing values
BurgundyDB3[is.na(BurgundyDB3$WINE),];

#Create a data frame to identify the WINE with the highest frequency and keep the top 10
WINESummary<-data.frame(table(BurgundyDB3$WINE));WINESummary;
colnames(WINESummary)<-c("WINE","Freq");
WINESummary<-WINESummary[order(WINESummary$Freq, decreasing = TRUE),][c(1:10),1];WINESummary;
droplevels(WINESummary);

#replace the WINE variables that are not in the top 10 by the category "Other" including the NA
OutWINE<-which(!(BurgundyDB3$WINE %in% WINESummary));OutWINE;
BurgundyDB3$WINE<-factor(BurgundyDB3$WINE,levels=c(levels(BurgundyDB3$WINE),"Other"));
BurgundyDB3[OutWINE,"WINE"]="Other";

BurgundyDB3[is.na(BurgundyDB3$WINE),];

##check an treat variable REG and treat the missing values

BurgundyDB3[is.na(BurgundyDB3$REG),];  

#Create a data frame to identify the Names with the highest frequency and keep the top 10 
REGSummary<-data.frame(table(BurgundyDB3$REG)); 
colnames(REGSummary)<-c("REG","Freq"); 
REGSummary<-REGSummary[order(REGSummary$Freq, decreasing = TRUE),][c(1:10),1];
REGSummary; droplevels(REGSummary);  

#replace the reg that are not in the top 10 by the category "Other" including the NA 
OutREG<-which(!(BurgundyDB3$REG %in% REGSummary));OutREG; 
BurgundyDB3$REG<-factor(BurgundyDB3$REG,levels=c(levels(BurgundyDB3$REG),c("Other"))); 
BurgundyDB3[OutREG,"REG"]="Other";  
BurgundyDB3[is.na(BurgundyDB3$REG),];

###########Check the variable RT and treat the missing values

BurgundyDB3[is.na(BurgundyDB3$RT),]; 
summary(BurgundyDB3)

#Treating the missing values 
BurgundyDB3$RT[is.na(BurgundyDB3$RT)] <- (as.integer(mean(BurgundyDB3$RT,na.rm=T)));
summary(BurgundyDB3);

############Check the variable YR and treat the missing values 
BurgundyDB3$YR <- as.integer(BurgundyDB3$YR);
BurgundyDB3[is.na(BurgundyDB3$YR),];

#Check if exist a patron with other variables 
table(BurgundyDB3$TP, BurgundyDB3$YR);
BurgundyAuxY1 <- is.na(BurgundyDB3$YR);
BurgundyAuxY2 <- BurgundyDB3[BurgundyAuxY1,];
summary(BurgundyAuxY2);
boxplot.stats(BurgundyDB3$YR);
BurgundyAuxY3 <- BurgundyDB3[BurgundyDB3$REG=="Jerez-Xeres-Sherry",];
summary(BurgundyAuxY3);
boxplot(BurgundyAuxY3$YR);
data.frame(BurgundyAuxY3$YR);
is.na(BurgundyAuxY3);
summary(BurgundyAuxY3$YR);

rm(BurgundyAuxY1,BurgundyAuxY2,BurgundyAuxY3);

#Replace YR Missing values using the mean of YR. 
BurgundyDB3$YR[is.na(BurgundyDB3$YR)] <- (as.integer(mean(BurgundyDB3$YR,na.rm=T)));
summary(BurgundyDB3);

############Check variable BD with categorical variables
aggregate(BurgundyDB3$BD~NAME,BurgundyDB3,mean);
aggregate(BurgundyDB3$BD~WINE,BurgundyDB3,mean);
aggregate(BurgundyDB3$BD~REG,BurgundyDB3,mean);
aggregate(BurgundyDB3$BD~TP,BurgundyDB3,mean);

#replace missing values on BD by categorical variables
BD_mean<-data.frame(aggregate(BD~NAME+WINE+REG+TP,BurgundyDB3,mean));
BD_mean;

BDaux2<-BurgundyDB3[is.na(BurgundyDB3$BD),c(-10)];BDaux2;
BDaux3<-BurgundyDB3[!is.na(BurgundyDB3$BD),];

BDaux2<-merge(BDaux2,BD_mean,x.all=TRUE);BDaux2;
BurgundyDB3<-rbind(BDaux3,BDaux2);

rm(BD_mean);

############Check variable ACD with categorical variables
aggregate(BurgundyDB3$ACD~NAME,BurgundyDB3,mean);
aggregate(BurgundyDB3$ACD~WINE,BurgundyDB3,mean);
aggregate(BurgundyDB3$ACD~REG,BurgundyDB3,mean);
aggregate(BurgundyDB3$ACD~TP,BurgundyDB3,mean);

#replace missing values on ACD by categorical variables
ACD_mean<-data.frame(aggregate(ACD~NAME+WINE+REG+TP,BurgundyDB3,mean));
ACD_mean;

BDaux2<-BurgundyDB3[is.na(BurgundyDB3$ACD),c(-10)];BDaux2;
BDaux3<-BurgundyDB3[!is.na(BurgundyDB3$ACD),];

BDaux2<-merge(BDaux2,ACD_mean,x.all=TRUE);BDaux2;
BurgundyDB3<-rbind(BDaux3,BDaux2);

rm(ACD_mean,BDaux3,BDaux2);

###########Check variable PR with categorical variables
aggregate(BurgundyDB3$PR~NAME,BurgundyDB3,mean);
aggregate(BurgundyDB3$PR~WINE,BurgundyDB3,mean);
aggregate(BurgundyDB3$PR~REG,BurgundyDB3,mean);
aggregate(BurgundyDB3$PR~TP,BurgundyDB3,mean);

#Check variable PR with numerical variables
cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]);

#replace missing values on PR by categorical variables
PR_mean<-data.frame(aggregate(PR~NAME+WINE+REG+TP,BurgundyDB3,mean));
PR_mean;

BDaux2<-BurgundyDB3[is.na(BurgundyDB3$PR),c(-9)];BDaux2;
BDaux3<-BurgundyDB3[!is.na(BurgundyDB3$PR),];

BDaux2<-merge(BDaux2,PR_mean,x.all=TRUE);BDaux2;
BurgundyDB4<-rbind(BDaux3,BDaux2);

summary(BurgundyDB4);

rm(PR_mean);

#variable age
BurgundyDB4$AGE<-2022-BurgundyDB4$YR;

#Check the duplicates again
duplicates3<-duplicated(BurgundyDB4$SN,fromLast = F);
BurgundyDB4<-BurgundyDB4[!duplicates3,];

#checking the variable SN for duplicates
anyDuplicated(BurgundyDB4$SN);

########################################################TREATING OUTLIERS

#boxplot for numerical variables
boxplot(BurgundyDB4[,c("YR","RT","NUMR","PR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]);

#########Check outliers on Mean_RSG
boxplot(BurgundyDB4$Mean_RSG);

##### Check if there's a correlation between variable Mean_RSG and other numerical variables 
cor(BurgundyDB4[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]);

### Check the variable Mean_RSG with RT since the correlation is  of -0.92
ggplot(BurgundyDB4, aes(x = BurgundyDB4$RT, y = BurgundyDB4$Mean_RSG)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("RT")

# Create a new column "Rating labels" to subset the outliers in RT 
BurgundyDB4<- transform(BurgundyDB4,
                        RatingLabels=cut(BurgundyDB4$RT,breaks = c(3.9,4.0,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9,5.0),
                                         labels = c(4.0,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9,5.0)));
BurgundyDB4;
#treating the outliers by replacing with the trim mean
BurgundyDB4[BurgundyDB4$RatingLabels == "4.2",]
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels == "4.2",]$Mean_RSG)
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG);
OutRSG<-boxplot.stats(BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG)$out;OutRSG;
outind<-which(BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG %in% OutRSG);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG,trim = 0.0054);
TrimMean;
BurgundyDB4[BurgundyDB4$RatingLabels=="4.2",]$Mean_RSG[outind] = TrimMean;

BurgundyDB4[BurgundyDB4$RatingLabels == "4.4",]
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels == "4.4",]$Mean_RSG)
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG);
OutRSG<-boxplot.stats(BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG)$out;OutRSG;
outind<-which(BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG %in% OutRSG);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG,trim = 0.0016);
TrimMean;
BurgundyDB4[BurgundyDB4$RatingLabels=="4.4",]$Mean_RSG[outind] = TrimMean;

BurgundyDB4[BurgundyDB4$RatingLabels == "4.5",]
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels == "4.5",]$Mean_RSG)
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG);
OutRSG<-boxplot.stats(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG)$out;OutRSG;
outind<-which(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG %in% OutRSG);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG,trim = 0.019);
TrimMean;
BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG[outind] = TrimMean;

BurgundyDB4[BurgundyDB4$RatingLabels == "4.9",]
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels == "4.9",]$Mean_RSG)
boxplot(BurgundyDB4[BurgundyDB4$RatingLabels=="4.9",]$Mean_RSG);
OutRSG<-boxplot.stats(BurgundyDB4[BurgundyDB4$RatingLabels=="4.9",]$Mean_RSG)$out;OutRSG;
outind<-which(BurgundyDB4[BurgundyDB4$RatingLabels=="4.9",]$Mean_RSG %in% OutRSG);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$RatingLabels=="4.5",]$Mean_RSG);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$RatingLabels=="4.9",]$Mean_RSG,trim = 0.0064);
TrimMean;
BurgundyDB4[BurgundyDB4$RatingLabels=="4.9",]$Mean_RSG[outind] = TrimMean;

# Check YR (year) variable outliers 
boxplot(BurgundyDB4$YR);
#Check how much  the Min,  1st QU,  Median and 3rd Qu change without the outliers 
boxplot.stats(BurgundyDB4$YR);
summary(BurgundyDB4$YR);
#Check the correlation of year with numerical variables 
cor(BurgundyDB4[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN")]);

#plot  YR with categorical variables
ggplot(BurgundyDB4, aes(x = BurgundyDB4$NAME, y = BurgundyDB4$YR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("NAME")+
  ylab("YEAR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$WINE, y = BurgundyDB4$YR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("WINE")+
  ylab("YEAR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$TP, y = BurgundyDB4$YR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("TP")+
  ylab("YEAR")

####
aggregate(BurgundyDB4$YR~NAME,BurgundyDB4,mean);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Vega Sicilia",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="La Rioja Alta",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Contino",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Imperial",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="La Vicalanda",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Losada",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Matsu",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Portal del Priorat",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Ramon Bilbao",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Sierra Cantabria",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Vega Sicilia",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$NAME=="Losada",]$YR);

aggregate(BurgundyDB4$YR~WINE,BurgundyDB4,mean);
aggregate(BurgundyDB4$YR~TP,BurgundyDB4,mean);
boxplot(BurgundyDB4[BurgundyDB4$TP=="Albarino",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Mencia",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Red",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Sherry",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Tempranillo",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$YR);
boxplot (BurgundyDB4[BurgundyDB4$TP=="Other",]$YR);

#CHECKING THE VARIABLE BD FOR OUTLIERS
boxplot(BurgundyDB4$BD)
boxplot.stats(BurgundyDB4$BD);
summary(BurgundyDB4$BD);
#checking for the position and indexes of the outliers
iOuts <- boxplot.stats(BurgundyDB4$BD)$out
iOuts;
outind <- which(BurgundyDB4$BD%in% iOuts)
outind;
length(outind)
#calculating the trimmed mean of BD
trimV<-length(outind)/length(BurgundyDB4$BD);
trimV;

TrimMean<-mean(BurgundyDB4$BD,trim = 0.011);
TrimMean;
#Replacing the outliers with the trimmed mean
BurgundyDB4$BD[outind] = TrimMean;
boxplot(BurgundyDB4$BD)


#CHECKING THE VARIABLE ACD FOR OUTLIERS #This mess up the corr with the rest of variables
# boxplot(BurgundyDB4$ACD);
# boxplot.stats(BurgundyDB4$ACD);
# summary(BurgundyDB4$ACD);
# 
# #checking for the position and indexes of the outliers
# iOuts <- boxplot.stats(BurgundyDB4$ACD)$out
# iOuts;
# outind <- which(BurgundyDB4$ACD%in% iOuts)
# outind;
# length(outind)
# #calculating the trimmed mean of BD
# trimV<-length(outind)/length(BurgundyDB4$ACD);
# trimV;
# 
# TrimMean<-mean(BurgundyDB4$ACD,trim = 0.034);
# TrimMean;
# #Replacing the outliers with the trimmed mean
# BurgundyDB4$ACD[outind] = TrimMean;
# boxplot(BurgundyDB4$ACD)

#########Check Outliers on MEAN_DN variable
boxplot(BurgundyDB4$Mean_DN);
boxplot.stats(BurgundyDB4$Mean_DN)
summary(BurgundyDB4$Mean_DN)

#checking for the position and indexes of the outliers
iOuts <- boxplot.stats(BurgundyDB4$Mean_DN)$out
iOuts;
outind <- which(BurgundyDB4$Mean_DN%in% iOuts)
outind;
length(outind)
#calculating the trimmed mean
trimV<-length(outind)/length(BurgundyDB4$Mean_DN);
trimV;

TrimMean<-mean(BurgundyDB4$Mean_DN,trim = 0.003);
TrimMean;
#Replacing the outliers with the trimmed mean
BurgundyDB4$Mean_DN[outind] = TrimMean;
boxplot(BurgundyDB4$Mean_DN)

####NUMR
boxplot(BurgundyDB4$NUMR);
boxplot.stats(BurgundyDB4$NUMR)
summary(BurgundyDB4$NUMR)

#checking for the position and indexes of the outliers
iOuts <- boxplot.stats(BurgundyDB4$NUMR)$out
iOuts;
outind <- which(BurgundyDB4$NUMR%in% iOuts)
outind;
length(outind)
#calculating the trimmed mean of BD
trimV<-length(outind)/length(BurgundyDB4$NUMR);
trimV;

TrimMean<-mean(BurgundyDB4$NUMR,trim = 0.08);
TrimMean;

BurgundyDB4$NUMR[outind] = TrimMean;
boxplot(BurgundyDB4$NUMR)

boxplot.stats(BurgundyDB4$NUMR)

#plot NUMR with categorical variables
ggplot(BurgundyDB4, aes(x = BurgundyDB4$TP, y = BurgundyDB4$NUMR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("TP")+
  ylab("NUMR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$WINE, y = BurgundyDB4$NUMR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("WINE")+
  ylab("NUMR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$NAME, y = BurgundyDB4$NUMR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("NAME")
ylab("NUMR")

#########Check outliers on RT variable
boxplot(BurgundyDB4$RT,main="RT (Rating)");
min(boxplot.stats(BurgundyDB4$RT)$out);

#base on the boxplot the outliers are the numbers more than 4.72
#create an auxiliary data frame sub-setting RT>4.72 and checking the categorical variables

BDaux<-BurgundyDB4[BurgundyDB4$RT>4.72,];
BDaux2<-data.frame(table(BDaux$TP));
BDaux2<-data.frame(table(BDaux$NAME));
BDaux2<-data.frame(table(BDaux$WINE));#Other is the unique observation

ggplot(BurgundyDB4, aes(x = BurgundyDB4$NAME, y = BurgundyDB4$RT)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("NAME")+
  ylab("RT")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$TP, y = BurgundyDB4$RT)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("TP")+
  ylab("RT")

#Check the variable RT using a boxplot subsetting by TP

#treating the outliers by replacing with the trim mean
boxplot(BurgundyDB4[BurgundyDB4$TP=="Red",]$RT);
iOuts<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Red",]$RT)$out;iOuts;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Red",]$RT %in% iOuts);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Red",]$RT);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Red",]$RT,trim = 0.015);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Red",]$RT[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT);
iOuts<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT)$out;iOuts;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT %in% iOuts);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT,trim = 0.025);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Rioja White",]$RT[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT);
iOuts<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT)$out;iOuts;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT %in% iOuts);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT,trim = 0.0095);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Rioja Red",]$RT[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT);
iOuts<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT)$out;iOuts;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT %in% iOuts);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT,trim = 0.21);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$RT[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Other",]$RT);
iOuts<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Other",]$RT)$out;iOuts;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Other",]$RT %in% iOuts);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Other",]$RT);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Other",]$RT,trim = 0.005);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Other",]$RT[outind] = TrimMean;

#########Check outliers on PR variable
boxplot(BurgundyDB4$PR,main="PR (Price)");

#Check variable PR with numerical variables (there are not strong correlation)
cor(BDaux[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN","AGE")]);

#plot PR with categorical variables
ggplot(BurgundyDB4, aes(x = BurgundyDB4$TP, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("TP")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$WINE, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("WINE")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$NAME, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("NAME")+
  ylab("PR")

#treating the outliers by replacing with the trim mean
boxplot(BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR);
OutPR<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR)$out;OutPR;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR %in% OutPR);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR,trim = 0.15);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Ribera Del Duero Red",]$PR[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR);
OutPR<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR)$out;OutPR;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR %in% OutPR);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR,trim = 0.21);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Toro Red",]$PR[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Red",]$PR);
OutPR<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Red",]$PR)$out;OutPR;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Red",]$PR %in% OutPR);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Red",]$PR);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Red",]$PR,trim = 0.13);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Red",]$PR[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR);
OutPR<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR)$out;OutPR;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR %in% OutPR);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR,trim = 0.19);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Priorat Red",]$PR[outind] = TrimMean;

boxplot(BurgundyDB4[BurgundyDB4$TP=="Other",]$PR);
OutPR<-boxplot.stats(BurgundyDB4[BurgundyDB4$TP=="Other",]$PR)$out;OutPR;
outind<-which(BurgundyDB4[BurgundyDB4$TP=="Other",]$PR %in% OutPR);outind;
trimV<-length(outind)/length(BurgundyDB4[BurgundyDB4$TP=="Other",]$PR);
trimV;
TrimMean<-mean(BurgundyDB4[BurgundyDB4$TP=="Other",]$PR,trim = 0.083);
TrimMean;
BurgundyDB4[BurgundyDB4$TP=="Other",]$PR[outind] = TrimMean;

################################################DESCRIPTIVE ANALYSIS

summary(BurgundyDB4);

#Mean and Sd of each numerical variable
round(apply(BurgundyDB4[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN","AGE")],2,mean),2);
round(apply(BurgundyDB4[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN","AGE")],2,sd),2);

BurgundyDB4 %>%
  na.omit() %>% 
  select_if(is.numeric) %>% 
  chart.Correlation()

#correlation matrix
corr<-round(cor(BurgundyDB4[,c("RT","NUMR","PR","YR","BD","ACD","Mean_RSG","Mean_AL","Mean_DN","AGE")]),2);

#Correlation heat map

corr<-melt(corr);
ggplot(data = corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 3) +
  scale_fill_gradient2(low = "yellow", high = "red",
                       limit = c(-1,1))+
  xlab("")+
  ylab("")+
  ggtitle("Burgundy correlation");

#summarization
aggregate(PR~(WINE+NAME+TP),BurgundyDB4,mean);
aggregate(PR~(RatingLabels),BurgundyDB4,mean);
setNames(cbind(aggregate(PR~(TP),BurgundyDB4,mean),aggregate(PR~(TP),BurgundyDB4,sd))[,-3],c("TP","Mean","Sd"));

#check on histograms
hist(BurgundyDB4$PR,freq=F,main="Histogram for PR",xlab="PRICE (PR)");lines(density(BurgundyDB4$PR), lwd=2, col=1);
hist(BurgundyDB4$RT);
hist(BurgundyDB4$YR);
hist(BurgundyDB4$NUMR);
hist(BurgundyDB4$BD);
hist(BurgundyDB4$ACD);
hist(BurgundyDB4$Mean_RSG);
hist(BurgundyDB4$Mean_AL);
hist(BurgundyDB4$Mean_DN);

#Plotting PR variable vs the numerical variables
ggplot(BurgundyDB4, aes(x = BurgundyDB4$RT, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("RT")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$NUMR, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("NUMR")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$YR, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("YR")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$BD, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("BD")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$Mean_RSG, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Mean RSG")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$Mean_AL, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Mean AL")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$Mean_DN, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Mean DN")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$TP, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("TP")+
  ylab("PR")

ggplot(BurgundyDB4, aes(x = BurgundyDB4$REG, y = BurgundyDB4$PR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("REG")+
  ylab("PR")

################################PREDICTIVE ANALYSIS

#train and test subset
trainInd <- sample(nrow(BurgundyDB4),nrow(BurgundyDB4)*0.8); 
trainInd;

Burgundy_train <- BurgundyDB4[trainInd,];
Burgundy_test <- BurgundyDB4[-trainInd,];

nrow(Burgundy_train);
nrow(Burgundy_test);

#Linear regression models

BurgundyLM1 <- lm(PR~(RT+NUMR+BD+ACD+Mean_RSG+Mean_AL+Mean_DN+AGE),data=Burgundy_train);
summary(BurgundyLM1);

BurgundyLM2 <- lm(PR~(RT+NUMR+BD+Mean_RSG+Mean_AL+AGE),data=Burgundy_train);
summary(BurgundyLM2);

#No linear regression models

BurgundyNLM1 <- loess(PR~(RT+NUMR+BD+AGE),data=Burgundy_train);
summary(BurgundyNLM1);

BurgundyNLM2 <- loess(PR~(BD+Mean_RSG+Mean_AL+AGE),data=Burgundy_train);
summary(BurgundyNLM2);

BurgundyNLM3 <- loess(PR~(RT+Mean_RSG+AGE),data=Burgundy_train);
summary(BurgundyNLM3);

##BurgundyLM1 PR~(RT+NUMR+BD+ACD+Mean_RSG+Mean_AL+Mean_DN+AGE) Residual standard error: 46.85
##BurgundyLM2 PR~(RT+NUMR+BD+Mean_RSG+Mean_AL+AGE) Residual standard error: 46.94
##BurgundyNLM1 PR~(RT+NUMR+BD+AGE) Residual Standard Error: 45.29
##BurgundyNLM2 PR~(BD+Mean_RSG+Mean_AL+AGE) Residual Standard Error: 41.8
##BurgundyNLM3 PR~(RT+Mean_RSG+AGE) Residual Standard Error: 114
##The two better models are BurgundyNLM1 and BurgundyNLM2

#predict the results

Burgundy_test$predictionLM1<-predict(BurgundyLM1, Burgundy_test);
Burgundy_test$predictionLM2<-predict(BurgundyLM1, Burgundy_test);
Burgundy_test$predictionNLM1<-predict(BurgundyNLM1, Burgundy_test);
Burgundy_test$predictionNLM2<-predict(BurgundyNLM2, Burgundy_test);
Burgundy_test$predictionNLM3<-predict(BurgundyNLM3, Burgundy_test);

#plot the results

ggplot(Burgundy_test, aes(predictionLM1,PR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("PR - Predicted Values")+
  ylab("PR - Observed Values")+
  ggtitle("Model BurgundyLM1");

ggplot(Burgundy_test, aes(predictionLM2,PR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("PR - Predicted Values")+
  ylab("PR - Observed Values")+
  ggtitle("Model BurgundyLM2");

ggplot(Burgundy_test, aes(predictionNLM1,PR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("PR - Predicted Values")+
  ylab("PR - Observed Values")+
  ggtitle("Model BurgundyNLM1");

ggplot(Burgundy_test, aes(predictionNLM2,PR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("PR - Predicted Values")+
  ylab("PR - Observed Values")+
  ggtitle("Model BurgundyNLM2");

ggplot(Burgundy_test, aes(predictionNLM3,PR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("PR - Predicted Values")+
  ylab("PR - Observed Values")+
  ggtitle("Model BurgundyNLM3");

#################################################### CLUSTER-BASED ANALYSIS AND FINDINGS
#Plotting the dependant variable with numerical variables
plot(BurgundyDB4[,c("RT","PR")], pch=21);
plot(BurgundyDB4[,c("AGE","PR")], pch=21);
plot(BurgundyDB4[,c("BD","PR")], pch=21);
plot(BurgundyDB4[,c("NUMR","PR")], pch=21);
plot(BurgundyDB4[,c("Mean_AL","PR")], pch=21);
plot(BurgundyDB4[,c("Mean_RSG","PR")], pch=21);

#K-Means Cluster Analysis

fitBurgundy1 <- kmeans(BurgundyDB4[,c("PR","AGE","RT")], 3);
fitBurgundy1;

scatterplot3d(x=BurgundyDB4$PR, y=BurgundyDB4$NUMR, 
        z=BurgundyDB4$RT, angle =200, pch = 21, color=fitBurgundy1$cluster * 2);

fitBurgundy2 <- kmeans(BurgundyDB4[,c("PR","Mean_AL","Mean_RSG")], 3);
fitBurgundy2;

scatterplot3d(x=BurgundyDB4$PR, y=BurgundyDB4$Mean_AL, 
              z=BurgundyDB4$Mean_RSG, angle=360, pch = 21, color=fitBurgundy2$cluster * 2);

fitBurgundy3 <- kmeans(BurgundyDB4[,c("PR","BD","AGE")], 3);
fitBurgundy3;

scatterplot3d(x=BurgundyDB4$PR, y=BurgundyDB4$AGE, 
              z=BurgundyDB4$BD, angle=150, pch = 21, color=fitBurgundy3$cluster * 2);


