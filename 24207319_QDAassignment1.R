# settinmg the working directory
setwd("~/Downloads/R/qda2024/Assignment 1")


#loading the dataset
susrtlxdata= read.csv("SusRtlx.csv")

# number of rows 
nrow(susrtlxdata)#100

#getting first few rows of the dataset
head(susrtlxdata)

#getting last rows of the dataset
tail(susrtlxdata)

#checking for any missing values
any(is.na(susrtlxdata)) #The result is FALSE, there are no NA values

#checking for any score in SUS <0
which(susrtlxdata$SUS.Score<0)#result =92, hence this column value needs to be replaced or row must be removed 
#checking for any score in SUS >100 
which(susrtlxdata$SUS.Score>100) #result=88 , hence this column value needs to replaced or the row can be removed

#cleaning of data by removing the row
susrtlxdata1.clean=susrtlxdata[-c(92,88),]

#viewing the csv file after cleaning it 
View(susrtlxdata1.clean)

# checking for any score in RTLX <0 or >126
which(susrtlxdata1.clean$RTLX.Score<0) # result=integer(0) which mean no value in dataset is <0
which(susrtlxdata1.clean$RTLX.Score>126) # result=integer(0) which mean no value in dataset is >100

#Descriptive statistics for the new cleaned datasets for SUS and RTLX scores

min(susrtlxdata1.clean$SUS.Score)
max(susrtlxdata1.clean$SUS.Score)
mean(susrtlxdata1.clean$SUS.Score)
median(susrtlxdata1.clean$SUS.Score)
sd(susrtlxdata1.clean$SUS.Score) # sd=23.2
IQR(susrtlxdata1.clean$SUS.Score)# IQR=34.37

min(susrtlxdata1.clean$RTLX.Score)
max(susrtlxdata1.clean$RTLX.Score)
mean(susrtlxdata1.clean$RTLX.Score)
median(susrtlxdata1.clean$RTLX.Score)
sd(susrtlxdata1.clean$RTLX.Score) # sd=9.37
IQR(susrtlxdata1.clean$RTLX.Score)# IQR=12.75

#install package ggplot2 for plotting graphs
install.packages('ggplot2')

#load the package
library(ggplot2)

#plotting the box plot to identify the outliers
boxplot(susrtlxdata2.clean$SUS.Score,susrtlxdata2.clean$RTLX.Score, main="Box plot for SUS and RTLX scores", ylim=c(0,100),ylab="Scores",names=c("SUS","RTLX"))#no outliers found

#plotting histogram to check the data distribution 

hist(susrtlxdata2.clean$SUS.Score, main="Histogram for SUS Score",xlab='SUS score', col='light green', xlim=c(0,150))
hist(susrtlxdata2.clean$RTLX.Score, main="Histogram for RTLX Score",xlab='RTLX score', col='light blue', xlim=c(0,100),ylim=c(0,50))
#install package ggplot2 for plotting graphs
install.packages('ggplot2')

#load the package
library(ggplot2)
# plotting scatter plot graph
ggplot(susrtlxdata2.clean,aes(x=RTLX.Score ,y=SUS.Score))+
  geom_point(color='red',size=2,alpha=0.8)+
  geom_smooth( method='lm' ,color='purple')+
  labs(title ="Scatter plot for scores",
       x="RTLX Scores(subjective workload)",
       y="SUS Scores(usability)")+
  theme_minimal()

#finding relation between RTLX and SUS USING pearsons correlation test
cor.test(susrtlxdata1.clean$SUS.Score, susrtlxdata1.clean$RTLX.Score) # r(96)=0.6809193, p<0.001(1.216e-14)

#H1 hypothesis is true where p<0.001 , hence we can reject the null hypothesis
#between workload and usability there is a  moderately strong relationship




