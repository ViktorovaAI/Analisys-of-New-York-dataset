#Dataset Analysis Report "New York"
#Aleksandra Viktorova
#February 2, 2020 

#----LIBRARY----
library(dplyr)
library(tidyr)
library(modEvA)
library(ggplot2)

#----UPLOAD DATASET----

#Load the data set into R from the file.
DataSet_NY<-read.csv(file=file.choose(),header=TRUE,sep=",")


#------Questions 1 and 2-----

#Select the necessary variables
AccIDENTS_Set<-as_tibble(select(DataSet_NY[rowSums(is.na(DataSet_NY)) == 0,],-DATE,-WEEKDAY,-MONTHYEAR,-MONTH,-DAYMONTH,-PRCP_LVL))

#Combine the logical variables
AccIDENTS_Set$NoWD<-as.numeric(AccIDENTS_Set$HOLIDAY | AccIDENTS_Set$WEEKEND)
AccIDENTS_Set$NoSR<-as.numeric(AccIDENTS_Set$WT01 | AccIDENTS_Set$WT02 |
                               AccIDENTS_Set$WT03 | AccIDENTS_Set$WT04 |
                               AccIDENTS_Set$WT06 | AccIDENTS_Set$WT08 |
                               AccIDENTS_Set$WT09)

#Calculate the correlation matrix
cor(select(AccIDENTS_Set, -HOLIDAY, -WEEKEND, -WT01, 
                          -WT02, -WT03, -WT04, -WT06, -WT08, -WT09))


model1<-glm(ACCIDENTS ~ NoWD + TRAFFIC, data=AccIDENTS_Set, family=Gamma)
summary(model1)
RsqGLM(model1)

#-----Question 3-----

#Select the necessary variables
BIKE_Set<-as_tibble(select(DataSet_NY,AWND,BIKE,TAVG,YEAR))

#Calculate the new correlation matrix
cor(BIKE_Set[rowSums(is.na(BIKE_Set)) == 0,])

#Build chart the number of of CitiBike Trips and average temperature by years
ggplot(data=BIKE_Set[rowSums(is.na(BIKE_Set)) == 0,])+
  geom_point(aes(x = TAVG, y = BIKE,color = YEAR))+
  labs(x = "Average temperature,°F",y = "Number of CitiBike Trips",color = "Year")+
  ggtitle("Relationship between bike rides and average temperature")


#-----Question 4------

#Select the necessary variables
MW_Accident<-as_tibble(select(DataSet_NY[DataSet_NY$YEAR != 2013,],WEEKDAY,MONTH, ACCIDENTS))

#Group the data by month
M_GROUPS<-group_by(MW_Accident, MONTH) %>% 
    summarise(AV_acc = mean(ACCIDENTS))

#Months with maximum and minimum values
M_GROUPS$MONTH[c(which.max(M_GROUPS$AV_acc),which.min(M_GROUPS$AV_acc))]

#Group the data by day of the week
WD_GROUPS<-group_by(MW_Accident, WEEKDAY) %>% 
  summarise(AV_acc = mean(ACCIDENTS))

#Days of the week with maximum and minimum values
WD_GROUPS$WEEKDAY[c(which.max(WD_GROUPS$AV_acc),which.min(WD_GROUPS$AV_acc))]

#------Question 5-------

#Select the necessary variables
Precipitation_TransportSet<-as_tibble(select(DataSet_NY,PRCP_LVL,BIKE,TAXI,GREEN))

#Group the data by Precipitation group
Groups_PT<-group_by(Precipitation_TransportSet[rowSums(is.na(Precipitation_TransportSet)) == 0,],
                       PRCP_LVL) %>% 
            summarise( 
                      sumBike = sum(BIKE),
                      sumTaxi=sum(TAXI),
                      sumGreen=sum(GREEN)) 

#Create data table
Groups_Set<-as.table(rbind(Groups_PT$sumBike,Groups_PT$sumTaxi,Groups_PT$sumGreen))
dimnames(Groups_Set) <- list(Transports = names(Precipitation_TransportSet[2:4]),
                                       Precipitation = unique(Precipitation_TransportSet$PRCP_LVL))

#Chi-Square Test
chisq.test(Groups_Set)



      



