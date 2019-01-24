library(readr)
NYC<- read_csv("NYPD_Motor_Vehicle_Collisions.csv")
View(NYC)
library('dplyr')
#1. Compute the number of boroughs in the dataset
unique(NYC$BOROUGH,NA.RM=F)

#2. Compute the number of persons who got injured in each borough (10points)
NYC1<-NYC[which(!is.na(NYC$`NUMBER OF PERSONS INJURED`)), ]
tapply(NYC1$`NUMBER OF PERSONS INJURED`,NYC1$BOROUGH,sum)
summarise(group_by(NYC,BOROUGH), sum_injured=sum(`NUMBER OF PERSONS INJURED`,na.rm=T))
#3. Compute the number of persons who got killed in each borough (10points)
NYC2<-NYC[which(!is.na(NYC$`NUMBER OF PERSONS KILLED`)), ]
tapply(NYC2$`NUMBER OF PERSONS KILLED`,NYC2$BOROUGH,sum)
summarise(group_by(NYC,BOROUGH), sum_injured=sum(`NUMBER OF PERSONS KILLED`,na.rm=T))
#4. List the top 10 on street location from Brooklyn from where most of the injuries were reported (20 points)
library(magrittr)
a<-top10%>%
  group_by(`ON STREET NAME`)%>%
  summarise(sum_injured=sum(`NUMBER OF PERSONS INJURED`, na.rm=TRUE)) %>%
  arrange(desc(sum_injured))
a[c(1:11),]
top10<- NYC[which(NYC$BOROUGH  == "BROOKLYN"), ]
top10a<-top10[which(!is.na(top10$`NUMBER OF PERSONS INJURED`)), ]
a1<-tapply(top10a$`NUMBER OF PERSONS INJURED`,top10a$`ON STREET NAME`,sum)
a1<-sort(a1,decreasing = T, na.last = NA)
a1[c(1:10)]

#5. Create a new column "Address" and populate them with combined values of
#BOROUGH, ZIP CODE, ON STREET NAME, each string separated by a coma.
#Use Stringer package for this purpose (10 points)
install.packages("stringr")
library(stringr)
NYC$Address<-str_c(NYC$BOROUGH,NYC$`ZIP CODE`,NYC$`ON STREET NAME`,sep = ",")
View(NYC$Address)
#6. Calculate the average of number of people injured according to time in a day and
#print the top 10 hours of the day where this average was maximum in the
#Brooklyn borough(20 points)
install.packages("lubridate")
library(lubridate)
top101<- NYC[which(NYC$BOROUGH  == "BROOKLYN"&NYC$DATE=="11/15/2018"), ]
top101$TIME<-hms::as.hms(top101$TIME)
b<-top101%>%
  group_by(hour(TIME))%>%
  summarise(average=mean(`NUMBER OF PERSONS INJURED`, na.rm=TRUE)) %>%
  arrange(desc(average))
b[c(1:10),]

top101a<-top101[which(!is.na(top101$`NUMBER OF PERSONS INJURED`)), ]
b1<-tapply(top101a$`NUMBER OF PERSONS INJURED`,hour(top101a$TIME),mean)
b1<-sort(b1,decreasing = T, na.last = NA)
b1[c(1:10)]

#7. How does the number of person injured,and the number of persons killed vary
#according to each year for all the boroughs? (20 points)
NYC3$DATE <- as.Date(NYC3$DATE, "%m/%d/%Y")
summarise(group_by(NYC3,year(DATE)), count_injured=sum(`NUMBER OF PERSONS INJURED`,na.rm=T), count_killed=sum(`NUMBER OF PERSONS KILLED`,na.rm=T))

