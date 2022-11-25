#Flights project   Maryann Meilika 
#installing package

library(ggplot2)
library(dplyr)


# Read flights data
mydata = read.csv("C:\\big data2\\big2\\flight.csv")
View(mydata)

#exploring dataset
nrow(mydata)
names(mydata)
summary(mydata)
str(mydata)
head(mydata)
tail(mydata) 

#Creating Vectors of days
days <- c("sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "saturday" ) 

#MATRIX 
# create a matrix of origin and dest and air port code from a series of vectors  

Origin<-c("OAK","RDU","SLC","TPA","TUS","ABQ","ALB","AMA","AUS")
Dest <-c("PHL","PHX","MDW", "SAN","BWI","DAL","DEN","LAS","LAX") 
Airport.Code <-c("PHL","PHX","SAN","DEN","DAL","MDW","BWI","MCO","LAX")


# Create box_office 
box_office <- c(Origin, Dest,Airport.Code) 

#and now the matrix 
flight_matrix <- matrix(box_office , byrow = TRUE,nrow = 3 ) 


# Vectors titles, used for naming 
titles <- c("Origin", "Dest", "Airport.Code") 

# Name the rows with titles 
rownames(flight_matrix) <- titles 
flight_matrix 

# Create a data frame of delayed
# Definition of vectors
Delays.Security <- c(15,2,3,0,1,14,17) 
Delays.Weather  <- c(599,85,60,47,76,171,413) 
Delays.Carrier <- c(1302,450,371,303,360,850,1078)
TaxiIn  <- c(5,5,5,3,2,4,4)
TaxiOut <- c(21,6,5,6,14,6,7)

delayed_df <- data.frame(Delays.Security,Delays.Weather,Delays.Carrier,TaxiIn,TaxiOut) 
delayed_df 



#	Using the DPLYR library 

#The code below keeps variable 'TailNum' in the front and the remaining variables follow that.
mydata1 = select(mydata, TailNum, everything())
mydata1

#adding delay_result column
mydata2 <- mutate(mydata, DELAY_RESULT = Flights.Delayed  + ArrDelay )
mydata2

#filter rows which origin= RDU and dest = PHL 
mydata3 <- mydata %>% filter(Origin == "RDU")%>% filter(Dest == "PHL")
mydata3

#filter rows which origin= SLC and dest = DAL and using Pipe
mydata4 <- mydata %>% filter(Origin == "SLC")%>% filter(Dest == "DAL")
mydata4

#filter rows of Distance =422 and  ArrDelay = 302
mydata5 <- mydata %>% filter(Distance == 422 , ArrDelay == 302)
mydata5

#Arrange rows with arrange()
mydata6 <-mydata %>% arrange(Flights.Diverted, Flights.Delayed)
mydata6

#Select  columns 
mydata7 <- mydata %>% select( "Month.Name", "TailNum","TaxiIn", "TaxiOut","Origin", "Dest" ,"Distance")
mydata7

#filter rows using Pipe that CarrierDelay !=NA and select some columns and arrange them by descending of distance
mydata8 <-mydata %>% filter(Delays.Carrier !="NA")%>% 
  select( "Month.Name", "TailNum","TaxiIn", "TaxiOut","Origin", "Dest" ,"Distance") %>%
  arrange(desc(Distance))%>%group_by(TailNum)
mydata8

#Calculate Rank for Variables

# calculate rank for variables from Flights.On.Time to Flights.Delayed.
mydata9 = mutate_at(mydata, vars(Flights.On.Time:Flights.Delayed), funs(Rank=min_rank(.)))
mydata9
#Reverse Rank  by highest number 
mydata13 = mutate_at(mydata, vars(Flights.On.Time:Flights.Delayed), funs(Rank=min_rank(desc(.))))
mydata13

#Charting

#	plot   
plot(mydata$Delays.Security) 

# Graph Delays.Security with blue line and circle points  
plot(mydata$Delays.Security, type="o", col="blue", ylim=c(1,20) , axes=FALSE, ann=TRUE) 


# Graph TaxiOut with red dashed line and square points 
lines(mydata$TaxiOut, type="o", pch=22, lty=2, col="red") 
# Make x axis
axis(1, at=1:180) 

axis(2, las=1, at=4*0:g_range[2])

# Make y axis with horizontal labels 
axis(2, las=1, at=4*0:g_range[2]) 


# Create a title with a red, bold/italic font 
title(main="Delays.Security & Time Out", col.main="red", font.main=4) 

# Create a legend at (1, g_range[2]) that is slightly smaller  
legend(1, g_range[2], c("Delays.Security","TaxiOut"), cex=0.8,  
       col=c("blue","red"), pch=21:22, lty=1:2); 

# Create box around plot 
box() 








## Selecting 7 rows
values_7<- head(mydata,7)

## Fetching Names for Labels


## Getting all the values of  ArrDelay  col
ArrDelay <- c(values_7[,11])


## Concatenation the values
v<-paste(days,"|",ArrDelay)


## create pie chart 
pie(values_7$ArrDelay, main="pie chart shows ArrDelay on days of the week ",col=rainbow(length(values_7)),labels=c(v), cex=0.8)

legend(-3.2, 1.0, c(days), cex=0.6, fill=rainbow(length(values_7)))


#creating a bar graph,that shows Total of carriers in my data 
# with a subset of Delays.Weather>=138, and  the count.
ggplot(data= subset(mydata,Delays.Weather>=138), aes(x =Carriers.Total )) + geom_bar( )+
  geom_text(stat='count', aes(label=..count..),vjust=-0.3)


#creating a bar graph,that shows Total of carriers in my data 
# with a subset of Delays.Security>=10, and  the count.
ggplot(data= subset(mydata,Delays.Security>=10), aes(x =Carriers.Total )) + geom_bar( )+
  geom_text(stat='count', aes(label=..count..),vjust=-0.3)


#creating a stacked bar graph of the Delays.Weather and
# divided out byMonth.Name with  fill=factor
ggplot(data= mydata,aes (x = factor (Month.Name),fill =factor(Delays.Weather<150))) + 
  xlab("Month.Name") +
  ylab("Delays.Weather") +
  geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#To create different boxplots for Delays.Weather for different levels of x= TaxiOut
mydata$TaxiOut = factor(mydata$TaxiOut)
ggplot(mydata, aes(x=TaxiOut, y=Delays.Weather)) + geom_boxplot()

#shows outlier for Delays.Weather.
ggplot(mydata, aes(x=TaxiOut, y=Delays.Weather)) + geom_boxplot(notch = TRUE)

# Creating a histogram and define the number of bins 
ggplot(data = mydata , aes( x = Flights.Cancelled)) + geom_histogram(color="black", fill="white", bins = 10)

#Creating Density Plot to present the distribution of a Delays.Late.Aircraft.
ggplot(mydata, aes( x = Delays.Late.Aircraft)) + geom_density( )


#Faceting   
#Faceting for Month.Name
ggplot(mydata, aes(TaxiOut, Delays.Weather)) +  geom_point() +  facet_wrap(~Month.Name,nrow = 3)













