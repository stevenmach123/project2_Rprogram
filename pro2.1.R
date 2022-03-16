library(leaflet)
library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(rgeos)

lote2 <- read.table(file = "2010_plnt_data.csv", sep = ",",skipNul="TRUE",header = TRUE)
lote4 <- read.table(file = "2000_plnt_data.csv", sep = ",",skipNul="TRUE",header = TRUE)

sapply(lote2,class)
lote2 <-subset(lote2, !is.na(Lat) & !is.na(Long))
names(lote2)[names(lote2) == "STATE"] <- "State"
lote21 <-melt(data =lote2,id.vars=c("State","Plant","Lat","Long"))
lote21$value <- gsub(",","",lote21$value,fixed=TRUE)
lote21$value <- as.numeric(lote21$value)
lote21$value<-ifelse(lote21$value<0,0,lote21$value)

#lote27 <- group_by(lote21,State,Plant,Long,Lat,variable) %>% summarise_at(vars(value),list(value=sum))

lote22 <- dcast(data=lote21,State+Plant+Long+Lat~variable)


lote22$Other <-ifelse(lote22$Other1==0,lote22$Other2,lote22$Other1) 
lote22<-lote22[names(lote22) !="Other1" & names(lote22) !="Other2"] 

lote23 <-subset(lote22,Coal==0 & Gas==0 & Oil==0 &Nuclear==0 & Hydro==0 & Solar==0 & Biomass==0& Wind==0& Geo==0 &Other==0)

lote22 <-subset(lote22,Coal>0 |Gas>0 |Oil>0 | Nuclear>0| Hydro>0 | Solar>0| Biomass>0| Wind>0| Geo>0 |Other>0 )


names(lote4)[names(lote4)=="Wood"]  <- "Biomass"
sapply(lote4,class)


lote43 <- subset(lote4,Lat != "N/A" & Long != "N/A" & Coal != "N/A" & Gas != "N/A" & Oil!="N/A" & Nuclear != "N/A" & Hydro != "N/A" & Solar !="N/A" & Biomass != "N/A" & Wind!= "N/A" & Geo != "N/A" & Other != "N/A")
lote43 <-subset(lote43,State !="")
lote4w <- lote43

lote43 <- melt(data =lote43,id.vars=c("State","Plant","Lat","Long"))
lote44 <-lote43
lote44$Lat <- as.numeric(lote43$Lat)
lote44$Long  <- as.numeric(lote43$Long)
lote44$value <- as.numeric(lote43$value)

lote44 <-subset(lote44,State !="")

lote432 <-group_by(lote44,State,Plant,Lat,Long,variable) %>%  summarise_at(vars(value),list(sum_val=sum))


lote433 <-dcast(lote432,State+Plant+Lat+Long~variable,value.var ="sum_val")


#lote41$Lat<-as.numeric(lote41$Lat)
#lote41$Long  <- as.numeric(lote41$Long)
#lote41$value <- as.numeric(lote41$value)
#lote41 <- subset(lote41,State !="")
#lote42 <- dcast(data=lote41,State+Plant+Long+Lat~variable)
#lote42 <-subset(lote41, !is.na(Lat)  & !is.na(Long) & !is.na(Coal) & !is.na(Gas) & !is.na(Oil) & !is.na(Nuclear) & !is.na(Hydro) & !is.na(Solar) & !is.na(Biomass) & !is.na(Wind) & !is.na(Geo) & !is.na(Other))


st<-c("AK","IL")
er<-lote22[lote22$State %in% st,]

round(12.2,digits=1)
