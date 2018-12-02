library(tidyverse)
library(rgdal)
library(rgeos)
library(tmap)
library(sp)
library(RColorBrewer)
library(leaflet)
#Import the shape files
setwd("~/SofiaHack/sofia_data/Population/Shape")
Output.Areas<- readOGR(".", "Naselenie_kvartali")
#Convert Encoding
Output.Areas@data$RegName<- as.character(Output.Areas@data$RegName)
Encoding(Output.Areas@data$RegName) <- "UTF-8"
Output.Areas@data$RegName

Output.Areas@data$Rajon<- as.character(Output.Areas@data$Rajon)
Encoding(Output.Areas@data$Rajon) <- "UTF-8"
Output.Areas@data$Rajon

Output.Areas@data$NSettlemen <- as.character(Output.Areas@data$NSettlemen)
Encoding(Output.Areas@data$NSettlemen) <- "UTF-8"
Output.Areas@data$NSettlemen


Output.Areas@data[, 4:ncol(Output.Areas@data)] <- lapply(4:ncol(Output.Areas@data), function(x) as.numeric(Output.Areas@data[[x]]))#DO a simple plot

#plot(Output.Areas)

#NA
Output.Areas@data[,4:22][Output.Areas@data[,4:22] == 0] <- NA

sum(is.na(Output.Areas$Broi_Lica))/ NROW(Output.Areas@data)
#41 % of the regions are missing

#Calculate population density
Output.Areas$Broi_Lica<-as.numeric(Output.Areas$Broi_Lica)
Output.Areas$Density <- Output.Areas$Broi_Lica/Output.Areas$Area_m2

#Plot the population density
qtm(shp = Output.Areas, fill = "Density")


dd_pop<-Output.Areas@data %>%
  arrange_(~ desc(Density)) %>%
  head(n = 5)
write_csv(dd_pop,"top_dense.csv")
# select rows of data where population density is more than 0.0010
dense_pop <- Output.Areas@data[Output.Areas$Density > 0.0010, ]
write_csv(dense_pop,"dense_pop.csv")

#plot the number of apartments
Output.Areas$Broi_Jil<-as.numeric(Output.Areas$Broi_Jil)

tm_shape(Output.Areas) + tm_fill("Broi_Jil", title = "# of Apartments", palette = "RdPu") +
  tm_borders(alpha = 1)


Output.Areas$Pperhouse <- Output.Areas$Broi_Lica / Output.Areas$Broi_Jil


tm_shape(Output.Areas) + tm_fill("Pperhouse", title = "People Per House", palette = "OrRd") +
  tm_borders(alpha = 1)

dense_people <- Output.Areas@data[Output.Areas$Pperhouse > 20, ]
write_csv(dense_people,"dense_people.csv")
Output.Areas@data[Output.Areas$Pperhouse > 20, ] <- NA

tm_shape(Output.Areas) + tm_fill("Pperhouse", title = "People Per House", palette = "OrRd") +
  tm_borders(alpha = 1)

median(Output.Areas@data$Broi_Jil, na.rm=T) #45
median(Output.Areas$Pperhouse, na.rm = T) # 1

dense_people <- Output.Areas@data[Output.Areas$Pperhouse > 10, ]
write_csv(dense_people,"dense_people2.csv")

Output.Areas$houseperarea <- Output.Areas$Area_m2 / Output.Areas$Broi_Jil
mean(Output.Areas$houseperarea,na.rm = T) #964537.1
median(Output.Areas$houseperarea,na.rm = T) #964537.1
#M/F ratio

Output.Areas$mfratio <- Output.Areas$Male / Output.Areas$Female
median(Output.Areas$mfratio , na.rm=T)
moremen<- Output.Areas@data[Output.Areas$mfratio > 1, ]
write_csv(moremen,"moremen.csv")

morewomen<- Output.Areas@data[Output.Areas$mfratio < 1, ]
write_csv(morewomen,"morewomen.csv")
#Young

Output.Areas$YoungPer<- Output.Areas$Age15_24/Output.Areas$Broi_Lica
tm_shape(Output.Areas) + tm_fill("YoungPer", title = "% of Age 15-24", palette = "OrRd") +
  tm_borders(alpha = 1)
young_people <- Output.Areas@data[Output.Areas$YoungPer > 10, ]
write_csv(young_people,"young_people.csv")

dd_pop<-Output.Areas@data %>%
  arrange_(~ desc(YoungPer)) %>%
  head(n = 5)
write_csv(dd_pop,"top_youngг.csv")
#Higher Ed
Output.Areas$HighEdu<- Output.Areas$Educ1/Output.Areas$Broi_Lica
tm_shape(Output.Areas) + tm_fill("HighEdu", title = "% of Higher Ed.", palette = "OrRd") +
  tm_borders(alpha = 1)

edu_people <- Output.Areas@data[Output.Areas$HighEdu > 10, ]
write_csv(edu_people,"edu_people.csv")

setwd("~/SofiaHack/sofia_data/Transport/Subway")
Subway<- readOGR(".", "Subway", encoding = "UTF-8")
plot(Subway)

proj4string(Subway)
proj4string(Output.Areas)



tm_shape(Output.Areas) +
  tm_borders(alpha = 1) +
  tm_shape(Subway) + tm_dots(size = .25, col = 'blue')


# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(Subway, Output.Areas)

# need to bind the census data to our original points
Subway@data <- cbind(Subway@data, pip)

Output.Areas@data$Subway <- ifelse(Output.Areas@data$RegName %in% Subway@data$RegName, 1, 0)
sum(Output.Areas@data$Subway)#44

###########
setwd("~/SofiaHack/sofia_data/Transport/Bus & Tram/STOPS")
Stops<- readOGR(".", "ALL_STOPS_UTM_point", encoding = "UTF-8")
plot(Stops)
proj4string(Stops)

# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(Stops, Output.Areas)

nstopsdf <- pip %>% 
  count(RegName)
colnames(nstopsdf)[2]<- "nstops"

Output.Areas@data <- merge(x = Output.Areas@data, y = nstopsdf, by = "RegName", all.x = TRUE)
# need to bind the census data to our original points
Output.Areas$nstops

tm_shape(Output.Areas) + tm_fill("nstops", title = "Stops", palette = "Greens") +
  tm_borders(alpha = 1)

nstopsdf <- Output.Areas@data[Output.Areas$nstops > 20, ]
write_csv(nstopsdf,"manystops.csv")
library(ggpmisc)
library(Hmisc)
Output.Areas@data %>%
  ggplot(aes(log(Area_m2),nstops))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x, color="black")+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +stat_summary(fun.data=mean_cl_normal)        



##
setwd("~/SofiaHack/sofia_data/Sport/Sportni Ploshtadki")
sport_pl<- readOGR(".", "Sportni_ploshtadki", encoding = "UTF-8")
plot(sport_pl)
proj4string(sport_pl)

# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(sport_pl, Output.Areas)

nsportsdf <- pip %>% 
  count(RegName)

colnames(nsportsdf)[2]<- "nsports"

Output.Areas@data <- merge(x = Output.Areas@data, y = nsportsdf, by = "RegName", all.x = TRUE)
# need to bind the census data to our original points
Output.Areas$nsports

tm_shape(Output.Areas) + tm_fill("nsports", title = "Public Sport", palette = "Blues") +
  tm_borders(alpha = 1)


####
library(readxl)
Multisport <- read_excel("~/SofiaHack/sofia_data/Sport/Multisport.xlsx")
Multisport <- Multisport[Multisport[,2] == "София",]
spclubsc<- Multisport[,c(7,6)]
names(spclubsc)<- c("X","Y")
spclubsc<- as.data.frame(spclubsc)
spclubsc$X<- as.numeric(spclubsc$X)
spclubsc$Y<- as.numeric(spclubsc$Y)
spclubsc<- spclubsc[complete.cases(spclubsc), ]

coordinates(spclubsc) <- c("X", "Y")
proj4string(spclubsc) <- CRS("+proj=longlat +datum=WGS84")  ## for example
spclubsc <- spTransform(spclubsc, CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#msportdf <- SpatialPointsDataFrame(spclubsc, data.frame(id=1:NROW(spclubsc)))


# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(spclubsc, Output.Areas)

nsportsdf <- pip %>% 
  count(RegName)

colnames(nsportsdf)[2]<- "nclubs"

Output.Areas@data <- merge(x = Output.Areas@data, y = nsportsdf, by = "RegName", all.x = TRUE)
# need to bind the census data to our original points
Output.Areas$nclubs

tm_shape(Output.Areas) + tm_fill("nclubs", title = "Sport Clubs", palette = "Purples") +
  tm_borders(alpha = 1)


Output.Areas@data$SportOv<- Output.Areas@data$nclubs + Output.Areas@data$nsports
sportov<-Output.Areas@data %>%
  arrange_(~ desc(SportOv)) %>%
  head(n = 5)
write_csv(sportov,"sportov.csv")

Output.Areas@data %>%
  ggplot(aes(Density,log(SportOv)))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x, color="black")+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +stat_summary(fun.data=mean_cl_normal)      

###############
library("jsonlite")
#library("rjson")
setwd("~/SofiaHack/sofia_data/Civic reports")
# Import data from json file
problems <- fromJSON(txt= "problems.json" )
prob_df <- as.data.frame(cbind(problems$id,problems$violationType))
prob_df <- cbind(prob_df,problems$location$longitude,problems$location$latitude)
names(prob_df) <- c("id","violationType", "longitute","latitude")
#problems <- as.data.frame(problems)
#View(head(problems))
#View(head(prob_df))

prob_df$violationType<- as.character(prob_df$violationType)
Encoding(prob_df$violationType) <- "UTF-8"
#prob_df$violationType
#prob_df$location <- NULL

names(prob_df)[4:3]<- c("X","Y")
prob_df<- prob_df[complete.cases(prob_df), ]

coordinates(prob_df) <- c("X", "Y")
proj4string(prob_df) <- CRS("+proj=longlat +datum=WGS84")  ## for example
prob_df <- spTransform(prob_df, CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(prob_df, Output.Areas)

nproblemsdf <- pip %>% 
  count(RegName)

colnames(nproblemsdf)[2]<- "nproblems"

Output.Areas@data <- merge(x = Output.Areas@data, y = nproblemsdf, by = "RegName", all.x = TRUE)
# 
Output.Areas$nproblems
tmap_mode("plot")
tm_shape(Output.Areas) + tm_fill("nproblems", title = "Civic Reports", palette = "Reds") +
  tm_borders(alpha = 1)



View(head(Output.Areas@data))



Output.Areas@data<- Output.Areas@data[complete.cases(Output.Areas@data[,c("Density","Subway","nstops","SportOv","nproblems")]),]
Output.Areas@data$Index <- mean(-1*Output.Areas@data$Density+Output.Areas@data$Subway+
                                  Output.Areas@data$nstops+Output.Areas@data$SportOv-
                                  Output.Areas@data$nproblems, na.rm=T)
index<-Output.Areas@data %>%
  arrange_(~ desc(Index)) %>%
  head(n = 50)
write_csv(index,"index.csv")

# creates a coloured dot map
# turns view map on
tmap_mode("view")
tm_shape(Output.Areas) + tm_borders(alpha=.4) +
  tm_shape(prob_df) + tm_dots(scale = 1.5, palette = "Reds", shape = "violationType", title = "Civic Reports")
  
# counts (or sums of weights)
g <- ggplot(prob_df@data, aes(reorder(violationType, table(violationType)[violationType])))
g + geom_bar(position = position_stack(reverse = TRUE)) +
  coord_flip()

####
AirQ <- read_csv("~/SofiaHack/sofia_data/Air Quality/Air Tube/AirQ.csv")

names(AirQ)[10:9]<- c("X","Y")
AirQ<- AirQ[complete.cases(AirQ), ]

coordinates(AirQ) <- c("X", "Y")
proj4string(AirQ) <- CRS("+proj=longlat +datum=WGS84")  ## for example
AirQ <- spTransform(AirQ, CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


medi$AVG_MED<- as.numeric(medi$AVG_MED)
#########
g <- ggplot(medi, aes())
g + geom_bar(position = position_stack(reverse = TRUE)) +
  coord_flip()

medi<- medi%>%
  arrange_(~desc(AVG_MED)) %>%
  head(NROW(medi))

# Basic barplot
p<-ggplot(data=medi, aes(x=reorder(MUNICIPALITY,AVG_MED,sum), y=AVG_MED)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip() + xlab("ОБЩИНА") + ylab("Среден брой лекари")
