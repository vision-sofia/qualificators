library(tidyverse)
library(rgdal)
library(rgeos)
library(tmap)
library(sp)
library(RColorBrewer)
library(leaflet)
#Import the shape files
setwd("~/SofiaHack/sofia_data/Population/Shape")
Output.Areas<- readOGR(".", "Naselenie_kvartali", encoding = "Cyrillic")
#DO a simple plot
plot(Output.Areas)

#NA
Output.Areas@data[,4:22][Output.Areas@data[,4:22] == 0] <- NA

sum(is.na(Output.Areas$Broi_Lica))/ NROW(Output.Areas@data)
#41 % of the regions are missing

#Calculate population density
Output.Areas$Broi_Lica<-as.numeric(Output.Areas$Broi_Lica)
Output.Areas$Density <- Output.Areas$Broi_Lica/Output.Areas$Area_m2

#Plot the population density
qtm(shp = Output.Areas, fill = "Density")

# select rows of data where population density is more than 0.0010
dense_pop <- Output.Areas@data[Output.Areas$Density > 0.0010, ]
write_csv(dense_pop,"dense_pop.csv")

#plot the number of apartments
Output.Areas$Broi_Jil<-as.numeric(Output.Areas$Broi_Jil)

tm_shape(Output.Areas) + tm_fill("Broi_Jil", title = "# of Apartments", palette = "RdPu") +
  tm_borders(alpha = 1)


Output.Areas$Pperhouse <- Output.Areas$Broi_Jil/Output.Areas$Broi_Lica
mean(Output.Areas$Pperhouse, na.rm = T) #2.773392

tm_shape(Output.Areas) + tm_fill("Pperhouse", title = "People Per House", palette = "OrRd") +
  tm_borders(alpha = 1)

dense_people <- Output.Areas@data[Output.Areas$Pperhouse > 20, ]
write_csv(dense_people,"dense_people.csv")

Output.Areas$houseperarea <- Output.Areas$Broi_Jil/Output.Areas$Area_m2
mean(Output.Areas$houseperarea,na.rm = T) #0.0001952982

#M/F ratio
Output.Areas$Male<- as.numeric(Output.Areas$Male)
Output.Areas$Female <- as.numeric(Output.Areas$Female)
mean(Output.Areas$Female / Output.Areas$Male, na.rm=T)
#2.105585

#Young
Output.Areas$Age15_24<- as.numeric(Output.Areas$Age15_24)
Output.Areas$YoungPer<- Output.Areas$Age15_24/Output.Areas$Broi_Lica
tm_shape(Output.Areas) + tm_fill("YoungPer", title = "% of Age 15-24", palette = "OrRd") +
  tm_borders(alpha = 1)

Output.Areas$Educ3 <- as.numeric(Output.Areas$Educ3)
Output.Areas$Lowedu<- Output.Areas$Educ3/Output.Areas$Broi_Lica
tm_shape(Output.Areas) + tm_fill("Lowedu", title = "% of Age 15-24", palette = "OrRd") +
  tm_borders(alpha = 1)


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

tm_shape(Output.Areas) + tm_fill("nstops", title = "Stops", palette = "OrRd") +
  tm_borders(alpha = 1)

##
UTMcoor.df <- SpatialPointsDataFrame(UTMcoor, data.frame(id=1:length(UTMcoor)))
##
library(devtools)
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
devtools::install_github("ironholds/geohash")
install.packages("Rtools")
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

tm_shape(Output.Areas) + tm_fill("nsports", title = "Stops", palette = "OrRd") +
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

tm_shape(Output.Areas) + tm_fill("nsports", title = "Stops", palette = "OrRd") +
  tm_borders(alpha = 1)

###############
library("jsonlite")
#library("rjson")
setwd("~/SofiaHack/sofia_data/Civic reports")
# Import data from json file
problems <- fromJSON(txt= "problems.json" )
problems <- as.data.frame(problems)
View(head(problems))
prob_df <- problems$location
names(prob_df)[1:2]<- c("X","Y")
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
# need to bind the census data to our original points
Output.Areas$nproblems

tm_shape(Output.Areas) + tm_fill("nproblems", title = "Stops", palette = "OrRd") +
  tm_borders(alpha = 1)
