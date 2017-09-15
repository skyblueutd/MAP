#install.packages("raster")
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("plyr")
#install.packages("scales")
#install.packages("maps")
#install.packages("rgdal")
#install.packages("maptools")
#install.packages("mapproj")
#install.packages("rgeos")
#install.packages("RColorBrewer")

library (raster)
library (ggplot2)
library (ggmap)
library (plyr)
library (scales)  ###set up the library we need
library (maps)
library (rgdal)
library (maptools)
library (mapproj)
library (rgeos)

#import the map data which is donwloaded from U.S.Census
usa.df <- readOGR(dsn="C:/Users/yangx/Documents/cb_2015_us_state_20m", layer="cb_2015_us_state_20m")
usa.df <- spTransform(usa.df, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
usa.df@data$id <- rownames(usa.df@data)

#move and reshape the map of alaska state
alaska <- usa.df[usa.df$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.2)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(usa.df)

#move and reshape the map of hawaii state
hawaii <- usa.df[usa.df$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(usa.df)

#cut the maps (such as DC) which are not states and combine it with alaska and hawaii
states48 <- usa.df[!usa.df$STATEFP %in% c("02", "15", "72","66","60","69","74","78",'11'),]
states.final <- rbind(states48, alaska, hawaii)
states.final@data$state=tolower(states.final@data$NAME)

#impot the chance file 
usa.dat <- read.csv(file="C:/Users/yangx/Documents/R/clinton_chance_sep19_2016.csv", header = T, sep = ",")
usa.dat$State <- tolower(usa.dat$State)
colnames(usa.dat) <- c("state", "Chance")

#fortify the map data of the 50 states 
states.final@data<-merge(states.final@data, usa.dat,by='state',sort = F,all.x=T)
states.plotting<-fortify(states.final, region="state")

#merge in the data we need to plot the graph
state.all<-merge(states.plotting,usa.dat,by.x='id',by.y = 'state', sort = F,all.x=T)  
state.all=state.all[order(state.all$order),]

#plotting function
p <- function(data, brks, title) {
  ggp <- ggplot() + 
    geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = Chance), color = "black", size = 0.15) + 
    scale_fill_distiller(palette = "RdBu", breaks = brks, trans = "reverse") + 
    theme_nothing(legend = TRUE) + labs(title = title, fill = "Hillary%")  
  return(ggp)
}

#set up plotting function and save the picture
brks.to.use <- seq(0, 100, by = 10)
figure.title <- "2016 election prediction"
ggsave(p(state.all, brks.to.use, figure.title), height = 4, width = 4*1.9, file = "usa_2016 election_prediction2.jpg")

