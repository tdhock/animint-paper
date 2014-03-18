#' 2006 Data Expo: 
#' Data source: 
#' NASA Goddard Institute for Space Studies (GISS)
#' subset of the monthly climatology of the International 
#' Satellite Cloud Climatology Project (ISCCP), which was
#' developed “to collect weather satellite radiance
#' measurements and to analyze them to infer the
#' global distribution of clouds, their properties, and
#' their diurnal, seasonal and interannual variations.”
#' 
#' Data contains: Monthly observations of atmospheric variables 1995-2000, 
#' between 113.75ºW-56.25ºW, 21.25ºS-36.25ºN with 2.5º grid
#' spacing.
#' 
#' Variables: pressure, temperature, ozone and low, medium, 
#' high cloud cover.
#' The pressure, ozone and near-surface temperature
#' observations are computed from the TIROS Optical Vertical 
#' Sounder (TOVS) satellite climatology observations
#'
#' Temperatures are given in degrees celsius (original data had Kelvin).

works_with_R("3.0.2",
             animint="2014.3.13",
             ggplot2="0.9.3.1",
             maps="2.3.6",
             lubridate="1.3.3",
             plyr="1.8.1")


data(climate)
climate$time2 <- decimal_date(ymd(as.character(climate$date)))

countries <- map_data("world")
# Map coordinate limits chosen so that the polygons displayed are at least reasonably complete. 
countries <- subset(countries, (lat < 38)&(lat>-24))
countries <- subset(countries, ((-long)>54)&((-long)<118))

# Create variable showing temp-avg.monthly.temp at that location
climate <- ddply(climate, .(id, month), transform, tempdev = temperature - mean(temperature), surfdev = surftemp - mean(surftemp))
climate <- climate[order(climate$date, climate$id), ]

# data frame with formatted labels
dates <- ddply(climate, .(date), summarise, month=month[1], year = year[1], time2 = time2[1], textdate = paste(month.name[month], year))
dates <- dates[order(dates$date),]

one.time <- subset(climate, time2==time2[1])
str(one.time)

# we will re-use this set of elements, so let's define a function to add them to a plot p with tiles.
# we can't define the base plot first because the path must be drawn on top of the tiles.
plainmap <- function(p){
  p +
    geom_path(data=countries, aes(x=long, y=lat, group=group), col="grey") + 
  geom_text(data=dates, aes(x=-86, y=39, label=textdate, showSelected=time2))+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())
}

# tiles with temperature data to serve as the background for the plot.
temptiles <- ggplot() + 
  geom_tile(aes(long, lat, fill=tempdev,
                clickSelects=id, showSelected=time2),
            data=climate)+ 
  geom_tile(aes(long, lat, fill=tempdev,
                showSelected=id, showSelected2=time2),
            data=climate, color="black")+
  scale_fill_gradient2("deg. C", low="blue", mid="white", high="red", limits=c(-20, 20), midpoint=0) + 
  ggtitle("Deviation from monthly norm")

climate <- climate[order(climate$time2),]

viz <-
  list(timeSeries=ggplot()+
       make_text(climate, 1998, 39, "id", "region col-row = %s")+
       geom_hline(yintercept=0)+
       make_tallrect(climate, "time2") +
       xlab("Year of measurement")+
       ylab("Surface temperature (degrees Celsius)")+
       geom_line(aes(time2, temperature, group=id, clickSelects=id),
                 data=climate, alpha=1/2 + 3/100),
       ##geom_text(aes(x=1998, y=-5, label=textdate, showSelected=time2),
       ##          data=dates),
       surface=plainmap(ggplot() + 
         geom_tile(aes(long, lat, fill=surftemp,
                       clickSelects=id, showSelected=time2),
                   data=climate)+ 
         geom_tile(aes(long, lat, fill=surftemp,
                       showSelected=id, showSelected2=time2),
                   data=climate, color="black")+
         scale_fill_gradient2("deg. C", low="blue", mid="white", high="red",
                              limits=c(-10, 45), midpoint=0) + 
         ggtitle("Surface Temperature")),
       air=plainmap(temptiles)
       ##time = list(variable="time2", ms=3000),
       ##width = list(450),
       ##height = list(450)
       )

gg2animint(viz, "climate")
