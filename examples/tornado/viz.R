library(maps)
library(plyr)
library(animint)
data(UStornadoes)
ord <- order(unique(UStornadoes$TornadoesSqMile), decreasing=T)
stateOrder <- data.frame(state = unique(UStornadoes$state)[ord], rank = 1:49)
UStornadoes$state <-
  factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]
UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))
seg.color <- "#55B1F7"
viz <-
  list(map=ggplot()+
       theme_animint(width=750, height=500)+
       make_text(UStornadoes, -100, 50, "year",
                 "Tornado paths and endpoints in %d")+
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour=seg.color, data=UStornadoes)+
       geom_point(aes(endLong, endLat, showSelected=year),
                    colour=seg.color,
                  data=UStornadoes)+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="grey", colour="black", alpha=3/4)+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank()),
       ##time=list(variable="year", ms=2000),
       ts=ggplot()+
       theme_animint(width=300, height=400)+
       xlab("year")+
       ylab("Number of tornadoes")+
       geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                data=UStornadoCounts, stat="identity", position="identity")+
       make_text(UStornadoes, 1980, 200, "state")+
       geom_text(aes(year, count + 5, label=count,
                     showSelected2=year, showSelected=state),
                data=UStornadoCounts, size=20))
