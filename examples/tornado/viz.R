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
years <- data.frame(year=unique(UStornadoes$year))
states <- data.frame(state=unique(UStornadoes$state))
viz <- list(
  map=ggplot()+
    theme_bw()+
    theme_animint(width=750, height=500)+
    geom_text(aes(
      -100, 50, label=paste(
                  "Tornado paths and endpoints in ", year),
      showSelected=year),
              data=years)+
    geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                     showSelected=year),
                 colour=seg.color, data=UStornadoes)+
    geom_point(aes(endLong, endLat, showSelected=year),
               colour=seg.color,
               fill=NA,
               data=UStornadoes)+
    geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                 data=USpolygons, fill="grey", colour="black", alpha=3/4)+
    theme(axis.line=element_blank(), axis.text=element_blank(),
          panel.background=element_rect(color=NA),
          panel.border=element_rect(color=NA),
          axis.ticks=element_blank(), axis.title=element_blank()),
  ts=ggplot()+
    theme_bw()+
    theme_animint(width=300, height=400)+
    xlab("year")+
    ylab("Number of tornadoes")+
    geom_bar(aes(year, count, clickSelects=year, showSelected=state),
             data=UStornadoCounts, stat="identity", position="identity")+
    geom_text(aes(
      1980, 200, label=paste("state =", state), showSelected=state),
              data=states)+
    geom_text(aes(year, count + 5, label=count,
                  showSelected2=year, showSelected=state),
              data=UStornadoCounts, size=20))
animint2dir(viz)
