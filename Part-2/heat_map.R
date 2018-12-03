#use file StateDataforMap_2017-18week4.csv
#clear previous environmnet variables
heatmap<-read.csv(file.choose())

library(ggplot2)
library(maps)
region <- tolower(heatmap$STATENAME)
states <- map_data("state")
x <- c("Alabama",	"Alaska",	"Arizona",	"Arkansas",	"California",	"Colorado",	"Connecticut",	"Delaware",	"District of Columbia",	"Florida",	"Georgia",	"Hawaii",	"Idaho",	"Illinois",	"Indiana",	"Iowa",	"Kansas",	"Kentucky",	"Louisiana",	"Maine",	"Maryland",	"Massachusetts",	"Michigan",	"Minnesota",	"Mississippi",	"Missouri",	"Montana",	"Nebraska",	"Nevada",	"New Hampshire",	"New Jersey",	"New Mexico",	"New York",	"North Carolina",	"North Dakota",	"Ohio",	"Oklahoma",	"Oregon",	"Pennsylvania",	"Rhode Island",	"South Carolina",	"South Dakota",	"Tennessee",	"Texas",	"Utah",	"Vermont",	"Virginia",	"Washington",	"West Virginia",	"Wisconsin",	"Wyoming",	"Guam",	"Puerto Rico",	"Virgin Islands")
heatmap$stateabb<-state.abb[match(x,state.name)]
heatmap$ACTIVITY.LEVEL <- (gsub('Level 10', 'High1', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 9', 'High2', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 8', 'High3', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 7', 'Moderate1', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 6', 'Moderate2', heatmap$ACTIVITY.LEVEL))

heatmap$ACTIVITY.LEVEL <- (gsub('Level 5', 'Low1', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 4', 'Low2', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 3', 'Minimal1', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 2', 'Minimal2', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 1', 'Minimal3', heatmap$ACTIVITY.LEVEL))
heatmap$ACTIVITY.LEVEL <- (gsub('Level 0', 'Insufficient Data', heatmap$ACTIVITY.LEVEL))

ILI.Activity.Level<-heatmap$ACTIVITY.LEVEL
geo_data<-data.frame(region=unique(region),ILI.Activity.Level)
map.df <- merge(states,geo_data, by="region", all.x =T)
v<-qplot(long,lat, data=map.df, geom = "polygon",
         main="2017-18 Influenza Season Week 4 ending Jan 27, 2018",group=group, fill=ILI.Activity.Level)

v<-v + scale_fill_manual(values = c("firebrick4", "firebrick1", "darkorange3", "white", "darkolivegreen1", "darkolivegreen4", "green", "yellow"))
v
ggsave("heat_map.pdf")