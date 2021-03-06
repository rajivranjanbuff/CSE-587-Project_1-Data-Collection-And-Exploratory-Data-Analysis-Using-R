#chosse file NCHSData04.csv
data<-read.csv(file.choose())
head(data)
library(dplyr)
library(ggplot2)
library(reshape2)
subset<- select(data, Year,Week,Percent.of.Deaths.Due.to.Pneumonia.and.Influenza,Expected,Threshold)
head(subset)
subset_row<- filter(subset, (Year>=2013))
subset_row <- subset_row[-c(1:39), ]
colnames(subset_row)[colnames(subset_row)=="Week"]<-"MMWR.Week"
colnames(subset_row)[colnames(subset_row)=="Percent.of.Deaths.Due.to.Pneumonia.and.Influenza"]<-"Percent.of.All.Deaths.Due.to.P.and.I"
subset_row$combined<-paste0(subset_row$Year,"-",subset_row$MMWR.Week)

p1<-ggplot(subset_row,aes(combined))+geom_line(aes(y=Percent.of.All.Deaths.Due.to.P.and.I,group=1,color="red",linetype="solid"))
p1<-p1+geom_line(aes(y=Threshold,group=1),color="black",linetype="solid")
p1<-p1+geom_line(aes(y=Expected,group=1),color="black",linetype="solid")
p1<-p1+scale_x_discrete(limits=c("2013-40",	"2013-41",	"2013-42",	"2013-43",	"2013-44",	"2013-45",	"2013-46",	"2013-47",	"2013-48",	"2013-49",	"2013-50",	"2013-51",	"2013-52",	"2014-1",	"2014-2",	"2014-3",	"2014-4",	"2014-5",	"2014-6",	"2014-7",	"2014-8",	"2014-9",	"2014-10",	"2014-11",	"2014-12",	"2014-13",	"2014-14",	"2014-15",	"2014-16",	"2014-17",	"2014-18",	"2014-19",	"2014-20",	"2014-21",	"2014-22",	"2014-23",	"2014-24",	"2014-25",	"2014-26",	"2014-27",	"2014-28",	"2014-29",	"2014-30",	"2014-31",	"2014-32",	"2014-33",	"2014-34",	"2014-35",	"2014-36",	"2014-37",	"2014-38",	"2014-39",	"2014-40",	"2014-41",	"2014-42",	"2014-43",	"2014-44",	"2014-45",	"2014-46",	"2014-47",	"2014-48",	"2014-49",	"2014-50",	"2014-51",	"2014-52",	"2014-53",	"2015-1",	"2015-2",	"2015-3",	"2015-4",	"2015-5",	"2015-6",	"2015-7",	"2015-8",	"2015-9",	"2015-10",	"2015-11",	"2015-12",	"2015-13",	"2015-14",	"2015-15",	"2015-16",	"2015-17",	"2015-18",	"2015-19",	"2015-20",	"2015-21",	"2015-22",	"2015-23",	"2015-24",	"2015-25",	"2015-26",	"2015-27",	"2015-28",	"2015-29",	"2015-30",	"2015-31",	"2015-32",	"2015-33",	"2015-34",	"2015-35",	"2015-36",	"2015-37",	"2015-38",	"2015-39",	"2015-40",	"2015-41",	"2015-42",	"2015-43",	"2015-44",	"2015-45",	"2015-46",	"2015-47",	"2015-48",	"2015-49",	"2015-50",	"2015-51",	"2015-52",	"2016-1",	"2016-2",	"2016-3",	"2016-4",	"2016-5",	"2016-6",	"2016-7",	"2016-8",	"2016-9",	"2016-10",	"2016-11",	"2016-12",	"2016-13",	"2016-14",	"2016-15",	"2016-16",	"2016-17",	"2016-18",	"2016-19",	"2016-20",	"2016-21",	"2016-22",	"2016-23",	"2016-24",	"2016-25",	"2016-26",	"2016-27",	"2016-28",	"2016-29",	"2016-30",	"2016-31",	"2016-32",	"2016-33",	"2016-34",	"2016-35",	"2016-36",	"2016-37",	"2016-38",	"2016-39",	"2016-40",	"2016-41",	"2016-42",	"2016-43",	"2016-44",	"2016-45",	"2016-46",	"2016-47",	"2016-48",	"2016-49",	"2016-50",	"2016-51",	"2016-52",	"2017-1",	"2017-2",	"2017-3",	"2017-4",	"2017-5",	"2017-6",	"2017-7",	"2017-8",	"2017-9",	"2017-10",	"2017-11",	"2017-12",	"2017-13",	"2017-14",	"2017-15",	"2017-16",	"2017-17",	"2017-18",	"2017-19",	"2017-20",	"2017-21",	"2017-22",	"2017-23",	"2017-24",	"2017-25",	"2017-26",	"2017-27",	"2017-28",	"2017-29",	"2017-30",	"2017-31",	"2017-32",	"2017-33",	"2017-34",	"2017-35",	"2017-36",	"2017-37",	"2017-38",	"2017-39",	"2017-40",	"2017-41",	"2017-42",	"2017-43",	"2017-44",	"2017-45",	"2017-46",	"2017-47",	"2017-48",	"2017-49",	"2017-50",	"2017-51",	"2017-52",	"2018-1",	"2018-2"),labels=c("40",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"50",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"10",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"20",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"30",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"40",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"50",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"10",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"20",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"30",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"40",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"50",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"10",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"20",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"30",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"40",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"50",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"10",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"20",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"30",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"40",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	" ",	"50",	" ",	" ",	" ",	" "))
p1<-p1+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1<-p1+ylim(4,12)
p1<-p1+theme(axis.ticks.x = element_blank())
p1<-p1+labs(y="% of All Deaths Due to P&I",x="MMWR Week")
library(grid)
p1<-p1+labs(title="Pneumonia and Influenza Moratlity from \n the national Center for Health Statistics Mortality Surveillance System",subtitle="Data through the week ending January 13, 2018,as of February 1,2018")+theme(plot.title = element_text(hjust=0.5))
p1<-p1+theme(legend.position = "none")
p1<-p1+annotate("text",x="2014-40",y=9,,label="Epidemic Threshold")
p1<-p1+annotate("text",x="2015-20",y=5,,label="Seasonal Baseline")
plot(p1)
ggsave("4th_graph.pdf")
