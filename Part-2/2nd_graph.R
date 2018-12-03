# Please choose 2nd_graph.csv as input file 
data<-read.csv(file.choose()) 
colnames(data)[colnames(data)=="A.H3N2v."]<-"A(H3N2)"
colnames(data)[colnames(data)=="A.Subtyping.not.performed."]<-"A(subtying not performed)"
colnames(data)[colnames(data)=="A..H1N1.pdm09"]<-"A(H1N1)pdm09"
colnames(data)[colnames(data)=="B"]<-"B(lineage not performed)"
colnames(data)[colnames(data)=="BVIC"]<-"B(Victoria Lineage)"
colnames(data)[colnames(data)=="BYAM"]<-"B(Yamagata Lineage)"
colnames(data)[colnames(data)=="A(H3N2)"]<-"A(H3N2v)"
colnames(data)[colnames(data)=="A.H3."]<-"A(H3N2)"
colnames(data)[colnames(data)=="A(H3N2v)"]<-"H3N2v"
keeps<-c("Week","A(subtying not performed)","A(H1N1)pdm09","A(H3N2)","H3N2v","B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)")
data_subset<-data[keeps]
library(scales)
library(reshape2)
library(ggplot2)
mdfr<-melt(data_subset,id.vars="Week")
mdfr$Week<-as.character(mdfr$Week)
p<-ggplot(data_subset,aes(x=Week,y=value,fill=variable))+geom_bar(stat="identity")
p<-ggplot(data_subset,aes(x=Week,y=value,fill=variable))+geom_bar(stat="identity")
p<-p+scale_fill_manual(values=c("yellow","orange","red","mediumorchid4","palegreen4","palegoldenrod","palegreen"))
p<-ggplot(mdfr,aes(x=Week,y=value,fill=variable))+geom_bar(stat="identity",color="black")
p<-p+scale_x_discrete(limits=c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749","201750","201751","201752","201801","201802","201803","201804","201805","201806","201807","201808","201809","201810","201811","201812","201813","201814","201815","201816","201817","201818","201819","201820"),labels=c("201740"," ","201742"," "," 201744"," ","201746"," ","201748"," ","201750"," ","201752"," ","201802"," ","201804"," ","201806"," ","201808"," ","201810"," ","201812"," ","201814"," ","201816","","201818"," "," 201820"),expand = c(0, 0))
p<-p+ scale_y_continuous(breaks=seq(0,4000,500),expand = c(0, 0))
p<-p+theme(axis.text.x = element_text(angle=60, hjust=1))
p<-p+ylab("Number of Positive Specimens")
p<-p+ggtitle("Inflenza Positive Tests reported to CDC by U.S. Public Health \n Laboratories, National Summary, 2017-2018 Season")
p<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p<-p+guides(fill=guide_legend(title=NULL))
p<-p+theme(legend.key = element_rect(colour = 'black', size = 0.5, linetype='solid'))
p<-p+theme(legend.key.size = unit(0.4, "cm"))
plot(p)
ggsave("2ndGraph.pdf")

