#choose file first_hist.csv
#clear previous environment variable
data_ques1<-read.csv(file.choose())
colnames(data_ques1)[colnames(data_ques1)=="Total.A"]<-"A"
colnames(data_ques1)[colnames(data_ques1)=="Total.B"]<-"B"
colnames(data_ques1)[colnames(data_ques1)=="X..Positive"]<-"Percent Positive"
colnames(data_ques1)[colnames(data_ques1)=="Percent.Positive.A"]<-"% Positive Flu A"
colnames(data_ques1)[colnames(data_ques1)=="Percent.Positive.B"]<-"% Positive Flu B"
data_ques1$Week<-as.character(data_ques1$Week)
library(dplyr)
partial_dataset<-select(data_ques1, Week,A,B,"Percent Positive","% Positive Flu A","% Positive Flu B")
partial_dataset$'Percent Positive'<-as.character(partial_dataset$'Percent Positive')
partial_dataset$'Percent Positive'<-as.double(partial_dataset$'Percent Positive')

partial_dataset$'A'<-as.character(partial_dataset$'A')
partial_dataset$'A'<-as.numeric(partial_dataset$'A')

partial_dataset$'B'<-as.character(partial_dataset$'B')
partial_dataset$'B'<-as.numeric(partial_dataset$'B')

partial_dataset$'% Positive Flu A'<-as.character(partial_dataset$'% Positive Flu A')
partial_dataset$'% Positive Flu A'<-as.numeric(partial_dataset$'% Positive Flu A')

partial_dataset$'% Positive Flu B'<-as.character(partial_dataset$'% Positive Flu B')
partial_dataset$'% Positive Flu B'<-as.numeric(partial_dataset$'% Positive Flu B')

partial_dataset[1:17,4]=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
partial_dataset[1:17,5]=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
partial_dataset[1:17,6]=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)



colnames(partial_dataset)[colnames(partial_dataset)=="Percent Positive"]<-"Percent.Positive"

colnames(partial_dataset)[colnames(partial_dataset)=="% Positive Flu A"]<-"Percent.Positive.Flu.A"

colnames(partial_dataset)[colnames(partial_dataset)=="% Positive Flu B"]<-"Percent.Positive.Flu.B"
library(reshape2)
partial_melt<-melt(partial_dataset, id.vars = "Week")
partial_melt$value<-as.numeric(partial_melt$value)
library(ggplot2)

p<-ggplot(partial_melt,aes(x=Week,y=value,fill=variable))+geom_bar(stat="identity",color="black")
p<-p+scale_fill_manual(values=c("yellow","green","black","tomato","darkblue"))
p<-p+scale_x_discrete(limits=c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749","201750","201751","201752","201801","201802","201803","201804","201805","201806","201807","201808","201809","201810","201811","201812","201813","201814","201815","201816","201817","201818","201819","201820"),labels=c("201740"," ","201742"," "," 201744"," ","201746"," ","201748"," ","201750"," ","201752"," ","201802"," ","201804"," ","201806"," ","201808"," ","201810"," ","201812"," ","201814"," ","201816","","201818"," "," 201820"),expand = c(0, 0))
p<-p+ scale_y_continuous(breaks=seq(0,20000,2000),expand = c(0, 0), sec.axis = sec_axis(~.*(1/600),
                                                                                        name="Percent Positive",breaks=seq(0,30,2)))

p<-p+theme(axis.text.x = element_text(angle=60, hjust=1))
p<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p<-p+guides(fill=guide_legend(title=NULL,keywidth =3, keyheight = 0.1))
p<-p+labs(y="Number of Positive Specimens",title="Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories, \n National Summary, 2017-2018 Season")+theme(plot.title = element_text(hjust=0.5))



partial_dataset2<-select(data_ques1, Week,"Percent Positive","% Positive Flu A","% Positive Flu B")
partial_dataset2$'Percent Positive'<-as.character(partial_dataset2$'Percent Positive')
partial_dataset2$'Percent Positive'<-as.double(partial_dataset2$'Percent Positive')

partial_dataset2$'% Positive Flu A'<-as.character(partial_dataset2$'% Positive Flu A')
partial_dataset2$'% Positive Flu A'<-as.numeric(partial_dataset2$'% Positive Flu A')

partial_dataset2$'% Positive Flu B'<-as.character(partial_dataset2$'% Positive Flu B')
partial_dataset2$'% Positive Flu B'<-as.numeric(partial_dataset2$'% Positive Flu B')
colnames(partial_dataset2)[colnames(partial_dataset2)=="Percent Positive"]<-"Percent.Positive"

colnames(partial_dataset2)[colnames(partial_dataset2)=="% Positive Flu A"]<-"Percent.Positive.Flu.A"

colnames(partial_dataset2)[colnames(partial_dataset2)=="% Positive Flu B"]<-"Percent.Positive.Flu.B"

p2<-ggplot(partial_dataset2,aes(Week))+geom_line(aes(y=Percent.Positive,group=1,color="a"),linetype="solid",size=1,show.legend = TRUE)
p2<-p2+geom_line(aes(y=Percent.Positive.Flu.A,group=1,color="d"),linetype="dashed",size=1,show.legend =TRUE)
p2<-p2+geom_line(aes(y=Percent.Positive.Flu.B,group=1,color="f"),,linetype="dashed",size=1,show.legend = TRUE)
p2<-p2+scale_color_manual(name="",values=c("a"="black","d"="tomato","f"= "darkblue"),labels=c("Percent Positive","% Positive Flu A","% Positive Flu B"))
p2<-p2+theme(legend.position=c("201815",8))
p2<-p2+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
p2<-p2+ scale_y_continuous(breaks=c(0,4,8,12,16,20,24,28,32),expand = c(0, 0))
p2<-p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


library(gtable)
g1 <- ggplot_gtable(ggplot_build(p))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
plot(g)
ggsave("1st_graph.pdf")
