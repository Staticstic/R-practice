library(data.table)
library(ggplot2)

# Create data
Initial=c(72.22,	18.06,	8.33,	1.39)
Followup=c(68.06,	15.28,	16.67,	0)
label2=c(72.22,68.06,18.06,15.28,8.33,16.67,1.39,0)
data=rbind(Initial, Followup)
colnames(data)=c('Stage I','Stage II','Stage III','Stage IV')


# Grouped barplot
b_chart=barplot(data, yaxt='n',
        col=colors()[c(131,35)] , 
        border=NA, 
        font.axis=1, 
        beside=T, 
        legend=(c('Baseline','Last Follow-up')) , args.legend = list(x='top',horiz=T, cex=0.9),
        xlab="MD Stage",ylim=c(0,80), main="Tafluprost",
        font.lab=2, las=1)
text(x=b_chart, y=label2+2.5, labels=paste(label2,"%"  ),
     col="black", font=2,cex=0.8)
y_tick=seq(0,80,20)
axis(2, at=y_tick, lab=paste(y_tick,'%'), las=1)




# create a dataset
years = c(rep("year1" ,4) , rep("year2" ,4) , rep("year3" ,4) , rep("year4" ,4), rep("year5",4))
stage <- rep(c("Stage1" , "Stage2" , "Stage3", "Stage4") , 5)
value <- c(67.8,
           20.34,
           10.17,
           1.69,
           76.19,
           12.7,
           9.52,
           1.59,
           72.13,
           16.39,
           9.84,
           1.64,
           73.02,
           15.87,
           9.52,
           1.59,
           69.23,
           13.85,
           16.92,
           0
)
data <- data.frame(years,stage,value)
data$value <- data$value/100
# Stacked + percent
a<-ggplot(data, aes(fill=stage, y=value, x=years)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous( labels = scales::percent)+
  theme_classic()+theme(legend.position = "bottom", 
                        axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 11),
                        axis.title.x = element_text(size= 15),
                        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("", values = c("Stage1" = "firebrick2", "Stage2" = "yellowgreen", "Stage3" = "gold","Stage4"="dodgerblue3"))+
  geom_text(aes(label=ifelse(round(value*100)>=10,round(value*100),"")),size = 4.5, position = position_stack(vjust = 0.5))+
  ylab("")+
  scale_x_discrete(labels = c("year 1",'year 2','year 3','year 4','Last visit'))+
  ggtitle("IOP reduction rate <25%")


#25 up
years = c(rep("year1" ,4) , rep("year2" ,4) , rep("year3" ,4) , rep("year4" ,4), rep("year5",4))
stage <- rep(c("Stage1" , "Stage2" , "Stage3", "Stage4") , 5)
value <- c(76.92,
           15.38,
           7.69,
           0,
           77.78,
           22.22,
           0,
           0,
           81.82,
           18.18,
           0,
           0,
           77.78,
           22.22,
           0,
           0,
           57.14,
           28.57,
           14.29,
           0
)
data <- data.frame(years,stage,value)
data$value <- data$value/100
# Stacked + percent
b=ggplot(data, aes(fill=stage, y=value, x=years)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous( labels = scales::percent)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size= 15),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual("", values = c("Stage1" = "firebrick2", "Stage2" = "yellowgreen", "Stage3" = "gold","Stage4"="dodgerblue3"))+
  geom_text(aes(label=ifelse(round(value*100)>=10,round(value*100),"")),size = 4.5, position = position_stack(vjust = 0.5))+
  ylab("")+
  scale_x_discrete(labels = c("year 1",'year 2','year 3','year 4','Last visit'))+
  ggtitle("IOP reduction rate กร25%")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(a)


library(gridExtra)
#grid.arrange(a,b, nrow=1, ncol=2)

final <- grid.arrange(arrangeGrob(a + theme(legend.position="none"),
                               b + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

