data2020<-read.csv("2020DataQNQ.csv")
data2019<-read.csv("2019DataQNQ.csv")
data2018<-read.csv("2018DataQNQ.csv")
data2017<-read.csv("2017DataQNQ.csv")
data2016<-read.csv("2016DataQNQ.csv")
data2015<-read.csv("2015DataQNQ.csv")
data2014<-read.csv("2014DataQNQ.csv")
data2013<-read.csv("2013DataQNQ.csv")
data2012<-read.csv("2012DataQNQ.csv")
data2011<-read.csv("2011DataQNQ.csv")
data2010<-read.csv("2010DataQNQ.csv")
data2009<-read.csv("2009DataQNQ.csv")
years<-c(2009:2020)
alldata<-list(data2009,data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,
              data2018,data2019,data2020)



data2020<-read.csv("2020DataQNQ.csv")
sch<-data2020[c(18:21,23:26,28,29,31:36)]

schoolsdat<-read.csv("post-primary-schools-2019-2020.csv")

proj<-vector()
qual<-vector()
deisproj<-list()
deisqual<-list()










c<-c(203, 202, 202, 195, 195, 193, 192, 190, 185, 198,198,198)
years<-c(2009:2020)



roirl<-list()
totsch<-vector()
totqual<-vector()
qualified<-list()
for(i in 1:12){
  roirl[[i]]<-subset(alldata[[i]],alldata[[i]]$School.Country=="Republic of Ireland")
  deisproj[[i]]<-subset(roirl[[i]],roirl[[i]]$School.Disadvantaged.==TRUE)
  proj[i]<-length(unique(deisproj[[i]]$RoleNumber))
  deisqual[[i]]<-subset(deisproj[[i]],deisproj[[i]]$Project.Status=="Phase 2: Qualified")
  qual[i]<-length(unique(deisqual[[i]]$RoleNumber))
  totsch[i]<-length(unique(roirl[[i]]$RoleNumber))
  qualified[[i]]<-subset(roirl[[i]],roirl[[i]]$Project.Status=="Phase 2: Qualified")
  totqual[i]<-length(unique(qualified[[i]]$RoleNumber))
}

#proportion of Deis that took part
deisent<-proj/c
#proportion of Deis taking part that qualified
deisqualify<-qual/proj
otherent<-(totsch-proj)/(nrow(schoolsdat)-c)
otherqual<-(totqual-qual)/(totsch-proj)


plot(otherent~years,type = 'l',col="red", ylim = c(0.2,0.6),
     ylab = "Proportion Entering",main="",lwd=3)
lines(deisent~years,col="blue",lwd=3)
legend("topleft",legend = c("DEIS","Non-DEIS"),col=c("blue","red"),lty = 1,cex=0.8,lwd=3)

plot(otherqual~years,type = 'l',col="red", ylim = c(0.4,0.8),
     ylab = "Proportion Qualifying",main="",lwd=3)
lines(deisqualify~years,col="blue",lwd=3)
legend("topleft",legend = c("DEIS","Non-DEIS"),col=c("blue","red"),lty = 1,cex=0.8,lwd=3)


cat<-rep(c("DEIS school",
           "Non-DEIS school"),each=12)
count<-c(deisent,otherent)
yrs<-rep(years,times=2)
df2<-data.frame(cat,count,yrs)
# Everything on the same plot
p<-ggplot(df2, aes(x=yrs, y=count, group=cat)) +
  geom_line(aes(color=cat),lwd=1.3)+
  geom_point(aes(color=cat),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Proportion of schools with projects entered") +
  theme(legend.position = "bottom")+
  scale_color_discrete(limits=c("DEIS school","Non-DEIS school"))

p$labels$colour <- "Status"
print(p)



cat<-rep(c("DEIS school",
           "Non-DEIS school"),each=12)
count<-c(deisqualify,otherqual)
yrs<-rep(years,times=2)
df2<-data.frame(cat,count,yrs)
# Everything on the same plot
p<-ggplot(df2, aes(x=yrs, y=count, group=cat)) +
  geom_line(aes(color=cat),lwd=1.3)+
  geom_point(aes(color=cat),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Proportion of entered schools qualified") +
  theme(legend.position = "bottom")+
  scale_color_discrete(limits=c("DEIS school","Non-DEIS school"))

p$labels$colour <- "Status"
print(p)
