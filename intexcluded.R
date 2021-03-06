library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)
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
nischools<-read.csv("School level - post primary schools data 1920 suppressed (1).csv")
schoolsdat<-read.csv("post-primary-schools-2019-2020.csv")
nischools<-nischools[1:193,]
nischools$county
table(nischools$county)
alldata<-list(data2009,data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,
              data2018,data2019,data2020)

num<-matrix(, nrow = 12, ncol = 33)
lev<-levels(unique(data2018$School.County))




exint<-list()

for(i in 1:12){
  exint[[i]]<-filter(alldata[[i]],alldata[[i]]$Age.Group!="Intermediate")
  for(j in 1:33){
    num[i,j]<-length(unique(exint[[i]]$RoleNumber[exint[[i]]$School.County==lev[j]]))
    #q[i,j]<-length(unique(qualdata[[i]]$RoleNumber[qualdata[[i]]$School.County==lev[j]]))
  }
}
counties<-unique(data2018$School.County)
counties<-sort(counties)
counties<-counties[-1]
counties[c(1,2,8,10,18,28)]
num<-num[,-1]
counties
num
roinum<-num[,-c(1,2,8,10,18,28)]
roinum
ninum<-num[,c(1,2,8,10,18,28)]
ninum
lev
table(schoolsdat$County)
co<-as.array(table(schoolsdat$County))
b<-co[-1]
c<-table(nischools$county)
c<-c[-1]
c
ninum
proproi<-sweep(roinum,MARGIN=2,FUN="/",STATS=b)
propni<-sweep(ninum,MARGIN=2,FUN="/",STATS=c)






propni


years<-c(2009:2020)
years
all_ireland_cty = st_read("all_ireland.json", quiet=TRUE)
all_ireland_cty = all_ireland_cty %>% mutate(COUNTY = as.character(COUNTY))
all_ireland_cty$COUNTY
prop<-cbind(proproi,propni[,c(1,2,5,3,4,6)])  

for(i in 1:12){
  all_ireland_cty = st_read("all_ireland.json", quiet=TRUE)
  all_ireland_cty = all_ireland_cty %>% mutate(COUNTY = as.character(COUNTY))
  all_ireland_cty = all_ireland_cty %>% mutate(PROP = prop[i,])
  print(ggplot(all_ireland_cty, aes(fill=PROP)) + geom_sf()
        + scale_fill_viridis_c(name = "Proportion of schools entering",limits=c(0,1))
        +ggtitle(as.character(years[i])))
}