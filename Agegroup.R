library(dplyr)
library(ggplot2)
source("data.R")

agesplit<-list() 
#Plot by age group
for(i in 1:12){
  agesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Age.Group)
}
countjun<-vector()
for(i in 1:12){
  countjun[i]<-nrow(agesplit[[i]][[2]])
}
countint<-vector()
for(i in 1:12){
  countint[i]<-nrow(agesplit[[i]][[1]])
}
countsen<-vector()
for(i in 1:12){
  countsen[i]<-nrow(agesplit[[i]][[3]])
}

age<-rep(c("Junior","Intermediate","Senior"),each=12)
count<-c(countjun,countint,countsen)
yrs<-rep(years,times=3)
df2<-data.frame(age,count,yrs)
# Everything on the same plot
p<-ggplot(df2, aes(x=yrs, y=count, group=age)) +
  geom_line(aes(color=age),lwd=1.3)+
  geom_point(aes(color=age),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Number of projects") +
  theme(legend.position = "bottom")+
  scale_color_discrete(limits=c("Junior", "Intermediate", "Senior"))

p$labels$colour <- "Age group"
print(p)

nire<-list()
for(i in 1:12){
  nire[[i]]<-filter(alldata[[i]],School.Country=="Northern Ireland")
}

agesplitni<-list()

for(i in 1:12){
  agesplitni[[i]]<-nire[[i]][,c(4,5,18:21,26:40)] %>% group_split(Age.Group)
}
countjunni<-vector()
for(i in 1:12){
  countjunni[i]<-nrow(agesplitni[[i]][[2]])
}
countintni<-vector()
for(i in 1:12){
  countintni[i]<-nrow(agesplitni[[i]][[1]])
}
countsenni<-vector()
for(i in 1:12){
  countsenni[i]<-nrow(agesplitni[[i]][[3]])
}

age<-rep(c("Junior","Intermediate","Senior"),each=12)
count<-c(countjunni,countintni,countsenni)
yrs<-rep(years,times=3)
df3<-data.frame(age,count,yrs)
# Everything on the same plot
p<-ggplot(df3, aes(x=yrs, y=count, group=age)) +
  geom_line(aes(color=age),lwd=1.3)+
  geom_point(aes(color=age),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Number of projects") +
  theme(legend.position = "bottom")+
  scale_color_discrete(limits=c("Junior", "Intermediate", "Senior"))
  
p$labels$colour <- "Age group"
print(p)


#plot by number of students
numsplit<-list()

for(i in 1:12){
  numsplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
}
countone<-vector()
for(i in 1:12){
  countone[i]<-nrow(numsplit[[i]][[2]])
}
counttwo<-vector()
for(i in 1:12){
  counttwo[i]<-nrow(numsplit[[i]][[1]])
}
countthree<-vector()
for(i in 1:12){
  countthree[i]<-nrow(numsplit[[i]][[1]])
}

nums<-rep(c("1","2","3"),each=12)
count<-c(countone,counttwo,countthree)
yrs<-rep(years,times=3)
df2<-data.frame(nums,count,yrs)
# Everything on the same plot
p<-ggplot(df2, aes(x=yrs, y=count, group=nums)) +
  geom_line(aes(color=nums),lwd=1.3)+
  geom_point(aes(color=nums),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Number of projects") +
  theme(legend.position = "bottom")
  #scale_color_discrete(limits=c("Junior", "Intermediate", "Senior"))

print(p)
