source("data.R")
library(ggplot2)
#Split up and count categories
catsplit<-list()

for(i in 1:12){
  catsplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Category)
}
countbio<-vector()
for(i in 1:12){
  countbio[i]<-nrow(catsplit[[i]][[1]])
}
countchem<-vector()
for(i in 1:12){
  countchem[i]<-nrow(catsplit[[i]][[2]])
}
countsoc<-vector()
for(i in 1:12){
  countsoc[i]<-nrow(catsplit[[i]][[3]])
}
counttec<-vector()
for(i in 1:12){
  counttec[i]<-nrow(catsplit[[i]][[4]])
}
cat<-rep(c("BES",
           "CPM",
           "SBS","Tech"),each=12)
count<-c(countbio,countchem,countsoc,counttec)
yrs<-rep(years,times=4)
df2<-data.frame(cat,count,yrs)
# Plot category counts
p<-ggplot(df2, aes(x=yrs, y=count, group=cat)) +
  geom_line(aes(color=cat),lwd=1.3)+
  geom_point(aes(color=cat),lwd=1.3)+
  scale_x_discrete(name="Year",limits=years) +
  labs(x="Year",y="Number of projects") +
  theme(legend.position = "bottom")+
  scale_color_discrete(limits=c("BES",
                                "CPM",
                                "SBS","Tech"))

p$labels$colour <- "Category"
print(p)