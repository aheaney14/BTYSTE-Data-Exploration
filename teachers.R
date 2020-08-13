library(lme4)
source("data.R")


totaldata<-bind_rows(data2009,data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,
                     data2018,data2019,data2020)

df<-list()
num<-vector()
#for(i in 1:12){
#  alldata[[i]]$teacherfull <- paste(alldata[[i]]$Teacher.First.Name,
#                                    alldata[[i]]$Teacher.Surname, sep=" ")
#  df[[i]]<-as.data.frame(table(alldata[[i]]$teacherfull))
#  num[i]<-nrow(df[[i]])
#}
#num
#mat<-matrix(,nrow = 12,ncol=12)
#common<-matrix(list(),nrow = 12,ncol = 12)
#over10<-data.frame()
#high<-list()
#allt<-data.frame()
#for(i in 1:12){
#  for(j in 1:12){
#    common[[i]][[j]]<-inner_join(df[[i]],df[[j]],by="Var1")
#    mat[i,j]<-nrow(common[[i]][[j]])
#    
#  }
#  high[[i]]<-filter(df[[i]],Freq>10)
#  over10<-rbind(over10,high[[i]])
#  allt<-rbind(allt,df[[i]])
#}
#sort(table(droplevels(over10$Var1)),decreasing = TRUE)
#head(sort(table(droplevels(allt$Var1)),decreasing = TRUE),20)
#table(sort(table(droplevels(allt$Var1)),decreasing = TRUE))

totaldata$teacherfull <- paste(totaldata$Teacher.First.Name,
                                  totaldata$Teacher.Surname, sep="")
totaldata$teacherfull <- gsub(" ", "", totaldata$teacherfull, fixed = TRUE)
totaldata$teacherfull <- gsub("'","",totaldata$teacherfull,fixed=TRUE)
totaldata$teacherfull <- tolower(totaldata$teacherfull)



for(i in 1:12){
  alldata[[i]]$teacherfull <- paste(alldata[[i]]$Teacher.First.Name,
                                    alldata[[i]]$Teacher.Surname, sep="")
  alldata[[i]]$teacherfull <- gsub(" ", "", alldata[[i]]$teacherfull, fixed = TRUE)
  alldata[[i]]$teacherfull <- gsub("'","",alldata[[i]]$teacherfull,fixed=TRUE)
  alldata[[i]]$teacherfull <- tolower(alldata[[i]]$teacherfull)
}


fit<-glmer(as.numeric(Project.Status=="Phase 2: Qualified")~School.Disadvantaged.+(1|RoleNumber),data=alldata[[12]],
           family = 'binomial')

summary(fit)

totaldata$RoleNumber<-gsub("-", "", totaldata$RoleNumber)
over100<-as.data.frame(head(sort(table(totaldata$RoleNumber[!totaldata$RoleNumber==""]),decreasing = TRUE),43))
names(over100)[1]<-"RoleNumber"
df<-left_join(over100,govdat,by="RoleNumber")
df<-left_join(df,govni,by="RoleNumber")
df[,1:8]
dfsubset<-df[c(1,3,38,42),]
dfsubset
join<-left_join(dfsubset,totaldata,by="RoleNumber")
join
head(sort(table(join$teacherfull),decreasing=TRUE),30)



newtotal <- totaldata %>%
  group_by(teacherfull) %>%
  filter(n() > 10) %>%
  ungroup()

tot<-as.data.frame(table(newtotal$teacherfull))
qual<-as.data.frame(table(newtotal$teacherfull[newtotal$Project.Status=="Phase 2: Qualified"]))
joined<-left_join(tot,qual,by="Var1")
joined
joined[is.na(joined)] <- 0
joined<-joined[-1,]
joined$prop<-joined$Freq.y/joined$Freq.x
min(joined$prop)
totalentered<-as.data.frame(head(sort(table(totaldata$teacherfull),decreasing = TRUE),30))[-1,]
totalentered
joined
totaldata$Year[totaldata$teacherfull=="annblanking"]
nireland<-filter(totaldata,School.Country=="Northern Ireland")
nrow(nireland)
xtable(sort(joined,by=prop,decreasing = TRUE))
