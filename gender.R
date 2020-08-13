source("data.R")

sizesplit<-list()

for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
}

male<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
female<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
for(i in 1:12){
    new<-as.data.frame(table(sizesplit[[i]][[1]]$Student1Sex))
    if(i==4||i==2){
      new<-new[-1,]
    }
    rownames(new)=NULL
    male<-rbind(male,new[2,])
    female<-rbind(female,new[1,])
}
rownames(male)=NULL
com<-cbind(male[,2],female[,2])
barplot(t(com)[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Gender of Individual Entrants",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years)
plot(t(com)[1,1:12],type='l')
lines(t(com)[2,1:12])

male2<-list()
female2<-list()
mixed2<-list()
count<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male2[[i]]<-filter(as.data.frame(sizesplit[[i]][[2]]),Student1Sex=="Male" & Student2Sex=="Male")
  female2[[i]]<-filter(as.data.frame(sizesplit[[i]][[2]]),Student1Sex=="Female" & Student2Sex=="Female")
  mixed2[[i]]<-filter(as.data.frame(sizesplit[[i]][[2]]),(Student1Sex=="Female" & Student2Sex=="Male")|(Student1Sex=="Male" & Student2Sex=="Female"))
  count[,i]<-c(nrow(male2[[i]]),nrow(female2[[i]]),nrow(mixed2[[i]]))
}


barplot(count[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Gender of Groups of 2",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years)
plot(count[1,]~years,type='l',col="blue",ylim=c(0,600),main="Gender profile of groups of 2",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)



male3<-list()
female3<-list()
mixed3<-list()
count2<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male3[[i]]<-filter(as.data.frame(sizesplit[[i]][[3]]),Student1Sex=="Male" & Student2Sex=="Male" & Student3Sex=="Male")
  female3[[i]]<-filter(as.data.frame(sizesplit[[i]][[3]]),Student1Sex=="Female" & Student2Sex=="Female" & Student3Sex=="Female")
  count2[,i]<-c(nrow(male3[[i]]),nrow(female3[[i]]),nrow(as.data.frame(sizesplit[[i]][[3]]))-
                                                          sum(nrow(male3[[i]]),nrow(female3[[i]])))
}



barplot(count2[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Gender of Groups of 3",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,600))
plot(count2[1,]~years,type='l',col="blue",ylim=c(0,620),main="Gender profile of groups of 3",ylab="Number of projects")
lines(count2[2,]~years,col="red")
lines(count2[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)


plot(t(com)[1,]~years,type='l',col="blue",ylim=c(100,500),main="Size of all Male groups",ylab="Number of projects")
lines(count[1,]~years,col="red")
lines(count2[1,]~years,col="green")
legend("topleft",legend = c("Indivduals","Groups of 2","Groups of 3"),col=c("blue","red","green"),lty = 1,cex=0.8)


plot(t(com)[2,]~years,type='l',col="blue",ylim=c(100,600),main="Size of all Female groups",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count2[2,]~years,col="green")
legend("topleft",legend = c("Indivduals","Groups of 2","Groups of 3"),col=c("blue","red","green"),lty = 1,cex=0.8)


#comparing categories
#individuals divided into categories
catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[1]] %>% group_split(Category)
}

male<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
female<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
for(i in 1:12){
  new<-as.data.frame(table(catsplit[[i]][[1]]$Student1Sex))
  new
  if(i==4||i==2){
    new<-new[-1,]
  }
  rownames(new)=NULL
  male<-rbind(male,new[2,])
  female<-rbind(female,new[1,])
}
female
#catsplit order: bio,chem,social,tech
rownames(male)=NULL
com<-cbind(male[,2],female[,2])
barplot(t(com)[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Individual Entries",
        names.arg=years,ylim = c(0,85),las=2)


#indiv chemical


male<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
female<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
for(i in 1:12){
  new<-as.data.frame(table(catsplit[[i]][[2]]$Student1Sex))
  new
  if(i==4||i==2){
    new<-new[-1,]
  }
  rownames(new)=NULL
  male<-rbind(male,new[2,])
  female<-rbind(female,new[1,])
}
male
#catsplit order: bio,chem,social,tech
rownames(male)=NULL
com<-cbind(male[,2],female[,2])
barplot(t(com)[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Individual Entries",
        names.arg=years,ylim = c(0,70),las=2)

#indivsocial

male<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
female<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
for(i in 1:12){
  new<-as.data.frame(table(catsplit[[i]][[3]]$Student1Sex))
  new
  if(i==4||i==2){
    new<-new[-1,]
  }
  rownames(new)=NULL
  male<-rbind(male,new[2,])
  female<-rbind(female,new[1,])
}
#catsplit order: bio,chem,social,tech
rownames(male)=NULL
com<-cbind(male[,2],female[,2])
barplot(t(com)[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Individual Entries",
        names.arg=years,ylim = c(0,140),las=2)

#indiv tech

male<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
female<-data.frame(gender=character(),freq=numeric(),stringsAsFactors = F)
for(i in 1:12){
  new<-as.data.frame(table(catsplit[[i]][[4]]$Student1Sex))
  new
  if(i==4||i==2){
    new<-new[-1,]
  }
  rownames(new)=NULL
  male<-rbind(male,new[2,])
  female<-rbind(female,new[1,])
}
#catsplit order: bio,chem,social,tech
rownames(male)=NULL
com<-cbind(male[,2],female[,2])
barplot(t(com)[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Individual Entries",
        names.arg=years,ylim = c(0,100),las=2)

#Groups of 2 bio

catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[2]] %>% group_split(Category)
}


male2<-list()
female2<-list()
mixed2<-list()
count<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male2[[i]]<-filter(as.data.frame(catsplit[[i]][[1]]),Student1Sex=="Male" & Student2Sex=="Male")
  female2[[i]]<-filter(as.data.frame(catsplit[[i]][[1]]),Student1Sex=="Female" & Student2Sex=="Female")
  mixed2[[i]]<-filter(as.data.frame(catsplit[[i]][[1]]),(Student1Sex=="Female" & Student2Sex=="Male")|(Student1Sex=="Male" & Student2Sex=="Female"))
  count[,i]<-c(nrow(male2[[i]]),nrow(female2[[i]]),nrow(mixed2[[i]]))
}
filter(as.data.frame(catsplit[[2]][[1]]),Student1Sex=="Male" & Student2Sex=="Male")
as.data.frame(catsplit[[1]][[1]])$Student1Sex
barplot(count[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 2:Biological",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years)

plot(count[1,]~years,type='l',col="blue",ylim=c(0,200),main="Groups of 2:Biological",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)




#groups of 2 chemical


catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[2]] %>% group_split(Category)
}


male2<-list()
female2<-list()
mixed2<-list()
count<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male2[[i]]<-filter(as.data.frame(catsplit[[i]][[2]]),Student1Sex=="Male" & Student2Sex=="Male")
  female2[[i]]<-filter(as.data.frame(catsplit[[i]][[2]]),Student1Sex=="Female" & Student2Sex=="Female")
  mixed2[[i]]<-filter(as.data.frame(catsplit[[i]][[2]]),(Student1Sex=="Female" & Student2Sex=="Male")|(Student1Sex=="Male" & Student2Sex=="Female"))
  count[,i]<-c(nrow(male2[[i]]),nrow(female2[[i]]),nrow(mixed2[[i]]))
}

barplot(count[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 2:Chem/Phys/Math",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,80))

plot(count[1,]~years,type='l',col="blue",ylim=c(0,160),main="Groups of 2:Chem/Phy/Math",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)


#Groups of 2 social

catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[2]] %>% group_split(Category)
}


male2<-list()
female2<-list()
mixed2<-list()
count<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male2[[i]]<-filter(as.data.frame(catsplit[[i]][[3]]),Student1Sex=="Male" & Student2Sex=="Male")
  female2[[i]]<-filter(as.data.frame(catsplit[[i]][[3]]),Student1Sex=="Female" & Student2Sex=="Female")
  mixed2[[i]]<-filter(as.data.frame(catsplit[[i]][[3]]),(Student1Sex=="Female" & Student2Sex=="Male")|(Student1Sex=="Male" & Student2Sex=="Female"))
  count[,i]<-c(nrow(male2[[i]]),nrow(female2[[i]]),nrow(mixed2[[i]]))
}

barplot(count[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 2:Social",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years)

plot(count[1,]~years,type='l',col="blue",ylim=c(0,220),main="Groups of 2:Social",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)



#groups of 2:Tech

catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[2]] %>% group_split(Category)
}


male2<-list()
female2<-list()
mixed2<-list()
count<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male2[[i]]<-filter(as.data.frame(catsplit[[i]][[4]]),Student1Sex=="Male" & Student2Sex=="Male")
  female2[[i]]<-filter(as.data.frame(catsplit[[i]][[4]]),Student1Sex=="Female" & Student2Sex=="Female")
  mixed2[[i]]<-filter(as.data.frame(catsplit[[i]][[4]]),(Student1Sex=="Female" & Student2Sex=="Male")|(Student1Sex=="Male" & Student2Sex=="Female"))
  count[,i]<-c(nrow(male2[[i]]),nrow(female2[[i]]),nrow(mixed2[[i]]))
}

barplot(count[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 2:Technology",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,85))

plot(count[1,]~years,type='l',col="blue",ylim=c(0,100),main="Groups of 2:Technology",ylab="Number of projects")
lines(count[2,]~years,col="red")
lines(count[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)


#groups of 3 biological

catsplit<-list()
for(i in 1:12){
  sizesplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Num.Students)
  catsplit[[i]]<-sizesplit[[i]][[3]] %>% group_split(Category)
}

male3<-list()
female3<-list()
mixed3<-list()
count2<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male3[[i]]<-filter(as.data.frame(catsplit[[i]][[1]]),Student1Sex=="Male" & Student2Sex=="Male" & Student3Sex=="Male")
  female3[[i]]<-filter(as.data.frame(catsplit[[i]][[1]]),Student1Sex=="Female" & Student2Sex=="Female" & Student3Sex=="Female")
  count2[,i]<-c(nrow(male3[[i]]),nrow(female3[[i]]),nrow(as.data.frame(catsplit[[i]][[1]]))-
                  sum(nrow(male3[[i]]),nrow(female3[[i]])))
}



barplot(count2[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 3:Biological",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,200))
plot(count2[1,]~years,type='l',col="blue",main="Biological: groups of 3",ylim=c(0,200),ylab="Number of projects")
lines(count2[2,]~years,col="red")
lines(count2[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)

#Groups of 3:chem

male3<-list()
female3<-list()
mixed3<-list()
count2<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male3[[i]]<-filter(as.data.frame(catsplit[[i]][[2]]),Student1Sex=="Male" & Student2Sex=="Male" & Student3Sex=="Male")
  female3[[i]]<-filter(as.data.frame(catsplit[[i]][[2]]),Student1Sex=="Female" & Student2Sex=="Female" & Student3Sex=="Female")
  count2[,i]<-c(nrow(male3[[i]]),nrow(female3[[i]]),nrow(as.data.frame(catsplit[[i]][[2]]))-
                  sum(nrow(male3[[i]]),nrow(female3[[i]])))
}



barplot(count2[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 3:Chem/Phys/Math",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,100))
plot(count2[1,]~years,type='l',col="blue",main="Chem/Phys/Math:groups of 3",ylim=c(0,100),ylab="Number of projects")
lines(count2[2,]~years,col="red")
lines(count2[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)


#3s:Social 

male3<-list()
female3<-list()
mixed3<-list()
count2<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male3[[i]]<-filter(as.data.frame(catsplit[[i]][[3]]),Student1Sex=="Male" & Student2Sex=="Male" & Student3Sex=="Male")
  female3[[i]]<-filter(as.data.frame(catsplit[[i]][[3]]),Student1Sex=="Female" & Student2Sex=="Female" & Student3Sex=="Female")
  count2[,i]<-c(nrow(male3[[i]]),nrow(female3[[i]]),nrow(as.data.frame(catsplit[[i]][[3]]))-
                  sum(nrow(male3[[i]]),nrow(female3[[i]])))
}



barplot(count2[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 3:Social",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,240))
plot(count2[1,]~years,type='l',col="blue",main="Social:groups of 3",ylim=c(0,270),ylab="Number of projects")
lines(count2[2,]~years,col="red")
lines(count2[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)


#Groups of 3 Tech

male3<-list()
female3<-list()
mixed3<-list()
count2<-matrix(,ncol = 12,nrow=3)
for(i in 1:12){
  male3[[i]]<-filter(as.data.frame(catsplit[[i]][[4]]),Student1Sex=="Male" & Student2Sex=="Male" & Student3Sex=="Male")
  female3[[i]]<-filter(as.data.frame(catsplit[[i]][[4]]),Student1Sex=="Female" & Student2Sex=="Female" & Student3Sex=="Female")
  count2[,i]<-c(nrow(male3[[i]]),nrow(female3[[i]]),nrow(as.data.frame(catsplit[[i]][[4]]))-
                  sum(nrow(male3[[i]]),nrow(female3[[i]])))
}



barplot(count2[1:2,1:12],beside = TRUE, col = c("blue","red"),
        main = "Groups of 3:Technology",legend.text = c("Male","Female"),
        args.legend = list(x ='topleft', bty='n'),xlab = "Year",ylab = "Number of Projects",
        names.arg=years,ylim = c(0,130))
plot(count2[1,]~years,type='l',col="blue",main="Technology:groups of 3",ylim=c(0,130),ylab="Number of projects")
lines(count2[2,]~years,col="red")
lines(count2[3,]~years,col="green")
legend("topleft",legend = c("Male","Female","Mixed"),col=c("blue","red","green"),lty = 1,cex=0.8)




#Plot of overall categories
categorysplit<-list()
for(i in 1:12){
  categorysplit[[i]]<-alldata[[i]][,c(4,5,18:21,26:40)] %>% group_split(Category)
}

countcat<-matrix(,ncol = 12,nrow=4)

for(i in 1:12){
  for(j in 1:4){
    countcat[j,i]<-nrow(categorysplit[[i]][[j]])
  }
  
}

plot(countcat[1,]~years,type='l',col="blue",main="",ylim=c(0,1200),ylab="Number of entries",
     xlab="Year",las=2,xaxt="n",lwd=3)
axis(1,xaxp=c(2009,2020,11),las=2)
lines(countcat[2,]~years,col="red",lwd=3)
lines(countcat[3,]~years,col="green",lwd=3)
lines(countcat[4,]~years,col="black",lwd=3)
legend("topleft",legend = c("BES","CPM","SBS","Tech"),
       col=c("blue","red","green","black"),lty = 1,cex=0.8,lwd=3)






