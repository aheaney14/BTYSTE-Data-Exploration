source("data.R")
library(MASS)
library(dplyr)
#Indexes 1-12 = years 2009-2020
#Deis schools model
for(i in 1:12){
  alldata[[i]]$School.Disadvantaged.<-alldata[[i]]$School.Disadvantaged. %>% replace_na(FALSE)
}
for(i in 1:12){
  roi[[i]]$School.Disadvantaged.<-roi[[i]]$School.Disadvantaged. %>% replace_na(FALSE)
}
deismod<-list()
#Run model for each year
for(i in 1:12){
  deismod[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ School.Disadvantaged.,
                 data = roi[[i]],family = 'binomial')
}
summary(deismod[[12]])
#Change index for each year 1-12: 2009-2020
summary(deismod[[12]])


#public/private ROI
for(i in 1:12){
  roi[[i]]$Fee.Paying.School..Y.N.<-replace_na(roi[[i]]$Fee.Paying.School..Y.N.,"Y")
}
pubpriv<-list()
#Run model for each year
for(i in 1:12){
  pubpriv[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ Fee.Paying.School..Y.N.,
                    data = roi[[i]],
                    family = 'binomial')
}
#Change index to change year 1-12: 2009-2020
summary(pubpriv[[5]])
newdata = data.frame(Fee.Paying.School..Y.N.="N")
predict(pubpriv[[5]],newdata,type="response")
newdata = data.frame(Fee.Paying.School..Y.N.="Y")
predict(pubpriv[[5]],newdata,type="response")
xtable(summary(pubpriv[[5]]))

#Gaeilscoils
lang<-list()
#Run model for each year
for(i in 1:12){
  lang[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ LanguageText,
                 data = roi[[i]],
                 family = 'binomial')
}
#Change index to change year 1-12: 2009-2020
summary(lang[[9]])
xtable(summary(lang[[9]]))

#School type Northern Ireland
table(ni[[1]]$management.type)
ni[[9]]$management.type

for(i in 1:12){
  levels(ni[[i]]$management.type)<-c(levels(ni[[i]]$management.type),"Further Education")
  ni[[i]]$management.type<-ni[[i]]$management.type %>% replace_na("Further Education")
}

type<-list()
for(i in 1:12){
  type[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ management.type,
                 data = ni[[i]],
                 family = 'binomial')
}





#Group size

grsize<-list()
for(i in 1:12){
  grsize[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ factor(Num.Students),
                   data = alldata[[i]],
                   family = 'binomial')
}
summary(grsize[[1]])

#Category

#Group size

grsize<-list()
for(i in 1:12){
  grsize[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ factor(Num.Students),
                   data = alldata[[i]],
                   family = 'binomial')
}
summary(grsize[[10]])
xtable(summary(grsize[[10]]))
#Gender-Need to create new variable for all male, all-female, mixed (currently have
#gender of each member)
#Group size

catg<-list()
for(i in 1:12){
  catg[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ Category,
                   data = alldata[[i]],
                   family = 'binomial')
}
summary(catg[[12]])
xtable(summary(catg[[12]]))

genreg<-list()
for(i in 1:12){
  genreg[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ Gender,
                   data = alldata[[i]],
                   family = 'binomial')
}
alldata[[12]]$Gender
summary(genreg[[1]])
xtable(summary(genreg[[7]]))
#Number of entries from a school
schreg<-list()
for(i in 1:12){
  schreg[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ Freq,
                 data = alldata[[i]],
                 family = 'binomial')
}
#Change index to change year 1-12: 2009-2020
summary(schreg[[12]])


library(lme4)
#Teacher effect

table(alldata[[12]]$teacherfull)
new<- alldata[[12]] %>%
  group_by(teacherfull) %>%
  filter(n() > 5) %>%
  ungroup()
fit<-glmer(as.numeric(Project.Status=="Phase 2: Qualified")~(Age.Group=="Senior")+(1|teacherfull),data=new,
family = 'binomial')
fit2<-glm(as.numeric(Project.Status=="Phase 2: Qualified")~(Age.Group=="Senior"),data=new,
           family = 'binomial')
summary(fit)



table(alldata[[12]]$Teacher.Title)
#Age group
agemod<-list()
#Remove unknown age group
alldata[[12]]<-subset(alldata[[12]], Age.Group !="Unknown ")
#Run model for each year
for(i in 1:12){
  agemod[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ Age.Group,
                    data = alldata[[i]],family = 'binomial')
}
summary(agemod[[12]])
newdata = data.frame(Age.Group="Senior")
predict(agemod[[12]],newdata,type="response")

xtable(summary(agemod[[12]]))


county<-list()
#Run model for each year
for(i in 1:12){
  county[[i]]<-glm(as.numeric(Project.Status=="Phase 2: Qualified") ~ relevel(School.County,ref="Dublin"),
                 data = alldata[[i]][!(is.na(alldata[[i]]$School.County) | alldata[[i]]$School.County==""), ],
                 family = 'binomial')
}
#Change index to change year 1-12: 2009-2020
xtable(summary(county[[12]]))
table(alldata[[1]]$School.County)

#Remove unknown age group
roi[[12]]<-subset(roi[[12]], Age.Group !="Unknown ")

#Stepwise regression
modeldata<-list()
model<-list()
for(i in 1:12){
  modeldata[[i]]<-roi[[i]] %>% dplyr::select(Project.Status,Num.Students,School.Disadvantaged.,LanguageText,Gender,Age.Group,
                                             Fee.Paying.School..Y.N.,School.County,Category)
  modeldata[[i]]$Num.Students<-factor(modeldata[[i]]$Num.Students)
  model[[i]] <- glm(as.numeric(Project.Status=="Phase 2: Qualified") ~., data = modeldata[[i]], family = binomial) %>%
    stepAIC(trace = FALSE,direction="both")
}



# Summarize the final selected model for each year-change index to change year
xtable(summary(model[[12]]))
newdata = data.frame(School.Disadvantaged.=FALSE,Category="Chemical Physical &amp; Mathematical Sciences")
predict(model[[12]],newdata,type="response")


dropdata<-list()
model<-list()
test<-vector()
for(i in 1:12){
  alldata[[i]]<-filter(alldata[[i]],teacherfull!="")
  model[[i]] <- glmer(as.numeric(Project.Status=="Phase 2: Qualified") ~ (1|teacherfull),
                       alldata[[i]],family = 'binomial')
  test[i]<-(1-pchisq(model[[i]]@theta,df=1))/2
}

test
summary(model[[12]])
teachers<-list()
h<-list()
top<-list()
topteachers<-list()
aboveav<-list()
for(i in 1:12){
  h[[i]]<-predict(model[[i]],alldata[[i]],type="response")
  teachers[[i]]<-alldata[[i]]$teacherfull[which(h[[i]]>0.3)]
  top[[i]]<-alldata[[i]]$teacherfull[which(h[[i]]>0.5)]
  aboveav[[i]]<-unique(teachers[[i]])
  topteachers[[i]]<-unique(top[[i]])
}
top
hist(unique(h[[12]]),main="",ylab = "Number of teachers",xlab = "Probability of qualifying",col = "green")
count<-list()
t<-list()
j<-list()
for(i in 1:12){
  count[[i]]<-as.data.frame(table(top[[i]]))
  t[[i]]<-as.data.frame(unique(top[[i]]))
  names(t[[i]])<-"Var1"
  j[[i]]<-left_join(t[[i]],count[[i]],by="Var1")
}
j[[9]]
mat<-matrix(,nrow = 12,ncol=12)
for(i in 1:12){
  for(j in 1:12){
    mat[i,j]<-sum(unique(teachers[[i]]) %in% unique(teachers[[j]]))
  }
}
mat

#Table of teachers over 0.3 multiple years
head(table(unlist(aboveav)) %>%
  as.data.frame() %>% 
  arrange(desc(Freq)),40)
xtable(head(table(unlist(aboveav)) %>%
         as.data.frame() %>% 
         arrange(desc(Freq)),40))



#Table of teachers over 0.5 multiple years
table(unlist(topteachers)) %>%
  as.data.frame() %>% 
    arrange(desc(Freq))
xtable(table(unlist(topteachers)) %>%
         as.data.frame() %>% 
         arrange(desc(Freq)))

w<-left_join(head(table(unlist(aboveav)) %>%
                 as.data.frame() %>% 
                 arrange(desc(Freq)),40),table(unlist(topteachers)) %>%
            as.data.frame() %>% 
            arrange(desc(Freq)),by="Var1")
write.table(w, file = "table.txt", sep = ",", quote = FALSE, row.names = F)

totaldata$teacherfull <- paste(totaldata$Teacher.First.Name,
                               totaldata$Teacher.Surname, sep="")
totaldata$teacherfull <- gsub(" ", "", totaldata$teacherfull, fixed = TRUE)
totaldata$teacherfull <- gsub("'","",totaldata$teacherfull,fixed=TRUE)
totaldata$teacherfull <- tolower(totaldata$teacherfull)
head(sort(table(totaldata$teacherfull),decreasing = TRUE),40)


for(i in 1:12){
  model[[i]] <- glmer(as.numeric(Project.Status=="Phase 2: Qualified") ~ (1|RoleNumber),
                      alldata[[i]],family = 'binomial')
  test[i]<-(1-pchisq(model[[i]]@theta,df=1))/2
}

for(i in 1:12){
  h[[i]]<-predict(model[[i]],alldata[[i]],type="response")
}
hist(unique(h[[12]]))
for(i in 1:12){
  model[[i]] <- glmer(as.numeric(Project.Status=="Phase 2: Qualified") ~ (1|School.County),
                      alldata[[i]],family = 'binomial')
  test[i]<-(1-pchisq(model[[i]]@theta,df=1))/2
}