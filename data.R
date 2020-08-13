#Data cleaning code
library(dplyr)
library(tidyr)
#Load data
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
totaldata<-bind_rows(data2009,data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,
                                data2018,data2019,data2020)
#Add gender column- All male, all female or mixed
gen<-list()
for(i in 1:12){
  gen[[i]]<-vector()
  for(j in 1:nrow(alldata[[i]])){
    if(alldata[[i]]$Student1Sex[j]=="Male" & alldata[[i]]$Student2Sex[j]!="Female" 
       & alldata[[i]]$Student3Sex[j]!="Female"){
      gen[[i]][j]<-"All Male"
    }
    else if(alldata[[i]]$Student1Sex[j]=="Female" & alldata[[i]]$Student2Sex[j]!="Male" 
            & alldata[[i]]$Student3Sex[j]!="Male"){
      gen[[i]][j]<-"All Female"
    }
    else{
      gen[[i]][j]<-"Mixed"
    }
  }
  alldata[[i]]$Gender<-gen[[i]]
}
#Add teacher column
for(i in 1:12){
  alldata[[i]]$teacherfull <- paste(alldata[[i]]$Teacher.First.Name,
                                    alldata[[i]]$Teacher.Surname, sep="")
  alldata[[i]]$teacherfull <- gsub(" ", "", alldata[[i]]$teacherfull, fixed = TRUE)
  alldata[[i]]$teacherfull <- gsub("'","",alldata[[i]]$teacherfull,fixed=TRUE)
  alldata[[i]]$teacherfull <- tolower(alldata[[i]]$teacherfull)
}

alldata[[12]]<-subset(alldata[[12]], Age.Group !="Unknown ")
#Add number of entries per school to data
schoolcount<-list()

for(i in 1:12){
  schoolcount[[i]]<-as.data.frame(table(alldata[[i]]$RoleNumber))
}


for(i in 1:12){
  names(schoolcount[[i]])[1]<-"RoleNumber"
  alldata[[i]]<-left_join(alldata[[i]],schoolcount[[i]],by="RoleNumber")
}
#correct to current school roll numbers- including schools that have amalgamated
govdat<-read.csv("post-primary-schools-2019-2020.csv")
wrongrn<-list()
names(govdat)[1]<-"RoleNumber"
roi<-list()
for(i in 1:12){
  roi[[i]]<-filter(alldata[[i]],School.Country=="Republic of Ireland")
  roi[[i]]$RoleNumber<-toupper(roi[[i]]$RoleNumber)
  wrongrn[[i]]<-as.data.frame(anti_join(roi[[i]],govdat,by="RoleNumber"))[19:30]
}
wrongrn[[1]]
wrongrn[[2]]
wrongrn[[3]]
wrongrn[[4]]
wrongrn[[5]]
wrongrn[[6]]
wrongrn[[7]]
wrongrn[[8]]
wrongrn[[9]]
wrongrn[[10]]
wrongrn[[11]]
wrongrn[[12]]

#Correct wrong roll numbers and amalgamated schools
for(i in 1:12){
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="762730"]="76273O"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="698123K"]="76194S"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="63191O"]="68325L" 
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="63211R"]="68325L"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="76098 W"]="76098W"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="980781S"]="91516B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="76173K "]="76173K"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="76103N"]="76103M"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="71920T"]="76414G"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="71250B"]="76534Q"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="62490T"]="91516B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="71090E"]="91516B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="62480Q"]="91516B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="64600K"]="76334I"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="63460P"]="91519H"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="64040V"]="68121S"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="64050B"]="68121S"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="64923L"]="68141B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="72070D"]="76150V"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="72500C"]="76105Q"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="61700W"]="91530S"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="70820K"]="76099B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="61850S"]="76099B"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="70930R"]="76090G"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="62080A"]="76090G"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="71870H"]="76093M"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="62510W"]="91513S"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="63010H"]="68285C"
  roi[[i]]$RoleNumber[roi[[i]]$RoleNumber=="64490G"]="76150V"
}


for(i in 1:12){
  roi[[i]]<-left_join(roi[[i]],govdat,by="RoleNumber")
}

newdata<-roi[[7]]
#Languagetext column missing from 2015- can extract from dept. data
newdata$LanguageText<-ifelse(newdata$Irish.Classification...Post.Primary==
                               "All pupils taught all subjects through Irish"|
                               newdata$Irish.Classification...Post.Primary==
                               "Some pupils taught some subjects through Irish"|
                               newdata$Irish.Classification...Post.Primary==
                               "Some pupils taught all subjects through Irish","Irish ","English ")

newdata$LanguageText[is.na(newdata$LanguageText)] <- "English "
roi[[7]]<-newdata

#School 
govni<-read.csv("School level - post primary schools data 1920 suppressed (1).csv")
ni<-list()
wrong<-list()
names(govni)[1]<-"RoleNumber"
govni$RoleNumber<-as.character(govni$RoleNumber)
for(i in 1:12){
  ni[[i]]<-filter(alldata[[i]],School.Country=="Northern Ireland")
  ni[[i]]$RoleNumber<-gsub("-", "", ni[[i]]$RoleNumber)
  #ni[[i]]$RoleNumber<-toupper(droplevels(ni[[i]]$RoleNumber))
  wrong[[i]]<-as.data.frame(anti_join(ni[[i]],govni,by="RoleNumber"))[15:30]
}

for(i in 1:12){
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="2420042"]="2420320"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="1230026"]="1230321"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="1230231"]="1230321"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="5230256"]="5230321"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="2230180"]="2420320"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="3230151"]="3230318"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="2230190"]="2230322"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="3230151"]="3230318"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="545613"]="3420317"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="3410033"]="3420317"
  ni[[i]]$RoleNumber[ni[[i]]$RoleNumber=="5230070"]="5420314"
}


for(i in 1:12){
  ni[[i]]<-left_join(ni[[i]],govni,by="RoleNumber")
}

for(i in 1:12){
  roi[[i]]$LanguageText[is.na(roi[[i]]$LanguageText)]<-"English "
  roi[[i]]$LanguageText[roi[[i]]$LanguageText=="Unknown "]<-"English " 
  roi[[i]]$LanguageText[roi[[i]]$LanguageText==""]<-"English "
}

