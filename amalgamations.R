library(dplyr)
data<-read.csv("2019DataQNQ.csv")
govdat<-read.csv("post-primary-schools-2019-2020.csv")
roi<-list()
for(i in 1:12){
  
  alldata[[i]]$RoleNumber<-toupper(droplevels(alldata[[i]]$RoleNumber))
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="762730"]="76273O"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="698123K"]="76194S"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="63191O"]="68325L" 
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="63211R"]="68325L"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="76098 W"]="76098W"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="980781S"]="91516B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="76173K "]="76173K"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="76103N"]="76103M"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="71920T"]="76414G"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="71250B"]="76534Q"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="62490T"]="91516B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="71090E"]="91516B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="62480Q"]="91516B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="64600K"]="76334I"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="63460P"]="91519H"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="64040V"]="68121S"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="64050B"]="68121S"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="64923L"]="68141B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="72070D"]="76150V"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="72500C"]="76105Q"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="61700W"]="91530S"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="70820K"]="76099B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="61850S"]="76099B"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="70930R"]="76090G"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="62080A"]="76090G"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="71870H"]="76093M"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="62510W"]="91513S"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="63010H"]="68285C"
  alldata[[i]]$RoleNumber[alldata[[i]]$RoleNumber=="64490G"]="76150V"
  roi[[i]]<-filter(alldata[[i]],School.Country=="Republic of Ireland")
}

names(govdat)[1]<-"RoleNumber"

newdata<-left_join(roi[[7]], govdat, by = "RoleNumber")

newdata$LanguageText<-ifelse(newdata$Irish.Classification...Post.Primary==
                               "All pupils taught all subjects through Irish"|
                               newdata$Irish.Classification...Post.Primary==
                               "Some pupils taught some subjects through Irish"|
                               newdata$Irish.Classification...Post.Primary==
                               "Some pupils taught all subjects through Irish","Irish ","English ")

newdata$LanguageText[is.na(newdata$LanguageText)] <- "English "
roi[[7]]<-newdata


