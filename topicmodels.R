library(tm)
library(topicmodels)
library(dplyr)
library(wordcloud2)
library(slam)
library(xtable)
source("data.R")
categorysplit<-list()
langsplit<-list()
#English language projects only
for(i in 1:12){
  langsplit[[i]]<-alldata[[i]][(alldata[[i]]$LanguageText=="English "),]
  categorysplit[[i]]<-as.data.frame(langsplit[[i]]) %>% 
    group_split(Category)
}

totalenglish<-filter(totaldata,LanguageText!="Irish ")
catsplit<-totalenglish %>% 
  group_split(Category)
#topicmodels for all years -English
#change index for different categories: 1-Bio, 2-chem, 3-social, 4- tech
docs <- Corpus(VectorSource(as.data.frame(catsplit[[4]])[,5]))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"),"i","we","statistical",
                                    "analysis","want","analysis","analyse","my","analysing",
                                    "will","want","whether", "study","aim","statistics",
                                    "survey","surveys","aims","made","improve","use","investigate",
                                    "using","different","idea","research","source",
                                    "investigation","investigates","students","find","like",
                                    "such","project","researching","effect","affect","make",
                                    "investigating","investigated","affects","can","effects","help",
                                    "see","test","check","way","ways","awareness","reduce",
                                    "people","agus","ireland","irish","used","wish","levels","based",
                                    "amount","number","time","create","testing","results","result",
                                    "plan","determine","chun","system","impact","examine","well",
                                    "compare","hope","going","also","change","effective","quality",
                                    "better","various","secondary","effectiveness","difference",
                                    "data","towards","one","rates","amp","assess","affected","tests",
                                    "possible","problem","issues","experiments","discover","behaviour",
                                    "experiment","influence","information","benefits","negative","causes",
                                    "behind","prevent","factors","comparison","develop","common","easier",
                                    "new","bhfuil","dhanamh","best","mid","look","solutions","many",
                                    "case","get","much","relation","design","conditions","rate","changes",
                                    "types","performance","looking","properties","comparing","uses","potential",
                                    "work","side","actually","influences","solution","around",
                                    "intend","differences","intend","creating","certain","cause","build",
                                    "order","among","identify","visually","samples","know","knowledge","carry",
                                    "content","perceptions","show","explore","within","raise",
                                    "long","put","dhanamh","amach","ifeacht","day","patterns",
                                    "present","impacts","year","without","link","may","enough",
                                    "method","reducing","three","person","peoples","persons",
                                    "value","habits","primary","two","type","years","level","stop",
                                    "first","current","making","series","dhanamh","try","carrying",
                                    "found","another","examining","tionchar","establish","studying",
                                    "role","methods","measure","feel","increase","decrease","suitable",
                                    "means","correlation","compared","prove","developing","real","aware",
                                    "range","part","really","issue","become","less","more","most","least",
                                    "learn","due","developed","observe","daoine","question","fil","collected",
                                    "linn","involves","model","relationship","taking","contributing",
                                    "contribute","higher","versus","enhance","generate","take","prevalence",
                                    "designed","helps","need","increasing","focusing","take","good","getting",
                                    "importance","trying","think","important","come","major","makes","overall",
                                    "highlight","control","designing","usage","objective","increases","affecting",
                                    "conduct","examines","quantitative","cad","caused","record","simulate","predict",
                                    "understanding","finding","applied","factor","ask","need","develop",
                                    "provide","product","produce","experience","experiment","area","areas",
                                    "human","learn","give","detect","given","groups","etc"))
docs <- tm_map(docs, removePunctuation)
# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stemDocument)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace,c("dhanamh"))
docs <- tm_map(docs, toSpace,c("iarraidh"))
docs <- tm_map(docs, toSpace,c("muid"))
docs <- tm_map(docs, toSpace,c("fil"))

dtm<-DocumentTermMatrix(docs)
inspect(dtm)
summary(col_sums(dtm))
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)

#dtm<-dtm[,term_tfidf>=0.5]
dtm<-dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
#Remove rows with all zero entries
rowTotals <- apply(dtm , 1, sum) 
dtm.new   <- dtm[rowTotals> 0, ]

TM <- list(VEM = LDA(dtm.new, k = 3, control = list(seed = 213)),
            VEM_fixed = LDA(dtm.new, k = 3,
                            control = list(estimate.alpha = FALSE, seed = 213)),
          Gibbs = LDA(dtm.new, k = 3, method = "Gibbs",
                        control = list(seed = 213, burnin = 1000,
                                         thin = 100, iter = 1000)))
          

sapply(TM[1:2],slot,"alpha")
terms(TM[["VEM"]],10)
terms(TM[["VEM_fixed"]],10)
terms(TM[["Gibbs"]],10)


#Wordcloud
df <- data.frame(term = TM[["Gibbs"]]@terms, p = exp(TM[["Gibbs"]]@beta[1,]))
head(df[order(-df$p),])
df.new<-df[df$p>0.004,]
wordcloud2(df.new[order(-df.new$p),],size=0.5)


xtable(terms(TM[["Gibbs"]],10))

#First index changes year 1->12=2009->2020, Second index changes category 1-Bio, 2-chem, 3-social, 4- tech
docs <- Corpus(VectorSource(as.data.frame(categorysplit[[12]][[4]])[,5]))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"),"i","we","statistical",
                                    "analysis","want","analysis","analyse","my","analysing",
                                    "will","want","whether", "study","aim","statistics",
                                    "survey","surveys","aims","made","improve","use","investigate",
                                    "using","different","idea","research","source",
                                    "investigation","investigates","students","find","like",
                                    "such","project","researching","effect","affect","make",
                                    "investigating","investigated","affects","can","effects","help",
                                    "see","test","check","way","ways","awareness","reduce",
                                    "people","agus","ireland","irish","used","wish","levels","based",
                                    "amount","number","time","create","testing","results","result",
                                    "plan","determine","chun","system","impact","examine","well",
                                    "compare","hope","going","also","change","effective","quality",
                                    "better","various","secondary","effectiveness","difference",
                                    "data","towards","one","rates","amp","assess","affected","tests",
                                    "possible","problem","issues","experiments","discover","behaviour",
                                    "experiment","influence","information","benefits","negative","causes",
                                    "behind","prevent","factors","comparison","develop","common","easier",
                                    "new","bhfuil","dhanamh","best","mid","look","solutions","many",
                                    "case","get","much","relation","design","conditions","rate","changes",
                                    "types","performance","looking","properties","comparing","uses","potential",
                                    "work","side","actually","influences","solution","around",
                                    "intend","differences","intend","creating","certain","cause","build",
                                    "order","among","identify","visually","samples","know","knowledge","carry",
                                    "content","perceptions","show","explore","within","raise",
                                    "long","put","dhanamh","amach","ifeacht","day","patterns",
                                    "present","impacts","year","without","link","may","enough",
                                    "method","reducing","three","person","peoples","persons",
                                    "value","habits","primary","two","type","years","level","stop",
                                    "first","current","making","series","dhanamh","try","carrying",
                                    "found","another","examining","tionchar","establish","studying",
                                    "role","methods","measure","feel","increase","decrease","suitable",
                                    "means","correlation","compared","prove","developing","real","aware",
                                    "range","part","really","issue","become","less","more","most","least",
                                    "learn","due","developed","observe","daoine","question","fil","collected",
                                    "linn","involves","model","relationship","taking","contributing",
                                    "contribute","higher","versus","enhance","generate","take","prevalence",
                                    "designed","helps","need","increasing","focusing","take","good","getting",
                                    "importance","trying","think","important","come","major","makes","overall",
                                    "highlight","control","designing","usage","objective","increases","affecting",
                                    "conduct","examines","quantitative","cad","caused","record","simulate","predict",
                                    "understanding","finding","applied","factor","ask","need","develop",
                                    "provide","product","produce","experience","experiment","area","areas",
                                    "human","learn","give","detect","given","groups","etc"))
docs <- tm_map(docs, removePunctuation)
# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stemDocument)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace,c("dhanamh"))
docs <- tm_map(docs, toSpace,c("iarraidh"))
docs <- tm_map(docs, toSpace,c("muid"))
docs <- tm_map(docs, toSpace,c("fil"))

dtm<-DocumentTermMatrix(docs)
inspect(dtm)
summary(col_sums(dtm))
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)

#dtm<-dtm[,term_tfidf>=0.5]
dtm<-dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
#Remove rows with all zero entries
rowTotals <- apply(dtm , 1, sum) 
dtm.new   <- dtm[rowTotals> 0, ]

TM <- list(VEM = LDA(dtm.new, k = 3, control = list(seed = 213)),
           VEM_fixed = LDA(dtm.new, k = 3,
                           control = list(estimate.alpha = FALSE, seed = 213)),
           Gibbs = LDA(dtm.new, k = 3, method = "Gibbs",
                       control = list(seed = 213, burnin = 1000,
                                      thin = 100, iter = 1000)))


sapply(TM[1:2],slot,"alpha")
terms(TM[["VEM"]],10)
terms(TM[["VEM_fixed"]],10)
terms(TM[["Gibbs"]],10)



df <- data.frame(term = TM[["Gibbs"]]@terms, p = exp(TM[["Gibbs"]]@beta[2,]))
head(df[order(-df$p),])
df.new<-df[df$p>0.004,]
wordcloud2(df.new[order(-df.new$p),],size=0.5)

#Gibbs chosen for each model
xtable(terms(TM[["Gibbs"]],10))




#Irish 
totalirish<-filter(totaldata,LanguageText=="Irish ")
catsplit<-totalirish %>% 
  group_split(Category)

#Categories: 1-Bio, 2-chem, 3-social, 4- tech
#Change first index to change category
docs <- Corpus(VectorSource(as.data.frame(catsplit[[1]])[,5]))
#Replace fadas in irish
a <- content_transformer(function (x , pattern ) gsub(pattern, "a", x))
docs <- tm_map(docs, a ,c("&#225;"))
e <- content_transformer(function (x , pattern ) gsub(pattern, "e", x))
docs <- tm_map(docs, e ,c("&#233;"))
docs <- tm_map(docs, e ,c("&#201;"))
i <- content_transformer(function (x , pattern ) gsub(pattern, "i", x))
docs <- tm_map(docs, i ,c("&#237;"))
o <- content_transformer(function (x , pattern ) gsub(pattern, "o", x))
docs <- tm_map(docs, o ,c("&#243;"))
u <- content_transformer(function (x , pattern ) gsub(pattern, "u", x))
docs <- tm_map(docs, u ,c("&#250;"))
docs <- tm_map(docs, content_transformer(tolower))
#Remove common irrelevant words
docs <- tm_map(docs, removeWords, c(stopwords("english"),"i","we","statistical",
                                    "analysis","want","analysis","analyse","my","analysing",
                                    "will","want","whether", "study","aim","statistics",
                                    "survey","surveys","aims","made","improve","use","investigate",
                                    "using","different","idea","research","source",
                                    "investigation","investigates","students","find","like",
                                    "such","project","researching","effect","affect","make",
                                    "investigating","investigated","affects","can","effects","help",
                                    "see","test","check","way","ways","awareness","reduce",
                                    "people","agus","ireland","irish","used","wish","levels","based",
                                    "amount","number","time","create","testing","results","result",
                                    "plan","determine","chun","system","impact","examine","well",
                                    "compare","hope","going","also","change","effective","quality",
                                    "better","various","secondary","effectiveness","difference",
                                    "data","towards","one","rates","amp","assess","affected","tests",
                                    "possible","problem","issues","experiments","discover","behaviour",
                                    "experiment","influence","information","benefits","negative","causes",
                                    "behind","prevent","factors","comparison","develop","common","easier",
                                    "new","bhfuil","dhanamh","best","mid","look","solutions","many",
                                    "case","get","much","relation","design","conditions","rate","changes",
                                    "types","performance","looking","properties","comparing","uses","potential",
                                    "work","side","actually","influences","solution","around",
                                    "intend","differences","intend","creating","certain","cause","build",
                                    "order","among","identify","visually","samples","know","knowledge","carry",
                                    "content","perceptions","show","explore","within","raise",
                                    "long","put","dhanamh","amach","ifeacht","day","patterns",
                                    "present","impacts","year","without","link","may","enough",
                                    "method","reducing","three","person","peoples","persons",
                                    "value","habits","primary","two","type","years","level","stop",
                                    "first","current","making","series","dhanamh","try","carrying",
                                    "found","another","examining","tionchar","establish","studying",
                                    "role","methods","measure","feel","increase","decrease","suitable",
                                    "means","correlation","compared","prove","developing","real","aware",
                                    "range","part","really","issue","become","less","more","most","least",
                                    "learn","due","developed","observe","daoine","question","fil","collected",
                                    "linn","involves","model","relationship","taking","contributing",
                                    "contribute","higher","versus","enhance","generate","take","prevalence",
                                    "designed","helps","need","increasing","focusing","take","good","getting",
                                    "importance","trying","think","important","come","major","makes","overall",
                                    "highlight","control","designing","usage","objective","increases","affecting",
                                    "conduct","examines","quantitative","cad","caused","record","simulate","predict",
                                    "understanding","finding","applied","factor","ask","need","develop",
                                    "provide","product","produce","experience","experiment","area","areas",
                                    "human","learn","give","detect","given","groups","etc"))
docs <- tm_map(docs, removePunctuation)
# Remove numbers
docs <- tm_map(docs, removeNumbers)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, stemDocument)
delete<-c("dheanamh","usaid","tastail","iad","feidir","difriula",
          "growth","fail","nios","deanamh","cen","fhail","eagsula",
          "faoi","fearr","don","water","taimid","fiosru","seo","staidear",
          "idir","meid","nuair","comparaid","tri","trid","feachaint",
          "maith","beidh","tionscnamh","bhionn","difriocht","nio","imscrudu",
          "isteach","chomh","mhaith","ata","eifeacht","chur","aidhm","cona","conas",
          "mar","san","mbionn","chuir","tionscad","taim","sin","ach","lion","iarraidh",
          "muid","eile","ach","leis","scrudu","fhiosru","againn","chuir","chuid","fath",
          "acu","gan","tar","ann","tai","dean","eis","thar","modh","faigh","gaol",
          "said","cht","dhean","comh","gnath","rgi","soicind","bheith","bheath",
          "iniuchadh","den","ina","aige","liom","dul","iompar","duit","orthu","ceadfa",
          "ceadfai","anailis","gur","ceard","leibheal","chuig","eile","aon")
for(i in 1:length(delete)){
  docs <- tm_map(docs, toSpace,c(delete[i]))
}





dtm<-DocumentTermMatrix(docs)
inspect(dtm)
summary(col_sums(dtm))
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)

#dtm<-dtm[,term_tfidf>=0.6]
dtm<-dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
#Remove rows with all zero entries
rowTotals <- apply(dtm , 1, sum) 
dtm.new   <- dtm[rowTotals> 0, ]

TM <- list(VEM = LDA(dtm.new, k = 2, control = list(seed = 213)),
           VEM_fixed = LDA(dtm.new, k = 2,
                           control = list(estimate.alpha = FALSE, seed = 213)),
           Gibbs = LDA(dtm.new, k = 2, method = "Gibbs",
                       control = list(seed = 213, burnin = 1000,
                                      thin = 100, iter = 1000)))
#,CTM = CTM(dtm.new, k = 4, control = list(seed = 213)))

sapply(TM[1:2],slot,"alpha")
terms(TM[["VEM"]],10)
terms(TM[["VEM_fixed"]],10)
terms(TM[["Gibbs"]],10)
xtable(terms(TM[["Gibbs"]],10))
