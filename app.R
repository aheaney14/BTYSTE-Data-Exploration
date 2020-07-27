source("countymaps.R")
source("rollnumbers.R")
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(tm)
library(topicmodels)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(slam)
library(dplyr)
library(wordcloud2)
for(i in 1:12){
  alldata[[i]] %>% rename_all(list(~make.names(.)))
}
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


categorysplit<-list()
for(i in 1:12){
  categorysplit[[i]]<-alldata[[i]] %>% 
    group_split(Category)
}
englishcat<-list()
langsplit<-list()
for(i in 1:12){
  langsplit[[i]]<-alldata[[i]][(alldata[[i]]$LanguageText=="English "),]
  englishcat[[i]]<-as.data.frame(langsplit[[i]]) %>% 
    group_split(Category)
}






for(i in 1:12){
  alldata[[i]]$Project.Status<-as.character(alldata[[i]]$Project.Status)
  alldata[[i]]$Project.Status[alldata[[i]]$Project.Status=="Phase 2: Qualified"]<-"Qualified"
  alldata[[i]]$Project.Status[alldata[[i]]$Project.Status=="Phase 2: Non Qualified"]<-"Not Qualified"
}
categorysplit<-list()
for(i in 1:12){
  categorysplit[[i]]<-alldata[[i]] %>% 
    group_split(Category)
}

ui <- dashboardPage(title = "BTYSTE Data Hub",
    dashboardHeader(title=tags$a(href='btyoungscientist.shinyapps.io/trends',tags$img(src="logo.png",width='220')),
                    tags$li(class = "dropdown", tags$a(HTML(paste(
                      "Welcome to the BT Young Scientist and Technology Exhibition Data Hub- Use this application
                      to interactively explore trends in the competition over the last 12 years", textOutput("Refresh1"))))),titleWidth=250),
    dashboardSidebar(
      sidebarMenu(id="menu",sidebarMenuOutput("menu")
      )
    
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$style(
          "tr:nth-child(1) {font-weight: bold;}
             tr:nth-child(6) {font-weight: bold;}
            "
        )
      ),
      tabItems(
        
        
      tabItem(tabName = "categories",h2("Entries by category"),
      fluidRow(
        box(width=12,
          column(2,checkboxGroupInput("check1","Number of Students",choices=c(1,2,3),selected = c(1,2,3))),
          column(2,checkboxGroupInput("check2","Gender",choices=c("All Male","All Female","Mixed"),
                               selected = c("All Male","All Female","Mixed"))),
                 column(2,checkboxGroupInput("check3","Age Group", c("Junior","Intermediate","Senior"),
                               selected = c("Junior","Intermediate","Senior"))),
                        column(2,checkboxGroupInput("check4","Project Status",c("Qualified",
                                                           "Not Qualified"),
                               selected = c("Qualified",
                                            "Not Qualified"))),
          column(2,checkboxGroupInput("check5","DEIS Status",c("DEIS","Non-DEIS"),
                                      selected = c("DEIS","Non-DEIS"))))),
      fluidRow(box(plotOutput("plot2"),width = 12))
      ),
       tabItem(tabName = "maps",h2("Analysis by County"),
               fluidRow(
               box(tabsetPanel(
          tabPanel("Proportion of schools entering",plotOutput("plot")),
          tabPanel("Proportion of schools entering that qualify",plotOutput("plot3")),
          sliderInput("years", label="Year",min = 2009, max = 2020, value = 2009,sep = "",
                      animate=animationOptions(interval = 2000), step=1))
        ),
        box(selectInput("county","Select County",choices=levels(alldata[[1]]$School.County)[-1]),
          tableOutput("table1"),
          tableOutput("table2"))
        
      )
       ),
      tabItem(tabName = "topics",h2("Compare common topics"),fluidRow(
              box(column(9,selectInput("category1","Select Category",choices = c("Biological and Ecological Sciences",
                                                                                "Chemical, Physical and Mathematical Sciences",
                                                                       "Social and Behavioural Sciences","Technology"))),
                  column(3,selectInput("yrwc1","Select Year",choices = as.numeric(years)))),
              box(column(9,selectInput("category2","Select Category",choices = c("Biological and Ecological Sciences",
                                                                                "Chemical, Physical and Mathematical Sciences",
                                                                                "Social and Behavioural Sciences","Technology"))),
                  column(3,selectInput("yrwc2","Select Year",choices = as.numeric(years))))),
              fluidRow(
              box(wordcloud2Output("wordcloud1"),width = 6),
              box(wordcloud2Output("wordcloud2"),width = 6))

              ),
      tabItem(tabName = "about",h2("About the app"),
              "This app was developed by Andrew Heaney, MSc Statistics student in the
                School of Mathematics and Statistics at University College Dublin, in conjunction with the board of the BTYSTE, 
                as part of a dissertation-Exploring trends in entries 
              to the BT Young Scientist and Technology Exhibition across the years 2009-2020. For more information on the BT
              Young Scientist and Technology Exhibition, go to",tags$a(href="https://btyoungscientist.com/",
                                                        "btyoungscientist.com"))
      
      )
    )
)
server <- function(input, output,session) {
  output$plot <- renderPlot ({
    #map plots for 12 years for schools entering
      yr<-as.numeric(input$years)-2008
      ggplot(all_ireland_cty, aes(fill=df[,yr])) + geom_sf() + scale_fill_viridis_c(name = "Proportion of schools entering",limits=c(0,1))
  })
  output$plot3 <- renderPlot ({
    #map plots for 12 years for schools qualifying
    df2<-as.data.frame(t(propor),col.names = years)
    colnames(df)<-years
    yr<-as.numeric(input$years)-2008
    ggplot(all_ireland_cty, aes(fill=df2[,yr])) + geom_sf() + scale_fill_viridis_c(name = "Proportion of schools qualifying",limits=c(0,1))
  })
  
  

  output$plot2<- renderPlot({

    
    biodata<-list()
    chemdata<-list()
    socdata<-list()
    tecdata<-list()
    for(i in 1:12){
      biodata[[i]] <- categorysplit[[i]][[1]] %>% filter(Num.Students %in% input$check1 &
                                                           Gender %in% input$check2 &
                                             Age.Group %in% input$check3 &
                                               Project.Status %in% input$check4 &
                                               DEIS %in% input$check5
                                               )
      chemdata[[i]] <- categorysplit[[i]][[2]] %>% filter(Num.Students %in% input$check1 &
                                                            Gender %in% input$check2 &
                                             Age.Group %in% input$check3 &
                                               Project.Status %in% input$check4 &
                                               DEIS %in% input$check5)
      socdata[[i]] <- categorysplit[[i]][[3]] %>% filter(Num.Students %in% input$check1 &
                                                           Gender %in% input$check2 &
                                             Age.Group %in% input$check3 &
                                               Project.Status %in% input$check4 &
                                               DEIS %in% input$check5)
      tecdata[[i]] <- categorysplit[[i]][[4]] %>% filter(Num.Students %in% input$check1 &
                                                           Gender %in% input$check2 &
                                             Age.Group %in% input$check3 &
                                               Project.Status %in% input$check4 &
                                               DEIS %in% input$check5)
    }
    
    
    
    countbio<-vector()
    countchem<-vector()
    countsoc<-vector()
    counttec<-vector()
    
    for(i in 1:12){
      countbio[i]<-nrow(biodata[[i]])
      countchem[i]<-nrow(chemdata[[i]])
      countsoc[i]<-nrow(socdata[[i]])
      counttec[i]<-nrow(tecdata[[i]])
    }
    Category<-rep(c("Biological and Ecological Sciences",
                 "Chemical, Physical and Mathematical Sciences",
                 "Social and Behavioural Sciences","Technology"),each=12)
    count<-c(countbio,countchem,countsoc,counttec)
    yrs<-rep(years,times=4)
    df2<-data.frame(Category,count,yrs)
    # Everything on the same plot
    ggplot(df2, aes(x=yrs, y=count, group=Category)) +
    geom_line(aes(color=Category),lwd=1.3)+
    geom_point(aes(color=Category),lwd=1.3)+
    scale_x_discrete(name="Year",limits=years) +
    labs(x="Year",y="Number of projects") +
    theme(legend.position = "bottom")
    
    
    
    
    #plot(countbio~years,type='l',col="blue",main="",ylim=c(0,max(countsoc)+40),ylab="Number of entries",
    #     xlab="Year")
    #lines(countchem~years,col="red")
    #lines(countsoc~years,col="green")
    #lines(counttec~years,col="black")
    #legend("topleft",legend = c("Biological and Ecological Sciences",
    #                            "Chemical,Physical and Mathematical Sciences",
    #                            "Social and Behavioural Sciences","Technology"),
    #       col=c("blue","red","green","black"),lty = 1,cex=0.8)
    
    
    
  })
  output$wordcloud1<-renderWordcloud2({
    yr<-as.numeric(input$yrwc1)-2008
    if(input$category1=="Biological and Ecological Sciences"){
      cat<-1
    }
    else if(input$category1=="Chemical, Physical and Mathematical Sciences"){
      cat<-2
    }
    else if(input$category1=="Social and Behavioural Sciences"){
      cat<-3
    }
    else{
      cat<-4
    }
    docs <- Corpus(VectorSource(as.data.frame(englishcat[[yr]][[cat]])[,5]))
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
                                        "understanding","finding","applied","factor","ask","need","give","looks","done",
                                        "likes","develop",
                                        "provide","product","produce","experience","experiment","area","areas",
                                        "human","learn","give","detect","given","groups","etc"))
    docs <- tm_map(docs, removePunctuation)
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    #Word cloud
    
    termdoc <- TermDocumentMatrix(docs)
    m <- as.matrix(termdoc)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    d<-filter(d,freq>4)
    
    
    
    
    
    
    
    
    
    
    wordcloud2(d,size=0.6)
    
  }
    
  )
  
  
  output$wordcloud2<-renderWordcloud2({
    yr<-as.numeric(input$yrwc2)-2008
    if(input$category2=="Biological and Ecological Sciences"){
      cat<-1
    }
    else if(input$category2=="Chemical, Physical and Mathematical Sciences"){
      cat<-2
    }
    else if(input$category2=="Social and Behavioural Sciences"){
      cat<-3
    }
    else{
      cat<-4
    }
    docs <- Corpus(VectorSource(as.data.frame(englishcat[[yr]][[cat]])[,5]))
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
                                        "understanding","finding","applied","factor","ask","need","give","looks","done",
                                        "likes","develop",
                                        "provide","product","produce","experience","experiment","area","areas",
                                        "learn","give","detect","given","groups","etc"))
    docs <- tm_map(docs, removePunctuation)
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    #Word cloud
    
    termdoc <- TermDocumentMatrix(docs)
    m <- as.matrix(termdoc)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    d<-filter(d,freq>4)
    
    
    
    
    
    
    
    
    
    wordcloud2(d,size = 0.6)
    
  }
  
  )
  
  output$menu <- renderMenu({
    sidebarMenu(
      #menuItem("Home",tabName = "home",icon = icon("home")),
      menuItem("Analysis of Categories", tabName = "categories", icon = icon("line-chart")),
      menuItem("Analysis by County", tabName = "maps",icon = icon("map-marker-alt")),
      menuItem("Topic Word Clouds",tabName = "topics",icon = icon("lightbulb-o")),
      menuItem("About app",tabName = "about",icon=icon("info-circle"))
      )
  })
  output$table1<-renderTable({
    yr<-as.numeric(input$years)-2008
    tot<-vector()
    bio<-vector()
    chem<-vector()
    soc<-vector()
    tech<-vector()
    bi<-vector()
    ch<-vector()
    so<-vector()
    te<-vector()
    qu<-vector()
    
    for(i in 1:12){
      tot[i]<-sum(alldata[[i]]$School.County==input$county)
      qu[i]<-sum(alldata[[i]]$School.County==input$county & alldata[[i]]$Project.Status=="Qualified")
      bio[i]<-sum(categorysplit[[i]][[1]]$School.County==input$county)
      chem[i]<-sum(categorysplit[[i]][[2]]$School.County==input$county)
      soc[i]<-sum(categorysplit[[i]][[3]]$School.County==input$county)
      tech[i]<-sum(categorysplit[[i]][[4]]$School.County==input$county)
      
      bi[i]<-sum(categorysplit[[i]][[1]]$School.County==input$county & 
                  categorysplit[[i]][[1]]$Project.Status=="Qualified")
      ch[i]<-sum(categorysplit[[i]][[2]]$School.County==input$county & 
                  categorysplit[[i]][[2]]$Project.Status=="Qualified")
      so[i]<-sum(categorysplit[[i]][[3]]$School.County==input$county & 
                  categorysplit[[i]][[3]]$Project.Status=="Qualified")
      te[i]<-sum(categorysplit[[i]][[4]]$School.County==input$county & 
                  categorysplit[[i]][[4]]$Project.Status=="Qualified")
    }
    
    df1<-as.data.frame(rbind(cbind("Total projects for year selected on map",sum(alldata[[yr]]$School.County==input$county),sum(alldata[[yr]]$School.County==input$county &
              alldata[[yr]]$Project.Status=="Qualified")),
      cbind("Biological and Ecological Sciences",bio[yr],bi[yr]),
      cbind("Chemical, Physical and Mathematical Sciences",chem[yr],ch[yr]),
      cbind("Social and Behavioural Sciences",soc[yr],so[yr]),
      cbind("Technology",tech[yr],te[yr])
      ))
    names(df1)<-c("","Entered","Qualified")
    df1
  })
  output$table2<-renderTable({
    yr<-as.numeric(input$years)-2008
    tot<-vector()
    bio<-vector()
    chem<-vector()
    soc<-vector()
    tech<-vector()
    bi<-vector()
    ch<-vector()
    so<-vector()
    te<-vector()
    qu<-vector()
    
    for(i in 1:12){
      tot[i]<-sum(alldata[[i]]$School.County==input$county)
      qu[i]<-sum(alldata[[i]]$School.County==input$county & alldata[[i]]$Project.Status=="Qualified")
      bio[i]<-sum(categorysplit[[i]][[1]]$School.County==input$county)
      chem[i]<-sum(categorysplit[[i]][[2]]$School.County==input$county)
      soc[i]<-sum(categorysplit[[i]][[3]]$School.County==input$county)
      tech[i]<-sum(categorysplit[[i]][[4]]$School.County==input$county)
      
      bi[i]<-sum(categorysplit[[i]][[1]]$School.County==input$county & 
                   categorysplit[[i]][[1]]$Project.Status=="Qualified")
      ch[i]<-sum(categorysplit[[i]][[2]]$School.County==input$county & 
                   categorysplit[[i]][[2]]$Project.Status=="Qualified")
      so[i]<-sum(categorysplit[[i]][[3]]$School.County==input$county & 
                   categorysplit[[i]][[3]]$Project.Status=="Qualified")
      te[i]<-sum(categorysplit[[i]][[4]]$School.County==input$county & 
                   categorysplit[[i]][[4]]$Project.Status=="Qualified")
    }
    
    df2<-as.data.frame(rbind(cbind("Total projects since 2009",sum(tot),sum(qu)),
                            cbind("Biological and Ecological Sciences",sum(bio),sum(bi)),
                            cbind("Chemical, Physical and Mathematical Sciences",sum(chem),sum(ch)),
                            cbind("Social and Behavioural Sciences",sum(soc),sum(so)),
                            cbind("Technology",sum(tech),sum(te))
    ))
    names(df2)<-c("","Entered","Qualified")
    df2
  })
}
    

shinyApp(ui, server)


