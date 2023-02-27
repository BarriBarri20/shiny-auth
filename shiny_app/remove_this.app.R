###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
##### Install Libraries and Packages
#load the libraries into the session
library(ggplot2)
library(gcookbook)
library(data.table)
library(plyr)
library(dplyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(ssh)
library(pool)
library(DBI)
library(RMySQL)
library(reticulate)
library(bslib)
library(stringi)
library(mongolite)

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
##### UI Code
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  #theme = bs_theme_update(theme, bootswatch = "minty"),
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  titlePanel(
    "Working progress"
  ),
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  tabsetPanel(
    tabPanel(
      "Workload",
      br(),
      tags$head(
        HTML(
          "
          <script>
          var socket_timeout_interval
          var n=0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
        )
      ),
      textOutput("keepAlive"),
      fluidRow(
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberofcasestext")
                   ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberofcasesnumber")
                   )
                 )
               ),
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberofinterventionsthismonthtext")
                 ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberofinterventionsthismonthnumber")
                 )
               )
               ),
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberofnewreferralsthismonthtext")
                 ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberofnewreferralsthismonthnumber")
                 )
                 #,
                 #tags$h3(
                #   class = "card-text",
                #   textOutput("numberoftier1studentsnumber")
                # )
               )
        ),
        
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberoftier1studentstext")
                 ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberoftier1studentsnumber")
                 )
               )
        ),
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberoftier2studentstext")
                 ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberoftier2studentsnumber")
                 )
               )
        ),
        column(2,
               wellPanel(
                 class = "card border-info mb-3",
                 #class = "card text-white bg-info mb-3",
                 tags$h6(
                   class = "card-title",
                   textOutput("numberoftier3studentstext")
                 ),
                 tags$h3(
                   class = "card-text",
                   textOutput("numberoftier3studentsnumber")
                 )
               )
        )
      ),
      br(),
      fluidRow(
        column(10,
          plotOutput("actionsplot")
        ),
        column(2,
               dateInput("datefrom1", label = h5("Date From"), value = "2022-06-01"),
               dateInput("dateto1", label = h5("Date To"), value = NULL)
        )
      ),
      br(),
      fluidRow(
        column(6,
               plotOutput("tasksplot")
        ),
        column(6,
               plotOutput("wheelassessmentcount")  
        )
      )
    ),
    ###########################################################################################################################################################
    ###########################################################################################################################################################
    tabPanel(
      "Cohort",
      fluidRow(
        column(10,
               plotOutput("galleryplot")
        ),
        column(2,
               dateInput("datefrom2", label = h5("Date From"), value = "2022-01-01"),
               dateInput("dateto2", label = h5("Date To"), value = NULL)
        )
      ),
      br(),
      fluidRow(
        column(4,
          plotOutput("ethnicityplot")
        ),
        column(4,
          plotOutput("genderplot")
        ),
        column(4,
               plotOutput("ageplot")
        )
      )
    ),
    ###########################################################################################################################################################
    ###########################################################################################################################################################
    tabPanel(
      "Analytics",
      fluidRow(
          column(4,
                 fluidRow(
                   br(),
                   column(6,
                          br(),
                          wellPanel(
                            class = "card border-info mb-3",
                            #class = "card text-white bg-info mb-3",
                            tags$h6(
                              class = "card-title",
                              textOutput("wheelmeantext")
                            ),
                            tags$h3(
                              class = "card-text",
                              textOutput("wheelmeannumber")
                            )
                          )
                          ),
                   column(6,
                          br(),
                          wellPanel(
                            class = "card border-info mb-3",
                            #class = "card text-white bg-info mb-3",
                            tags$h6(
                              class = "card-title",
                              textOutput("wheelsdtext")
                            ),
                            tags$h3(
                              class = "card-text",
                              textOutput("wheelsdnumber")
                            )
                          )
                          )
                 ),
                 fluidRow(
                   column(6,
                          br(),
                          wellPanel(
                            class = "card border-info mb-3",
                            #class = "card text-white bg-info mb-3",
                            tags$h6(
                              class = "card-title",
                              textOutput("numberwheelstext")
                            ),
                            tags$h3(
                              class = "card-text",
                              textOutput("numberwheelsnumber")
                            )
                          )
                   ),
                   column(6,
                          br(),
                          wellPanel(
                            class = "card border-info mb-3",
                            #class = "card text-white bg-info mb-3",
                            tags$h6(
                              class = "card-title",
                              textOutput("mostimprovedcategorytext")
                            ),
                            tags$h4(
                              class = "card-text",
                              textOutput("mostimprovedcategorynumber")
                            )
                          )
                   )
                 )
          ),
          column(6,
                 br(),
                 plotOutput("normaldistwheel")
                 ),
          column(2,
                 br(),
                dateInput("datefrom3", label = h5("Date From"), value = "2022-01-01"),
                 dateInput("dateto3", label = h5("Date To"), value = NULL)
         )
        )
      )
    )
  )
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
##### Server Code
server <- function(input, output, session) {
  #bslib::bs_themer()
  thematic::thematic_shiny()
  tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
  )
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  #####MySQL data manipulation in the system
  con=DBI::dbConnect(RMySQL::MySQL(),
                     dbname="",
                     host="",
                     port=,
                     username="",
                     password="$")
  rmmcon = DBI::dbConnect(RMySQL::MySQL(),
                          dbname="",
                          host="",
                          port=,
                          username="",
                          password="$")
  ###DB Connections
  enforcementactioninter <- dbSendQuery(con,"SELECT * FROM enforcement_action;")
  enforcement_action <- dbFetch(enforcementactioninter)
  dbClearResult(enforcementactioninter)
  #Get the Profile Table
  profileinter <- dbSendQuery(con,"SELECT * FROM profile ORDER BY profile_id DESC LIMIT 100;")
  profile <- dbFetch(profileinter)
  dbClearResult(profileinter)
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  #### KPI Information on the Workload Page
  start_date <- floor_date(Sys.Date() %m-% months(1), 'month')
  end_date <- ceiling_date(Sys.Date() %m-% months(1), 'month') %m-% days(1)
  #Number of Cases
  output$numberofcasestext <- renderText({
    paste("New Cases")
  })
  output$numberofcasesnumber <- renderText({
    casesdatequery <- paste("SELECT * FROM cases WHERE date_added BETWEEN CONVERT('",start_date,"',DATETIME) AND CONVERT('",end_date,"',DATETIME);",sep = "")
    casesinter <- dbSendQuery(con,casesdatequery)
    cases <- dbFetch(casesinter)
    dbClearResult(casesinter)
    paste(as.numeric(length(cases$cases_id)))
  })
  #Number of Interventions
  output$numberofinterventionsthismonthtext <- renderText({
    "Interventions This Month"
  })
  output$numberofinterventionsthismonthnumber <- renderText({
    enforcementactionmapdatequery <- paste("SELECT * FROM enforcement_action_map WHERE date_added BETWEEN CONVERT('",start_date,"',DATETIME) AND CONVERT('",end_date,"',DATETIME);",sep = "")
    enforcementactionmapinter <- dbSendQuery(con,enforcementactionmapdatequery)
    enforcement_action_map <- dbFetch(enforcementactionmapinter)
    dbClearResult(enforcementactionmapinter)
    as.numeric(length(enforcement_action_map[(as.numeric(as.Date(enforcement_action_map$date_added)) %in% start_date:end_date),1]))
  })
  #Number of New Referrals this Month
  output$numberofnewreferralsthismonthtext <- renderText({
    "New Referrals This Month"
  })
  output$numberofnewreferralsthismonthnumber <- renderText({
    newreferalsquerey <- paste("SELECT * FROM reports WHERE created_at BETWEEN CONVERT('",start_date,"',DATETIME) AND CONVERT('",end_date,"',DATETIME);",sep = "")
    newreferralinter <- dbSendQuery(rmmcon,newreferalsquerey)
    newreferalsnumberinter <- dbFetch(newreferralinter)
    dbClearResult(newreferralinter)
    referralcreatedat <- newreferalsnumberinter$created_at  
    as.numeric(length(referralcreatedat)) 
  })
  #Number of Tier 1 Students
  output$numberoftier1studentstext <- renderText({
    "Tier 1 Students"
  })
  output$numberoftier1studentsnumber <- renderText({
    gallerytieronedatequery <- paste("SELECT * FROM gallery_snapshot WHERE snapshot_id=(SELECT MAX(snapshot_id) FROM gallery_snapshot);",sep = "")
    gallerytieroneinter <- dbSendQuery(con,gallerytieronedatequery)
    gallerytierone <- dbFetch(gallerytieroneinter)
    dbClearResult(gallerytieroneinter)
    tieronenumber <- as.numeric(gallerytierone$gallery_1_high[1]) + as.numeric(gallerytierone$gallery_1_medium[1]) + as.numeric(gallerytierone$gallery_1_low[1])
    as.numeric(tieronenumber)
  })
  #Number of Tier 2 Students
  output$numberoftier2studentstext <- renderText({
    "Tier 2 Students"
  })
  output$numberoftier2studentsnumber <- renderText({
    gallerytiertwodatequery <- paste("SELECT * FROM gallery_snapshot WHERE snapshot_id=(SELECT MAX(snapshot_id) FROM gallery_snapshot);",sep = "")
    gallerytiertwointer <- dbSendQuery(con,gallerytiertwodatequery)
    gallerytiertwo <- dbFetch(gallerytiertwointer)
    dbClearResult(gallerytiertwointer)
    tiertwonumber <- as.numeric(gallerytiertwo$gallery_2_high[1]) + as.numeric(gallerytiertwo$gallery_2_medium[1]) + as.numeric(gallerytiertwo$gallery_2_low[1])
    as.numeric(tiertwonumber)
  })
  #Number of Tier 3 Students
  output$numberoftier3studentstext <- renderText({
    "Tier 3 Students"
  })
  output$numberoftier3studentsnumber <- renderText({
    gallerytierthreedatequery <- paste("SELECT * FROM gallery_snapshot WHERE snapshot_id=(SELECT MAX(snapshot_id) FROM gallery_snapshot);",sep = "")
    gallerytierthreeinter <- dbSendQuery(con,gallerytierthreedatequery)
    gallerytierthree <- dbFetch(gallerytierthreeinter)
    dbClearResult(gallerytierthreeinter)
    tierthreenumber <- as.numeric(gallerytierthree$gallery_3_high[1]) + as.numeric(gallerytierthree$gallery_3_medium[1]) + as.numeric(gallerytierthree$gallery_3_low[1])
    as.numeric(tierthreenumber)
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  ####Graphs on the Workload Page
  #Interventions
  reactivedatefrom <- reactive(as.Date(input$datefrom1))
  reactivedateto <- reactive(as.Date(input$dateto1))
  output$actionsplot <- renderPlot({
    reactivedatefrom1 <- reactivedatefrom()
    reactivedateto1 <- reactivedateto()
    enforcementactionmapdatequery <- paste("SELECT * FROM enforcement_action_map WHERE date_added BETWEEN CONVERT('",reactivedatefrom1,"',DATETIME) AND CONVERT('",reactivedateto1,"',DATETIME);",sep = "")
    enforcementactionmapinter <- dbSendQuery(con,enforcementactionmapdatequery)
    enforcement_action_map <- dbFetch(enforcementactionmapinter)
    dbClearResult(enforcementactionmapinter)
    enforcement_action_map$date_added <- as.Date(enforcement_action_map$date_added)
    stri_enc_toutf8(enforcement_action_map, is_unknown_8bit = FALSE, validate = FALSE)
    for (i in 1:length(enforcement_action_map$enforcement_action_id))
    {
      for (j in 1:length(enforcement_action$enforcement_action_id))
      {
        if(enforcement_action_map$enforcement_action_id[i] == enforcement_action$enforcement_action_id[j])
          enforcement_action_map$enforcement_action_id[i] <- as.character(enforcement_action$enforcement_action[j])
      }
      enforcement_action_map$enforcement_action_map_id <- 1
    }
    ggplot(enforcement_action_map,aes(x=date_added,y=enforcement_action_map_id,fill=enforcement_action_id)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number of Interventions") + labs(fill="Intervention Type") + theme(axis.title = element_text(size = 15)) + theme(legend.title = element_text(size = 15))
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  #Tasks
  output$tasksplot <- renderPlot({
    reactivedatefrom1 <- reactivedatefrom()
    reactivedateto1 <- reactivedateto()
    tasksdatequery <- paste("SELECT * FROM tasks WHERE date_added BETWEEN CONVERT('",reactivedatefrom1,"',DATETIME) AND CONVERT('",reactivedateto1,"',DATETIME);",sep = "")
    tasksinter <- dbSendQuery(con,tasksdatequery)
    tasks <- dbFetch(tasksinter)
    dbClearResult(tasksinter)
    if(length(tasks$tasks_id)>0){
      for(i in 1:length(tasks$tasks_id)){
        tasks$tasks_id[i] <- 1
        stri_enc_toutf8(tasks$task_type_options, is_unknown_8bit = FALSE, validate = FALSE)
      }
    }
    ggplot(tasks,aes(x=completion_date,y=tasks_id,fill=task_type_options)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number of Tasks") + labs(fill="Task Type") + theme(axis.title = element_text(size = 15)) + theme(legend.title = element_text(size = 15))
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  #Wheel Assessment
  output$wheelassessmentcount <- renderPlot({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom()
    reactivedateto1 <- reactivedateto()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true}'
    )
    if(length(wheelassessments) == 0){
      plot.new()
    }else {
      wheelassessments$updated <- as.Date(wheelassessments$updated,origin="1970-01-01")
      testdf <- dplyr::count(wheelassessments,updated)
      ggplot(testdf[as.numeric(testdf$updated) %in% as.numeric(as.Date(reactivedatefrom1,origin="1970-01-01")):as.numeric(as.Date(reactivedateto1,origin="1970-01-01")),],aes(x=updated,y=cumsum(n)))+geom_line(stat="identity",color = "lightblue",size=2)+geom_point(mapping=aes(updated),color = "lightblue",size=3) + xlab("Date") + ylab("Number of Wheel Assessments") + theme(axis.title = element_text(size = 15)) 
    }
  
    })
  output$wheelassessmentcountnumber <- renderText({
    reactivedatefrom1 <- reactivedatefrom()
    reactivedateto1 <- reactivedateto()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true}'
    )
    wheelassessments$updated <- as.Date(as.character(as.POSIXct(wheelassessments$updated)))
    testdf <- count(wheelassessments,updated)
    print(length(testdf))
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  reactivedatefrom2 <- reactive(as.Date(input$datefrom2))
  reactivedateto2 <- reactive(as.Date(input$dateto2))
  output$galleryplot <- renderPlot({
    gallerydetailquery <- paste("SELECT * FROM gallery_snapshot WHERE snapshot_id=(SELECT MAX(snapshot_id) FROM gallery_snapshot);",sep = "")
    gallerydetailinter <- dbSendQuery(con,gallerydetailquery)
    gallerydetail <- dbFetch(gallerydetailinter)
    dbClearResult(gallerydetailinter)
    gallery1lowmap <- (rep(1, gallerydetail$gallery_1_low[1]))
    gallery1lowcategory <- (rep(1, gallerydetail$gallery_1_low[1]))
    gallery1lowpriority <- (rep(1, gallerydetail$gallery_1_low[1]))
    gallery1mediummap <- (rep(1, gallerydetail$gallery_1_medium[1]))
    gallery1mediumcategory <- (rep(1, gallerydetail$gallery_1_medium[1]))
    gallery1mediumpriority <- (rep(2, gallerydetail$gallery_1_medium[1]))
    gallery1highmap <- (rep(1, gallerydetail$gallery_1_high[1]))
    gallery1highcategory <- (rep(1, gallerydetail$gallery_1_high[1]))
    gallery1highpriority <- (rep(3, gallerydetail$gallery_1_high[1]))
    gallery2lowmap <- (rep(1, gallerydetail$gallery_2_low[1]))
    gallery2lowcategory <- (rep(2, gallerydetail$gallery_2_low[1]))
    gallery2lowpriority <- (rep(1, gallerydetail$gallery_2_low[1]))
    gallery2mediummap <- (rep(1, gallerydetail$gallery_2_medium[1]))
    gallery2mediumcategory <- (rep(2, gallerydetail$gallery_2_medium[1]))
    gallery2mediumpriority <- (rep(2, gallerydetail$gallery_2_medium[1]))
    gallery2highmap <- (rep(1, gallerydetail$gallery_2_high[1]))
    gallery2highcategory <- (rep(2, gallerydetail$gallery_2_high[1]))
    gallery2highpriority <- (rep(3, gallerydetail$gallery_2_high[1]))
    gallery3lowmap <- (rep(1, gallerydetail$gallery_3_low[1]))
    gallery3lowcategory <- (rep(3, gallerydetail$gallery_3_low[1]))
    gallery3lowpriority <- (rep(1, gallerydetail$gallery_3_low[1]))
    gallery3mediummap <- (rep(1, gallerydetail$gallery_3_medium[1]))
    gallery3mediumcategory <- (rep(3, gallerydetail$gallery_3_medium[1]))
    gallery3mediumpriority <- (rep(2, gallerydetail$gallery_3_medium[1]))
    gallery3highmap <- (rep(1, gallerydetail$gallery_3_high[1]))
    gallery3highcategory <- (rep(3, gallerydetail$gallery_3_high[1]))
    gallery3highpriority <- (rep(3, gallerydetail$gallery_3_high[1]))
    mapID <- list(gallery1lowmap,gallery1mediummap,gallery1highmap,gallery2lowmap,gallery2mediummap,gallery2highmap,gallery3lowmap,gallery3mediummap,gallery3highmap)
    mapID <- rbindlist(lapply(mapID,as.data.frame))
    categoryID <- list(gallery1lowcategory,gallery1mediumcategory,gallery1highcategory,gallery2lowcategory,gallery2mediumcategory,gallery2highcategory,gallery3lowcategory,gallery3mediumcategory,gallery3highcategory)
    categoryID <- rbindlist(lapply(categoryID,as.data.frame))
    priorityID <- list(gallery1lowpriority,gallery1mediumpriority,gallery1highpriority,gallery2lowpriority,gallery2mediumpriority,gallery2highpriority,gallery3lowpriority,gallery3mediumpriority,gallery3highpriority)
    priorityID <- rbindlist(lapply(priorityID,as.data.frame))
    gallerydetailfinal <- data.frame(mapID,categoryID,priorityID)
    colnames(gallerydetailfinal) <- c("mapID","categoryID","priorityID")
    ggplot(gallerydetailfinal, aes(priorityID, mapID)) + geom_bar(aes(fill=priorityID), stat="identity") + facet_grid( ~ categoryID) + xlab("Tier") + ylab("Number of Students") + labs(fill="Tier") + theme(axis.title = element_text(size = 15))
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  output$genderplot <- renderPlot({
    reactivedatefrom2 <- reactivedatefrom2()
    reactivedateto2 <- reactivedateto2()
    profilegenderquery <- paste("SELECT gender FROM profile WHERE date_added BETWEEN CONVERT('",reactivedatefrom2,"',DATETIME) AND CONVERT('",reactivedateto2,"',DATETIME);",sep = "")
    profilegenderinter <- dbSendQuery(con,profilegenderquery)
    profile_gender <- dbFetch(profilegenderinter)
    dbClearResult(profilegenderinter)
    if(length(profile_gender$gender) == 0){
      plot.new()
    }else {
      ggplot(profile_gender, aes(x="", y=gender, fill=gender)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("") + xlab("") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + labs(fill="Gender") + theme(axis.title = element_text(size = 15)) + theme(legend.title = element_text(size = 15))
    }  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  output$ageplot <- renderPlot({
    reactivedatefrom2 <- reactivedatefrom2()
    reactivedateto2 <- reactivedateto2()
    profileagequery <- paste("SELECT age FROM profile WHERE date_added BETWEEN CONVERT('",reactivedatefrom2,"',DATETIME) AND CONVERT('",reactivedateto2,"',DATETIME);",sep = "")
    profileageinter <- dbSendQuery(con,profileagequery)
    profile_age <- dbFetch(profileageinter)
    dbClearResult(profileageinter)
    if(length(profile_age$age) == 0){
      plot.new()
    }else {
      ggplot(profile_age, aes(x="", y=age, fill=age)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("") + xlab("") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + labs(fill="Age") + theme(axis.title = element_text(size = 15)) + theme(legend.title = element_text(size = 15))
    }
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  output$ethnicityplot <- renderPlot({
    reactivedatefrom2 <- reactivedatefrom2()
    reactivedateto2 <- reactivedateto2()
    profileethnicityquery <- paste("SELECT profile_id,ethnic_origin FROM profile WHERE date_added BETWEEN CONVERT('",reactivedatefrom2,"',DATETIME) AND CONVERT('",reactivedateto2,"',DATETIME);",sep = "")
    profileethnicityinter <- dbSendQuery(con,profileethnicityquery)
    profile_ethnicity <- dbFetch(profileethnicityinter)
    dbClearResult(profileethnicityinter)
    if(length(profile_ethnicity$profile_id) == 0){
      plot.new()
    }else {
      for(i in 1:length(profile_ethnicity$profile_id)){
        profile_ethnicity$profile_id[i] <- 1
      }
      ggplot(profile_ethnicity, aes(x=ethnic_origin, y=profile_id, fill=ethnic_origin)) + geom_bar(stat = "identity") + ylab("Number of Students") + xlab("Ethnicity") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + labs(fill="Ethnicity") + theme(axis.title = element_text(size = 15)) + theme(legend.title = element_text(size = 15))
    }  
  })
  ###########################################################################################################################################################
  ###########################################################################################################################################################
  reactivedatefrom3 <- reactive(as.numeric(as.Date(input$datefrom3)))
  reactivedateto3 <- reactive(as.numeric(as.Date(input$dateto3)))
  output$normaldistwheel <- renderPlot({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom3()
    reactivedateto1 <- reactivedateto3()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true, "wheelId" : true, "assessments.categories.score" : true}')
    newdf <- wheelassessments[wheelassessments$wheelId %in% "560281bd6d0ff9633c8b45d8",]
    scoreID <- data.frame()
    for (i in 1:length(newdf$assessments)) {
      newdf1<- newdf[[5]][[i]][[1]][[1]]
      for (j in 1:length(newdf1[,1])){
        scoreID[i,j] <- newdf1[j,]
      }
    }
    meanID <- array(0,length(scoreID$V1))
    for(i in 1:length(scoreID$V1)){
      for(j in 1:length(scoreID[1,])){
        if(scoreID[i,j] == ""){
          scoreID[i,j] <- 0
        }
        meanID[i] <- meanID[i] + as.numeric(scoreID[i,j])
      }
      meanID[i] <- meanID[i]/length(scoreID[1,])
    }
    meanx <- mean(meanID)
    sdx <- sd(meanID)
    y <- dnorm(meanID,meanx,sdx)
    x <- meanID
    datatest <- data.frame(x,y)
    if(meanx <= 2.5){
      ggplot(datatest,aes(x,y))+geom_line(color = "red")
    }else {
      ggplot(datatest,aes(x,y))+geom_line(color = "green")
    }
  })
  output$wheelmeantext <- renderText({
    "Mean Wheel Score"
  })
  output$wheelmeannumber <- renderText({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom3()
    reactivedateto1 <- reactivedateto3()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true, "wheelId" : true, "assessments.categories.score" : true}')
    newdf <- wheelassessments[wheelassessments$wheelId %in% "560281bd6d0ff9633c8b45d8",]
    scoreID <- data.frame()
    for (i in 1:length(newdf$assessments)) {
      newdf1<- newdf[[5]][[i]][[1]][[1]]
      for (j in 1:length(newdf1[,1])){
        scoreID[i,j] <- newdf1[j,]
      }
    }
    meanID <- array(0,length(scoreID$V1))
    for(i in 1:length(scoreID$V1)){
      for(j in 1:length(scoreID[1,])){
        if(scoreID[i,j] == ""){
          scoreID[i,j] <- 0
        }
        meanID[i] <- meanID[i] + as.numeric(scoreID[i,j])
      }
      meanID[i] <- meanID[i]/length(scoreID[1,])
    }
    mean(meanID)
  })
  output$wheelsdtext <- renderText({
    "Wheel Standard Deviation"
  })
  output$wheelsdnumber <- renderText({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom3()
    reactivedateto1 <- reactivedateto3()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true, "wheelId" : true, "assessments.categories.score" : true}')
    newdf <- wheelassessments[wheelassessments$wheelId %in% "560281bd6d0ff9633c8b45d8",]
    scoreID <- data.frame()
    for (i in 1:length(newdf$assessments)) {
      newdf1<- newdf[[5]][[i]][[1]][[1]]
      for (j in 1:length(newdf1[,1])){
        scoreID[i,j] <- newdf1[j,]
      }
    }
    meanID <- array(0,length(scoreID$V1))
    for(i in 1:length(scoreID$V1)){
      for(j in 1:length(scoreID[1,])){
        if(scoreID[i,j] == ""){
          scoreID[i,j] <- 0
        }
        meanID[i] <- meanID[i] + as.numeric(scoreID[i,j])
      }
      meanID[i] <- meanID[i]/length(scoreID[1,])
    }
    sd(meanID)
    })
  output$numberwheelstext <- renderText({
    "Number of Wheel Assessments"
  })
  output$numberwheelsnumber <- renderText({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom3()
    reactivedateto1 <- reactivedateto3()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true, "wheelId" : true, "assessments.categories.score" : true}')
    newdf <- wheelassessments[wheelassessments$wheelId %in% "560281bd6d0ff9633c8b45d8",]
    scoreID <- data.frame()
    for (i in 1:length(newdf$assessments)) {
      newdf1<- newdf[[5]][[i]][[1]][[1]]
      for (j in 1:length(newdf1[,1])){
        scoreID[i,j] <- newdf1[j,]
      }
    }
    meanID <- array(0,length(scoreID$V1))
    for(i in 1:length(scoreID$V1)){
      for(j in 1:length(scoreID[1,])){
        if(scoreID[i,j] == ""){
          scoreID[i,j] <- 0
        }
        meanID[i] <- meanID[i] + as.numeric(scoreID[i,j])
      }
      meanID[i] <- meanID[i]/length(scoreID[1,])
    }
    length(meanID)

      })
  output$mostimprovedcategorytext <- renderText({
    "Highest Factor"
  })
  output$mostimprovedcategorynumber <- renderText({
    con1=mongo(collection="",
               db="",
               url="")
    reactivedatefrom1 <- reactivedatefrom3()
    reactivedateto1 <- reactivedateto3()
    tm <- as.POSIXlt(as.Date(reactivedatefrom1,origin="1970-01-01"), "UTC")
    tm1 <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    wheelassessments <-  con1$find(
      query = paste0('{"updated": { "$gte" : { "$date" : "',tm1, '"}}}'),
      fields = '{"profileId" : true, "updated" : true, "wheelId" : true, "assessments.categories.score" : true}')
    newdf <- wheelassessments[wheelassessments$wheelId %in% "560281bd6d0ff9633c8b45d8",]
    scoreID <- data.frame()
    for (i in 1:length(newdf$assessments)) {
      newdf1<- newdf[[5]][[i]][[1]][[1]]
      for (j in 1:length(newdf1[,1])){
        scoreID[i,j] <- newdf1[j,]
      }
    }
    meanID2 <- array(0,length(scoreID[1,]))
    for(i in 1:length(scoreID[1,])){
      for(j in 1:length(scoreID$V1)){
        if(scoreID[j,i] == ""){
          scoreID[j,i] <- 0
        }
        meanID2[i] <- meanID2[i] + as.numeric(scoreID[j,i])
      }
    }
    x <- max(meanID2)
    for(i in 1:length(meanID2)){
      if(meanID2[i] == x){
        maximum <- i
      }
    }
    paste(maximum)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)


