library(shiny)
require(shiny)
library(png)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(shinydashboard)
library(plotly)
library(shinymanager)
library(shinyjs)
library(digest)
library(devtools)
library(DBI)
library(RODBC)
library(odbc)
library(dbplyr)
library(stringr)
library(randomForest)
library(shinyWidgets)
library(shinythemes)
library(shinyalert)
library(shinyBS)
library(shinydashboardPlus)

#check if odbc is installed
#sort(unique(odbcListDrivers()[[1]]))

credentials <- data.frame(
  user = c("user", "admin"), # mandatory
  password = c("user", "admin"), # mandatory
  stringsAsFactors = FALSE
)







df <- reactiveVal(data.frame("date" = character(), "predicted" = numeric()))


server<-shinyServer(function(input,output,session){
  
  
res_auth <- secure_server(
  check_credentials = check_credentials(credentials))
  
  
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  
  
  #defining database connection
  conn<- dbConnect(RMySQL::MySQL(),
                   dbname="dragon_ml",
                   Server="localhost",
                   port=3306,
                   user="root")
  
  train <-
    dbGetQuery(conn, "Select * 
                        from smst_train")
  
  model <- reactive({
    randomForest::randomForest(water_required ~ ., data = train)
  })
  

  
  date<- reactive({
    dbGetQuery(conn,"select ID
                     from dht
                      inner join 
                        (
                            Select max(ID) as LatestDate
                            from dht
                        )SubMax 
                        on dht.ID = SubMax.LatestDate")
  })
  
  predictValues<-reactive({
    dbGetQuery(conn,"select temp,soil_moist,humidity
                      from dht
                      inner join 
                        (
                            Select max(ID) as LatestDate
                            from dht
                        )SubMax 
                        on dht.ID = SubMax.LatestDate")
  })
  test <-
    dbGetQuery(conn, "Select * 
                        from dht")
  
  # Generate predictions
  predictions <- reactive({
    req(model(), predictValues())
    predict(model(), newdata = predictValues())
  })
  
  #value1<-date()
  #value2<- predictions()
  #isolate(value1)
  #isolate(value2)
  
  #vecRow<- data.frame("date"=as.character(value1),"predicted"=as.numeric(value2))
  
  #df<- bind_rows(df,vecRow)
  
  
  
  observeEvent(input$button, {
    value1 <- date()
    value2 <- predictions()
    
    vecRow <- data.frame("date" = as.character(value1), "predicted" = as.numeric(value2))
    
    current_df <- df()
    new_df <- bind_rows(current_df, vecRow)
    
    df(new_df()) 
  })
  
  
  
  print(df)
  
  output$prediction <- renderValueBox({
    valueBox(
      floor(predictions()),
      "prediction",
      icon=icon("hand-holding-droplet"),
      color = "aqua"
    )
  }) 
  
  
  
  nitrogen<-reactiveVal()
  phosphorous<-reactiveVal()
  potassium<-reactiveVal()
  humidity<-reactiveVal()
  temp<-reactiveVal()
  moist<-reactiveVal()
  hum1<-reactiveVal()
  hum2<-reactiveVal()
  hum3<-reactiveVal()
  hum4<-reactiveVal()
  hum5<-reactiveVal()
  tem1<-reactiveVal()
  tem2<-reactiveVal()
  tem3<-reactiveVal()
  tem4<-reactiveVal()
  tem5<-reactiveVal()
  moist1<-reactiveVal()
  moist2<-reactiveVal()
  moist3<-reactiveVal()
  moist4<-reactiveVal()
  moist5<-reactiveVal()
  n1<-reactiveVal()
  n2<-reactiveVal()
  n3<-reactiveVal()
  n4<-reactiveVal()
  n5<-reactiveVal()
  k1<-reactiveVal()
  k2<-reactiveVal()
  k3<-reactiveVal()
  k4<-reactiveVal()
  k5<-reactiveVal()
  p1<-reactiveVal()
  p2<-reactiveVal()
  p3<-reactiveVal()
  p4<-reactiveVal()
  p5<-reactiveVal()
  hum_array<-reactiveVal()
  tem_array<-reactiveVal()
  mois_array<-reactiveVal()
  
  update_data<-function(){
    humid <- 
      dbGetQuery(conn, "Select humidity 
                        from dht
                        inner join 
                        (
                            Select max(ID) as LatestDate
                            from dht
                        )SubMax 
                        on dht.ID = SubMax.LatestDate")
    temp <-
      dbGetQuery(conn, "Select temp 
                        from dht
                        inner join 
                        (
                            Select max(ID) as LatestDate
                            from dht
                        )SubMax 
                        on dht.ID = SubMax.LatestDate")
    moisture <-
      dbGetQuery(conn, "Select soil_moist 
                        from dht
                        inner join 
                        (
                            Select max(ID) as LatestDate
                            from dht
                        )SubMax 
                        on dht.ID = SubMax.LatestDate")
    
    nitro <- 
      dbGetQuery(conn, "Select nitrogen
                        from tr1
                        inner join 
                        (
                            Select max(sno) as LatestDate
                            from tr1
                        )SubMax 
                        on tr1.sno = SubMax.LatestDate")
    
    
    
    phosph <-
      dbGetQuery(conn, "Select phosphorous
                        from tr1
                        inner join 
                        (
                            Select max(sno) as LatestDate
                            from tr1
                        )SubMax 
                        on tr1.sno = SubMax.LatestDate")
    potass <-
      dbGetQuery(conn, "Select potassium
                        from tr1
                        inner join 
                        (
                            Select max(sno) as LatestDate
                            from tr1
                        )SubMax 
                        on tr1.sno = SubMax.LatestDate")
    
    h1 <- as.numeric(dbGetQuery(conn, "select humidity from dht order by ID DESC LIMIT 1;"))
    h2 <- as.numeric(dbGetQuery(conn, "select humidity from dht order by ID DESC LIMIT 1,1;"))
    h3 <- as.numeric(dbGetQuery(conn, "select humidity from dht order by ID DESC LIMIT 2,1;"))
    h4 <- as.numeric(dbGetQuery(conn, "select humidity from dht order by ID DESC LIMIT 3,1;"))
    h5 <- as.numeric(dbGetQuery(conn, "select humidity from dht order by ID DESC LIMIT 4,1;"))
    
    t1 <- as.numeric(dbGetQuery(conn, "select temp from dht order by ID DESC LIMIT 1;"))
    t2 <- as.numeric(dbGetQuery(conn, "select temp from dht order by ID DESC LIMIT 1,1;"))
    t3 <- as.numeric(dbGetQuery(conn, "select temp from dht order by ID DESC LIMIT 2,1;"))
    t4 <- as.numeric(dbGetQuery(conn, "select temp from dht order by ID DESC LIMIT 3,1;"))
    t5 <- as.numeric(dbGetQuery(conn, "select temp from dht order by ID DESC LIMIT 4,1;"))
    
    m1 <- as.numeric(dbGetQuery(conn, "select soil_moist from dht order by ID DESC LIMIT 1;"))
    m2 <- as.numeric(dbGetQuery(conn, "select soil_moist from dht order by ID DESC LIMIT 1,1;"))
    m3 <- as.numeric(dbGetQuery(conn, "select soil_moist from dht order by ID DESC LIMIT 1,1;"))
    m4 <- as.numeric(dbGetQuery(conn, "select soil_moist from dht order by ID DESC LIMIT 1,1;"))
    m5 <- as.numeric(dbGetQuery(conn, "select soil_moist from dht order by ID DESC LIMIT 1,1;"))
    
    nit1 <- as.numeric(dbGetQuery(conn, "select nitrogen from tr1 order by sno DESC LIMIT 1;"))
    nit2 <- as.numeric(dbGetQuery(conn, "select nitrogen from tr1 order by sno DESC LIMIT 1,1;"))
    nit3 <- as.numeric(dbGetQuery(conn, "select nitrogen from tr1 order by sno DESC LIMIT 2,1;"))
    nit4 <- as.numeric(dbGetQuery(conn, "select nitrogen from tr1 order by sno DESC LIMIT 3,1;"))
    nit5 <- as.numeric(dbGetQuery(conn, "select nitrogen from tr1 order by sno DESC LIMIT 4,1;"))
    
    phos1 <- as.numeric(dbGetQuery(conn, "select phosphorous from tr1 order by sno DESC LIMIT 1;"))
    phos2 <- as.numeric(dbGetQuery(conn, "select phosphorous from tr1 order by sno DESC LIMIT 1,1;"))
    phos3 <- as.numeric(dbGetQuery(conn, "select phosphorous from tr1 order by sno DESC LIMIT 2,1;"))
    phos4 <- as.numeric(dbGetQuery(conn, "select phosphorous from tr1 order by sno DESC LIMIT 3,1;"))
    phos5 <- as.numeric(dbGetQuery(conn, "select phosphorous from tr1 order by sno DESC LIMIT 4,1;"))
    
    pott1 <- as.numeric(dbGetQuery(conn, "select potassium from tr1 order by sno DESC LIMIT 1;"))
    pott2 <- as.numeric(dbGetQuery(conn, "select potassium from tr1 order by sno DESC LIMIT 1,1;"))
    pott3 <- as.numeric(dbGetQuery(conn, "select potassium from tr1 order by sno DESC LIMIT 2,1;"))
    pott4 <- as.numeric(dbGetQuery(conn, "select potassium from tr1 order by sno DESC LIMIT 3,1;"))
    pott5 <- as.numeric(dbGetQuery(conn, "select potassium from tr1 order by sno DESC LIMIT 4,1;"))
    
    n1(nit1)
    n2(nit2)
    n3(nit3)
    n4(nit4)
    n5(nit5)
    
    p1(phos1)
    p2(phos2)
    p3(phos3)
    p4(phos4)
    p5(phos5)
    
    k1(pott1)
    k2(pott2)
    k3(pott3)
    k4(pott4)
    k5(pott5)
    
    hum1(h1)
    hum2(h2)
    hum3(h3)
    hum4(h4)
    hum5(h5)
    
    moist1(m1)
    moist2(m2)
    moist3(m3)
    moist4(m4)
    moist5(m5)
    
    tem1(t1)
    tem2(t2)
    tem3(t3)
    tem4(t4)
    tem5(t5)
    
    humidity(humid)
    temp(temp)
    moist(moisture)
    
    nitrogen(nitro)
    phosphorous(phosph)
    potassium(potass)
    
    if(nitrogen()<150){
      createAlert(session,"alert1","exampleAlert1", title = "Alert", content = "Nitrogen is below threshold", append = FALSE,)
    }
    else{
      closeAlert(session,"exampleAlert1")
    }
      
    if(potassium()<150){
      createAlert(session,"alert2","exampleAlert2", title = "Alert", content = "Pottasium is below threshold", append = FALSE)
    }
    else{
      closeAlert(session,"exampleAlert2")
    }
      
    if(phosphorous()<75){
      createAlert(session,"alert3","exampleAlert3", title = "Alert", content = "Phosphorous is below threshold", append = FALSE)
    }
    else{
      closeAlert(session,"exampleAlert3")
    }
      
    if(phosphorous()>125){
      createAlert(session,"alert4","exampleAlert4", title = "Alert", content = "Phosphorous is above threshold", append = FALSE)
    }
    else{
      closeAlert(session,"exampleAlert4")
    }
      
    if(potassium()>200){
      createAlert(session,"alert5","exampleAlert5", title = "Alert", content = "Pottasium is above threshold", append = FALSE)
    }
    else{
      closeAlert(session,"exampleAlert5")
    } 
      
    if(nitrogen()>200){
      createAlert(session,"alert6","exampleAlert6", title = "Alert", content = "Nitrogen is above threshold", append = FALSE)
    }
    else{
      closeAlert(session,"exampleAlert6")
    }
    
    
    
    
    
    hum_array(c(hum5(), hum4(), hum3(), hum2(), hum1()))
    mois_array(c(moist1(),moist2(),moist3(),moist4(),moist5()))
    tem_array(c(tem1(),tem2(),tem3(),tem4(),tem5()))
  } 
  
  
  
  
  
  
  # Periodically update the data
  auto_refresh_interval <- 1000  # in milliseconds
  observe({
    
    invalidateLater(auto_refresh_interval, session)
    update_data()
  })
  
  output$lineplot <- renderPlotly({
    df <- data.frame(
      day = c('1', '2', '3', '4', '5'),
      humidity = c(hum1(), hum2(), hum3(), hum4(), hum5()),
      moisture = c(moist1(), moist2(), moist3(), moist4(), moist5()),
      temp = c(tem1(), tem2(), tem3(), tem4(), tem5())
    )
    
    plot_ly(df, x = ~day, y = ~humidity, name = "Humidity", type = "scatter", mode = "lines") %>%
      add_trace(y = ~moisture, name = "Soil Moisture", mode = "lines") %>%
      add_trace(y = ~temp, name = "Temperature", mode = "lines") %>%
      layout(title = "Environmental Data",
             xaxis = list(title = "Day"),
             yaxis = list(title = "Values"))
  })
  
  output$ineplot <- renderPlotly({
    df <- data.frame(
      day = c('1', '2', '3', '4', '5'),
      nit = c(n1(), n2(), n3(), n4(), n5()),
      phosp = c(p1(), p2(), p3(), p4(), p5()),
      pott = c(k1(), k2(), k3(), k4(), k5())
    )
    
    plot_ly(df, x = ~day, y = ~nit, name = "Nitrogen", type = "scatter", mode = "lines") %>%
      add_trace(y = ~phosp, name = "Phosphorous", mode = "lines") %>%
      add_trace(y = ~pott, name = "Pottasium", mode = "lines") %>%
      layout(title = "Soil Nutrient Data",
             xaxis = list(title = "Day"),
             yaxis = list(title = "Values"))
  })
  
  output$temp <- renderValueBox({
    valueBox(
      temp(),
      "temp",
      icon=icon("temperature-quarter"),
      color = "red"
    )
  })
  output$humiditty <- renderValueBox({
    valueBox(
      humidity(),
      "Humidity",
      icon=icon("droplet"),
      color = "blue"
    )
  })
  output$moist <- renderValueBox({
    valueBox(
      moist(),
      "Soil Moisture",
      icon=icon("seedling"),
      color = "olive"
    )
  })
  
  
  output$nitro <- renderValueBox({
    valueBox(
      nitrogen(),
      "Nitrogen",
      icon=icon("n"),
      color = "purple"
    )
  })
  output$phos <- renderValueBox({
    valueBox(
      phosphorous(),
      "Phosphorous",
      icon=icon("p"),
      color = "yellow"
    )
  })
  output$potta <- renderValueBox({
    valueBox(
      potassium(),
      "Potassium",
      icon=icon("k"),
      color = "maroon"
    )
  })
  
  
  
  
  
  
  
  observeEvent(input$predict, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  # Disconnect from the database
  onSessionEnded(function() {
    dbDisconnect(conn)
    print('hi')
  }, session)
  
})


ui<-shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title="Plantations"
                    # dropdownMenu(type = "notifications",
                    #             notificationItem(
                    #             text = "2 new tabs added to the dashboard",
                    #             icon = icon("dashboard"),
                    #             status = "success"
                    #             ))
                    #              notificationItem(
                    #                text = "server is currently running at 95% load",
                    #                icon = icon("warning"),
                    #                status = "warning"
                    #              )
                    #              )
                    
    ),
    
    
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Nilambur,Malappuram",tabName = "1", icon = icon("location-dot")),
        menuItem("Ettumanoor,Kottayam",tabName = "2", icon = icon("location-dot")),
        
        menuItem("Kolenchery,Ernakulam",tabName = "3", icon = icon("location-dot")),
        menuItem("Pala,Kottayam",tabName = "4", icon = icon("location-dot"))
      )),
    dashboardBody(
      setBackgroundImage(src="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBw8PDxUPDw8VFRUVFRUVFRUVFRUVFRUVFRUWFhUVFRUYHSggGBolHRUVITEhJSkrLi4uFx8zODMtNygtLisBCgoKDQ0NDw8NDysZFRkrKystLSstKy03Ky0rKzcrKy0rLS0tKzcrKzc3KysrKy0rLSsrKysrKysrLSsrKysrK//AABEIAKgBLAMBIgACEQEDEQH/xAAWAAEBAQAAAAAAAAAAAAAAAAAAAQf/xAAWEAEBAQAAAAAAAAAAAAAAAAAAARH/xAAVAQEBAAAAAAAAAAAAAAAAAAAAAf/EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAMAwEAAhEDEQA/AMUAUAAAACgAAABQAAAAAAAAAAABQICAAAAAAAAAAAAAAAAAAAAAAAAAUAAAAAKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAGgAAAACoAAAAAAAAAAAAAAAAAAABoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAKAAAAAAAAUAAAAAAAAAAAAAAAAAAAAAAAAAEAUAAAAAAAAQBQAAAAAMKAAgCgAAAAAAAAA//Z",shinydashboard = TRUE),
      shinyjs::useShinyjs(),
      bsAlert("alert1"),
      bsAlert("alert2"),
      bsAlert("alert3"),
      bsAlert("alert4"),
      bsAlert("alert5"),
      bsAlert("alert6"),
      tabItems(
        tabItem(tabName = "1",
                fluidRow(
                  
                  column(12,
                         valueBoxOutput("humiditty"),
                         valueBoxOutput("moist"),
                         valueBoxOutput("temp")
                         
                  )
                  ,column(12,
                          valueBoxOutput("nitro")
                          ,valueBoxOutput("phos")
                          ,valueBoxOutput("potta")
                  )
                  ,column(12,offset = 4,
                          valueBoxOutput("prediction")
                          
                  )
                  #,uiOutput("humidity_alert")
                  
                  
                  
                  
                  
                  
                  ,box(
                    title = 'Environmental Data',
                    plotlyOutput("lineplot", height = "200px")
                    
                  )
                  ,box(
                    title = 'Soil Nutrient Data',
                    plotlyOutput("ineplot", height = "200px")
                    
                  ),
                  
                  
                  
                  
                  
                  
                )
        ),
        tabItem(tabName = "2",
                tags$image(height=700,width=1300,src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEi3IQNA_mrDl4rwrLsOk2BPVKrsntm8SCMvTqDGc6kKW0LJEogbzkUqRn01gRnsufnOAKtufTZYFHNkvt4eHUx6g_ggkuoHdv0hQJkdHCS8qdKj6Bre7Yjfc49iGDlQjiFziT7aib_qpeMk8uln1_z1MUrWpEzBPN6cYZCq3sk21G2J9Gh_aKOGvFxt/w592-h318/1.png")
        ),
        tabItem(tabName = "3",
                tags$image(height=700,width=1300,src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEi3IQNA_mrDl4rwrLsOk2BPVKrsntm8SCMvTqDGc6kKW0LJEogbzkUqRn01gRnsufnOAKtufTZYFHNkvt4eHUx6g_ggkuoHdv0hQJkdHCS8qdKj6Bre7Yjfc49iGDlQjiFziT7aib_qpeMk8uln1_z1MUrWpEzBPN6cYZCq3sk21G2J9Gh_aKOGvFxt/w592-h318/1.png")
        ),
        tabItem(tabName = "4",
                tags$image(height=700,width=1300,src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEi3IQNA_mrDl4rwrLsOk2BPVKrsntm8SCMvTqDGc6kKW0LJEogbzkUqRn01gRnsufnOAKtufTZYFHNkvt4eHUx6g_ggkuoHdv0hQJkdHCS8qdKj6Bre7Yjfc49iGDlQjiFziT7aib_qpeMk8uln1_z1MUrWpEzBPN6cYZCq3sk21G2J9Gh_aKOGvFxt/w592-h318/1.png")
        )
      )
      
    )
  )
)


ui <- secure_app(ui)
shinyApp(ui = ui, server = server)


