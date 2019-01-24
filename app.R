

library(shiny)
library(magrittr)
library(jsonlite)
library(ggplot2)
library(shinyauthr)
library(shinyjs)
library(shinydashboard)
library(hashmap)

library(dplyr)
library(lubridate)

library(RSQLite)
library(DBI)
library(shinyalert)
library(reticulate)
library(hashmap)
#use_python("/usr/local/opt/python/bin/python3.6",required = TRUE)
#py_config()
source_python("create_df.py")
num <- fun()
print(num)
mydb <- dbConnect(RSQLite::SQLite(), "users.sqlite")
user_base <- data.frame(
  user = c("user", "admin"),
  password = c(
    digest::digest("user", algo = "md5"),
    digest::digest("admin", algo = "md5")
  ),
  permissions = c("user", "admin"),
  var1 = c("Maximum-MAX_CALL_DURATION_LAST_1D", "Minimum-MIN_CALL_DURATION_LAST_1D"),
  var2 = c("Maximum-MAX_CALL_DURATION_LAST_1D", "Minimum-MIN_CALL_DURATION_LAST_1D"),
  stringsAsFactors = FALSE
)

#DBI::dbRemoveTable(mydb,"users")

if (!DBI::dbExistsTable(mydb, "users")) {
  DBI::dbWriteTable(mydb, "users", user_base)
}

res <- dbSendQuery(mydb, "SELECT * FROM users")
user_base <- data.frame(dbFetch(res))
print(user_base)
dbClearResult(res)

temp <- read.csv(file = "dataset.csv", header = TRUE, sep = ",")
temp <- subset(temp, select = -X)
temp <-
  temp[temp$AVG_CALL_DURATION_LAST_1D <= temp$MAX_CALL_DURATION_LAST_1D,]
temp <-
  temp[temp$AVG_CALL_DURATION_LAST_3D <= temp$MAX_CALL_DURATION_LAST_3D,]
temp <-
  temp[temp$AVG_CALL_DURATION_LAST_7D <= temp$MAX_CALL_DURATION_LAST_7D,]

addKeys = function(nested_Vector) {
  keyed_nl = list()
  for (a in names(nested_Vector))
    keyed_nl[[a]] = paste0(a, "-", nested_Vector[a])
  keyed_nl
}

VectorOfItemsWithNames = c(
  "Average" = "AVG_CALL_DURATION_LAST_1D",
  "Total" = "TOTAL_CALL_DURATION_LAST_1D",
  "Maximum" = "MAX_CALL_DURATION_LAST_1D",
  "Minimum" = "MIN_CALL_DURATION_LAST_1D"
)

keyedList = addKeys(VectorOfItemsWithNames)


ui2 <- dashboardPage(
  dashboardHeader(title = "RKafka"),
  dashboardSidebar(sidebarMenuOutput("menu")),
  dashboardBody(
    shinyjs::useShinyjs(),
    
    useShinyalert(),
    
    shiny::div(
      id = "panel",
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        shiny::tags$h2("Please Log In", class = "text-center", style = "padding-top: 0;"),
        
        shiny::textInput("username" , "Username"),
        
        shiny::passwordInput("password", "Password"),
        
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton("button", "Log in", class = "btn-primary", style = "color: white;")
        ),
        
        shinyjs::hidden(shiny::div(
          id = "error",
          shiny::tags$p(
            "Invalid username or password",
            style = "color: red; font-weight: bold; padding-top: 5px;",
            class = "text-center"
          )
        ))
      )
    ),
    
    
    
    shinyjs::hidden(
      shiny::actionButton("logout", "Log Out", class = "btn-danger pull-right", style = "color: white;")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "subitem1",
              
              fluidRow(div(
                id = "auth",
                column(
                  width = 5,
                  valueBoxOutput("value1", width = NULL),
                  box(
                    width = NULL,
                    id = "box1"
                    ,
                    status = "primary"
                    ,
                    solidHeader = FALSE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("grid", height = 320)
                  ) ,
                  box(
                    width = NULL,
                    height = 80,
                    selectInput(
                      "var1",
                      label = "Choose type of call duration to be displayed",
                      choices = keyedList,
                      selected = "Average"
                    )
                    
                    
                    
                  )
                ),
                column(
                  width = 5,
                  valueBoxOutput("value2", width = NULL),
                  box(
                    width = NULL,
                    id = "box2"
                    ,
                    status = "primary"
                    ,
                    solidHeader = FALSE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("grid2", height = 320)
                  ),
                  box(
                    width = NULL,
                    height = 80,
                    selectInput(
                      "var2",
                      label = "Choose type of call duration to be displayed",
                      choices = keyedList,
                      selected = "Average"
                    )
                    )
                  
                  
                  
    
                              )
              ))),
      
      
      tabItem(tabName = "subitem2",

              fluidRow(div(id ="auth2",
                column(
                  width = 5,
                  box(
                    status = "primary",
                    width = NULL
                    ,
                    solidHeader = FALSE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("grid3", height = 520)
                  )),
                column(
                  width = 5,
                  box(
                    status = "primary",
                    width = NULL
                    ,
                    solidHeader = FALSE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("grid4", height = 520)
                  )
                  
                )
              ))),
      
      tabItem(tabName = "createUser",
              div( id ="add-user", box(
                textInput("name", "Name", ""),
                textInput("pw", "Password", ""),
                actionButton("addUser", "Add User", class = "button-primary")
              )))
    )
  )
)



index <- 0
get_new_data <- function() {
  #data <- fun2()
  #print(data)
  #print('---------------------------------------')
  data <- temp[index,]
  index <<- index + 1
  return(data)
}
values <<- NULL
values <<- get_new_data()
update_data <- function() {
  values <<- rbind(get_new_data(), values)
  values
}

server <- function(input, output, session) {
  valueBoxReacts <- reactiveValues()
  new_values <- reactive({
    data <- get_new_data()
    data
  })
  
  
  credentials <-
    shiny::reactiveValues(user_auth = FALSE, info = NULL)
  shinyjs::hide(id = "add-user")
  shinyjs::hide(id = "Sidebar")
  shinyjs::hide(id = "auth")
  shinyjs::hide(id = "auth2")
  
  
  output$selected_var <- renderText({
    paste("You have selected", input$var1)
  })
  
  
  
  initialized <- hashmap(c("admin", "user"), c( FALSE,FALSE))
  for (user in user_base$user ) {
    initialized[[user]] <- FALSE;
  }
  observe({
    if( credentials$user_auth && (credentials$info$var1 != input$var1 || credentials$info$var2 != input$var2)) {
      if(!initialized[[credentials$info$user]]) {
        
        updateSelectInput(session, "var1",
                          selected = credentials$info$var1);
        updateSelectInput(session, "var2",
                          selected = credentials$info$var2);
        initialized[[credentials$info$user]] <<- TRUE
      }
      else {
        res <- dbSendQuery(mydb, "SELECT * FROM users")
        users <- data.frame(dbFetch(res))
        users2<- users  
        users2[users2$user == credentials$info$user, ]$var2 <- input$var2;
        users2[users2$user == credentials$info$user, ]$var1 <- input$var1;
        
        credentials$info$var1 = input$var1;
        credentials$info$var2 = input$var2;
        
        DBI::dbWriteTable(mydb, "users", users2, overwrite = TRUE);
        
        dbClearResult(res)
        updateSelectInput(session, "var1",
                          selected = credentials$info$var1);
        updateSelectInput(session, "var2",
                          selected = credentials$info$var2);
      }
    
      
      
    }
    if (credentials$user_auth) {
      if (credentials$info$permissions == "admin") {
        shinyjs::show(id = "add-user")
      }
      shinyjs::show(id = "box1")
      shinyjs::show(id = "box2")
      shinyjs::show(id = "auth")
      shinyjs::show(id = "auth2")
      
    } else {
      shinyjs::hide(id = "Sidebar")
      shinyjs::hide(id = "add-user")
      shinyjs::hide(id = "box1")
      shinyjs::hide(id = "box2")
      shinyjs::hide(id = "auth")
      shinyjs::hide(id = "auth2")
      
    }
    
    
  })
  
  observeEvent(input$addUser, {
    if (input$name != '' && input$pw != '') {
      new_user <- data.frame(
        user = input$name,
        password = digest::digest(input$pw, algo = "md5"),
        permissions = "user",
        var1 = 'Minimum-MIN_CALL_DURATION_LAST_1D',
        var2 = 'Minimum-MIN_CALL_DURATION_LAST_1D',
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(mydb, "users", new_user, append = TRUE)
      
      res <- dbSendQuery(mydb, "SELECT * FROM users")
      
      user_base <<-
        data.frame(dbFetch(res))
      dbClearResult(res)
      initialized[[input$name]] = FALSE;
      shinyalert("Success", "New User Added", type = "success")
    }
  })
  
  observeEvent(input$button, {
    username <- input$username
    
    password <- digest::digest(input$password, algo = "md5")
    user <- data.frame(username, password)
    print(user)
    res <- dbSendQuery(mydb, "SELECT * FROM users")
    users <- data.frame(dbFetch(res))
    if (nrow(merge(user, users)) > 0) {
      shinyjs::hide(id = "panel")
      shinyjs::show(id = "logout")
      shinyjs::hide(id = "error")
     
      credentials$info <- subset(users, user == username)
      credentials$user_auth <- TRUE
      
      print(credentials$info)
      
    }
    else {
      shinyjs::show(id = "error")
      
    }
  })
  
  observeEvent(input$logout, {
    initialized[[credentials$info$user]] <<- FALSE;
    credentials$user_auth <- FALSE
    credentials$info <- NULL
    shinyjs::show(id = "panel")
    shinyjs::hide(id = "logout")
    shinyjs::hide(id = "error")
    
  })
  
  output$value1 <- renderValueBox({
    req(credentials$user_auth)
    valueBox(
      formatC(
        paste(valueBoxReacts$max_call, "s"),
        format = "d",
        big.mark = ','
      )
      ,
      "Longest call in seconds"
      ,
      icon = icon("stats", lib = 'glyphicon')
      ,
      color = "purple"
    )
    
    
  })
  
  
  output$value2 <- renderValueBox({
    req(credentials$user_auth)
    valueBox(
      formatC(
        valueBoxReacts$num_call,
        format = "d",
        big.mark = ','
      )
      ,
      "Total number of calls"
      ,
      icon = icon("stats", lib = 'glyphicon')
      ,
      color = "green"
    )
    
    
  })
  
  output$menu <- renderMenu({
    req(credentials$user_auth)
    menu <-
      sidebarMenu(
        menuItem(
          "Charts",
          icon = icon("bar-chart-o"),
          startExpanded = TRUE,
          menuSubItem("Call Duration", tabName = "subitem1"),
          menuSubItem("Date", tabName = "subitem2")
        ),
        if (credentials$info$permissions == "admin") {
          menuItem("Create User",
                   tabName = "createUser",
                   icon = icon("users"))
        }
      )
    menu
  })
  
  
  output$grid <- renderPlot({
    req(credentials$user_auth)
    nvalues <- update_data()
    valueBoxReacts$max_call <-
      max(nvalues$MAX_CALL_DURATION_LAST_7D)
    valueBoxReacts$num_call <- nrow(nvalues)
    invalidateLater(1000, session)
    gg <-
      #format(Sys.time(), format="%H:%M:%S") ---------------triba trenutno vrime stavit na x os
      ggplot(nvalues[1:25, ], aes_string(x = "ID", y = strsplit(input$var1, "-")[[1]][2]))
    gg <-
      gg + geom_point(col = "brown") + geom_line(col = "brown") + theme_bw() + labs(x =
                                                                                      "Time",
                                                                                    y = paste(strsplit(input$var1, "-")[[1]][1], "call duration", sep = " "))
    
    gg
  })
  
  output$grid2 <- renderPlot({
    nvalues <- update_data()
    valueBoxReacts$max_call <-
      max(nvalues$MAX_CALL_DURATION_LAST_7D)
    valueBoxReacts$num_call <- nrow(nvalues)
    # req(credentials()$user_auth)
    update_data()
    invalidateLater(1000, session)
    
    gg <-
      ggplot(data = nvalues, aes_string(x = strsplit(input$var2, "-")[[1]][2], fill = "CODE")) + #zbog koristenja dataset.csv preimenovano iz CALLER_COUNTRY u CODE
      geom_histogram(bins = 10) + labs(x = paste(strsplit(input$var2, "-")[[1]][1], "call duration", sep =
                                                   " "),
                                       y = "Number of occurrences")
    
    gg
    
    
  })
  
  output$grid3 <- renderPlot({
    req(credentials$user_auth)
    nvalues <- update_data()
    invalidateLater(1000, session)
    nvalues$DATE <- as.Date(nvalues$CALL_DATE,tryFormats = c("%Y-%d-%m"))
    nvalues$maxDate <-max(nvalues$DATE)
    nvalues <- filter(nvalues,nvalues$maxDate-nvalues$DATE<6) #only show last 5 days
    gg <- ggplot(data = nvalues) +
      geom_bar(mapping = aes(x = CALL_DATE, fill = CALL_DATE)) + guides(fill =
                                                                          FALSE) + labs(x = "Date", y = "Number of occurrences")
    
    
    gg
    
    
  })
  
  
  output$grid4 <- renderPlot({
    req(credentials$user_auth)
    nvalues <- update_data()
    invalidateLater(1000, session)
    nvalues$TOTAL_COUNT <- nvalues$CALLEE_CALL_COUNT_LAST_1D+nvalues$CALLER_CALL_COUNT_LAST_1D
    gg <- ggplot(data=nvalues)+
      geom_point(mapping = aes(x=AVG_CALL_DURATION_LAST_1D,y=TOTAL_COUNT,colour=CODE))+
      labs(x="Average call duration",y="Total call count",
           title = " With longer calls we have smaller number of total calls") + ylim(0,60)
    gg
    
    
  })
  
  
  session$onSessionEnded(function() {
    #   DBI::dbDisconnect(mydb)
    #  print("disconnected")
  })
  
  
}



shinyApp(ui = ui2, server = server)

