#install.packages("shinyjs")
#install.packages("shinydashboard")
library(shiny)
library(magrittr)
library(jsonlite)
library(ggplot2)
library(shinyauthr)
library(shinyjs)
library(shinydashboard)

library(lubridate)

library(RSQLite)
library(DBI)
library(shinyalert)
library(reticulate)
#use_python("/usr/local/opt/python/bin/python3.6",required = TRUE)
#py_config()
source_python("create_df.py")
num <- fun()
print(num)
mydb <- dbConnect(RSQLite::SQLite(), "users.sqlite")
user_base <- data.frame(
  user = c("user", "admin"),
  password = c("user", "admin"), 
  permissions = c("user", "admin"),
  name = c("User", "Admin"),
  stringsAsFactors = FALSE
)

#DBI::dbRemoveTable(mydb,"users")
if(!DBI::dbExistsTable(mydb, "users")) {
  DBI::dbWriteTable(mydb, "users", user_base)
}

res <- dbSendQuery(mydb, "SELECT * FROM users")
user_base<- data.frame(dbFetch(res))
print(user_base)
dbClearResult(res)

temp <- read.csv(file="dataset.csv", header=TRUE, sep=",")
temp <- subset(temp, select = -X)
temp <- temp[temp$AVG_CALL_DURATION_LAST_1D<=temp$MAX_CALL_DURATION_LAST_1D, ]
temp <- temp[temp$AVG_CALL_DURATION_LAST_3D<=temp$MAX_CALL_DURATION_LAST_3D, ]
temp <- temp[temp$AVG_CALL_DURATION_LAST_7D<=temp$MAX_CALL_DURATION_LAST_7D, ]


ui2 <-dashboardPage(
  dashboardHeader(title ="title"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Dashboard 3", tabName = "dashboard3", icon = icon("dashboard")),
      menuItem("Create User", tabName = "createUser", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    shinyjs::useShinyjs(),
    
    useShinyalert(),
    
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    shinyauthr::loginUI(id = "login"),
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
    
              fluidRow(
                column(width = 5,
                       valueBoxOutput("value1",width = NULL),
                       box(
                         width = NULL
                         ,status = "primary"
                         ,solidHeader = FALSE 
                         ,collapsible = TRUE 
                         ,plotOutput("grid",height = 320)
                       ) ,
                       box(width = NULL,height = 80,selectInput("var", 
                                                                label = "Choose a variable to display",
                                                                choices = c("AVG_CALL_DURATION_LAST_1D", 
                                                                            "TOTAL_CALL_DURATION_LAST_1D",
                                                                            "MAX_CALL_DURATION_LAST_1D", 
                                                                            "MIN_CALL_DURATION_LAST_1D"),
                                                                selected = "AVG_CALL_DURATION_LAST_1D")
                
               
                    
                ) ),column(width = 5, 
                           valueBoxOutput("value2",width = NULL),
                           box(
                  width = NULL
                  ,status = "primary"
                  ,solidHeader = FALSE 
                  ,collapsible = TRUE 
                  ,plotOutput("grid2",height = 320)
                ),box(width = NULL,height = 80,selectInput("var2", 
                                                           label = "Choose a variable to display",
                                                           choices = c("AVG_CALL_DURATION_LAST_1D", 
                                                                       "TOTAL_CALL_DURATION_LAST_1D",
                                                                       "MAX_CALL_DURATION_LAST_1D", 
                                                                       "MIN_CALL_DURATION_LAST_1D"),
                                                           selected = "AVG_CALL_DURATION_LAST_1D")
                      
                )
      
                           
                           
              )
      )),
      
      # Second tab content
      tabItem(tabName = "dashboard2",
              fluidRow( box(
                status = "primary"
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,plotOutput("grid3",height = 320)
              ))
              
      ),
      
      tabItem(tabName = "dashboard3",
              fluidRow( box(
                status = "primary"
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,plotOutput("grid4",height = 320)
              ))
              
      ),
      
      tabItem(tabName ="createUser",
          box(
            textInput("name", "Name", ""),
            textInput("pw", "Password",""),
            actionButton("addUser", "Add User", class="button-primary")
           ))
    )
  )
)
index<-0
get_new_data <- function(){
  #data <- fun2()
  #print(data)
  #print('---------------------------------------')
  data <- temp[index, ]
  index <<- index +1
  return(data)
}
values <<-NULL
values <<- get_new_data()
update_data <- function(){
  values <<- rbind(get_new_data(), values)
  values
}


server <- function(input, output, session) {
  valueBoxReacts <- reactiveValues()
  new_values <- reactive({data <- get_new_data()
                            data})
 
  
  shinyjs::hide(id = "add-user")
  shinyjs::hide(id = "Sidebar")
  
 
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  user_data <- reactive({credentials()$info})

  observe({
    if(credentials()$user_auth && credentials()$info$permissions == "admin") {
      shinyjs::show(id = "Sidebar")
      shinyjs::show(id = "add-user")
    } else {
     shinyjs::hide(id = "Sidebar")
      shinyjs::hide(id = "add-user")
    }
   
    
  })
  
  observeEvent(input$addUser, {
    if(input$name != '' && input$pw != '') {
      new_user <- data.frame(
        user = input$name,
        password = input$pw, 
        permissions = "user",
        name = input$name,
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(mydb, "users", new_user, append = TRUE)
      shinyalert("Success", "New User Added", type = "success")
    }
  })
  
  
  
  
  
  
  
  

  #output$var<- var

  output$value1 <- renderValueBox({
    valueBox(
      formatC(paste(valueBoxReacts$max_call,"s"), format="d", big.mark=',')
      ,"Longest call in seconds"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(valueBoxReacts$num_call, format="d", big.mark=',')
      ,"Total number of calls"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(valueBoxReacts$num_call, format="d", big.mark=',')
      ,"Total number of calls"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
    
  })
  
  #output$var<- var
  
  
  
  output$grid <- renderPlot({
    req(credentials()$user_auth)
    nvalues <- update_data()
    valueBoxReacts$max_call <- max(nvalues$MAX_CALL_DURATION_LAST_7D)
    valueBoxReacts$num_call <- nrow(nvalues)
    invalidateLater(1000, session)
    gg <-
      ggplot(nvalues, aes_string(x = "ID", y =input$var))
    gg <- gg + geom_point(col = "brown") + geom_line(col = "brown") + theme_bw() + labs(x="Time")
   
    gg
  })
  
  output$grid2 <- renderPlot({
    nvalues <- update_data()
    valueBoxReacts$max_call <- max(nvalues$MAX_CALL_DURATION_LAST_7D)
    valueBoxReacts$num_call <- nrow(nvalues)
    req(credentials()$user_auth)
    update_data()
    invalidateLater(1000, session)
    
      gg <- ggplot(data=nvalues, aes_string(x = input$var2,fill = "CODE")) + #zbog koristenja dataset.csv preimenovano iz CALLER_COUNTRY u CODE
        geom_histogram(bins = 10) + xlab(label = input$var2 )
      
      gg
          
  
  })
  
  
  output$grid3 <- renderPlot({
    req(credentials()$user_auth)
    nvalues <- update_data()
    invalidateLater(1000, session)
    
    gg <- ggplot(data=nvalues, aes_string(x = input$var2,fill = "CODE")) + #zbog koristenja dataset.csv preimenovano iz CALLER_COUNTRY u CODE
      geom_histogram(bins = 10) + xlab(label = input$var2 )
    
    gg
    
    
  })
  
  output$grid4 <- renderPlot({
    req(credentials()$user_auth)
    nvalues <- update_data()
    invalidateLater(1000, session)
    
    gg <- ggplot(data=nvalues, aes(x = CALL_DATE, y = mean(AVG_CALL_DURATION_LAST_1D))) + 
      geom_bar(stat = "identity", mapping = aes(fill = CALL_DATE)) + guides(fill=FALSE)
      #geom_bar(mapping = aes(x = CALL_DATE,fill = CALL_DATE)) + guides(fill=FALSE)
    
    gg
    
    
  })
  
  
  session$onSessionEnded(function() {
 #   DBI::dbDisconnect(mydb)
  #  print("disconnected")
  })
 
  
}

shinyApp(ui=ui2,server=server)
