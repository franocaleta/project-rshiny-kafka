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
temp1 <- subset(temp, select = -X)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  useShinyalert(),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  div(class = "pull-right", id="add-user",
      textInput("name", "Name", ""),
      textInput("pw", "Password",""),
      actionButton("addUser", "Add User", class="button-primary")),
  
  shinyauthr::loginUI(id = "login"),
 # titlePanel("Calls"),
  sidebarLayout(
    div(id="Sidebar", sidebarPanel(
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("AVG_CALL_DURATION_LAST_1D", 
                              "TOTAL_CALL_DURATION_LAST_1D",
                              "MAX_CALL_DURATION_LAST_1D", 
                              "MIN_CALL_DURATION_LAST_1D"),
                  selected = "AVG_CALL_DURATION_LAST_1D")
      
    )),
    
    mainPanel(
      plotOutput("grid")
    )
  )
)
ui2 <-dashboardPage(
  dashboardHeader(title ="title"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
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
                plotOutput("grid"),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of data frames displayed:", 10, 20, 15)
                ),
                box(selectInput("var", 
                                label = "Choose a variable to display",
                                choices = c("AVG_CALL_DURATION_LAST_1D", 
                                            "TOTAL_CALL_DURATION_LAST_1D",
                                            "MAX_CALL_DURATION_LAST_1D", 
                                            "MIN_CALL_DURATION_LAST_1D"),
                                selected = "AVG_CALL_DURATION_LAST_1D")
                    
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
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
server <- function(input, output, session) {
  shinyjs::hide(id = "add-user")
  shinyjs::hide(id = "Sidebar")
  index<-0
 
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
  
  get_new_data <- function(){
     data <- fun2()
     print(data)
     print('---------------------------------------')
     return(data)
  }
  
  values <<- get_new_data()
  
  
  update_data <- function(){
    values <<- rbind(get_new_data(), values)
  }
  

  #output$var<- var
  output$grid <- renderPlot({
    req(credentials()$user_auth)
    y_axis <- input$var
    x_axis <- 'CALLER'
    invalidateLater(1000, session)
    update_data()
    gg <-
      ggplot(values[1:input$slider,], aes_string(x = "ID", y =input$var))
    gg <- gg + geom_point(col = "brown") + geom_line(col = "brown") + theme_bw() + labs(x="Time")
   
    gg
  })
  
  
  session$onSessionEnded(function() {
 #   DBI::dbDisconnect(mydb)
  #  print("disconnected")
  })
 
  
}

shinyApp(ui=ui2,server=server)
