#install.packages("shinyjs")

library(shiny)
library(magrittr)
library(jsonlite)
library(ggplot2)
library(shinyauthr)
library(shinyjs)

library(lubridate)

# Load libraries and functions needed to create SQLite databases.
#dplyr::db
library(RSQLite)
library(DBI)
#library(RSQLite.extfuns)


mydb <- dbConnect(RSQLite::SQLite(), "users.sqlite")

user_base <- data.frame(
  user = c("user", "admin"),
  password = c("user", "admin"), 
  permissions = c("user", "admin"),
  name = c("User", "Admin"),
  stringsAsFactors = FALSE
)
if(!DBI::dbExistsTable(mydb, "users")) {
  DBI::dbWriteTable(mydb, "users", user_base)
}
dbListTables(mydb)
#user_base_db <- saveSQLite(user_base, "user_base")


res <- dbSendQuery(mydb, "SELECT * FROM users")
#print(res)
#print(dbFetch(res))
user_base<- data.frame(dbFetch(res))
#print(dbExistsTable(mydb, "users"))

temp <- '{"ID": 1, "CALLER": 0.0, "CALLEE": 38512773097.0, "CALL_DATE": "2017-01-03",
  "AVG_CALL_DURATION_LAST_1D": 46.0, "TOTAL_CALL_DURATION_LAST_1D": 146.0, "MAX_CALL_DURATION_LAST_1D": 196.0, "MIN_CALL_DURATION_LAST_1D": 16.0}'
temp <- fromJSON(temp)
print(temp)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  shinyauthr::loginUI(id = "login"),
#  titlePanel("Calls"),
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
server <- function(input, output, session) {
  
  index<-0
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  #shinyjs::hide(id = "Sidebar")
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
 # print(user_data)
  
  observe({
    if(credentials()$user_auth && credentials()$info$permissions == "admin") {
      shinyjs::show(id = "Sidebar")
    } else {
      shinyjs::hide(id = "Sidebar")
    }
  })
  
  get_new_data <- function(){
     data <- temp %>% data.frame
     data$CALLER <- index +1
     index <<- index +1
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
      ggplot(values[1:6,], aes_string(x = x_axis, y =y_axis))
    gg <- gg + geom_point()  + geom_text(aes(label=CALLEE), hjust=0, vjust=0)
   
    gg
  })
 
  
}

shinyApp(ui=ui,server=server)
