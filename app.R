#install.packages("shinyjs")

library(shiny)
library(magrittr)
library(jsonlite)
library(ggplot2)
library(shinyauthr)
library(shinyjs)

user_base <- data.frame(
  user = c("user", "admin"),
  password = c("user", "admin"), 
  permissions = c("user", "admin"),
  name = c("User", "Admin"),
  stringsAsFactors = FALSE
)


temp <- read.csv(file="dataset.csv", header=TRUE, sep=",")
temp1 <- subset(temp, select = -X)

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
     data <- temp1[index, ]
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
      ggplot(values, aes_string(x = x_axis, y =y_axis))
    gg <- gg + geom_point()  + geom_text(aes(label=CALLEE), hjust=0, vjust=0)
   
    gg
  })
 
  
}

shinyApp(ui=ui,server=server)
