#Loading Shiny Package
library(shiny)

#Creating Layouts
shinyUI(pageWithSidebar(
  headerPanel("Titanic Survival rate"),
  sidebarPanel(
    
    p("Fill the below details"),
    selectInput("c", label =h3("Crew/Passenger:"), list("1st Class Passenger" = "1st","2nd Class Passenger" = "2nd", "3rd Class Passenger" = "3rd", "Crew Member" = "Crew")),
    radioButtons("s", label = h3("Sex:"),
                 choices = list("Male" = "Male", "Female" = "Female"), 
                 selected = "Female"),
    radioButtons("a", label = h3("Age:"),
                 choices = list("Child" = "Child", "Adult" = "Adult"),
                 selected = "Adult")),
  mainPanel(
    h3("Survival Probability:"),
    h4(textOutput('prob')),
    p("This is estimated probability based on a logistic regression model.")
  )))
