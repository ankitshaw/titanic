#Loading Necessary packages
library(shiny)
library(datasets)
#Loading the Data
data(Titanic)
tit <- as.data.frame(Titanic)

#Constructing the model
tit_glm <- glm(Survived ~ Class + Sex + Age, binomial, tit, tit$Freq)

#Making Prediction
pred_tit <- function(class, sex, age ){
  inputdata <- c(class, sex, age)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("Class", "Sex", "Age")
  surv_prob <- predict(tit_glm,pred_data , type = "response" )
  return(surv_prob)
}

#Finding the probabality for input obtained
shinyServer(
  function(input, output) {
    output$prob <- renderText({pred_tit(input$c,input$s, input$a)})
  })