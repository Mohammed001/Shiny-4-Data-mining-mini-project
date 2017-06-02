library(shiny)

# Define UI for dataset viewer application
ui <- fluidPage(
  
  # Application title
  titlePanel("Mining in Adult dataset.by Mohammed Sulaiman & Abdulkadir Khatib"),
  
  actionButton("classify", "Classify"),
  h3(""),
  sidebarLayout(
    
    
    sidebarPanel(
    
      h4("Poor people are much more than rich ones. put Age to 44 and Marital.status to Married-AF-spouse and gain to 4000 to identify a rich person ^_^"),
      h6(""),
      h4("Hit classify button to get the prediction"),
      numericInput("Age", "Age:", 22),
      
      selectInput("Workclass", "Workclass:", 
                  choices = c("Federal-gov", "Local-gov", "Never-worked", "Private"
                              ,"Self-emp-inc", "Self-emp-not-inc", "State-gov", "Without-pay")
      ),
      
      numericInput("Education.num", "how many years have you study:", 15),
      
      selectInput("Marital.status", "Marital.status:", 
                  choices = c("Divorced", "Married-AF-spouse", "Married-civ-spouse"
                              , "Married-spouse-absent", "Never-married", "Separated"
                              , "Widowed")
      ),
      
      selectInput("Occupation", "Occupation:", 
                  choices = c("Adm-clerical", "Armed-Forces", "Craft-repair", "Exec-managerial"
                              , "Farming-fishing", "Handlers-cleaners", "Machine-op-inspct", "Other-service"
                              , "Priv-house-serv", "Prof-specialty", "Protective-serv", "Sales"
                              , "Tech-support", "Transport-moving")
      ),
      
      selectInput("Relationship", "Relationship:", 
                  choices = c("Husband", "Not-in-family", "Other-relative", "Own-child"
                              ,"Unmarried", "Wife")
      ),
      
      selectInput("Race", "Race:", 
                  choices = c("Black", "White", "Other", "Amer-Indian-Eskimo", "Asian-Pac-Islander")
      ),
      
      numericInput("Capital.gain", "Capital.gain[0,99999]:", 0),
      
      numericInput("Capital.loss", "Capital.loss[0,4356]:", 0),
      
      numericInput("Hours.per.week", "how much time do you work per week:", 0),
      
      textInput("Native.country", "Native.country(ex. Canada, China, France, Germany, India):", "United-States")
      
    ),
    
    
    mainPanel(
      h3(textOutput("caption", container = span))    )
  ))

server <- function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$classify, {
    userTest <- as.data.frame(list(Age = input$Age,Workclass = input$Workclass
                                   ,Education.num = input$Education.num,Marital.status = input$Marital.status
                                   ,Occupation = input$Occupation, Relationship = input$Relationship
                                   ,Race = input$Race, Capital.gain = input$Capital.gain
                                   ,Capital.loss = input$Capital.loss, Hours.per.week = input$Hours.per.week
                                   ,Native.country = input$Native.country)
    )
    
    # testing
    load("myLogistic.Rdata")
    suppressWarnings(res<-ifelse(predict(myLogistic, newdata = userTest ,type="response")>=0.5,1,0))
    v$data <- "this person annual income is: "
    v$data <- paste(v$data, ifelse(res==0,"<=50K", ">50K"))
    
  })
  
  # chose renderPrint insted of renderText
  output$caption <- renderText({
    if (is.null(v$data)) return()
    v$data
  })
  
  
} # end of function


# Run the application 
shinyApp(ui = ui, server = server)

