library(shiny)
library(shinythemes)
library(randomForest)
library(dplyr)
ui<-fluidPage(theme = shinytheme("sandstone"),
              navbarPage(
                "Stroke Predictor Widget",
                tabPanel("Home",
                         h1('INTRODUCTION'),
                         img(src = "stroke.jpg", height = 300, width = 250, style="display: block; margin-left: auto; margin-right: auto;padding-bottom: 5%;"),
                         p('Stroke or sometimes known as brain attack happens when when blood supply does not reach to the brain due to blockage or the blood vessels in the brain bursts.
A stroke can cause lasting brain damage, long-term disability, or even death because it is an important but complex organ as it controls most of our body functions.
There are two types of stroke. Ischemic stroke is where the blood vessels are block by blood clot or any particles and hemorrhagic stroke in which artery in the brain ruptured.
According to the World Health Organization (WHO) stroke is the 2nd leading cause of death globally, responsible for approximately 11% of total deaths. Early detection of stroke using prediction model can allow medical officers to provide immediate or efficient treatment that may minimize the long-term effects of a stroke and even prevent death.
A lot of factors contribute to the likelihood of someone getting a stroke such as age, gender, diseases that they have and smoking status.'),
                         h1('OBJECTIVE'),
                         p('To predict the probability of stroke on individuals based on their age, gender, hypertension, heart disease, marital status, occupation, residence type, glucose level, BMI and smoking status.')),
                tabPanel("Stroke Predictor",
                         sidebarPanel(
                           h1('Feature Selection'),
                           h1('==========='),
                           h4 (" How To Use The App? "),
                           h6 (" 1) Please Enter Your Name "),
                           h6 (" 2) Please select relevant options"),
                           h6 ("    Note: All input fields are required for prediction."),
                           h6 (" 3) Click Predict Button"),
                           h1('==========='),
                           textInput("name","Please enter your name:"),
                           selectInput("gender","Please select your gender:",choices=c("Select an option"="", "Male"=0,"Female"=1)),
                           selectInput("age","Select your age group:",choices = c("Select an option"="","0-9"=1,"10-19"=2,"20-29"=3,"30-39"=4,"40-49"=5,"50-59"=6,"60-69"=7,"70-79"=8, ">80"=9)),
                           selectInput("work","Select your work type:",choices = c("Select an option"="","Children"=1,"Government Job"=2,"Never worked"=3,"Private"=4,"Self Employed"=5)),
                           selectInput("residence","Select your residence type",choices=c("Select an option"="","Urban"=2,"Rural"=1)),
                           selectInput("status","Are you married?",choices=c("Select an option"="","Yes"=1,"No"=0)),
                           selectInput("glucose","How can you define your glucose level?",choices = c("Select an option"="","Low"=1,"Normal"=2,"Diabetes"=3)),
                           selectInput("bmi","Select your BMI:",choices = c("Select an option"="","Underweight(BMI < 18.5)"=1, "Healthy(18.5 <= BMI < 25.0)"=2,  "Overweight(25.0 <= BMI < 30.0)"=3, "Obese(30.0 <= BMI < 40.0)"=4, "Very Obese(40.0 <= BMI <+Inf)"=5)),
                           selectInput("hypertension","Have you ever had Hyper Tension?",choices=c("Select an option"="","Yes"=1,"No"=0)),
                           selectInput("heartdisease","Have you ever had Heart Disease?",choices=c("Select an option"="","Yes"=1,"No"=0)),
                           selectInput("smoking","Have you ever smoke before?",choices = c("Select an option"="","formerly smokes"=1,"never smoke"=2,"smokes"=3,"Unknown"=4)),
                           actionButton("go","Predict")
                         ), #sidebarpanel
                         mainPanel(
                           h3('STROKE PREDICTION'),
                           h6('_________________________________________'),
                           h4('Your Name is : '),
                           verbatimTextOutput("name"),
                           h4('Your likelyhood of getting stroke is : '),
                           textOutput("Pred")
                           
                         )#mainpanel
                )#tabPanel
              )#navbarpage
)#fluidpage


server<- function(input, output) {
  output$name <- renderText({(input$name)})
  observeEvent(input$go, {
    
    df<-data.frame(
      input$gender,
      input$age,
      input$hypertension,
      input$heartdisease,
      input$status,
      input$work,
      input$residence,
      input$glucose,
      input$bmi,
      input$smoking
    )
    
    colnames(df) <- c('gender',	'age',	'hypertension',	'heart_disease',	'ever_married',	'work_type',	'Residence_type',	'avg_glucose_level',	'bmi',	'smoking_status')
    df <- mutate_all(df, function(x) as.numeric((x)))
    classifier_RF<-readRDS("classifier_RF.rda")
    y_pred = reactive(predict(classifier_RF, newdata = df))
    if (as.numeric(y_pred())==1){
      result <- "Low"
    }else{
      result <- "high"
    }
    output$Pred<-renderText(result)
  })
  
  
}


shinyApp(ui = ui, server = server)