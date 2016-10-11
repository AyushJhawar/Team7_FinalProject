#Modeling Starts
library(shiny)
library(caret)
#Loading the data
install.packages(caret)
#Reading the data
winedata <- read.csv("D:/Purdue Classes/R for Analytics/Team7_FinalProject/wine.csv", header = TRUE)
str(winedata)
names(winedata)
class(winedata)
winedata
# converting class into factor variable
winedata$Class <- as.factor(winedata$Class)
class(winedata$Class)
str(winedata)
library(caret)
inTrain <- createDataPartition(winedata$Class,
                               p=.70,
                               list=F)
train <- winedata[inTrain,]
test <- winedata[-inTrain,]
nrow(train)
nrow(test)

ldafit <- train(Class ~.,
                data=train,
                method="lda")

# make predictions
ldaClasses <- predict(ldafit, newdata=test)
summary(ldaClasses)

# evaluate model
a<- confusionMatrix(data=ldaClasses, test$Class)
a
#Modeling ends

#UI Starts
ui <- fluidPage(
  headerPanel("AADR Wine Classification Portal"),
  sidebarLayout(
    sidebarPanel(
      img(src = "wine.jpg", height = 50, width = 100),
      fileInput('file1', 'Choose input CSV File for classification',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
 
    ),
    mainPanel(
     
      tabsetPanel(
        tabPanel("About Us",verbatimTextOutput("about")),
        tabPanel("Classification",tableOutput("table1")),
        tabPanel("Segmentation",tableOutput("ms"))
        
      )
    )
  )
)

#UI Ends

#Server Starts

server <- function(input,output){

  dataInput = reactive({
    
    in.file = input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    read.csv(in.file$datapath)
  })
  
  clusters <- reactive({
    
    df <- dataInput()
    
    WineClass <- predict(ldafit, newdata = df)
    dframe <- as.data.frame(WineClass)
    
  })
    
  output$about <- renderText({
    
                          HTML(paste0("AADR wines aims to classify wines into categories based on their chemical content and identify customer segments that buy those categories of wines. Based on this information, future wines can be classified into these categories which can help in procurement activities as we will have a clearer idea of which wines our customers prefer. It will also help AADR to send targeted emails with better discount deals via email to customers. This will result in more emails being opened and fetch higher revenues. 
The chemical content data of wines is used to classify the wines in 3 categories using LDA method. The predictive model is then tested on the test data. A shiny app is made with this predictive model running in the background. This app requires the user to upload the chemical data and displays the classification as output."))
    
                            })
  
  output$ms <- renderText({

                           HTML(paste0("<br/><b>","Target Market according to class","</b><br/><br/>",

                          "1: Preffered by the states of Indiana and North Carolina","<br/>",
                          "2: Preffered by the states of Colorado and Texas","<br/>",
                          "3: Preffered by the states of Ohio and Nebraska"))
                                
                        })
  
  output$table1 <- renderTable({
    toprint = clusters()
  })
  
}
# Server Ends
#Application Starts
shinyApp(ui=ui, server=server)
#Application Ends
