# load libraries
library(shiny)
library(readr)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)

# read data
bike <- read.csv("~/Documents/NCSU/ST558/repos/Project-3/SeoulBikeData.csv", header = FALSE)
colnames(bike) <- c("Date","Rented_Bike_Count","Hour","Temperature","Humidity","Wind_speed","Visibility","Dew_point_temperature","Solar_Radiation","Rainfall","Snowfall","Seasons","Holiday","Functioning_Day")
# remove the first row
bike<-bike[-1,]
# drop date
#bike <- subset(bike, select = -c(Date))

# create binary variable for response variable
# 1 if the number of bikes rented is greater than or equal to 700 and 0 otherwise
bike$binRent <- ifelse(bike$Rented_Bike_Count >= 700, 1, 0)

# Define UI for application
ui <- fluidPage(
    withMathJax(),
    tabsetPanel(
        
        # create about page
        tabPanel("About",
                 fuild=TRUE,
                 h2("About"),
                 mainPanel(
                     h3("Purpose of the app"),
                     tags$p("The purpose of this app is to explore the SeoulBikeData.csv data, fit and predict with three supervised learning models."),
                     h3("The data and its source"),
                     tags$p("The SeoulBikeData.csv is from the UCI Machine Learning Repository. This data set is about bike sharing rentals and contains weather information (Temperature, Humidity, Windspeed, Visibility, Dewpoint, Solar radiation, Snowfall, Rainfall), the number of bikes rented per hour, date information, and binRent variable ( 1 if the number of bikes rented is greater than or equal to 700 and 0 otherwise )."),
                     tags$p("You can learn more about the data ", a(href = "https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand", "here.")),
                     h3("Purpose of each tab"),
                     tags$ul(
                         tags$li(tags$b("About page"), " - The purpose of this page is to discribe some general information about the app and data which includes describing the purpose of the app, discussing the data and its source, telling the user the purpose of each tab, posting a picture related to the data."),
                         tags$li(tags$b("Data Exploration page"), " - This page allows the user to create numerical and graphical summaries, change the type of plot shown and type of summary reported, change the variables and filter the rows to change the data in the plots/summaries."),
                         tags$li(tags$b("Modeling page"), " - In this page, we will fit three supervised learning models which contains three tabs. The first tab is modeling info tab which explains these three modeling approaches, the benefits of each, and the drawbacks of each. The second tab is model fitting tab which. The third tab is prediction tab which gives the user a way to use one of the models for prediction."),
                         tags$li(tags$b("Data page"), " - In this page, we can see the whole data set, subset data set, and save the subsetted data as a file.")
                     ),
                     h3("Picture related"),
                     img(src="SeoulBike.png")
                 )
        ),
        
        
        # Create data exploration page
        
        tabPanel(title = "Data Exploration",
                 fuild=TRUE,
                 h2("Data Exploration"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         # Numerical Summaries
                         h4("Numerical Summaries"),
                         selectInput(
                             "Select_Variables",
                             "Select_Variables",
                             choices = c("Rented Bike Count"="Rented_Bike_Count",
                                         "BinRent vs. Season"="BinRentVsSeason"
                             ),
                             selected = "Rented_Bike_Count"
                         ),
                         
                         radioButtons(inputId = "SumOption",
                                      label = "Type of Summary",
                                      choiceNames = c("Common Summary Statistics","IQR" ),
                                      choiceValues = c("common","IQR")
                         ),
                         br(),
                         br(),
                         # Graphical Summaries
                         h4("Graphical Summaries"),
                         radioButtons("PlotType", 
                                      "Type of Plot",
                                      choiceNames = c("Histogram", "Scatter Plot"),
                                      choiceValues = c("Histogram", "scatter" ))
                     ),
                     
                     mainPanel(
                         h3("Numerical Summaries"),
                         DTOutput("Numerical_Summaries"),
                         
                         h3("Graphical Summaries"),
                         plotOutput('Graphical_Summaries')
                         
                     ),
                 )
        ),
        
        
        # Create modeling page
        
        tabPanel(title = "Modeling",
                 fuild=TRUE,
                 h2("Modeling"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4("Modeling Info"),
                         radioButtons("Select_model", 
                                      "Select model for Description",
                                      choiceNames = c("Multiple Linear Regression","Regression Tree","Random Forest"),
                                      choiceValues = c("MLR", "Rtree","RF")),
                         br(),
                         
                         #Model Fitting
                         h4("Model Fitting"),
                         
                         #Train/Test Data Split,
                         radioButtons("proportion",
                                      "Choose the proportion of training data",
                                      choiceNames = c("0.75","0.8"),
                                      choiceValues = c("0.75", "0.8")),
                         
                         checkboxGroupInput(inputId = "modelColumns", 
                                            label = "Select variable(s) of the model",
                                            choices = c("Rented_Bike_Count"="Rented_Bike_Count", "BinRent"="binRent"),
                                            selected = c("Rented_Bike_Count","binRent")),
                         
                         #set a button and fit all three models on the training data
                         actionButton("buttonRunModels", "Run Models"),
                         br(),
                         
                         #Predictions
                         h4("Predictions"),
                         radioButtons("rdoPredModel", 
                                      "Select model for Prediction", 
                                      choiceNames = c("Multiple Linear Regression","Regression Tree","Random Forest"),
                                      choiceValues = c("MLR", "Rtree","RF")),
                         
                         numericInput("predcount", "Rented_Bike_Count Prediction",100000, min = 100000, 
                                      max = 200000,step=10000),
                         
                         numericInput("predBinRent", "BinRent Prediction",100, min = 10, 
                                      max = 500,step=10)
                     ),
                     
                     mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Modeling Info",
                                              h3("Modeling Descriptions"),
                                              uiOutput("modelInfo"),
                                              plotOutput("plot")
                                     ),
                                     
                                     tabPanel("Model Fitting",
                                              h3("Model Fitting"),
                                              DTOutput('tblTestSplit'),
                                              
                                              h5("Multiple Linear Regression"),
                                              textOutput("modelFitMLR"),
                                              verbatimTextOutput(outputId = "model_MLR"),
                                              
                                              h5("Regression Tree"),
                                              textOutput("modelFitRT"),
                                              verbatimTextOutput(outputId = "model_RT"),
                                              plotOutput("modelPlotRTree"),
                                              
                                              h5("Random Forest"),
                                              textOutput("modelFitRF"),
                                              verbatimTextOutput(outputId = "model_RF")
                                     ),
                                     
                                     tabPanel("Prediction",
                                              h3("Model Predictions"),
                                              verbatimTextOutput(outputId = "modelPred")
                                     )
                         )
                     )
                 )
        ),
        
        
        # Create data page
        tabPanel(title = "Data", 
                 fuild=TRUE,
                 h2("Data"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         h4("Subset this data set"),
                         #Subset rows 
                         sliderInput("numberRows",
                                     label = "Subset the Rows",
                                     min=1,
                                     max=8761,
                                     value=4000),
                         
                         #Subset columns
                         checkboxGroupInput(inputId = "columns",
                                            label = "Select the column(s)",
                                            choices = c("Date"="Date","Rented Bike Count"="Rented_Bike_Count","Hour"="Hour","Temperature(°C)"="Temperature","Humidity(%)"="Humidity","Wind speed (m/s)"="Wind_speed","Visibility (10m)"="Visibility","Dew point temperature(°C)"="Dew_point_temperature","Solar Radiation (MJ/m2)"="Solar_Radiation","Rainfall(mm)"="Rainfall","Snowfall (cm)"="Snowfall","Seasons"="Seasons","Holiday"="Holiday","Functioning Day"="Functioning_Day","binRent"="binRent"),
                                            selected = c("Date","Rented_Bike_Count","Hour","Temperature","Humidity","Wind_speed","Visibility","Dew_point_temperature","Solar_Radiation","Rainfall","Snowfall","Seasons","Holiday","Functioning_Day","binRent")
                         ),
                         #Save the data as a file
                         h4("Download the Data Set"),
                         #download
                         downloadButton("downloadset", "Download")
                     ),
                     
                     mainPanel(
                         DTOutput('dataset')
                     )
                 )
        )
    )
)



# Define server for application

server <- function(input, output, session) {
    
    #Data Exploration
    
    #Numerical Summaries
    
    output$Numerical_Summaries <- DT::renderDataTable({
        
        if (input$Select_Variables=="Rented_Bike_Count"){
            if(input$SumOption=="common"){
                describe(bike$Rented_Bike_Count, fast=TRUE)
            }else if(input$SumOption=="IQR"){
                describe(bike$Rented_Bike_Count, IQR=TRUE)
            }
            
            else if (input$Select_Variables=="BinRent_vs._Season"){
                if(input$SumOption=="common"){
                    describe(bike$Rented_Bike_Count, fast=TRUE)
                }else if(input$SumOption=="IQR"){
                    describe(bike$Rented_Bike_Count, IQR=TRUE)
                }
            }
        }
        
    })
    
    
    #Graphical Summaries
    
    # output$Graphical_Summaries <- renderPlot({
    #     if(input$PlotType == "Histogram"){
    #         histVar<-input$HistogramVariable
    #         if (histVar=="Rented_Bike_Count "){
    #             xLabel="Rented_Bike_Count"
    #         }else if (histVar=="BinRent") {
    #             xLabel="BinRent"
    #         }
    #         
    #         x<-bike[[histVar]]
    #         ggplot(data = bike, aes(x = histVar))
    #         + geom_histogram() +
    #             labs(title = paste0("Histogram of ",xLabel), x = xLabel, y = "Count")
    #         
    #     }else {
    #         ###Scatter Plot
    #         # numerical variables from data set only
    #         bike1 <- subset(bike, select = -c(Seasons, Holiday, `Functioning_Day`, binRent))
    #         # scatterplot matrix of all numeric variables
    #         ggpairs(bike1)
    #     }
    # })
    
    
    # Modeling page
    
    # Model Info tab
    output$modelInfo <- renderUI({
        if(input$Select_model == "MLR"){
            withMathJax(helpText("Multiple linear regression refers to a statistical technique that is used to predict the outcome of a variable based on the value of two or more variables. The variable that we want to predict is known as the dependent variable, while the variables we use to predict the value of the dependent variable are known as independent or explanatory variables. There are some advantages of multiple linear regression model, for example: easy and simple implementation; space complex solution, and fast training. There are some disadvantages of multiple linear regression model, for example: applicable only if the solution is linear, however, in many real life scenarios, it may not be the case; algorithm assumes the input residuals (error) to be normal distributed, but may not be satisfied always.
            Multiple Linear Regression formula as following:
    $$Yi = \\beta_0 + \\beta_1x_{1i} + \\beta_2x_{2i} + \\beta_3x_{1i}x_{2i} + E_{i}$$"))
        }
        else if(input$Select_model == "Rtree"){
            withMathJax(helpText("Regression tree is basically a decision tree that is used for the task of regression which can be used to predict continuous valued outputs instead of discrete outputs. There are some advantages of regression tree model, for example: it does not require normalization and scaling of data; missing values in the data also do not affect the process. There are some disadvantages of regression tree model, for example: a small change in the data can cause a large change in the structure of the decision tree causing instability; it goes far more complex compared to other algorithms; it often involves higher time to train the model."))
        }
        else {
            withMathJax(helpText("Random Forest Model is a supervised machine learning algorithm that is used widely in classification and regression problems. It builds decision trees on different samples and takes their majority vote for classification and average in case of regression. There are some advantages of Random Forest, for example: random forest works well with both categorical and continuous variables; random forest can be used to solve both classification as well as regression problems; random forest can automatically handle missing values; no feature scaling required. There are some disadvantages of random forest, for example: complexity --- random forest creates a lot of trees (unlike only one tree in case of decision tree) and combines their outputs; longer Training Period --- random forest require much more time to train as compared to decision trees as it generates a lot of trees (instead of one tree in case of decision tree) and makes decision on the majority of votes." ))
        }
    })
    
    # # Model Fitting tab
    # propor<-output$proportion
    # # Data Split
    # set.seed(111) 
    # # Split the data into a training and test set
    # train <- sample(1:nrow(bike), size = nrow(bike)*propor) 
    # test <- setdiff(1:nrow(bike), train)
    # # trainiing and testing subsets
    # bikeTrain <- bike[train, ]
    # bikeTest <- bike[test, ]
    
    ###Run  models button
    #observeEvent(input$buttonRunModels, {
    #    splitDataList<-splitData()
    ###Run Linear Regression Model
    #    mlrFit <- train(modelVars(),
    #                    data = splitDataList$stockDataTrain,
    #                    method="lm",
    #                    trControl=trainControl(method="cv",number=5))
    
    #Display the summary statistics
    #    output$modelSummaryMLR <- renderPrint(
    #        summary(mlrFit)
    #    )
    #}
    
    #Data Page
    getDataAll <- reactive({
        #For complete dataset
        newData <- bike[1:input$numberRows,input$columns,drop=FALSE]
    })
    output$dataset = DT::renderDataTable(
        getDataAll(),
        #bike,
        options=list(
            searching = TRUE,
            autoWidth = TRUE,
            rownames = FALSE,
            scroller = TRUE,
            scrollX = TRUE,
            scrollY = "500px"
            #paging = FALSE
        )
    )
    
    output$downloadset <- downloadHandler(
        filename = "SubBikeData.csv",
        content = function(file) {
            write.csv(getDataAll(), file, row.names = FALSE)
        }
    )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

