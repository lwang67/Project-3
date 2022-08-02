# load libraries
library(shiny)
library(readr)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(tree)
library(GGally)

# read data
bike <- read.csv("~/Documents/NCSU/ST558/repos/Project-3/SeoulBikeData.csv", header = FALSE)

#Rename of columns
colnames(bike) <- c("Date","Rented_Bike_Count","Hour","Temperature","Humidity","Wind_speed","Visibility","Dew_point_temperature","Solar_Radiation","Rainfall","Snowfall","Seasons","Holiday","Functioning_Day")

# remove the first row
bike<-bike[-1,]

#Convert the data type into numeric
bike <- transform(bike,Rented_Bike_Count = as.numeric(Rented_Bike_Count),
                  Hour = as.numeric(Hour),
                  Temperature = as.numeric(Temperature),
                  Humidity = as.numeric(Humidity),
                  Wind_speed = as.numeric(Wind_speed),
                  Visibility = as.numeric(Visibility),
                  Dew_point_temperature = as.numeric(Dew_point_temperature),
                  Solar_Radiation = as.numeric(Solar_Radiation),
                  Rainfall = as.numeric(Rainfall),
                  Snowfall = as.numeric(Snowfall))


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
                             "Select Variables",
                             choices = c("Rented Bike Count"="Rented_Bike_Count","Temperature(°C)"="Temperature","Humidity(%)"="Humidity"
                             ),
                             selected = "Rented_Bike_Count"
                         ),
                         
                         #Filter the Rows
                         radioButtons(inputId = "rowoption",
                                      label = "Filter the Rows",
                                      choiceNames = c("Seasons_Spring","Seasons_Summer" ),
                                      choiceValues = c("Spring","Summer")
                         ),
                         
                         #Type of Summary
                         radioButtons(inputId = "SumOption",
                                      label = "Type of Summary",
                                      choiceNames = c("Measure Central Tendency","Measure Dispersion" ),
                                      choiceValues = c("common","IQR")
                         ),
                         
                         
                        
                         br(),
                         br(),
                         # Graphical Summaries
                         h4("Graphical Summaries"),
                         #Type of Plot
                         radioButtons("PlotType", 
                                      "Type of Plot",
                                      choiceNames = c("Histogram of Rented Bike Count", "Scatterplot Matrix of All Numeric Variables"),
                                      choiceValues = c("Histogram", "scatter" ))
                     ),
                     
                     mainPanel(
                         h3("Numerical Summaries"),
                         DTOutput("Numerical_Summaries"),
                         br(),
                         br(),
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
                         #Modeling Info
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
                         
                         # Select variable(s) of the model
                         checkboxGroupInput(inputId = "modelColumns", 
                                            label = "Select variable(s) of the model",
                                            choices = c("Hour"="Hour","Temperature(°C)"="Temperature","Humidity(%)"="Humidity","Wind speed (m/s)"="Wind_speed","Visibility (10m)"="Visibility","Dew point temperature(°C)"="Dew_point_temperature","Solar Radiation (MJ/m2)"="Solar_Radiation","Rainfall(mm)"="Rainfall","Snowfall (cm)"="Snowfall"),
                                            selected = c("Hour","Temperature","Humidity","Wind_speed","Visibility","Dew_point_temperature","Solar_Radiation","Rainfall","Snowfall")),
                         
                         
                         #set a button and fit all three models on the training data
                         actionButton("buttonRunModels", "Run Models"),
                         br(),
                         br(),
                         
                         #Predictions
                         h4("Predictions"),
                         radioButtons("rdoPredModel", 
                                      "Select model for Prediction", 
                                      choiceNames = c("Multiple Linear Regression","Regression Tree","Random Forest"),
                                      choiceValues = c("MLR", "Rtree","RF")),
                         
                         
                         numericInput("Hour", "Hour",10, min = 0, max = 23,step=1),
                         numericInput("Temperature", "Temperature(°C)",0, min = -17.8, max = 39.4,step=0.1),
                         numericInput("Humidity", "Humidity(%)",50, min = 0, max = 98,step=1),
                         numericInput("Wind_speed", "Wind speed(m/s)",5, min = 0, max = 7.4,step=0.1),
                         numericInput("Visibility", "Visibility(10m)",100, min = 27, max = 2000,step=10),
                         numericInput("Dew_point_temperature", "Dew point temperature(°C)",10, min = -30.6, max = 27.2,step=0.1),
                         numericInput("Solar_Radiation", "Solar Radiation(MJ/m2)",1, min = 0, max = 3.52,step=0.1),
                         numericInput("Rainfall", "Rainfall(mm)",25, min = 0, max = 35,step=1),
                         numericInput("Snowfall", "Snowfall(cm)",8, min = 0, max = 8.8,step=0.1)
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
    
    output$Numerical_Summaries <- DT::renderDataTable({
        if (input$Select_Variables=="Rented_Bike_Count"){
            if(input$SumOption=="common"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Mean = mean(Rented_Bike_Count), Median = median(Rented_Bike_Count))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Mean = mean(Rented_Bike_Count), Median = median(Rented_Bike_Count))
                }

            }else if(input$SumOption=="IQR"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Standard_Deviation = sd(Rented_Bike_Count))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Standard_Deviation = sd(Rented_Bike_Count))
                }
                
            }
            
        }else if (input$Select_Variables=="Temperature"){
            if(input$SumOption=="common"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Mean = mean(Temperature), Median = median(Temperature))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Mean = mean(Temperature), Median = median(Temperature))
                }
                
            }else if(input$SumOption=="IQR"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Standard_Deviation = sd(Temperature))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Standard_Deviation = sd(Temperature))
                }
        }
        }else{
            if(input$SumOption=="common"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Mean = mean(Humidity), Median = median(Humidity))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Mean = mean(Humidity), Median = median(Humidity))
                }
                
            }else if(input$SumOption=="IQR"){
                if(input$rowoption=="Spring"){
                    bike %>% filter(bike$Seasons=="Spring") %>%
                        summarise(Standard_Deviation = sd(Humidity))
                }else{
                    bike %>% filter(bike$Seasons=="Summer") %>%
                        summarise(Standard_Deviation = sd(Humidity))
                }
            }
        }
            
            
        
    })
    
    #Select_Variables
    #Graphical Summaries
    
    output$Graphical_Summaries <- renderPlot({
        if(input$PlotType == "Histogram"){

            ggplot(data = bike, aes(x = bike$Rented_Bike_Count)) +
             geom_histogram(color="black", fill="white") +
                labs(title = "Histogram of Rented Bike Count", x = "Rented Bike Count", y= "Count")+
                theme(plot.title = element_text(size=22))
        }else {
            ###Scatter Plot
            # numerical variables from data set only
            bike1 <- subset(bike, select = -c(Date,Seasons, Holiday, Functioning_Day, binRent))
            
            # scatterplot matrix of all numeric variables
            ggpairs(bike1)+
                labs(title="Scatterplot Matrix of All Numeric Variables")+
                theme(plot.title = element_text(size=22))
        }
    })
    
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
    
    # Model Fitting tab
    observeEvent(input$buttonRunModels, {
        propor<-as.numeric(input$proportion)
        # Data Split
        set.seed(111)
        bike1 <- subset(bike, select = -c(Date,Seasons, Holiday, Functioning_Day, binRent))
        # Split the data into a training and test set
        train <- sample(1:nrow(bike1), size = nrow(bike1)*propor)
        test <- setdiff(1:nrow(bike1), train)
        # # training and testing subsets
        bikeTrain <- bike1[train, ]
        bikeTest <- bike1[test, ]
        column_list<-c(input$modelColumns, "Rented_Bike_Count")
        
        subbikeTest <- bikeTest[,column_list,drop=FALSE]
        subbikeTrain <- bikeTrain[,column_list,drop=FALSE]
        
        
        #multiple linear regression
        mlrFit <- train(Rented_Bike_Count ~ ., 
                        data = subbikeTrain,
                      method = "lm",
                      # standardize the variables (center and scale each observation)
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
        output$model_MLR <- renderPrint(
            summary(mlrFit)
        )
        
       
        #Compare random forest model on Test Set
        p_LMNew <- predict(mlrFit, newdata = subbikeTest)
        # get RMSE's for testing set for multiple linear regression model
        rmse<-postResample(p_LMNew, obs = bike$Rented_Bike_Count)
        output_mlr<-paste0("Comparing on test set, the RMSE of the multiple linear regression model is: ",rmse[[1]])
        output$modelFitMLR <- renderText(
            output_mlr
        )
        
        
        #regression tree
        treeFit <- tree(Rented_Bike_Count ~ ., data = subbikeTrain)
        
        output$modelPlotRTree <- renderPlot({
            plot(treeFit);text(treeFit)
        })
        
        #Compare regression tree Model on Test Set
        p_rtNew <- predict(treeFit, newdata = subbikeTest)
        # get RMSE's for testing set for regression tree model
        rmse1<-postResample(p_rtNew, obs = bike$Rented_Bike_Count)
        output$model_RT <- renderPrint(
            rmse1
        )
        
        output_RT<-paste0("Comparing on test set, the RMSE of the regression tree model is: ",rmse1[[1]])
        output$modelFitRT <- renderText(
            output_RT
        )
        
        
        #random forest model.
        randomForestFit <- train(Rented_Bike_Count ~ ., 
                                 data = subbikeTrain,
                                 method="rf",
                                 preProcess=c("center","scale"),
                                 trControl=trainControl(method="repeatedcv",number=3,repeats=1),
                                 tuneGrid=data.frame(mtry=1:3))
        
        
        varImpOutput<-caret::varImp(randomForestFit, scale = FALSE)
        output$model_RF <- renderPrint(
            #Variable Importance
            varImpOutput
        )
        
        #Compare random forest model on Test Set
        p_rfNew <- predict(randomForestFit, newdata = subbikeTest)
        # get RMSE's for testing set for Linear Regression model
        rmse2<-postResample(p_rfNew, obs = bike$Rented_Bike_Count)
        output_RF<-paste0("Comparing on Test Set, the RMSE of the random forest model is: ",rmse2[[1]])
        output$modelFitRF <- renderText(
            output_RF
        )
        
        
        #Prediction tab
        output$modelPred <- renderPrint({
            dfPredictions<-data.frame(Hour=input$Hour,
                                      Temperature=input$Temperature,
                                      Humidity=input$Humidity,
                                      Wind_speed=input$Wind_speed,
                                      Visibility=input$Visibility,
                                      Dew_point_temperature=input$Dew_point_temperature,
                                      Solar_Radiation=input$Solar_Radiation,
                                      Rainfall=input$Rainfall,
                                      Snowfall=input$Snowfall
            )
            dataInput <- dfPredictions[,input$modelColumns]
            
            #Linear Regression Model
            p_LMNew <- predict(mlrFit, newdata = dataInput)
            p_LMOut<-paste0("The prediction of  Rented Bike Count for the Linear Regression Model is: ",p_LMNew)
            #Regression Tree Model
            p_RTNew <- predict(treeFit, newdata = dataInput)
            p_RTOut<-paste0("The prediction of  Rented Bike Count for the Regression Tree Model is: ",p_RTNew)
            #Random Forest Model
            p_RFNew <- predict(randomForestFit, newdata = dataInput)
            p_RFOut<-paste0("The prediction of Rented Bike Count for the Random Forest Model is: ",p_RFNew)
            
            if (input$rdoPredModel=="MLR"){
                p_LMOut
            }else if(input$rdoPredModel=="Rtree"){
                p_RTOut
            }else{
                p_RFOut
            }
        })
    })

    
    
    #Data Page
    getDataAll <- reactive({
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
            scrollY = "500px",
            paging = FALSE
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

