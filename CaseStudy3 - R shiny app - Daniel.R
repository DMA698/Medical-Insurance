library(shiny)
library(cluster)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(caret)
library(randomForest)

# Source for loading the dataset
data <- read.csv("Medicalpremium1.csv")
data <- na.omit(data)
data$Diabetes <- as.factor(data$Diabetes)
data$BloodPressureProblems <- as.factor(data$BloodPressureProblems)

#Regression machine learning model for training the dataset
# Splitting the data for testing and train the model:#
set.seed(123)
trainIndex <- createDataPartition(data$PremiumPrice,
                                  p = 0.7, list = FALSE, times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

model <- train(
  PremiumPrice ~ Age + Diabetes + BloodPressureProblems + Weight + Height, 
  data = train_data, 
  method = "rf"
)

#User-interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div(
      h1("Medical Insurance Premium Analysis"),
      h3("Predict Yearly Medical Cover Cost INR(???)")
    )),
  tabsetPanel(
    # Tab 1: This tab gives a detailed extensive analysis of the age, height & weight
    #with respect to different health parameter to perform analysis and visualization.
    #The subsequent stage is vital for data modeling.
    
    tabPanel("Data Exploration of Potential Health Parameters",
             sidebarLayout(
               sidebarPanel(
                 selectInput("ext_age", "Select your Age Group",
                             choices = c("All", "18-25", "26-35", "36-45", "46-60", "61+"),
                             selected = "36-45"),
                 selectInput("ext_diabetes", "Select your Diabetes", 
                             choices = c("0", "1"), selected = "1"),
                 selectInput("ext_bld_prs", "Select your B.P", 
                             choices = c("All", "Yes", "No")),
                 checkboxInput("any_chronic_diseases", "Any Chronic Diseases", 
                               value = FALSE),
                 checkboxInput("history_of_cancer", "History of Cancer in Family",
                               value = FALSE)
               ),
               mainPanel(
                 HTML("<b>Exploratory Data Analysis (EDA) </b> -  to perform 
                 deeper understanding and analysis of the relation between age
                 groups, weight and height altered with respect to different 
                 health conditions of patients "),
                 plotlyOutput("ext_plot"),
                 dataTableOutput("ext_table")
               )
             )
    ),
    #Tab 2: This tab gives data analysis of Insurance Premium vs. Age with respect 
    #to Diabetes & B.P.
    tabPanel("Analyzing Data for Insurance Premium vs. Age",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "age_grp", label = "Select desired Age Group", 
                             choices = c("18-25", "26-35", "36-45", "46-60", "61+"),
                             selected = "36-45"),
                 selectInput(inputId = "diabetes", label = "Select your Diabetes", 
                             choices = c("0", "1")),
                 sliderInput(inputId = "cover", 
                             label = "Select Insurance Premium coverage ??? (INR)", 
                             min = 0, max = 50000, value = c(0, 50000)),
                 selectInput(inputId = "bld_prs", label = "Select your B.P",
                             choices = c("Any", "Yes", "No")),
               ),
               mainPanel(
                 plotlyOutput("prem_plot"),
                 dataTableOutput("prem_table")
               )
             )
    ),
    
    # Tab 3: This tab is for the data modeling stage; which uses the machine learning K-means 
    #clustering model to find the observations of the different subgroups within the data set.
    #This gives us a overall holistic overview of the observation to potentially categorize 
    #the data for better refinement.
    
    tabPanel("Data Modeling: K-means Clustering",
             sidebarLayout(
               sidebarPanel(
                 # Input for the number of clusters
                 sliderInput("cls", "Number of clusters:", min = 1, max = 10,
                             value = 3),
                 selectInput("x_ax", "X-axis variable:", choices = names(data),
                             selected = "Age"),
                 selectInput("y_ax", "Y-axis variable:", choices = names(data),
                             selected = "PremiumPrice"),
                 
                 br(),
                 actionButton("update", "Update Clusters", value = 1)
               ),
               mainPanel(
                 HTML("<b>Data Modeling </b> - K-means clustering model to understand the
                      observations of different health parameters - which gives an overall
                      holistic overview of the observation to potentially catagorize the data
                      for better refinement!"),
                 plotlyOutput("plot"),
                 verbatimTextOutput("sse_output")
               )
             )
    ),
    # Tab 4: This tab is for modeling the analysed data using the regression machine 
    #learning algorithm - to predict the price covered by the insurance company (unknown).
    #Based on the given user input - this model predicts the premium coverage price.
    tabPanel("Data Modeling: Regression Prediction",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "prd_age", label = "Select your Age:", 
                             min = min(data$Age), max = max(data$Age), value = 30),
                 checkboxInput(inputId = "prd_diabetes", label = "Diabetes:", value = FALSE),
                 checkboxInput(inputId = "prd_bloodpressureproblem", label = "B.P:", value = FALSE),
                 sliderInput(inputId = "prd_weight", label = "Select your Weight:",
                             min = min(data$Weight), max = max(data$Weight), value = 70),
                 sliderInput(inputId = "prd_height", label = "Select your Height:",
                             min = min(data$Height), max = max(data$Height), value = 170),
                 verbatimTextOutput("predictedPriceValue")
               ),
               mainPanel(
                 plotlyOutput("prdPlot", height = "500px")
               )
             )
    )
  )
)

# Server function used to generate the output of the input (user) data visually
server <- function(input, output, session) {
  # 1) Exploratory Data analysis for Diabetes and B.P w.r.to Age, Height & Weight
  fltr_dat_ext <- reactive({
    data %>%
      filter(
        if (input$ext_age != "All") Age >= switch(
          input$ext_age,
          "18-25"= 18, "26-35"=26, "36-45"=36, "46-60" = 46, "61+" = 61
        ) else TRUE,
        if (input$ext_age != "All") Age <= switch(
          input$ext_age,
          "18-25" = 25, "26-35" = 35, "36-45" = 45,"46-60"=60, "61+" = Inf
        ) else TRUE,
        if (input$ext_diabetes != "All") 
          Diabetes == as.numeric(input$ext_diabetes) else TRUE,
        if (input$ext_bld_prs == "Any") TRUE
        else if (input$ext_bld_prs == "Yes") BloodPressureProblems == 1
        else if (input$ext_bld_prs == "No") BloodPressureProblems == 0
        else TRUE,
        if (input$any_chronic_diseases) AnyChronicDiseases == 1 else TRUE,
        if (input$history_of_cancer) HistoryOfCancerInFamily == 1 else TRUE
      )
  })
  
  output$ext_plot <- renderPlotly({
    plot_ly(data = fltr_dat_ext(),
            x = ~Age, y = ~Weight, z = ~Height,
            color = ~as.factor(Diabetes),
            symbol = ~as.factor(BloodPressureProblems),
            type = 'scatter3d', mode = 'markers',
            marker = list(size = 8, opacity = 0.7, colorscale = 'Blues'),
            text = ~paste('Age: ', Age, '<br>Weight: W', Weight,
                          '<br>Height: H' , Height),
            hoverinfo = 'text'
    ) %>%
      layout(scene = list(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Weight"),
        zaxis = list(title = "Height")
      ),
      showlegend = TRUE
      )
  })
  
  output$ext_table <- renderDataTable({
    fltr_dat_ext()
  })
  
  # 2) Insurance Premium vs. Age, Diabetes & B.P
  fltr_dat_age <- reactive({
    data %>%
      filter(
        Age >= switch(
          input$age_grp,
          "18-25"= 18, "26-35"=26, "36-45"=36, "46-60" = 46, "61+" = 61
        ),
        Age <= switch(
          input$age_grp,
          "18-25" = 25, "26-35" = 35, "36-45" = 45,"46-60"=60, "61+" = Inf
        ),
        Diabetes == input$diabetes,
        PremiumPrice >= input$cover[1],
        PremiumPrice <= input$cover[2],
        if (input$bld_prs == "Yes") BloodPressureProblems == 1 else if 
        (input$bld_prs == "No") 
          BloodPressureProblems == 0 else TRUE,
        if (input$diabetes == "Yes") Diabetes == 1 else if (input$diabetes == "No") 
          Diabetes == 0 else TRUE
      )
  })
  
  output$prem_plot <- renderPlotly({
    plot_ly(data = fltr_dat_age(),
            x = ~Age, y = ~PremiumPrice,
            color = ~PremiumPrice,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10, opacity = 0.7, colorscale = 'Viridis'),
            text = ~paste('Age: ', Age, '<br>Premium: ', PremiumPrice),
            hoverinfo = 'text'
    ) %>%
      
      layout(title = "<b>Data Analysis:</b> Insurance Premium vs Age w.r.t Diabetes & B.P",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Premium"),
             showlegend = FALSE)
  })
  
  output$prem_table <- renderDataTable({
    fltr_dat_age()
  })
  
  
  
  #3) K-means clustering machine learning model to classify the subcategories for 
  #potentially identifying the observations. 
  clstr_dat <- reactive({
    cls_dat <- data %>%
      select(input$x_ax, input$y_ax) %>%
      select(-matches("Diabetes|BloodPressureProblems"))  
    
    scl_data <- scale(cls_dat)
    
    kmeans_rslt <- kmeans(scl_data, centers = input$cls)
    
    data_with_clusters <- cbind(data, C = as.factor(kmeans_rslt$cluster))
    
    result_list <- list(data = data_with_clusters, kmeans_rslt = kmeans_rslt)
    return(result_list)
  })
  
  observeEvent(input$update, {
    output$plot <- renderPlotly({
      p <- plot_ly(data = clstr_dat()$data, x = ~get(input$x_ax),
                   y = ~get(input$y_ax),
                   color = ~as.factor(C), type = "scatter", mode = "markers", 
                   marker = list(size = 10))
      
      p <- add_markers(p, data = clstr_dat()$data, x = ~get(input$x_ax),
                       y = ~get(input$y_ax),
                       color = ~as.factor(C), marker = list(size = 10))
      
      p <- layout(p, title = "<b> K-means Clustering Model <b>", 
                  xaxis = list(title = input$x_ax),
                  yaxis = list(title = input$y_ax),
                  showlegend = TRUE, legend = list(title = "Cluster"))
      
      p
    })
    
    sse <- sum(clstr_dat()$kmeans_rslt$withinss)
    output$sse_output <- renderText(paste("Sum of Squared Errors (SSE):", round(sse, 2)))
  })
  #) Regression machine learning data modeling for Premium Prediction & Analysis.
  filter_data_prd <- reactive({
    data.frame(
      Age = input$prd_age,
      Diabetes = as.factor(ifelse(input$prd_diabetes, 1, 0)),
      BloodPressureProblems = as.factor(ifelse(input$prd_bloodpressureproblem, 1, 0)),
      Weight = input$prd_weight,
      Height = input$prd_height
    )
  })
  
  output$prdPlot <- renderPlotly({
    prediction <- predict(model, newdata = filter_data_prd())
    plot_ly(data = data, x = ~Age, y = ~PremiumPrice, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 1, colorscale = 'Viridis'),
            text = ~paste('Age: ', Age, '<br>Premium: ', PremiumPrice),
            hoverinfo = 'text'
    ) %>%
      add_trace(x = filter_data_prd()$Age, y = prediction, 
                type = 'scatter', mode = 'markers', 
                marker = list(size = 15, color = 'Red', symbol = '*'),
                text = ~paste('Predicted Insurance Premium coverage: ', round(prediction, 2)),
                hoverinfo = 'text'
      ) %>%
      layout(title = "Insurance Prediction coverage",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Premium"),
             showlegend = FALSE)
  })
  
  output$predictedPriceValue <- renderText({
    sprintf("Predicted Insurance ??? (INR): %.2f", predict(model, newdata = filter_data_prd()))
  })
}

# Run the application
shinyApp(ui, server)
