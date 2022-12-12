#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(DT)
library(psych)
library(shiny)
library(shinydashboard)
library(DescTools)
library(summarytools)
library(corrr)
library(DT)
library(caret)
library(randomForest)
trctrl <- trainControl(method = "cv" , number = 10)
coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

vec_remove_variables = c("owner", "farm_name", "lot_number","mill", "ico_number",
                         "company", "altitude", "region", "producer", "number_of_bags", "bag_weight", 
                         "in_country_partner", "harvest_year", "owner_1","certification_body", "certification_address",
                         "certification_contact", "unit_of_measurement", "altitude_low_meters", "altitude_high_meters")
vec_factors = c("country_of_origin","variety", "processing_method", "color", "day", "year")

df_coffee <- coffee_ratings %>% 
  #Removing non-predictive variables
  select(-vec_remove_variables) %>% 
  #Creating a day variable for when coffee graded
  mutate(day = wday(mdy(grading_date), label = TRUE, abbr = FALSE)) %>% 
  mutate(year = year(mdy(grading_date))) %>%
  filter(species == "Arabica") %>%
  #Removing unneeded variables
  select(-c("grading_date","expiration", "species")) %>%
  #Removing incomplete observations
  drop_na() %>%
  #Removing impossible altitude values
  filter(altitude_mean_meters < 4200) %>%
  #Changing categorical variables to factors
  mutate(across(vec_factors, factor)) 


df_continous_sum <- df_coffee %>%
  group_by(country_of_origin) %>%
  summarise(aroma = mean(aroma), flavor = mean(flavor), aftertaste = mean(aftertaste), acidity = mean(acidity), 
            body = mean(body), 
            balance = mean(balance), uniformity = mean(uniformity), clean_cup = mean(clean_cup), sweetness = mean(sweetness), 
            moisture = mean(moisture))

shinyServer(function(input, output) {
  
  #Variable Information Page
  output$scorePlot <- renderPlot({
    g_score <- ggplot(df_coffee, aes(x = total_cup_points)) +
      geom_histogram()
    g_score
  })
  output$scoreTable <- renderTable({
    tab_score <- describe(df_coffee$total_cup_points, fast = TRUE) %>%
                select(-vars)
    
  })
  
  output$varietyPlot <- renderPlot({
    g_variety <- ggplot(df_coffee, aes(x = variety)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45))
    g_variety
  })
  output$varietyTable <- renderTable({
    tab_variety <- table(df_coffee$variety)
    tab_variety
  })
  
  output$procPlot <- renderPlot({
    g_proc <- ggplot(df_coffee, aes(x = processing_method)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45))
    g_proc
  })
  
  output$procTable <- renderTable({
    tab_proc <- table(df_coffee$processing_method)
    tab_proc
  })
  
  output$aromaPlot <- renderPlot({
    g_aroma <- ggplot(df_coffee, aes(x = aroma)) +
      geom_histogram() 
    g_aroma
  })
  
  output$aromaTable <- renderTable({
    tab_aroma <- describe(df_coffee$aroma, fast = TRUE) %>%
      select(-vars)
    tab_aroma
  })
  
  output$flavorPlot <- renderPlot({
    g_flavor <- ggplot(df_coffee, aes(x = flavor)) +
      geom_histogram() 
    g_flavor
  })
  
  output$flavorTable <- renderTable({
    tab_flavor <- describe(df_coffee$flavor, fast = TRUE) %>%
      select(-vars)
    tab_flavor
  })
  
  output$tastePlot <- renderPlot({
    g_taste <- ggplot(df_coffee, aes(x = aftertaste)) +
      geom_histogram() 
    g_taste
  })
  
  output$tasteTable <- renderTable({
    tab_taste <- describe(df_coffee$aftertaste, fast = TRUE) %>%
      select(-vars)
    tab_taste
  })
  
  output$acidPlot <- renderPlot({
    g_acid <- ggplot(df_coffee, aes(x = acidity)) +
      geom_histogram() 
    g_acid
  })
  
  output$acidTable <- renderTable({
    tab_acid <- describe(df_coffee$acidity, fast = TRUE) %>%
      select(-vars)
    tab_acid
  })
  
  output$bodyPlot <- renderPlot({
    g_body <- ggplot(df_coffee, aes(x = body)) +
      geom_histogram() 
    g_body
  })
  
  output$bodyTable <- renderTable({
    tab_body <- describe(df_coffee$body, fast = TRUE) %>%
      select(-vars)
    tab_body
  })
  
  output$balancePlot <- renderPlot({
    g_balance <- ggplot(df_coffee, aes(x = balance)) +
      geom_histogram() 
    g_balance
  })
  
  output$balanceTable <- renderTable({
    tab_balance <- describe(df_coffee$balance, fast = TRUE) %>%
      select(-vars)
    tab_balance
  })
  
  
  output$uniformPlot <- renderPlot({
    g_uniform <- ggplot(df_coffee, aes(x = uniformity)) +
      geom_histogram(bins=30) 
    g_uniform
  })
  
  output$uniformTable <- renderTable({
    tab_uniform <- describe(df_coffee$uniformity, fast = TRUE) %>%
      select(-vars)
    tab_uniform
  })
  
  output$sweetPlot <- renderPlot({
    g_sweet <- ggplot(df_coffee, aes(x = sweetness)) +
      geom_histogram() 
    g_sweet
  })
  
  output$sweetTable <- renderTable({
    tab_sweet <- describe(df_coffee$sweetness, fast = TRUE) %>%
      select(-vars)
    tab_sweet
  })
  
  output$cleanPlot <- renderPlot({
    g_clean <- ggplot(df_coffee, aes(x = clean_cup)) +
      geom_histogram() 
    g_clean
  })
  
  output$cleanTable <- renderTable({
    tab_clean <- describe(df_coffee$clean_cup, fast = TRUE) %>%
      select(-vars)
    tab_clean
  })
  
  output$cleanPlot <- renderPlot({
    g_clean <- ggplot(df_coffee, aes(x = clean_cup)) +
      geom_histogram() 
    g_clean
  })
  
  output$cleanTable <- renderTable({
    tab_clean <- describe(df_coffee$clean_cup, fast = TRUE) %>%
      select(-vars)
    tab_clean
  })
  
  output$pointsPlot <- renderPlot({
    g_points <- ggplot(df_coffee, aes(x = cupper_points)) +
      geom_histogram() 
    g_points
  })
  
  output$pointsTable <- renderTable({
    tab_points <- describe(df_coffee$cupper_points, fast = TRUE) %>%
      select(-vars)
    tab_points
  })
  
  output$moistPlot <- renderPlot({
    g_moist <- ggplot(df_coffee, aes(x = moisture)) +
      geom_histogram() 
    g_moist
  })
  
  output$moistTable <- renderTable({
    tab_moist <- describe(df_coffee$moisture, fast = TRUE) %>%
      select(-vars)
    tab_moist
  })
  
  output$cat1Plot <- renderPlot({
    g_cat1 <- ggplot(df_coffee, aes(x = category_one_defects)) +
      geom_histogram() 
    g_cat1
  })
  
  output$cat1Table <- renderTable({
    tab_cat1 <- describe(df_coffee$category_one_defects, fast = TRUE) %>%
      select(-vars)
    tab_cat1
  })
  
  
  output$cat2Plot <- renderPlot({
    g_cat2 <- ggplot(df_coffee, aes(x = category_two_defects)) +
      geom_histogram() 
    g_cat2
  })
  
  output$cat2Table <- renderTable({
    tab_cat2 <- describe(df_coffee$category_two_defects, fast = TRUE) %>%
      select(-vars)
    tab_cat2
  })
  
  
  output$quakersPlot <- renderPlot({
    g_quakers <- ggplot(df_coffee, aes(x = quakers)) +
      geom_histogram() 
    g_quakers
  })
  
  output$quakersTable <- renderTable({
    tab_quakers <- describe(df_coffee$quakers, fast = TRUE) %>%
      select(-vars)
    tab_quakers
  })
  
  
  output$colorPlot <- renderPlot({
    g_color <- ggplot(df_coffee, aes(x = color)) +
      geom_bar() 
    g_color
  })
  
  output$colorTable <- renderTable({
    tab_color <- table(df_coffee$color)
    tab_color
  })
  
  output$altitudePlot <- renderPlot({
    g_altitude <- ggplot(df_coffee, aes(x = altitude_mean_meters)) +
      geom_histogram() 
    g_altitude
  })
  
  output$altitudeTable <- renderTable({
    tab_altitude <- describe(df_coffee$altitude_mean_meters, fast = TRUE) %>%
      select(-vars)
    tab_altitude
  })
  
  output$dayPlot <- renderPlot({
    g_color <- ggplot(df_coffee, aes(x = day)) +
      geom_bar() 
    g_color
  })
  
  output$dayTable <- renderTable({
    tab_day <- table(df_coffee$day)
    tab_day
  })
  
  output$yearPlot <- renderPlot({
    g_year <- ggplot(df_coffee, aes(x = year)) +
      geom_bar() 
    g_year
  })
  
  output$yearTable <- renderTable({
    tab_year <- table(df_coffee$year)
    tab_year
  })
  
  output$countryPlot <- renderPlot({
    g_country <- ggplot(df_coffee, aes(x = country_of_origin)) +
      geom_bar() 
    g_country
  })
  
  output$countryTable <- renderTable({
    tab_country <- table(df_coffee$country_of_origin)
    tab_country
  })
  #Data Exploration Page
  get_choices <- reactive({
    df_choices <- list(x = input$x_variable, y = input$y_variable, group = input$group, color = input$color, shape = input$shape)
    
  })
  
  output$expPlot <- renderPlot({
    df_plot <- get_choices()
  
    if(input$plot_type == "jitter"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$x, y = df_plot$y)) +
               geom_jitter(aes_string(color = df_plot$color, shape = df_plot$shape))
    }
    if(input$plot_type == "scatter"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$x, y = df_plot$y)) +
        geom_point(aes_string(color = df_plot$color, shape = df_plot$shape))
    }
    if(input$plot_type == "boxplot"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$x, y = df_plot$group)) +
        geom_boxplot(fill= "grey") +
        geom_jitter(aes_string(color = df_plot$color, shape = df_plot$shape))
    }
    if(input$plot_type == "histogram"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$x)) +
        geom_histogram(aes_string(color = df_plot$color)) 
    }
    if(input$plot_type == "density"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$x, fill =df_plot$color, color = df_plot$color)) +
        geom_density(aes_string( alpha = .1)) 
    }
    if(input$plot_type == "bar"){
      g_exp <- ggplot(df_coffee, aes_string(x = df_plot$group)) +
        geom_bar(aes_string(color = df_plot$color))
    }
    g_exp
  })
  output$expTable <- renderDataTable({
    df_coffee
  })
  
    #Model Fitting Tab
    #Changing input to a percentage for split

    train_split <- eventReactive(input$analysis,{
      input$split / 100
    })
    select_mtry <- eventReactive(input$analysis,{
      input$mtry
    })
    select_cp <- eventReactive(input$analysis,{
      input$cp
    })
    #Creating the split index
    set.seed(666)  
    trainingRowIndex <-
      eventReactive(input$analysis,{
        sample(1:nrow(df_coffee),
              train_split() * nrow(df_coffee))
      })
  #Creating Training Dataset
    trainingData <- eventReactive(input$analysis,{
      df_coffee[trainingRowIndex(), ]
    })
  #Creating Test Dataset
    testData <- eventReactive(input$analysis,{
      df_coffee[-trainingRowIndex(), ]
    })
  
    model_var <- eventReactive(input$analysis,{
      reformulate(input$preds, input$outcome)
    })
  
  model_reg <- reactive ({
    train(model_var(), data = trainingData(),
          preProcess = c("center", "scale"),
          method = "lm",
          trControl = trctrl)
  })
  model_rand <- reactive ({
    train(model_var(), data = trainingData(),
          preProcess = c("center", "scale"),
          method = "rf",
          tuneGrid = data.frame(mtry =1:select_mtry()),
          trControl = trctrl)
  })
  model_tree <- reactive ({
    train(model_var(), data = trainingData(),
          preProcess = c("center", "scale"),
          method = "rpart",
          tuneGrid = data.frame(cp = seq(0, select_cp(), 0.001)),
          trControl = trctrl)
  })
  output$reg <- renderPrint(summary(model_reg()))
  output$rand_table <- renderTable(model_rand()$results, digits = 3)
  output$tree_table <- renderTable(model_tree()$results, digits = 3)
  output$rand_imp <- renderPrint(varImp(model_rand(), scale = FALSE))
  output$tree_imp <- renderPrint(varImp(model_tree(), scale = FALSE))

  reg_pred <- reactive ({
    predict(model_reg(), newdata = testData())
  })
  rand_pred <- reactive ({
    predict(model_rand(), newdata = testData())
  })
  tree_pred <- reactive ({
    predict(model_tree(), newdata = testData())
  })
  #Results on test data
  output$rmse_reg <- renderPrint(postResample(reg_pred(), testData()$total_cup_points))
  output$rmse_rand <- renderPrint(postResample(rand_pred(), testData()$total_cup_points))
  output$rmse_tree <- renderPrint(postResample(tree_pred(), testData()$total_cup_points))
})

