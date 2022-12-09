
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(skin = "midnight",
                    
                    dashboardHeader(title="Final Project: Coffee Score Interactive Dashboard",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("mug-hot")),
                      menuItem("Variable Information", tabName = "var", icon = icon("eye"),
                               menuSubItem("Total Cup Points", tabName = "score"),
                               menuSubItem("Species", tabName = "species"),
                               menuSubItem("Variety", tabName = "variety"),
                               menuSubItem("Processing Method", tabName = "process"),
                               menuSubItem("Aroma", tabName = "aroma"),
                               menuSubItem("Flavor", tabName = "flavor"),
                               menuSubItem("Aftertaste", tabName = "taste"),
                               menuSubItem("Acidity", tabName = "acidity"),
                               menuSubItem("Body", tabName = "body"),
                               menuSubItem("Balance", tabName = "balance"),
                               menuSubItem("Uniformity", tabName = "uniformity"),
                               menuSubItem("Sweetness", tabName = "sweetness"),
                               menuSubItem("Cup Points", tabName = "score"),
                               menuSubItem("Cupper Points", tabName = "points"),
                               menuSubItem("Moisture", tabName = "moisture"),
                               menuSubItem("Category One Defects", tabName = "cat1"),
                               menuSubItem("Category Two Defects", tabName = "cat2"),
                               menuSubItem("Quakers", tabName = "quakers"),
                               menuSubItem("Color", tabName = "color"),
                               menuSubItem("Altitude", tabName = "altitude"),
                               menuSubItem("Day", tabName = "day")
                               ),
                      menuItem("Data Exploration", tabName = "exp",icon = icon("square-poll-vertical")),
                      menuItem("Modeling", tabName = "model",icon = icon("chalkboard-user"),
                               menuSubItem("Modeling Info", tabName = "info"),
                               menuSubItem("Model Fitting", tabName = "fit"),
                               menuSubItem("Prediction", tabName = "pred")
                               ),
                      menuItem("Data", tabName = "data",icon = icon("download"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  h1("Purpose"),
                                  p("This app allows a user to interact with a coffee ratings dataset provided by Tidy Tuesdays to explore the data, create models, create predictions, and download the data. The dataset can be found by clicking here."),
                                  h1("Pages"),
                                  h3("Variable Information"),
                                  p("This page contains a tab for each variable in the dataset. In each tab, the user can find a definition of the variable along with summary statistics."),
                                  h3("Data Exploration"),
                                  p("This page allows the user to select an x and y variable along with plot type then creates a graph based upon their choices. There are also options to add colors and shapes based upon categorical variables"),
                                  h3("Modeling"),
                                  p("This page holds three tabs: Modeling Info, Model Fitting, and Prediction."),
                                  p("In the first tab, the user will be given a description of each of the three models they can create along with pros and cons of each."),
                                  p("In the second tab, the user will be able to create models by specifying the proportion of data to be used as the training set, the variables to use as predictors, and model settings. After creation, the user will be supplied with fit statistics and summaries for each model to gauge performance."),
                                  p("In the final tab, the user will be able to input values in for each of the predictors and return a predicted score."),
                                  h3("Data"),
                                  p("Finally, the Data page will allow the user to create a subset of the data then download the result as a .csv file.")
                                )
                        ),
                        tabItem(tabName = "var"),
                        tabItem(tabName = "exp"),
                        tabItem(tabName = "model"),
                        tabItem(tabName = "data")
  
                      )
  
  
                      
))