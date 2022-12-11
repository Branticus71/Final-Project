
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

ui <- dashboardPage(skin = "midnight",
                    
                    dashboardHeader(title="Final Project: Coffee Score Interactive Dashboard",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("mug-hot")),
                      menuItem("Variable Information", tabName = "var", icon = icon("eye"),
                               menuSubItem("Total Cup Points", tabName = "score"),
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
                               menuSubItem("Clean Cup", tabName = "clean"),
                               menuSubItem("Cupper Points", tabName = "points"),
                               menuSubItem("Moisture", tabName = "moisture"),
                               menuSubItem("Category One Defects", tabName = "cat1"),
                               menuSubItem("Category Two Defects", tabName = "cat2"),
                               menuSubItem("Quakers", tabName = "quakers"),
                               menuSubItem("Color", tabName = "color"),
                               menuSubItem("Altitude", tabName = "altitude"),
                               menuSubItem("Day", tabName = "day"),
                               menuSubItem("Year", tabName = "year")
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
                                  p("This app allows a user to interact with a coffee ratings dataset for arabica coffee beans provided by Tidy Tuesdays to explore the data, create models, create predictions, and download the data. The dataset can be found by clicking here."),
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
                        tabItem(tabName = "score",
                                fluidRow(
                                  h1("Definition"),
                                  p("Total Cup Points is the sum of a particular cups scores in the aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness, and cupper points variables and is the rating for that cup of coffee."),
                                  plotOutput("scorePlot"),
                                  tableOutput("scoreTable")
                                )
                                ),
                        tabItem(tabName = "variety",
                                fluidRow(
                                  h1("Definition"),
                                  p("Variety is a categorical variable containing the variety of the coffee bean used. Varieties are distinct from species in that they are man made as opposed to naturally occurring. Some common examples are Sumatra, Kona, Bourbon, and Blue Mountain."),
                                  plotOutput("varietyPlot"),
                                  tableOutput("varietyTable")
                                )
                        ),
                        tabItem(tabName = "process",
                                fluidRow(
                                  h1("Definition"),
                                  p("Processing Method is a categorical variable containing the process used to remove the coffee beans from the fruit that contains them. Each is said to have an effect on the flavor profile of the bean produced."),
                                  plotOutput("procPlot"),
                                  tableOutput("procTable")
                                )
                        ),
                        tabItem(tabName = "aroma",
                                fluidRow(
                                  h1("Definition"),
                                  p("The combined grade of the coffee in Fragrance and Aroma. Fragrance being the smell of the coffee beans before making the coffee and aroma being the smell of the cup of coffee itself."),
                                  plotOutput("aromaPlot"),
                                  tableOutput("aromaTable")
                                )
                        ),
                        tabItem(tabName = "flavor",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee in areas of taste. It is evaluated based on intensity, quality, and complexity of the combined taste."),
                                  plotOutput("flavorPlot"),
                                  tableOutput("flavorTable")
                                )
                        ),
                        tabItem(tabName = "taste",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its aftertaste, specifically the length of time any good flavors persists after swallowing."),
                                  plotOutput("tastePlot"),
                                  tableOutput("tasteTable")
                                )
                        ),
                        tabItem(tabName = "acidity",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its acidity evaluated in the context of the coffee's flavor profile. So high acidity will result in good ratings in the proper flavor profiles and low ratings in others."),
                                  plotOutput("acidPlot"),
                                  tableOutput("acidTable")
                                )
                        ),
                        tabItem(tabName = "body",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its body or mouthfeel. This refers to the perceived weight and texture of the coffee so comparisons such as a desirable silky versus an undesirable thin watery texture."),
                                  plotOutput("bodyPlot"),
                                  tableOutput("bodyTable")
                                )
                        ),
                        tabItem(tabName = "balance",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its balance in taste. This refers to the balance between flavors with preference given to complex flavors that do not overwhelm each other."),
                                  plotOutput("balancePlot"),
                                  tableOutput("balanceTable")
                                )
                        ),
                        tabItem(tabName = "uniformity",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its uniformity. In each test, five cups are brewed and if they taste different they are deducted points."),
                                  plotOutput("uniformPlot"),
                                  tableOutput("uniformTable")
                                )
                        ),
                        tabItem(tabName = "sweetness",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its sweetness. The flavor profile of the sweetness changes across types of roasts but must be present and identifiable for a good score."),
                                  plotOutput("sweetPlot"),
                                  tableOutput("sweetTable")
                                )
                        ),
                        tabItem(tabName = "clean",
                                fluidRow(
                                  h1("Definition"),
                                  p("The grade of the coffee for its thorough taste. In each test, five cups are brewed and if the flavor deteriorates from start to finish 2 points are docked from the score."),
                                  plotOutput("cleanPlot"),
                                  tableOutput("cleanTable")
                                )
                        ),
                        tabItem(tabName = "points",
                                fluidRow(
                                  h1("Definition"),
                                  p("The cupper points are simply the grade the tester would give the coffee out of ten without a strict rating system. This is more of a holistic feeling rather than scientific grade."),
                                  plotOutput("pointsPlot"),
                                  tableOutput("pointsTable")
                                )
                        ),
                        tabItem(tabName = "moisture",
                                fluidRow(
                                  h1("Definition"),
                                  p("The moisture content of the coffee beans prior to roasting."),
                                  plotOutput("moistPlot"),
                                  tableOutput("moistTable")
                                )
                        ),
                        tabItem(tabName = "cat1",
                                fluidRow(
                                  h1("Definition"),
                                  p("The number of primary defects in the coffee beans. This could be insect damage, fungus damage, foreign matter, or something else."),
                                  plotOutput("cat1Plot"),
                                  tableOutput("cat1Table")
                                )
                        ),
                        tabItem(tabName = "cat2",
                                fluidRow(
                                  h1("Definition"),
                                  p("The number of secondary defects in the coffee beans. This could be broken beans, partial discoloring, presence of shell, or something else."),
                                  plotOutput("cat2Plot"),
                                  tableOutput("cat2Table")
                                )
                        ),
                        tabItem(tabName = "quakers",
                                fluidRow(
                                  h1("Definition"),
                                  p("The number of quakers in the coffee beans. These are beans that were picked before ripening and cannot roast properly."),
                                  plotOutput("quakersPlot"),
                                  tableOutput("quakersTable")
                                )
                        ),
                        tabItem(tabName = "color",
                                fluidRow(
                                  h1("Definition"),
                                  p("The color of the coffee beans prior to roasting. Certain colors are considered more desirable with a greenish blue color considered ideal by many."),
                                  plotOutput("colorPlot"),
                                  tableOutput("colorTable")
                                )
                        ),
                        tabItem(tabName = "altitude",
                                fluidRow(
                                  h1("Definition"),
                                  p("The average altitude in meters of the coffee farm location. Coffee beans grow slower in higher altitudes which leads to more complex and desirable flavors."),
                                  plotOutput("altitudePlot"),
                                  tableOutput("altitudeTable")
                                )
                        ),
                        tabItem(tabName = "day",
                                fluidRow(
                                  h1("Definition"),
                                  p("The day the coffee was rated."),
                                  plotOutput("dayPlot"),
                                  tableOutput("dayTable")
                                )
                        ),
                        tabItem(tabName = "year",
                                fluidRow(
                                  h1("Definition"),
                                  p("The year the coffee was rated."),
                                  plotOutput("yearPlot"),
                                  tableOutput("yearTable")
                                )
                        ),
                        tabItem(tabName = "exp",
                                fluidRow(
                                  h1("Data Exploration"),
                                  h5("Selections only affect relevant plot types. For example, your choice for 'Y Continuous Variable' will not matter if boxplot is chosen for 'Plot Type' and the graph will instead be created based on your 'X Continous Variable' and 'Group By' selection."),
                                  varSelectInput("x_variable", "X Continuous Variables:", select(df_coffee, where(is.numeric))),
                                  varSelectInput("y_variable", "Y Continuous Variables:", select(df_coffee, where(is.numeric))),
                                  selectInput("plot_type","Plot Type:", c(scatter = "scatter", "scatter + jitter" = "jitter", boxplot = "boxplot", histogram = "histogram", 
                                                                          density = "density", bar = "bar")
                                  ),
                                  varSelectInput("group","Group by:",select(df_coffee, where(is.factor))),
                                  selectInput("color","Colors by:",c(None = "NULL", Country = "country_of_origin", Variety = "variety", Process = "processing_method", 
                                                                     color = "color", day = "day", year = "year")),
                                  selectInput("shape","Shapes by:",c(None = "NULL", Process = "processing_method", color = "color")),
                                  plotOutput("expPlot", heigh = "600px"),
                                  dataTableOutput("expTable")
                                ) 
                        ),
                        tabItem(tabName = "model"),
                        tabItem(tabName = "info"),
                        tabItem(tabName = "fit",
                                fluidRow(
                                box(
                                  selectInput(
                                    "preds",
                                    label = "Select variables:",
                                    choices = names(df_coffee),
                                    multiple = TRUE
                                  ),
                                  solidHeader = TRUE,
                                  width = 3,
                                  status = "primary",
                                  title = "Predictor Variables"
                                ),
                                box(
                                  varSelectInput("outcome", label = "Select variable:", select(df_coffee, where(is.numeric))),
                                  solidHeader = TRUE,
                                  width = 3,
                                  status = "primary",
                                  title = "Outcome Variable"
                                ),
                                sliderInput(
                                  "split",
                                  label = h3("Train/Test Split %"),
                                  min = 50,
                                  max = 95,
                                  value = 75
                                ),
                                sliderInput(
                                  "mtry",
                                  label = h3("# of Randomly Selected Predictors for Random Forest"),
                                  min = 3,
                                  max = 22,
                                  value = 12
                                ),
                                actionButton("analysis","Analyze!"),
                                verbatimTextOutput("reg"),
                                verbatimTextOutput("rand")
                        )),
                        tabItem(tabName = "pred"),
                        tabItem(tabName = "data")
  
                      )
  
  
                      
))