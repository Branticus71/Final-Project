
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinycssloaders)
library(tidyverse)
library(lubridate)

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


ui <- dashboardPage(skin = "blue-light",
                    
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
                               menuSubItem("Year", tabName = "year"),
                               menuSubItem("Country", tabName = "country")
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
                                  h4("Total Cup Points is the sum of a particular cups scores in the aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness, and cupper points variables and is the rating for that cup of coffee."),
                                  plotOutput("scorePlot"),
                                  tableOutput("scoreTable")
                                )
                                ),
                        tabItem(tabName = "variety",
                                fluidRow(
                                  h1("Definition"),
                                  h4("Variety is a categorical variable containing the variety of the coffee bean used. Varieties are distinct from species in that they are man made as opposed to naturally occurring. Some common examples are Sumatra, Kona, Bourbon, and Blue Mountain."),
                                  plotOutput("varietyPlot"),
                                  tableOutput("varietyTable")
                                )
                        ),
                        tabItem(tabName = "process",
                                fluidRow(
                                  h1("Definition"),
                                  h4("Processing Method is a categorical variable containing the process used to remove the coffee beans from the fruit that contains them. Each is said to have an effect on the flavor profile of the bean produced."),
                                  plotOutput("procPlot"),
                                  tableOutput("procTable")
                                )
                        ),
                        tabItem(tabName = "aroma",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The combined grade of the coffee in Fragrance and Aroma. Fragrance being the smell of the coffee beans before making the coffee and aroma being the smell of the cup of coffee itself."),
                                  plotOutput("aromaPlot"),
                                  tableOutput("aromaTable")
                                )
                        ),
                        tabItem(tabName = "flavor",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee in areas of taste. It is evaluated based on intensity, quality, and complexity of the combined taste."),
                                  plotOutput("flavorPlot"),
                                  tableOutput("flavorTable")
                                )
                        ),
                        tabItem(tabName = "taste",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its aftertaste, specifically the length of time any good flavors persists after swallowing."),
                                  plotOutput("tastePlot"),
                                  tableOutput("tasteTable")
                                )
                        ),
                        tabItem(tabName = "acidity",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its acidity evaluated in the context of the coffee's flavor profile. So high acidity will result in good ratings in the proper flavor profiles and low ratings in others."),
                                  plotOutput("acidPlot"),
                                  tableOutput("acidTable")
                                )
                        ),
                        tabItem(tabName = "body",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its body or mouthfeel. This refers to the perceived weight and texture of the coffee so comparisons such as a desirable silky versus an undesirable thin watery texture."),
                                  plotOutput("bodyPlot"),
                                  tableOutput("bodyTable")
                                )
                        ),
                        tabItem(tabName = "balance",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its balance in taste. This refers to the balance between flavors with preference given to complex flavors that do not overwhelm each other."),
                                  plotOutput("balancePlot"),
                                  tableOutput("balanceTable")
                                )
                        ),
                        tabItem(tabName = "uniformity",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its uniformity. In each test, five cups are brewed and if they taste different they are deducted points."),
                                  plotOutput("uniformPlot"),
                                  tableOutput("uniformTable")
                                )
                        ),
                        tabItem(tabName = "sweetness",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its sweetness. The flavor profile of the sweetness changes across types of roasts but must be present and identifiable for a good score."),
                                  plotOutput("sweetPlot"),
                                  tableOutput("sweetTable")
                                )
                        ),
                        tabItem(tabName = "clean",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The grade of the coffee for its thorough taste. In each test, five cups are brewed and if the flavor deteriorates from start to finish 2 points are docked from the score."),
                                  plotOutput("cleanPlot"),
                                  tableOutput("cleanTable")
                                )
                        ),
                        tabItem(tabName = "points",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The cupper points are simply the grade the tester would give the coffee out of ten without a strict rating system. This is more of a holistic feeling rather than scientific grade."),
                                  plotOutput("pointsPlot"),
                                  tableOutput("pointsTable")
                                )
                        ),
                        tabItem(tabName = "moisture",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The moisture content of the coffee beans prior to roasting."),
                                  plotOutput("moistPlot"),
                                  tableOutput("moistTable")
                                )
                        ),
                        tabItem(tabName = "cat1",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The number of primary defects in the coffee beans. This could be insect damage, fungus damage, foreign matter, or something else."),
                                  plotOutput("cat1Plot"),
                                  tableOutput("cat1Table")
                                )
                        ),
                        tabItem(tabName = "cat2",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The number of secondary defects in the coffee beans. This could be broken beans, partial discoloring, presence of shell, or something else."),
                                  plotOutput("cat2Plot"),
                                  tableOutput("cat2Table")
                                )
                        ),
                        tabItem(tabName = "quakers",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The number of quakers in the coffee beans. These are beans that were picked before ripening and cannot roast properly."),
                                  plotOutput("quakersPlot"),
                                  tableOutput("quakersTable")
                                )
                        ),
                        tabItem(tabName = "color",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The color of the coffee beans prior to roasting. Certain colors are considered more desirable with a greenish blue color considered ideal by many."),
                                  plotOutput("colorPlot"),
                                  tableOutput("colorTable")
                                )
                        ),
                        tabItem(tabName = "altitude",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The average altitude in meters of the coffee farm location. Coffee beans grow slower in higher altitudes which leads to more complex and desirable flavors."),
                                  plotOutput("altitudePlot"),
                                  tableOutput("altitudeTable")
                                )
                        ),
                        tabItem(tabName = "day",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The day the coffee was rated."),
                                  plotOutput("dayPlot"),
                                  tableOutput("dayTable")
                                )
                        ),
                        tabItem(tabName = "year",
                                fluidRow(
                                  h1("Definition"),
                                  h4("The year the coffee was rated."),
                                  plotOutput("yearPlot"),
                                  tableOutput("yearTable")
                                )
                        ),
                        tabItem(tabName = "country",
                                fluidRow(
                                h1("Definition"),
                                h4("The country the coffee was grown in."),
                                plotOutput("countryPlot"),
                                tableOutput("countryTable")
                                )
                        ),
                        tabItem(tabName = "exp",
                                fluidRow(
                                  h1("Data Exploration"),
                                  column(width = 4,
                                    box( width = 12,
                                    h3("Selections only affect relevant plot types. For example, your choice for 'Y Continuous Variable' will not matter if boxplot is chosen for 'Plot Type' and the graph will instead be created based on your 'X Continous Variable' and 'Categorical Variable' selection. Additionally, things such as bar plots will only use the 'Categorical Variable' selection and nothing else."),
                                    solidHeader = TRUE,
                                    status = "primary",
                                    title = "About"
                                  )),
                                  column(width = 4,
                                    box( width = 12,
                                      varSelectInput("x_variable", "X Continuous Variables:", select(df_coffee, where(is.numeric))),
                                      varSelectInput("y_variable", "Y Continuous Variables:", select(df_coffee, where(is.numeric))),
                                      selectInput("plot_type","Plot Type:", c(scatter = "scatter", "scatter + jitter" = "jitter", boxplot = "boxplot", histogram = "histogram", 
                                                                              density = "density", bar = "bar")),
                                      varSelectInput("group","Categorical Variable:",select(df_coffee, where(is.factor))),
                                      selectInput("color","Colors by:",c(None = "NULL", Country = "country_of_origin", Variety = "variety", Process = "processing_method", 
                                                                        color = "color", day = "day", year = "year")),
                                      selectInput("shape","Shapes by:",c(None = "NULL", Process = "processing_method", color = "color")),
                                      sliderInput("size", "Size of Points on Graph", min = 1, max = 10, value = 5, step = 1),
                                      solidHeader = TRUE,
                                      status = "primary",
                                      title = "Graphing Selections"
                                    )),
                                  column(width = 4,
                                         box( width = 12,
                                           selectInput("day_exp",
                                                       "Day Values to Include:",
                                                       levels(df_coffee$day),
                                                       selected = levels(df_coffee$day),
                                                       multiple = TRUE),
                                           selectInput("process_exp",
                                                       "Processing Method Values to Include:",
                                                       levels(df_coffee$processing_method),
                                                       selected = levels(df_coffee$processing_method),
                                                       multiple = TRUE),
                                           selectInput("country_exp",
                                                       "Country of Origin Values to Include:",
                                                       levels(df_coffee$country_of_origin),
                                                       selected = levels(df_coffee$country_of_origin),
                                                       multiple = TRUE),
                                           selectInput("color_exp",
                                                       "Color Values to Include:",
                                                       levels(df_coffee$color),
                                                       selected = levels(df_coffee$color),
                                                       multiple = TRUE),
                                           selectInput("year_exp",
                                                       "Year Values to Include:",
                                                       levels(df_coffee$year),
                                                       selected = levels(df_coffee$year),
                                                       multiple = TRUE),
                                           selectInput("variety_exp",
                                                       "Variety Values to Include:",
                                                       levels(df_coffee$variety),
                                                       selected = levels(df_coffee$variety),
                                                       multiple = TRUE),
                                           solidHeader = TRUE,
                                           status = "primary",
                                           title = "Filter Selections"  
                                         )
                                         
                                         ),
                                 # selectInput(),
                                  column(width = 12,
                                    box(width = 12,
                                      plotOutput("expPlot", heigh = "600px"),
                                      solidHeader = TRUE,
                                      status = "primary",
                                      title = "Plot"
                                  )),
                                  dataTableOutput("expTable")
                                ) 
                        ),
                        tabItem(tabName = "model"),
                        tabItem(tabName = "info",
                                fluidRow(
                                  h1("Model Information"),
                                  column(width = 12,
                                          box(
                                              h1("Linear Regression works by taking in a single outcome variable and multiple predictor variables. It them minimizes the sum of squared residuals: \\(\\sum\\) ( \\(\\epsilon^{2}\\))"),
                                              h1("To calculate the \\(\\beta\\) for each predictor variable. The prediction is then found by multiplying each predictor variable's value by its \\(\\beta\\) and adding them together along with the intercept."),
                                              h1("Regressions are helpful because it is very easy to interpret the results of a regression by looking at your coefficients and their statistical significance. Unfortunately, this interpretability can suffer when dealing with generalized linear models and the statistical assumptions necessary to achieve unbiased coefficients can be rather strict."),
                                              solidHeader = TRUE,
                                              width = 4,
                                              status = "primary",
                                              title = "Linear Regression" 
                                          ),
                                         box(
                                             h1("Regression Trees take in predictors then find the optimal values to split at minimizing the Residual Sum of Squares: \\(\\sum\\)(\\(\\epsilon^{2}\\)) for the observed values on each side of the split. This continues on for a long time creating a very large tree with a predicted value for our outcome variable at the very end of all the splits."),
                                             h1("Trees are often desirable in that they are easy to understand, have no statistical assumptions that must be met, and account for variable interaction innately. Some downsides are that normally you need to prune the tree to reduce the number of splits and there is not an optimal algorithm to form the tree like there is for a regression."),
                                             solidHeader = TRUE,
                                             width = 4,
                                             status = "primary",
                                             title = "Regression Tree"
                                         ),
                                         box(
                                             h1("The Random Forest is a special case of a Bagged Tree. A Bagged Tree is like a Regression Tree but it is built upon taking bootstrap samples with replacement from your data then forming trees based on those samples then aggregating them to find the best model. Random Forests take this just a bit farther by randomly sampling the predictors to use in the model construction."),
                                             h1("This leads to trees that are less correlated with each other that are then aggregated to get a final model which often has better predictive power. The major downside is that while you retain measures like variable importance you do lose interpretability."),
                                             solidHeader = TRUE,
                                             width = 4,
                                             status = "primary",
                                             title = "Random Forest"
                                         )






                                         )







                                )
                        ),
                        tabItem(tabName = "fit",
                                fluidRow(
                                column(width = 12,
                                box(
                                  selectInput(
                                    "preds",
                                    label = "Select variables:",
                                    choices = names(select(df_coffee, -total_cup_points)),
                                    multiple = TRUE,
                                    selected = c("aroma","flavor", "aftertaste","acidity", "body","balance","uniformity","clean_cup", "sweetness","cupper_points", "moisture")
                                  ),
                                  solidHeader = TRUE,
                                  width = 2,
                                  status = "primary",
                                  title = "Predictor Variables"
                                ),
                                box(
                                  selectInput("outcome", label = "Select variable:", "total_cup_points"),
                                  solidHeader = TRUE,
                                  width = 2,
                                  status = "primary",
                                  title = "Outcome Variable",
                                  selected = "total_cup_points"
                                ),
                                box(
                                  sliderInput("split", label = h3("Percent of Data for Training:"),min = 50,max = 95,value = 75),
                                  solidHeader = TRUE,
                                  width = 2,
                                  status = "primary",
                                  title = "Splitting the Data"
                                ),
                                box(
                                  sliderInput("mtry",label = h3("Number of Randomly Selected Predictors for Random Forest."),min = 1,max = 15,value = 3),
                                  solidHeader = TRUE,
                                  width = 2,
                                  status = "primary",
                                  title = "Random Forest Parameter"
                                ),
                                box(
                                  sliderInput("cp",label = h3("Complexity Parameter for Regression Tree."),min = 0,max = .1,value = .005),
                                  solidHeader = TRUE,
                                  width = 2,
                                  status = "primary",
                                  title = "Regression Tree Parameter"
                                ),
                                actionButton("analysis", "Analyze!")
                                ),
                                box(width = 6,
                                    h3("Summary Information"),
                                    withSpinner(verbatimTextOutput("reg")),
                                    h3("RMSE from Predicting on the Test Data"),
                                    withSpinner(verbatimTextOutput("rmse_reg")),
                                    title = "Linear Regression",
                                    solidHeader = TRUE,
                                    status = "primary"
                                ),
                                box(width = 6,
                                     h3("Summary Information"),
                                     withSpinner(tableOutput("rand_table")),
                                     h3("Variable Importance"),
                                     h4("Note: If selected number for random predictors is greater than chosen number of predictors then output will not show."),
                                     withSpinner(verbatimTextOutput("rand_imp")),
                                     h3("RMSE from Predicting on the Test Data"),
                                     withSpinner(verbatimTextOutput("rmse_rand")),
                                     title = "Random Forest",
                                     solidHeader = TRUE,
                                     status = "primary"
                                ),
                                box(width = 6,
                                    h3("Summary Information"),
                                    withSpinner(tableOutput("tree_table")),
                                    h3("Variable Importance"),
                                    withSpinner(verbatimTextOutput("tree_imp")),
                                    h3("RMSE from Predicting on the Test Data"),
                                    withSpinner(verbatimTextOutput("rmse_tree")),
                                    title = "Regression Tree",
                                    solidHeader = TRUE,
                                    status = "primary"
                                    
                                )
                        )),
                        tabItem(tabName = "pred", 
                                h1("Prediction Using Regression Model Created from Model Fitting Page"),
                                h4("Note: The values of continuous variables are limited such that one cannot select values outside of the maximum or minimum value present for that variable in the data. In addition, all continuous variables default to their mean value. If you did not include a predictor in your previous model then changing it's value will not affect your predicted value."),
                                column(width = 3,
                                  box(width = 12,
                                    numericInput("aroma_val",
                                                "Aroma",
                                                value = round(mean(df_coffee$aroma),2),
                                                min = min(df_coffee$aroma),
                                                max= max(df_coffee$aroma),),
                                    numericInput("flavor_val",
                                                "Flavor",
                                                value = round(mean(df_coffee$flavor),2),
                                                min = min(df_coffee$flavor),
                                                max= max(df_coffee$flavor),),
                                    numericInput("aftertaste_val",
                                                "Aftertaste",
                                                value = round(mean(df_coffee$aftertaste),2),
                                                min = min(df_coffee$aftertaste),
                                                max= max(df_coffee$aftertaste),),
                                    numericInput("acidity_val",
                                                "Acidity",
                                                value = round(mean(df_coffee$acidity),2),
                                                min = min(df_coffee$acidity),
                                                max= max(df_coffee$acidity),),
                                    numericInput("body_val",
                                                "Body",
                                                value = round(mean(df_coffee$body),2),
                                                min = min(df_coffee$body),
                                                max= max(df_coffee$body),),
                                    numericInput("balance_val",
                                                "Balance",
                                                value = round(mean(df_coffee$balance),2),
                                                min = min(df_coffee$balance),
                                                max= max(df_coffee$balance),),
                                    numericInput("uniformity_val",
                                                "Uniformity",
                                                value = round(mean(df_coffee$uniformity),2),
                                                min = min(df_coffee$uniformity),
                                                max= max(df_coffee$uniformity),),
                                    numericInput("sweetness_val",
                                                "Sweetness",
                                                value = round(mean(df_coffee$sweetness),2),
                                                min = min(df_coffee$sweetness),
                                                max= max(df_coffee$sweetness),),
                                    numericInput("clean_val",
                                                "Clean Cup",
                                                value = round(mean(df_coffee$clean_cup),2),
                                                min = min(df_coffee$clean_cup),
                                                max= max(df_coffee$clean_cup),),
                                    numericInput("cupper_val",
                                                "Cupper Points",
                                                value = round(mean(df_coffee$cupper_points),2),
                                                min = min(df_coffee$cupper_points),
                                                max= max(df_coffee$cupper_points),),
                                    numericInput("moisture_val",
                                                "Moisture",
                                                value = round(mean(df_coffee$moisture),2),
                                                min = min(df_coffee$moisture),
                                                max= max(df_coffee$moisture),
                                                step =  .01,),
                                    numericInput("cat1_val",
                                                "Category One Defects",
                                                value = round(mean(df_coffee$category_one_defects),2),
                                                min = min(df_coffee$category_one_defects),
                                                max= max(df_coffee$category_one_defects),),
                                    numericInput("cat2_val",
                                                "Category Two Defects",
                                                value = round(mean(df_coffee$category_two_defects),2),
                                                min = min(df_coffee$category_two_defects),
                                                max= max(df_coffee$category_two_defects),),
                                    numericInput("quakers_val",
                                                "Quakers",
                                                value = round(mean(df_coffee$quakers),2),
                                                min = min(df_coffee$quakers),
                                                max= max(df_coffee$quakers),),
                                    numericInput("altitude_val",
                                                "Altitude",
                                                value = round(mean(df_coffee$altitude_mean_meters),2),
                                                min = min(df_coffee$altitude_mean_meters),
                                                max= max(df_coffee$altitude_mean_meters),
                                                step = 250,),
                                    selectInput("day_val",
                                                "Day",
                                                levels(df_coffee$day)),
                                    selectInput("process_val",
                                                "Processing Method",
                                                levels(df_coffee$processing_method)),
                                    selectInput("country_val",
                                                "Country of Origin",
                                                levels(df_coffee$country_of_origin)),
                                    selectInput("color_val",
                                                "Color",
                                                levels(df_coffee$color)),
                                    selectInput("year_val",
                                                "Year",
                                                levels(df_coffee$year)),
                                    selectInput("variety_val",
                                                "Variety",
                                                levels(df_coffee$variety)),
                                    solidHeader = TRUE,
                                    status = "primary",
                                    title = "Variable Input"
                                )),
                                column(width = 8,
                                fluidRow(
                                  box(width = 12,
                                    actionButton("start", "Predict!"),
                                    h1("Your Predicted Value for Total Cup Points:"),
                                    h1(verbatimTextOutput("predicted", placeholder = T)),
                                    solidHeader = TRUE,
                                    status = "primary",
                                    title = "Prediction"
                                  ),
                                )),
                        ),
                        tabItem(tabName = "data",
                          fluidRow(
                            box(
                              selectInput(
                                "selects",
                                label = "Select Continuous Variables to Include in your Data:",
                                choices =  names(select(df_coffee, where(is.numeric))),
                                multiple = TRUE,
                                selected = names(select(df_coffee, where(is.numeric)))),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Columns"
                            ),
                            box(
                              selectInput(
                                "filter_country",
                                label = "Select Country to Filter Data by:",
                                choices = levels(df_coffee$country_of_origin),
                                multiple = TRUE,
                                selected = levels(df_coffee$country_of_origin)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Country"
                            ),
                            box(
                              selectInput(
                                "filter_process",
                                label = "Select Processing Method to Filter Data by:",
                                choices = levels(df_coffee$processing_method),
                                multiple = TRUE,
                                selected = levels(df_coffee$processing_method)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Processing Method"
                            ),
                            box(
                              selectInput(
                                "filter_color",
                                label = "Select Color to Filter Data by:",
                                choices = levels(df_coffee$color),
                                multiple = TRUE,
                                selected = levels(df_coffee$color)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Color"
                            ),
                            box(
                              selectInput(
                                "filter_variety",
                                label = "Select Variety to Filter Data by:",
                                choices = levels(df_coffee$variety),
                                multiple = TRUE,
                                selected = levels(df_coffee$variety)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Variety"
                            ),
                            box(
                              selectInput(
                                "filter_day",
                                label = "Select Day to Filter Data by:",
                                choices = levels(df_coffee$day),
                                multiple = TRUE,
                                selected = levels(df_coffee$day)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Day"
                            ),
                            box(
                              selectInput(
                                "filter_year",
                                label = "Select Year to Filter Data by:",
                                choices = levels(df_coffee$year),
                                multiple = TRUE,
                                selected = levels(df_coffee$year)),
                              solidHeader = TRUE,
                              width = 3,
                              status = "primary",
                              title = "Year"
                            ),
                            downloadButton("download1","Download entire Table  as csv"),
                            box(
                              title = "Coffee Data Filtered by User",
                              dataTableOutput("data_table"),
                              width = 12
                            )
                          )  
                        )
                      )
))