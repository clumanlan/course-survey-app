#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(argonDash)
library(argonR)
library(htmltools)
library(magrittr)
library(highcharter)
library(tidyverse)
library(tidytext)
library(plotly)
library(ggraph)
library(shinyWidgets)
library(visNetwork)
library(igraph)
library(DT)
library(reactable)
library(googlesheets4)

    
    
ui <- (argonDashPage(
    title = "Course Survey Analysis",
    description = "Making Using Machine Learning for Surveys easy",
    author = "Carlyle Lumanlan",
    sidebar = argonDashSidebar(
        vertical = TRUE,
        skin = 'light',
        background = "Secondary",
        size = "md",
        side = "left",
        id = "sidebar",
        brand_url = "https://www.tidymodels.org/start/tuning/",
        brand_logo = "logo.png",
        argonSidebarMenu(title = "Main Menu",
            # Define the tabs
            
            argonSidebarItem(
                tabName = 'data_upload',
                icon =  argonIcon("atom"),
                icon_color = '#4298b5',
                'Survey Upload'
            ),
            argonSidebarItem(
                tabName = 'quant_analysis',
                icon =  argonIcon("chart-bar-32"),
                icon_color = '#4298b5',
                'Quantitative Question Analysis'
            ),
            argonSidebarItem(
                tabName = 'qual_analysis',
                icon = argonIcon("chat-round"),
                icon_color = '#4298b5',
                'Qualitative Question Analysis'
            )
        )),
    
    
    header = argonDashHeader(
        separator = TRUE,
        gradient = TRUE,
        color = 'primary',
        bottom_padding = 6,
        h4('Course Survey Analysis', style = 'color:white;text-align:center;font-size:2em;')
    ),
    
    body = argonDash::argonDashBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        argonTabItems(
            argonTabItem(
                
                #DATA UPLOAD PAGE -------------------------------------------------------------------------------
                
                tabName = 'data_upload',
                        argonRow(
                            argonCard(
                                title = "Data Upload",
                                icon = argonIcon("cloud-download-95"),
                                width = 6,
                                wellPanel(
                                    h2("Instructions"),
                                    HTML(paste("<p>This app was created so instructors with multiple classes or Learning Development Managers with multiple instructors can easily evaluate their survey responses.",
                                               "The Course Evaluation Survey is based on <a href = 'https://teaching.berkeley.edu/course-evaluations-question-bank'>UC Berkeley's Template</a>.",
                                               "<br><br/>",
                                               "After conducting a survey through Google's Form, download the results as a CSV.",
                                               "The survey results downloaded should mirror this <a href = 'https://docs.google.com/spreadsheets/d/1GJWlW65VcXtzfDKE_y7QanZfuwy9zz32MR1czM5jWTk/edit?usp=sharing'>Google Sheet</a>.", 
                                               "<br><br/>",
                                               "The CSV file can then be uploaded below.",
                                               "Happy Teaching!</p>")),
                                    uiOutput('upload_data')
                                )),
                            argonCard(
                                title = "Survey Response Bank",
                                icon = argonIcon("bullet-list-67"),
                                reactableOutput("instructor_class_list"),
                                width = 6
                            )),

                           
                    argonRow(
                        argonCard(
                            title = "Response Timeline",
                            highchartOutput("response_timeline"),
                            icon = argonIcon("calendar-grid-58"),
                            width = 6
                        ),
                        argonInfoCard(
                            value = textOutput("number_of_classes"),
                            title = "Total Number of Classes",
                            hover_lift = TRUE,
                            icon = argonIcon("atom")
                        ),
                        argonInfoCard(
                            value = textOutput("number_of_responses"),
                            title = "Total Number of Responses",
                            hover_lift = TRUE,
                            icon = argonIcon("circle-08")
                        )
                        )   
                    ),
            
            #Quantitative ANALYSIS PAGE  ---------------------------------------------------------------------------------
            argonTabItem(
             tabName = 'quant_analysis',
             argonRow(
                 argonCard(width = 4,
                        wellPanel(
                                uiOutput('quant_question_input'),
                                uiOutput('quant_intsructor_filter'),
                                uiOutput('quant_class_filter'),
                                uiOutput('quant_experience_filter')
                                )
                    ),
                 argonCard(width = 8,
                     highchartOutput("quant_boxplot")
                     )
                 ),
              argonRow(
                  argonInfoCard(
                      value = textOutput("quant_average"),
                      title = "Average Rating",
                      hover_lift = TRUE,
                      icon = argonIcon("sound-wave"),
                      width = 2
                  ),
                  argonInfoCard(
                      value = textOutput("quant_median"),
                      title = "Median Rating",
                      hover_lift = TRUE,
                      icon = argonIcon("fat-delete"),
                      width = 2
                  ),
                  argonCard(
                      highchartOutput("quant_rating_graph"),
                      width = 8
                  )
              )),
            
            # QUALITATIVE ANALYSIS PAGE -----------------------------------------------------------------------------------
            argonTabItem(
                tabName = 'qual_analysis',
                argonRow(
                    argonCard(
                        width = 4,
                        uiOutput("qual_question_filter")
                    ),
                    argonCard(
                        width = 4,
                        uiOutput("qual_instructor_filter")
                    ),
                    argonCard(
                        width = 4,
                        numericInput("word_count", "Choose Number of Words to View:", 10, min = 1, max = 30)
                        
                    )
                ),
                argonRow(
                    argonCard(status = "primary",
                              gradient = TRUE,
                              width = 4,
                              title = "Most Used Words",
                              icon = argonIcon("check-bold"),
                        highchartOutput('most_used_word_plot')
                    ),
                    argonCard(
                            width = 8,
                            title = "Word Count by Sentiment",
                            plotlyOutput("sentiment_word_plot")
                        )),
                argonRow(
                    argonH1(
                        display = 3,
                        center = TRUE,
                        "Words Most Used Together"),
                    visNetworkOutput("network", height = "1000px") 
                ),
                argonRow(
                    dataTableOutput("bigram_table")
                )
                )
            )
        )
    )
)
