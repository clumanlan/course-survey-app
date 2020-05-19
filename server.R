#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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



shinyServer(function(input, output) {
  
  
  course_eval <- reactive({
    
    
    if (is.null(input$file1)){
      
      df <- read_csv("data/course_evaluation_data.csv") %>%
        mutate(Timestamp = as.Date(Timestamp)) %>%
        rename(Class = 2)
      
    } else
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath, check.names=FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    
    
  })
  
  course_eval_quant <- reactive({
    
    course_eval()[,c(1:5,6:15)] %>%
      gather(question, response, -c(1:5)) %>%
      rename(class = 2, instructor = 3, experience = 5) 
    
  })
  
  course_eval_qual <- reactive ({
    
    question_text_data <- course_eval()[,c(1:5,19:21)] %>%
      gather(key = "question", value = "response", -c(1:5)) %>%
      rename(class = 2, instructor = 3, experience = 5) 
    
  })

  
  bigram_df <- reactive({
    
    count_bigrams <- function(dataset, response) {
      dataset %>%
        unnest_tokens(bigram, response, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !is.na(word1),
               !is.na(word2)) %>%
        count(word1, word2, sort = TRUE)%>%
        top_n(20)  
    }
    
    count_bigrams(course_eval_qual())
    
  })
  
 
  
# Data Upload page ----------------------------------------------------------------------------------------------------

  
  output$upload_data <- renderUI({
    
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    
  })
  
  output$choose_data_type <- renderUI({
    
    checkboxInput(inputId, label, value = FALSE, width = NULL)
    
    
  })
  
  output$number_of_classes <- renderText({
    
   length(unique(course_eval()$Class))
    
  })
  
  output$number_of_responses <- renderText({
    
    nrow(course_eval())
    
  })
  
  output$response_timeline <- renderHighchart({
    
    course_eval() %>%
      count(Timestamp) %>%
      hchart("line", hcaes(x = Timestamp, y = n))
    
  })
  
  output$question_count_summary <- renderHighchart2({
    temp_df() %>%
      count(variable) %>%
      hchart("bar", hcaes(x = variable, y = n))  
    
  })
  
  
  
  output$instructor_class_list <- renderReactable({
    
    table <- course_eval() %>% 
      group_by(Instructor, Class) %>%
      summarize("Number of Responses" = n()) 
    
    reactable(table)
    
  })
  
  
# Quantitative Question Analysis Page -----------------------------------------------------------------------------
  
  
  
  course_eval_quant_filtered <- reactive({
    
    course_eval_quant() %>%
      filter(question %in% input$quant_question_input, instructor %in% input$quant_intsructor_filter, class %in% input$quant_class_filter, 
             experience %in% input$quant_experience_filter)
    
  })
  
  output$quant_question_input <- renderUI({
    
    question <- unique(course_eval_quant()$question)
    
    pickerInput("quant_question_input","Choose Question(s)", choices = question, selected = question,
                options = list(`actions-box` = TRUE), multiple = TRUE)
  
  })
  
  output$quant_intsructor_filter <- renderUI({
    
    instructors <- unique(course_eval_quant()$instructor)
    
    pickerInput("quant_intsructor_filter", label = h4("Choose Instructor(s)"), choices = instructors, selected = instructors,
                  options = list(`actions-box` = TRUE),multiple = TRUE)

  })

  output$quant_class_filter <- renderUI({
    
    class <- unique(course_eval_quant()$class)
    
    pickerInput("quant_class_filter", label = h4("Choose Class(es)"), choices = class, selected = class,
                options = list(`actions-box` = TRUE),multiple = TRUE)
   
    
  })
  
  output$quant_experience_filter <- renderUI({
    
    experience <- unique(course_eval_quant()$experience)
    
    pickerInput("quant_experience_filter", label = h4("Choose Experience Level"), choices = experience, selected = experience,
                options = list(`actions-box` = TRUE),multiple = TRUE)
 
  })
  
  output$quant_average <- renderText({
    
    round(mean(course_eval_quant_filtered()$response, na.rm = TRUE),2)
    
  })
  
  output$quant_median <- renderText({
    
    median(course_eval_quant_filtered()$response)
    
  })
  
  
  output$quant_boxplot <- renderHighchart({
    
    hcboxplot(x = course_eval_quant_filtered()$response, var = course_eval_quant_filtered()$question,
              name = "Length", color = "#2980b9") %>%
    hc_yAxis(min = 0)
    
  })
  
  output$quant_rating_graph <- renderHighchart({
    
    hchart(as.factor(course_eval_quant_filtered()$response), type = "column") 
    
  })
  
  
# Qualitative Question Analysis Page ------------------------------------------------------------------------------------------
  
  course_eval_qual_filtered <- reactive ({
    course_eval_qual() %>%
      filter(question %in% input$qual_question_filter, instructor %in% input$qual_instructor_filter)
    
  })
  
  course_tidy <- reactive({
    
    tidy_reviews_filtered <- course_eval_qual_filtered() %>%
      unnest_tokens(word, response) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
  })
  
  output$qual_question_filter <- renderUI({
    
    questions <- unique(course_eval_qual()$question)
    
    pickerInput("qual_question_filter", label = h4("Choose Question(s)"), choices = questions, selected = questions,
                options = list(`actions-box` = TRUE),multiple = TRUE)
    
  })
  
  output$qual_instructor_filter <- renderUI({
    
    instructors <- unique(course_eval_qual()$instructor)
    
    pickerInput("qual_instructor_filter", label = h4("Choose Instructor(s)"), choices = instructors, selected = instructors,
                options = list(`actions-box` = TRUE),multiple = TRUE)
    
  })
  
  
  word_count_reactive <- reactive({ input$word_count })


  
  output$most_used_word_plot <- renderHighchart({
    
    course_tidy() %>%
      top_n(word_count_reactive()) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      hchart("bar", hcaes(x = word, y = n)) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(tickInterval = 1, 
               title = list(text = "Count", value =1)) 
    
    
  })
  
  output$sentiment_word_plot <- renderPlotly({
    
    p <- course_tidy() %>%
      group_by(sentiment) %>%
      top_n(word_count_reactive()) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(x = word, y = n, fill = sentiment)) +
      geom_col() +
      coord_flip() +
      labs(x = "",
           y = "") +
      facet_wrap(~sentiment, scales = "free") +
      theme(legend.position = "bottom") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(showlegend = FALSE)
    
  })
  
  
  # network graph ------------------------------------
  
  graph2 <- reactive({
    
    graph_from_data_frame(bigram_df())
    
  })
  
  
  
  output$plot <- renderPlot({
    
    set.seed(2017)
    p <- ggraph(graph2, layout = "nicely") + 
      geom_edge_link() + 
      geom_node_point()
    
    plot_df <- ggplot_build(p)
    
    coords <- plot_df$data[[2]]
    
    p
                            
                            })
  

  output$network <- renderVisNetwork({
    
    igraph_network <- graph_from_data_frame(bigram_df())
    
    # minimal example
    data <- toVisNetworkData(igraph_network)
    visNetwork(nodes = data$nodes, edges = data$edges, height = "800px")
  })
  
  
  output$bigram_table <- renderDataTable({
    
    DT::datatable({
    
      bigram_df() %>%
      rename("Word 1" = word1, "Word 2" = word2, "Count" = n) 
      
    })
    
  })
  
 
  
  
            
}
)
  
