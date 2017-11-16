#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinydashboard)
library(tidyverse)
library(dplyr)
library(plotly)
library(janitor)
library(forcats)
library(shiny)
library(rsconnect)

instacart_data = read_csv("../data/instacart_train_data.csv.zip") %>%
  clean_names()
departments = instacart_data %>% distinct(department) %>% pull()

ui <- dashboardPage(
  
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        checkboxGroupInput("department_scatter", label = h3("Department"), 
                           choices = departments, selected = "produce")
      )
    )
  )
)

server <- function(input, output) {
  
  instacart_data_order = instacart_data %>%
    group_by(product_name, department) %>%
    summarize(num_order = n())
  
  instacart_data_reorder = instacart_data %>%
    filter(reordered == 1) %>%
    group_by(product_name) %>%
    summarize(num_reorder = n())
  
  instacart_data_order_reorder = left_join(instacart_data_order, instacart_data_reorder, by = "product_name") %>%
    replace_na(list(num_reorder = 0))
  
  
  output$plot1 = renderPlotly({
    instacart_data_order_reorder %>% 
    filter(department %in% input$department_scatter) %>%
    plot_ly(x = ~num_order, y = ~num_reorder, type = "scatter", color = ~department, alpha = .5)})
  
  
}

shinyApp(ui, server)