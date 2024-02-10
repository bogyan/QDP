library(shiny)
library(readr) 
library(ggplot2) 
library(forcats) 
library(tidyverse) 

churn_data <- read_csv("Bank Customer Churn Prediction.csv")

columns_to_factorize <- c("churn",'active_member','credit_card','country','gender')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_factorize), as.factor)

columns_to_int <- c('products_number','tenure','balance')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_int), as.integer)


shinyUI(fluidPage(
  
  sidebarLayout( 
    
    sidebarPanel(
      
      selectInput(inputId = "wybraneP",  
                  
                  label = "Wybierz państwo do analizy", 
                  
                  choices = sort(levels(churn_data$country)), 
                  
                  selected = "France"),
      
      selectInput("histogram_T", "Wybierz typ wykresu supkowego",
                  choices = c("dodge" = "dodge", "stacked" = "stacked", 
                              "fill" = "fill", "identity" = "identity"),
                  selected = "dodge")
      
      
      
    ), 
    
    mainPanel( 
      
      p("Wykres gęstości wieku osób w zależności od państwa"), 
      
      plotOutput("wykres"),
      
      
      p("Wykres słupkowy zmiennej products_number w zależnosci od państwa"), 
      
      plotOutput("wykres2") 
      
      
    ) 
    
  ) 
  
)) 

