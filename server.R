library(readr) 
library(ggplot2) 
library(forcats) 
library(tidyverse) 

churn_data <- read_csv("Bank Customer Churn Prediction.csv")

columns_to_factorize <- c("churn",'active_member','credit_card','country','gender')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_factorize), as.factor)

columns_to_int <- c('products_number','tenure','balance','estimated_salary')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_int), as.integer)


shinyServer(function(input, output) { 
  
  output$wykres = renderPlot({ 
    churn_D <- churn_data[churn_data$country == input$wybraneP, ] 
    ggplot(churn_D, aes(x = age)) +
      geom_density(color="darkblue", fill="lightblue") }) 
  
  
  output$wykres2 = renderPlot({ 
    
    churn_D <- churn_data[churn_data$country == input$wybraneP, ]
    
    if (input$histogram_T == "dodge") {
      ggplot(churn_D, aes(x=products_number, fill=as.factor(churn))) +
        geom_bar(position = "dodge") +
        labs(x = "products_number",
             fill = "Churn") +
        theme(plot.title = element_text(hjust = 0.5))

    } else if (input$histogram_T == "stacked") {
      ggplot(churn_D, aes(x=products_number, fill=as.factor(churn))) +
        geom_bar(position = "stack") +
        labs(x = "products_number",
             fill = "Churn") +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$histogram_T == "fill") {
      ggplot(churn_D, aes(x=products_number, fill=as.factor(churn))) +
        geom_bar(position = "fill") +
        labs(x = "products_number",
             fill = "Churn") +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$histogram_T == "identity") {
      ggplot(churn_D, aes(x=products_number, fill=as.factor(churn))) +
        geom_bar(position = "Identity") +
        labs(x = "products_number",
             fill = "Churn") +
        theme(plot.title = element_text(hjust = 0.5))
    } 
    
  }) 
  
}) 