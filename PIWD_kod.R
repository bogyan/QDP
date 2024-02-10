# Bogdan Yanovich 109072

# Biblioteki
library(readr)
library(dplyr)
library(rpart)
library(grid)
library(rpart.plot)
library(ROCR)
library(pROC)
library(MASS)
library(mlbench)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(caret)
library(Information)
library(ROSE)
#install.packages("fastDummies")
library(fastDummies)
# Wgranie zbioru danych
churn_data <- read_csv("Bank Customer Churn Prediction.csv")
#View(churn_data)
names(churn_data)
# Sprawdzimy, ile jest obserwacji, zmiennych oraz czy istnieją braki danych
nrow(churn_data) #10000 obserwacji
ncol(churn_data) #12 zmiennych, w tym 11 zmiennych objaśniających i zmienna objaśniana churn
str(churn_data) #opis danych
sum(is.na(churn_data)) # nie występują braki danych w zbiorze

# Warto usunąć zmienną customer_id, ponieważ nie możemy wyciągnąć żadnych wniosków (zmienna jest unikalna)
churn_data <- churn_data[, 2:12]
# W poniżsczej tabeli możemy sprawdzić min, max i inne dla zmiennych liczbowych
summary(churn_data)

# Analiza Danych
# Zmienna objaśniana Churn
barplot(table(churn_data$churn),
        main='Udział klasy negatywnej i pozytywnej zmiennej objaśnianej Churn', ylim=c(0,8000),
        col = rgb(0.2,0.4,0.6,0.6))
table(churn_data$churn)
# Zbiór danych jest miezbalansowany: 7963 obserwacji (80% zbioru danych) najeżą do klasy niegatywnej,
# a 2037 obserwacji (20%) należą do klasy pozytywnej

# Wykresy zmiennych kategorialnych
country <- ggplot(churn_data, aes(x = factor(country))) +
  geom_bar(fill="red", color = "black") +
  labs(title = "(1) Rozkład zmiennej country", x = "Państwo") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

gender <- ggplot(churn_data, aes(x = factor(gender))) +
  geom_bar(fill="#A2A76B", color = "black") +
  labs(title = "(2) Rozkład zmiennej gender", x = "Płeć") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

credit_card <- ggplot(churn_data, aes(x = factor(credit_card))) +
  geom_bar(fill="lightblue", color = "black") +
  labs(title = "(3) Rozkład zmiennej credit_card", x = "Posiadanie karty kredytowej") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

active_member <- ggplot(churn_data, aes(x = factor(active_member))) +
  geom_bar(fill="green", color = "black") +
  labs(title = "(4) Rozkład zmiennej active_member", x = "Aktywny użytkownik") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

grid.arrange(arrangeGrob(country, gender, credit_card, active_member, ncol = 2),
             top = textGrob("Rozkłady zmiennych kategorycznych", gp = gpar(fontsize = 16)))

# Jeszcze wykresy
ggplot(churn_data, aes(x = factor(products_number))) +
  geom_bar(fill = '#005D68', color = "black") +
  labs(title = "Rozkład zmiennej products_number", x = "Liczba produktów") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

table(churn_data$products_number)
ggplot(churn_data, aes(x = factor(tenure), fill = factor(tenure))) +
  geom_bar(fill = '#7FAEB3', color = "black") +
  labs(title = "Rozkład zmiennej tenure", x = "Długość posiadania konta w banku") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

# Zmienne numeryczne

credit_score <- ggplot(churn_data, aes(x = "credit_score", y = credit_score)) +
  geom_boxplot(fill="red", color = "black") +
  labs(title = "(1) Boxplot zmiennej credit_score", x = NULL, y = "Values") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

age <- ggplot(churn_data, aes(x = "age", y = age)) +
  geom_boxplot(fill="#A2A76B", color = "black") +
  labs(title = "(2) Boxplot zmiennej age", x = NULL, y = "Values") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

balance <- ggplot(churn_data, aes(x = "balance", y = balance)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "(3) Boxplot zmiennej balance", x = NULL, y = "Values") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

est_salary <- ggplot(churn_data, aes(x = "estimated_salary", y = estimated_salary)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "(4) Boxplot zmiennej estimated_salary", x = NULL, y = "Values") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"))

combined_plots2 <- grid.arrange(credit_score, age, balance, est_salary, ncol = 2,
                    top = textGrob("Rozkłady zmiennych numerycznych", gp = gpar(fontsize = 16)))

# Information Value
IV_before <- create_infotables(data = churn_data, y='churn')
IV_before$Summary

# Wykes zależności między zmienną gender a zmienną churn
ggplot(churn_data, aes(x=gender, fill=as.factor(churn))) +
  geom_bar(position = "dodge") +
  labs(title = "Wykes zależności między zmienną gender a zmienną churn",
       x = "gender",
       fill = "Churn") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

# Wykes zależności między zmienną credit_card a zmienną churn
ggplot(churn_data, aes(x=as.factor(credit_card), fill=as.factor(churn))) +
  geom_bar(position = "dodge") +
  labs(title = "Wykes zależności między zmienną credit_card a zmienną churn",
       x = "credit_card",
       fill = "Churn") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

# Wykes zależności między zmienną products_number a zmienną churn
ggplot(churn_data, aes(x=as.factor(products_number), fill=as.factor(churn))) +
  geom_bar(position = "stack") +
  labs(title = "Wykes zależności między zmienną products_number a zmienną churn",
       x = "products_number",
       fill = "Churn") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

table(churn_data$products_number)

ggplot(churn_data, aes(x=as.factor(country), fill=as.factor(churn))) +
  geom_bar(position = "stack") +
  labs(title = "Wykes zależności między zmienną coutries a zmienną churn",
       x = "Countries",
       fill = "Churn") +
  theme(panel.grid = element_line(color = "gray", linetype = "dashed"), plot.title = element_text(hjust = 0.5))

# Przeksztalcanie zmiennych
columns_to_factorize <- c("churn",'active_member','credit_card','country','gender')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_factorize), as.factor)

columns_to_int <- c('products_number','tenure')
churn_data <- churn_data %>%
  mutate_at(all_of(columns_to_int), as.integer)

# Korelacja zmiennych objaśniających (wybieramy tylko zmienne mierzalne)
churn_data_for_cor <- churn_data %>% 
  dplyr::select(-c('churn')) %>% 
  dplyr::select_if(is.numeric)

melted_cormat <- melt(round(cor(churn_data_for_cor), 2))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  scale_fill_gradient(low = "orange", high = "darkred") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Macierz korelacji zmiennych objaśniających')
# z macierzy korelacji możemy wywnioskować, żę
# istnieje tylko slaba ujemna korelacja pomiedzy liczba produktow a stanem konta

# Dummy variables
churn_data_ml <- churn_data %>%
  dummy_cols(select_columns = c("country", "gender", "products_number","tenure"), remove_first_dummy = TRUE)

churn_data_ml <- subset(churn_data_ml, select = -c(country, gender, products_number, tenure))
str(churn_data_ml)
View(churn_data_ml)
# Pozielimy zbiór danych na czesc treningową i testową
set.seed(42)
index <- sample(nrow(churn_data_ml), 0.75 * nrow(churn_data_ml))
train <- churn_data_ml[index, ]
test <- churn_data_ml[-index, ]

#Undersampling
churn_balanced_under <- ovun.sample(churn ~ ., data = train, method = "under", N = 3078, seed=42)$data
table(churn_balanced_under$churn)


# Drzewo klasyfikacyjne
class_tree <- rpart(churn ~ ., 
                    data = churn_balanced_under,
                    method = "class", control = rpart.control(minbucket = 100))

rpart.plot(class_tree, under = FALSE,tweak = 1.2, fallen.leaves = TRUE)
class_tree$variable.importance

# Weryfikacja jakości klasyfikacji -  MACIERZ POMYŁEK  
confmat_tree <- table(predict(class_tree, new = test, type = "class"), test$churn)
confmat_tree

# Regresja logistyczna

under_reg_log <- glm(churn ~ ., 
                     data = churn_balanced_under, 
                     family = binomial
)

summary(under_reg_log)
confmat_log <- table(ifelse(predict(under_reg_log, new = churn_balanced_under, type = "response") > 0.5, 1, 0), churn_balanced_under$churn)
confmat_log

# Statystyki pochondne
CM <- list()
CM[["tree"]] <- confmat_tree
CM[["under_reg_log"]] <- confmat_log

statystyki <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

lapply(CM, statystyki)
sapply(CM, statystyki)

# Krzywa ROC
preds <- list()
### Drzewa
preds[["drzewo klasyfikacyjne"]] <- as.vector(predict(class_tree, newdata = test)[, 2])
### Regresje
preds[["regresja logistyczna"]] <- as.vector(predict(under_reg_log, newdata = test, type = "response"))

## krzywa ROC (Receiver Operating Characteristic) - potrzebuje "ciaglej" prognozy
for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], test$churn), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)
title(main = "Krzywe ROC dla drzewa klasyfikacyjnego oraz regresji logistycznej", cex.main = 1.4)

# AUC (Area Under Curve) - pole pod krzywa ROC
for (i in 1:length(preds)){
  cat(names(preds)[i], ": ", performance(prediction(preds[[i]], test$churn), "auc")@y.values[[1]], "\n")
}

