---
title: "Airbnb"
output:
  pdf_document: default
  html_document: default
---
Airbnb Project 

Description:

This is a listing of over 25,000 Airbnb rentals in New York City. The goal of this competition is to predict the price for a rental using over 90 variables on the property, host, and past reviews.

Goal:

To predict price of an Airbnb rental. Generate a prediction for each id in scoringData.csv.
Metric

Submissions will be evaluated based on RMSE (root mean squared error) (Wikipedia). Lower the RMSE, better the model.



Install Packages

```{r}
install.packages('DescTools')
install.packages('mice')
install.packages('corrplot')
install.packages('glmnet')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('caret')

library(DescTools)
library(mice)
library(corrplot)
library(glmnet)
library(dplyr)
library(tidyverse)
library(caret)
```

Import data
```{r}
analysis_tbl <- read_csv("analysisData.csv") 
scoring_tbl <- read_csv("scoringData.csv")
```

Explore data
```{r}
str(analysis_tbl)
summary(analysis_tbl)
glimpse(analysis_tbl)
```

categorical casting (using useful variables)
```{r}
analysis_tbl_clean <- analysis_tbl %>% 
  mutate(price = gsub("\\$", "", price), #remove $ signs which turned data from character to numeric
         price = as.numeric(price),
         weekly_price = as.numeric(gsub("\\$", "", weekly_price)),
         monthly_price = as.numeric(gsub("\\$", "", monthly_price)),
         security_deposit = as.numeric(gsub("\\$", "", security_deposit)),
         cleaning_fee = as.numeric(gsub("\\$", "", cleaning_fee)),
         extra_people = as.numeric(gsub("\\$", "", extra_people)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate))) %>% 
  select(price, weekly_price, monthly_price, security_deposit, cleaning_fee, extra_people,
         host_response_rate, host_listings_count, host_total_listings_count,accommodates,
         bathrooms, bedrooms, beds, square_feet, minimum_nights, maximum_nights, 
         number_of_reviews, review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
         review_scores_checkin, review_scores_communication,review_scores_location, 
         review_scores_value, calculated_host_listings_count, reviews_per_month)

scoring_tbl_clean <- scoring_tbl %>% 
  mutate(weekly_price = as.numeric(gsub("\\$", "", weekly_price)),
         monthly_price = as.numeric(gsub("\\$", "", monthly_price)),
         security_deposit = as.numeric(gsub("\\$", "", security_deposit)),
         cleaning_fee = as.numeric(gsub("\\$", "", cleaning_fee)),
         extra_people = as.numeric(gsub("\\$", "", extra_people)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate)))%>% 
  select(weekly_price, monthly_price, security_deposit, cleaning_fee, extra_people,
         host_response_rate, host_listings_count, host_total_listings_count,accommodates,
         bathrooms, bedrooms, beds, square_feet, minimum_nights, maximum_nights, 
         number_of_reviews, review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
         review_scores_checkin, review_scores_communication,review_scores_location, 
         review_scores_value, calculated_host_listings_count, reviews_per_month)
```

implement missing values imputation
```{r}
analysis_tbl_mod <- mice(analysis_tbl_clean, method="mean", m = 2)  # perform mice imputation
analysis_tbl_mod_output <- complete(analysis_tbl_mod)  # generate the completed data.

anyNA(analysis_tbl_mod_output)

analysis_tbl_mod_output <- analysis_tbl_mod_output[complete.cases(analysis_tbl_mod_output),]

scoring_tbl_mod <- mice(scoring_tbl_clean, method="mean", m = 2)  # perform mice imputation
scoring_tbl_mod_output <- complete(scoring_tbl_mod)  # generate the completed data.

anyNA(scoring_tbl_mod_output)

model1 <- lm(price ~ ., data = analysis_tbl_mod_output,
             na.action = "na.exclude")
```


stepwise regression model
```{r}


model2 <- step(model1, direction = "both")

summary(model2)
```

visualize the important variables
```{r}

var_imp <- varImp(model2)

variables <- row.names(var_imp)
effect_size <- var_imp$Overall

var_imp_tbl <- tibble(variables, effect_size)


ggplot(data = var_imp_tbl, mapping = aes(x = reorder(variables,effect_size), 
                                         y = effect_size)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  coord_flip() +
  theme_bw() +
  labs(x = "Predictors",
       y = "Importance",
       title = "variable importance model 2") 
```

feature selection - create best 6 predictor model
```{r}
subsets = leaps::regsubsets(price~weekly_price + security_deposit + cleaning_fee + extra_people + 
                              host_response_rate + host_listings_count + accommodates + bathrooms +
                              bedrooms +  beds + minimum_nights + number_of_reviews + review_scores_rating                               +review_scores_cleanliness + review_scores_checkin +            review_scores_communication +review_scores_location + review_scores_value + reviews_per_month ,data=analysis_tbl_mod_output, nvmax=6)

summary(subsets)

which.min(summary(subsets)$cp)
coef(subsets,which.min(summary(subsets)$cp))
```

Visualize best 6 predictor model
```{r}
var_imp_model8 <- varImp(model8)

variables_model8 <- row.names(var_imp_model8)
effect_size_model8 <- var_imp_model8$Overall

var_imp_tbl_model8 <- tibble(variables_model8, effect_size_model8)


ggplot(data = var_imp_tbl_model8, mapping = aes(x = reorder(variables_model8,effect_size_model8), 
                                         y = effect_size_model8)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  coord_flip() +
  theme_bw() +
  labs(x = "Predictors",
       y = "Importance",
       title = "variable importance") +
  geom_label(aes(label = round(effect_size_model8, 2)), size = 3)

```
