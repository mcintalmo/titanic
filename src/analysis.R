# tidyverse for data manipulation
# ggplot2 for prettier presentation of data
# mice for imputation
# caret for machine learning
# glmnet, ranger for machine learning algorithms controlled by caret
# ROCR to more easily implement the AUC ROC method
# ggrepel for text data visualization
packages <- c("tidyverse", "ggplot2", "mice", "caret", "glmnet", "ranger", "ROCR", "ggrepel")

# load all packages
# installs any packages not present before loading
sapply(packages, function(package){  
  if (!require(package, character.only = TRUE))
    {
    install.packages(pkgs = package, repos = "http://cran.r-project.org")
    require(package, character.only = TRUE)
    }
  })

rm(packages)

# load the data
source(file.path(getwd(), "src", "load_data.R"))


####
# Data Summary
####
numeric_summary <-
  titanic %>%
  summarise_if(is.numeric, list(min, max, mean, sd), na.rm = TRUE) %>%
  pivot_longer(cols = everything(),
               names_pattern = "^(.*)_(fn\\d)$",
               names_to = c("Variable", "function")) %>%
  pivot_wider(names_from = "function") %>%
  rename(Min = fn1, Max = fn2, Mean = fn3, SD = fn4)

sex_summary <- 
  titanic %>%
  summarize(male = sum(sex == "male"),
            female = sum(sex == "female"),
            other = sum(!(sex %in% c("male", "female"))),
            total = n())


#### 
# Visualizing the Data
####

age_histogram <- 
  titanic %>%
  ggplot(aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age",
       y = "Count")

age_surival_rate <- 
  titanic %>%
  ggplot(aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Age",
       y = "Count")

sib_sb_histogram <- 
  titanic %>%
  ggplot(aes(x = sib_sp, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Number of Siblings and Spouses",
       y = "Count")

sib_sb_survival_rate <- 
  titanic %>%
  ggplot(aes(x = sib_sp, fill = survived)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Number of Siblings and Spouses",
       y = "Count")

par_ch_histogram <- 
  titanic %>%
  ggplot(aes(x = par_ch, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Number of Parents and Children",
       y = "Count")

par_ch_survival_rate <- 
  titanic %>%
  ggplot(aes(x = par_ch, fill = survived)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Number of Parents and Children",
       y = "Count")

fare_histogram <- 
  titanic %>%
  ggplot(aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 10) +
  labs(x = "Ticket Fare",
      y = "Count")

class_bar <- 
  titanic %>%
  ggplot(aes(x = class, fill = survived)) +
  geom_bar() +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("1st", "2nd", "3rd")) +
  labs(x = "Class", 
       y = "Count")

sex_bar <-
  titanic %>%
  ggplot(aes(x = sex, fill = survived)) +
  geom_bar() +
  scale_x_discrete(breaks = c("female", "male"),
                   labels = c("Female", "Male")) +
  labs(x = "Sex", 
       y = "Count")

embarked_bar <- 
  titanic %>%
  group_by(embarked, survived) %>%
  tally() %>%
  ggplot(aes(x = reorder(embarked, -n), y = n, fill = survived)) +
  geom_col() +
  scale_x_discrete(breaks = c("", "C", "Q", "S"),
                   labels = c("NA", "Cherbourg", "Queenstown", "South Hampton")) +
  labs(x = "Embark Location",
       y = "Count")


##############################
# Further breaking down variables
# Attempting to split names via regex
# Work necessary is -not- worth the reward. Use of backslashes, parenthesis, 
#  and quotations for nick names and maidens name is tough

# last_name_regex <- "^([^,]+),\\s"
# title_regex <- "([^.]+)\\.\\s"
# first_name_regex <- "([^\\s]*)\\s?" # Not all passengers have listed first names
# middle_name_regex <- "([^\\s]*)\\s?"
# maiden_name_regex <- "\\(?([^\\)]*)\\)?\\s?" # Needs to be further parsed. Shouldn't include nick names
# nick_name_regex <- "(\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)?\")$" # Needs work
# 
# titanic %>%
#   extract(col = name, 
#           into = c("last_name", "title", "first_name", "middle_name", "maiden_name", "nick_name"), 
#           regex = paste0(last_name_regex, title_regex, first_name_regex, middle_name_regex, maiden_name_regex, nick_name_regex),
#           remove = FALSE) %>%
#   select(-survived, -age, -sib_sp, -par_ch, -ticket, -fare, -cabin) %>%
#   print(n = 150)

#####
# Just getting last name, relationships may be valuable
last_name_regex <- "^([^,]*),"
title_regex <- "\\s([^\\.]*)\\."
first_name_regex <- "\\s([^\\s.]*)\\s*"
full_regex <- paste0(last_name_regex, title_regex, first_name_regex)
last_names <- titanic %>%
  extract(col = name, into = c("last_name", "title", "first_name"), regex = full_regex, remove = FALSE) %>%
  select(id, last_name, title, first_name)

#ordered by count
title_histogram <-
  titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(title) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(title, -count), fill = survived)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Title",
       y = "Count")

# Combine values that only appear once
last_names <-
  last_names %>%
  group_by(title) %>%
  mutate(title = ifelse(n() < 2, "Other", title)) %>%
  ungroup()

#ordered by survival rate
title_survival_rate <-
  titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(title) %>%
  mutate(rate = mean(survived == TRUE)) %>%
  ggplot(aes(x = reorder(title, -rate), fill = survived)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Title",
       y = "Count")

# compare to par/ch and sib/sp columns
last_name_histogram <-
  titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(last_name) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = count, fill = survived)) +
  geom_bar() +
  labs(x = "Number of Other Passengers with Last Name",
       y = "Count")

last_name_survival_rate <- 
  titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(last_name) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = count, fill = survived)) +
  geom_bar(position = "fill") +
  labs(x = "Number of Other Passengers with Last Name",
       y = "Survival Rate")

#### 
# Extracting cabin info
cabins <- titanic %>%
  extract(col = cabin, 
          into = c("cab_let_1", "cab_num_1",
                   "cab_let_2", "cab_num_2",
                   "cab_let_3", "cab_num_3",
                   "cab_let_4", "cab_num_4"),
          regex = "^([A-Z])(\\d+)\\s?(?:([A-Z])(\\d+))?\\s?(?:([A-Z])(\\d+))?\\s?(?:([A-Z])(\\d+))?$",
          remove = FALSE) %>%
  mutate(cab_num_1 = as.numeric(cab_num_1), 
         cab_num_2 = as.numeric(cab_num_2),
         cab_num_3 = as.numeric(cab_num_3),
         cab_num_4 = as.numeric(cab_num_4)) %>%
  rowwise() %>%
  mutate(cab_letter = cab_let_1) %>%
  mutate(med_cab_num = median(c_across(cols = c(cab_num_1, cab_num_2, cab_num_3, cab_num_4)), na.rm = TRUE)) %>%
  mutate(num_cab = sum(as.logical(c_across(cols = c(cab_num_1, cab_num_2, cab_num_3, cab_num_4))), na.rm = TRUE)) %>%
  select(id, cab_letter, med_cab_num, num_cab)

# ordered by letter
cabin_histogram <- 
  titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(cab_letter, -count), fill = survived)) +
  geom_bar()

# ordered by survival rate
cabin_survial_rate <- 
  titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  mutate(rate = mean(survived == TRUE)) %>%
  ggplot(aes(x = reorder(cab_letter, -rate), fill = survived)) +
  geom_bar(position = "fill")

# faceted grid by cabin for obvious patterns
cabin_histogram_facet <- 
  titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  ggplot(aes(x = med_cab_num, fill = survived)) +
  geom_histogram(binwidth = 2) +
  facet_grid(rows = vars(cab_letter))


#### 
# Missing Values
# Better demonstrated by the md.pattern function from the mice package
# titanic %>%
#   summarize(across(everything(), is.na)) %>%
#   summarize(across(everything(), sum))

# might need to be run specifically in RMD to get the plot output
missing_observations_table <-
  titanic %>% 
  md.pattern(rotate.names = TRUE) # from mice

missing_data <- 
  titanic %>%
  mice(m = 5)
  
#### Putting it all together
# Renaming factor levels to avoid problems with different caret functions
titanic <- 
  missing_data %>%
  complete(5) %>%
  as_tibble() %>%
  # Add cabin information
  left_join(cabins) %>%
  mutate(cab_letter = factor(cab_letter)) %>%
  mutate(cab_letter = addNA(cab_letter)) %>%
  select(-cabin, -ticket, -med_cab_num) %>%
  # add last names
  left_join(last_names) %>%
  mutate(title = factor(title),
         last_name = factor(last_name)) %>%
  select(-name, -first_name, -last_name) %>%
  # rename factors that will cause problems
  mutate(survived = recode_factor(survived, "TRUE" = "T", "FALSE" = "F"),
         class = recode_factor(class, "1" = "One", "2" = "Two", "3" = "Three")) %>%
  select(-id)


####
# splitting the titanic data into a train set and a test set
set.seed(1)
test_index <- createDataPartition(titanic$survived, p = 0.2, list = FALSE)
test_set <- titanic %>% 
  slice(test_index)
train_set <- titanic %>%
  slice(-test_index)


#####
# Logistic regression
train_control = trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 5,
                             search = "random",
                             summaryFunction = twoClassSummary, 
                             classProbs = TRUE,
                             savePredictions = TRUE)

fit_logistic <- train(survived ~ ., 
                      method = "glmnet", 
                      trControl = train_control,
                      data = train_set, 
                      family = "binomial",
                      type.measure = "auc")

# Variable importance (note heavy weight on some titles)
logistic_variable_importance <-
  fit_logistic %>%
  varImp() %>%
  ggplot()

auc.tmp <- predict(fit_logistic, newdata = test_set, type = "prob") %>%
  pull(T) %>%
  prediction(labels = test_set$survived) %>%
  performance("auc")
roc_logistic <- auc.tmp@y.values[[1]]

pred_logistic <- predict(fit_logistic, newdata = test_set)
confusion_logistic <- confusionMatrix(data = pred_logistic, reference = test_set$survived)

results <- tibble(method = "Logistic Regression",
                  roc = roc_logistic,
                  accuracy = confusion_logistic$overall[["Accuracy"]])


#####
# Random Forest
set.seed(1)
train_control <- trainControl(method = "repeatedcv", 
                             number = 5,
                             repeats = 3,
                             summaryFunction = twoClassSummary, 
                             classProbs = TRUE,
                             savePredictions = TRUE)

tuneGrid <- expand_grid(
  mtry =  c(1, 5, 10, 25, 50, 100),
  splitrule = "gini",
  min.node.size = 10)

fit_rf <- train(survived ~ .,
               method = "ranger",
               importance = "impurity",
               num.trees = 500,
               metric = "ROC",
               trControl = train_control,
               data = train_set)

# Importance
rf_variable_importance <-
  fit_rf %>%
  varImp() %>%
  ggplot()

auc.tmp <- predict(fit_rf, newdata = test_set, type = "prob") %>%
  pull(T) %>%
  prediction(labels = test_set$survived) %>%
  performance("auc")
roc_rf <- auc.tmp@y.values[[1]]

pred_rf <- predict(fit_rf, newdata = test_set)
confusion_rf <- confusionMatrix(data = pred_rf, reference = test_set$survived)

results <- results %>%
  add_row(method = "Random Forest",
          roc = roc_rf,
          accuracy = confusion_rf$overall[["Accuracy"]])


####
# Ensemble

make_prediction <- function(method, test_data_set){
  set.seed(1)
  train_control = trainControl(method = "cv",
                               summaryFunction = twoClassSummary, 
                               classProbs = TRUE,
                               savePredictions = TRUE)
  fit <- train(survived ~ .,
               method = method,
               trControl = train_control,
               data = train_set,
               metric = "ROC")
  
  pred <- fit %>%
    predict(newdata = test_data_set)
  
  pred_prob <- fit %>%
    predict(newdata = test_data_set, type = "prob") %>%
    pull(T)
  
  auc.tmp <- pred_prob %>%
    prediction(labels = test_data_set$survived) %>%
    performance("auc")
  
  roc <- auc.tmp@y.values[[1]]
  
  accuracy <- pred %>%
    confusionMatrix(reference = test_data_set$survived) %>%
    .$overall %>%
    .[["Accuracy"]]
  
  return(c(pred_prob = pred_prob, roc = roc, accuracy = accuracy))
}

## Classification Models
if(file.exists("save/ensemble_train_data.dat")){
  
  load("save/ensemble_train_data.dat")
  
} else {
  
  train_models <- c("avNNet", "rf", "ranger", "naive_bayes",  "knn",
                    "svmLinear", "gamLoess",  "gam", "wsrf", "Rborist", "mlp", 
                    "monmlp", "gbm", "adaboost", "svmRadial", "svmRadialCost", 
                    "svmRadialSigma" )
  
  train_ensemble <- sapply(train_models, function(model){
    print(model)
    make_prediction(model, train_set)
  })
  
  save(train_ensemble, file = "save/ensemble_train_data.dat")
}

# For making decisions about which of the algorithms may perform most favorably
#  in an ensemble
train_ensemble_scatter_plot <-
  train_ensemble[2:3,] %>%
  t() %>%
  as_tibble(rownames = "method") %>%
  mutate(roc = unlist(roc),
         accuracy = unlist(accuracy)) %>%
  ggplot(aes(x = roc, y = accuracy, label = method)) +
  geom_point() +
  geom_text_repel()

if(file.exists("save/ensemble_test_data.dat")){
  
  load("save/ensemble_test_data.dat")
  
} else {
  test_models <- c("avNNet", "rf", "ranger", "naive_bayes",  "knn",
              "svmLinear", "gamLoess",  "gam", "wsrf", "Rborist", "mlp", 
              "monmlp", "gbm", "adaboost", "svmRadial", "svmRadialCost", 
              "svmRadialSigma")
  
  test_ensemble <-  sapply(test_models, function(model){
    print(model)
    make_prediction(model, test_set)
  })
    
  save(test_ensemble, file = "save/ensemble_test_data.dat")
}

results <- test_ensemble[2:3,] %>%
  t() %>%
  as_tibble(rownames = "method") %>%
  filter(method %in% c("adaboost", "ranger", "rf", "wsrf")) %>%
  mutate(roc = unlist(roc),
         accuracy = unlist(accuracy)) %>%
  bind_rows(results)

ense_pred_prob <- test_ensemble[1,] %>%
  as_tibble() %>%
  select(adaboost, ranger, rf, wsrf) %>%
  rowMeans()  

auc.tmp <- ense_pred_prob %>%
  prediction(labels = test_set$survived) %>%
  performance("auc")
ense_roc <- auc.tmp@y.values[[1]]

ense_accuracy <- ifelse(ense_pred_prob < 0.5, "F", "T") %>%
  factor() %>%
  confusionMatrix(reference = test_set$survived) %>%
  .$overall %>%
  .[["Accuracy"]]

results <- results %>%
  add_row(method = "Ensemble",
          roc = ense_roc,
          accuracy = ense_accuracy)

results <- 
  results %>%
  arrange(-roc) %>%
  mutate(method = ifelse(method == "wsrf", "Weighted Subspace Random Forest", method),
         method = ifelse(method == "ranger", "Untuned Ranger", method),
         method = ifelse(method == "rf", "Untuned Random Forest", method),
         method = ifelse(method == "adaboost", "Adaboost.M1", method),
         method = ifelse(method == "Random Forest", "Tuned Random Forest (ranger)", method))

results_scatter_plot <- 
  results %>%
  ggplot(aes(x = roc, y = accuracy, label = method)) +
  geom_point() +
  ggrepel::geom_text_repel()


## Encorperating into validation

val_cabins <- 
  validation %>%
  extract(col = cabin, 
          into = c("cab_let_1", "cab_num_1",
                   "cab_let_2", "cab_num_2",
                   "cab_let_3", "cab_num_3",
                   "cab_let_4", "cab_num_4"),
          regex = "^([A-Z])(\\d+)\\s?(?:([A-Z])(\\d+))?\\s?(?:([A-Z])(\\d+))?\\s?(?:([A-Z])(\\d+))?$",
          remove = FALSE) %>%
  mutate(cab_num_1 = as.numeric(cab_num_1), 
         cab_num_2 = as.numeric(cab_num_2),
         cab_num_3 = as.numeric(cab_num_3),
         cab_num_4 = as.numeric(cab_num_4)) %>%
  rowwise() %>%
  mutate(cab_letter = cab_let_1) %>%
  mutate(med_cab_num = median(c_across(cols = c(cab_num_1, cab_num_2, cab_num_3, cab_num_4)), na.rm = TRUE)) %>%
  mutate(num_cab = sum(as.logical(c_across(cols = c(cab_num_1, cab_num_2, cab_num_3, cab_num_4))), na.rm = TRUE)) %>%
  select(id, cab_letter, med_cab_num, num_cab)

val_last_names <- validation %>%
  extract(col = name, into = c("last_name", "title", "first_name"), regex = full_regex, remove = FALSE) %>%
  select(id, last_name, title, first_name) %>%
  group_by(title) %>%
  mutate(title = ifelse(n() < 2, "Other", title)) %>%
  ungroup()

# Validation cleaned and prepped for prediction
validation <-
  validation %>%
  mice(m = 5) %>%
  complete(5) %>%
  as_tibble() %>%
  # Add cabin information
  left_join(val_cabins) %>%
  mutate(cab_letter = factor(cab_letter)) %>%
  mutate(cab_letter = addNA(cab_letter)) %>%
  select(-cabin, -ticket, -med_cab_num) %>%
  # add last names
  left_join(val_last_names) %>%
  mutate(title = factor(title),
         last_name = factor(last_name)) %>%
  select(-name, -first_name, -last_name) %>%
  # rename factors that will cause problems
  mutate(class = recode_factor(class, "1" = "One", "2" = "Two", "3" = "Three"))
  
# Making the final prediction
final_pred <- predict(fit_rf, newdata = validation)

# human readable output
validation <-
  validation %>%
  add_column(survived = (final_pred == "T"))

validation %>%
  transmute(PassengerId = id, Survived = as.integer(survived)) %>%
  write_csv("submission.csv")

# Final result: 0.75837

