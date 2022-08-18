# The ensure package function installs any packages not present before loading
#  them into the library.
ensure_package <- function(package)
{
  package <- as.character(package)
  if (!require(package, character.only = TRUE))
  {
    install.packages(pkgs = package, repos = "http://cran.r-project.org")
    require(package, character.only = TRUE)
  }
}

# tidyverse for data manipulation
# ggplot2 for prettier presentation of data
packages <- c("tidyverse", "ggplot2", "mice", "caret", "glmnet", "ranger", "ROCR")

# load all packages
sapply(packages, ensure_package)

# load the data
source_path <- file.path(getwd(), "src")
source(file.path(source_path, "load_data.R"))

####
# Data Summary
####
titanic %>%
  summarise_if(is.numeric, list(min, max, mean, sd), na.rm = TRUE) %>%
  pivot_longer(cols = everything(),
               names_pattern = "^(.*)_(fn\\d)$",
               names_to = c("Variable", "function")) %>%
  pivot_wider(names_from = "function") %>%
  rename(Min = fn1, Max = fn2, Mean = fn3, SD = fn4)

titanic %>%
summarize(male = sum(sex == "male"),
          female = sum(sex == "female"),
          other = sum(!(sex %in% c("male", "female"))),
          total = n())


#### 
# Visualizing the Data
####

titanic %>%
  ggplot(aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age",
       y = "Count")

titanic %>%
  ggplot(aes(x = sib_sp, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Number of Siblings and Spouses",
       y = "Count")

titanic %>%
  ggplot(aes(x = sib_sp, fill = survived)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Number of Siblings and Spouses",
       y = "Count")

titanic %>%
  ggplot(aes(x = par_ch, fill = survived)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Number of Parents and Children",
       y = "Count")

titanic %>%
  ggplot(aes(x = par_ch, fill = survived)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Number of Parents and Children",
       y = "Count")

titanic %>%
  ggplot(aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 10) +
  labs(x = "Ticket Fare",
      y = "Count")

titanic %>%
  ggplot(aes(x = class, fill = survived)) +
  geom_bar() +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("1st", "2nd", "3rd")) +
  labs(x = "Sex", 
       y = "Count")

titanic %>%
  ggplot(aes(x = sex, fill = survived)) +
  geom_bar() +
  scale_x_discrete(breaks = c("female", "male"),
                   labels = c("Female", "Male")) +
  labs(x = "Sex", 
       y = "Count")

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

titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(title) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(title, -count), fill = survived)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(title) %>%
  mutate(rate = mean(survived == TRUE)) %>%
  ggplot(aes(x = reorder(title, -rate), fill = survived)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

titanic %>%
  left_join(last_names, by = "id") %>%
  group_by(last_name) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = count, fill = survived)) +
  geom_bar() +
  labs(x = "Number of Other Passengers with Last Name",
       y = "Count")

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

titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(cab_letter, -count), fill = survived)) +
  geom_bar()

titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  mutate(rate = mean(survived == TRUE)) %>%
  ggplot(aes(x = reorder(cab_letter, -rate), fill = survived)) +
  geom_bar(position = "fill")

titanic %>%
  left_join(cabins) %>%
  group_by(cab_letter) %>%
  ggplot(aes(x = med_cab_num, fill = survived)) +
  geom_histogram(binwidth = 2) +
  facet_grid(rows = vars(cab_letter))


#### 
# Missing Values
titanic %>%
  summarize(across(everything(), is.na)) %>%
  summarize(across(everything(), sum))

titanic %>% 
  md.pattern() # from mice

source(file.path(source_path, "load_data.R"))
missing_data <- titanic %>%
  mice(m = 5)
  
#### Putting it all together
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
                      family = "binomial")

fit_logistic %>%
  varImp() %>%
  ggplot()

pred_logistic <- predict(fit_logistic, newdata = test_set, type = "prob")
pred <- prediction(pred_logistic$T, labels = test_set$survived)
auc.tmp <- performance(pred, "auc")
auc.tmp@y.values[[1]]

confusion_logistic <- confusionMatrix(data = pred@labels[[1]], reference = test_set$survived)

results <- tibble(method = "Logistic Regression",
                  accuracy = confusion_logistic$overall[["Accuracy"]])


#####
# Random Forest
train_control = trainControl(method = "cv", 
                             summaryFunction = twoClassSummary, 
                             classProbs = TRUE,
                             savePredictions = TRUE)

fit_rf = train(survived ~ .,
               method = "ranger",
               trControl = train_control,
               preProc = c("center", "scale"),
               data = train_set)

fit_rf %>%
  filterVarImp(x = select(train_set, -survived), y = train_set$survived) %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  rename(importance = T) %>%
  select(-F) %>%
  ggplot(aes(x = reorder(variable, -importance), y = importance)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

pred_rf = predict(fit_rf, newdata = test_set)

confusion_rf <- confusionMatrix(data = pred_rf, reference = test_set$survived)

results <- results %>%
  add_row(method = "Random Forest",
          accuracy = confusion_rf$overall[["Accuracy"]])



