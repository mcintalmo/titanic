# Read the data from the train and test csv. I prefer working in tibbles
data_path <- getwd() %>%
  file.path("data")

make_comfy <- function(data) {
  data %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    rename(id = PassengerId,
           class = Pclass, # Passenger Class
           name = Name,
           sex = Sex,
           age = Age,
           sib_sp = SibSp, # Siblings or Spouses
           par_ch = Parch, # parents or children
           ticket = Ticket,
           fare = Fare,
           cabin = Cabin,
           embarked = Embarked) %>%
    mutate(class = factor(class),
           sex = factor(sex),
           embarked = factor(embarked))
}

titanic <- data_path %>%
  file.path("train.csv") %>%
  read.csv() %>%
  as_tibble() %>%
  make_comfy() %>%
  rename(survived = Survived) %>%
  mutate(survived = factor(ifelse(survived, TRUE, FALSE)))
  

validation <- data_path %>%
  file.path("test.csv") %>%
  read.csv() %>%
  as_tibble() %>%
  make_comfy()

rm(data_path, make_comfy)
