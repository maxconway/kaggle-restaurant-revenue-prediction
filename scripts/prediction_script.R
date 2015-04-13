library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(magrittr)

# Helpers
RMSE <- function(predicted, actual){
  sqrt(mean((predicted-actual)^2,na.rm=TRUE))
}
logical2factor <- function(logical){
  ifelse(logical, 'yes', 'no') %>% factor(levels=c('yes','no'))
}


data <- read_csv('data/train.csv')

preprocess <- function(x){x %>%
                            mutate(`Open Date` = dmy(`Open Date`),
                                   month = month(`Open Date`),
                                   has_time = logical2factor(`Open Date`!=floor_date(`Open Date`,'day')),
                                   in_Istanbul = logical2string(City=='İstanbul'),
                                   Type = factor(Type, levels=c('FC', 'IL', 'DT', 'MB')),
                                   `City Group` = factor(`City Group`, levels=c('Big Cities', 'Other'))
                            )}

data <- data %>% preprocess()
real_train <- data

crossvalidate <- function(modelfun, data, iterations=10, sample_ratio=1/nrow(data)){
  laply(1:iterations, function(ignored){
    assignment <- sample.int(nrow(data),sample_ratio*nrow(data))
    train <- data %>% filter(!(row_number() %in% assignment))
    test <- data %>% filter((row_number() %in% assignment))
    mod <- modelfun(train)
    predict(mod, test)-test$revenue
  }) %>% as.vector() %>% `^`(2) %>% mean %>% sqrt
}


# rpart prediction
library(rpart)
controls <- rpart.control(2,5)
modelfun <- function(train){
  train %>% select(-Id,-City) %>% rpart(revenue ~ ., .,  control = controls)
}
crossvalidate(modelfun, data)

usedmod <- real_train %>% select(-Id,-City) %>% rpart(revenue ~ ., ., control=controls)

# Cubist prediction
library(Cubist)
modelfun <- function(train){
  cubist(x = train %>% select(-Id, -revenue, -`Open Date`, -City),
         y = train$revenue)
}
crossvalidate(modelfun, data)


# usedmod <- cubist(x = real_train %>% select(-Id, -revenue, -`Open Date`, -City),
#                   y = real_train$revenue)





real_test <- read_csv('data/test.csv') %>% preprocess
submission <- data_frame(Id = real_test$Id,
                         Prediction = predict(usedmod, real_test)
)
write_csv(submission, paste0('out/','submission',Sys.Date() %>% format('%FT%H:%M:%S'),'.csv'))
