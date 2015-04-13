library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(magrittr)

# Helpers
RMSE <- function(predicted, actual){
  sqrt(mean((predicted-actual)^2,na.rm=TRUE))
}
logical2string <- function(logical){
  ifelse(logical, 'yes', 'no')
}


data <- read_csv('data/train.csv')

preprocess <- {. %>%
                 mutate(`Open Date` = dmy(`Open Date`),
                        month = month(`Open Date`),
                        has_time = logical2string(`Open Date`!=floor_date(`Open Date`,'day')),
                        in_Istanbul = logical2string(City=='İstanbul'),
                        Type = factor(Type, levels=c('FC', 'IL', 'DT', 'MB'))
                 )}

data <- data %>% preprocess()
real_train <- data
  
assignment <- sample.int(nrow(data),0.9*nrow(data))
train <- data %>% filter(row_number() %in% assignment)
test <- data %>% filter(!(row_number() %in% assignment))


# rpart prediction
library(rpart)
controls <- rpart.control(5, 2)
mod <- train %>% select(-Id,-City) %>% rpart(revenue ~ ., ., control = controls)
RMSE(predict(mod, test),test$revenue)

usedmod <- real_train %>% select(-Id,-City) %>% rpart(revenue ~ ., ., control=controls)

# Cubist prediction
library(Cubist)
mod <- cubist(x = train %>% select(-Id, -revenue, -`Open Date`, -City),
              y = train$revenue)
RMSE(predict(mod, test),test$revenue)


# usedmod <- cubist(x = real_train %>% select(-Id, -revenue, -`Open Date`, -City),
#                   y = real_train$revenue)





real_test <- read_csv('data/test.csv') %>% preprocess
submission <- data_frame(Id = real_test$Id,
                         Prediction = predict(usedmod, real_test)
                         )
write_csv(submission, paste0('out/','submission',Sys.Date() %>% format('%FT%H:%M:%S'),'.csv'))
