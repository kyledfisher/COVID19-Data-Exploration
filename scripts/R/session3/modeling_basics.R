library(data.table)
library(sf)
library(RPostgreSQL)
library(magrittr)
library(randomForest)
library(Metrics)
library(ggplot2)
library(keras)
library(tensorflow)
# devtools::install_github("rstudio/keras")

##---------------------------------------------------------------
## Read In Data and Munge                                      --
##---------------------------------------------------------------

# read in and munge Michael's data
ad_col_names <- c('State', 'pop_dense', 'sh_date')
ad <- fread("E:/Active_Projects/COVID19-Data-Exploration/data/Pop_Density_By_State.csv")
ad <- ad[,c('State:', 'Pop Density per square mile', 'Stay at home order date enacted:')]
colnames(ad) <- ad_col_names
ad$sh_date <- format(as.Date(ad$sh_date, "%m/%d/%y"), "20%y-%m-%d")

# connect to database and read in data
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Control_Test",
                 host = "10.12.50.107", port = 5432, 
                 user = 'covid_users', password = 'thissucks19')

report_data.dt <- dbGetQuery(con, 'SELECT * FROM covid_data.report_data') %>% data.table
report_data <- na.omit(report_data.dt, c('confirmed', 'deaths', 'province_state', 'country_region'))

# munge data to get data into the correct format to be a time series
us_data.dt <- report_data[which(report_data$country_region == 'US'),]
setkey(us_data.dt, 'province_state', 'last_update')
suppressMessages(us_data_sum.dt<-us_data.dt[,list(confirmed=sum(confirmed), deaths=sum(deaths), recovered=sum(recovered)),by=key(us_data.dt)])
us_data_sum.dt$id <- 1:nrow(us_data_sum.dt)
us_data_sum.dt <- merge(us_data_sum.dt, ad[,c('State', 'pop_dense', 'sh_date')],
                        by.x='province_state', by.y='State')

# split data by state and then remove states that don't have enough valid data
us_data.split.dt <- split(us_data_sum.dt, by='province_state')
us_data.split.dt <- lapply(us_data.split.dt, function(x){if(nrow(x) > 9){return(x)}})
us_data.split.dt <- us_data.split.dt[lengths(us_data.split.dt) != 0]


# convert long form into wide form time series
column.names <- c('province_state', '1', '2', '3', '4', '5', '6' ,'7' ,'8', '9', '10')
x <- us_data.split.dt[[1]]
y <- 1
us_time_series.dt <- lapply(us_data.split.dt, function(x){
  print(x$province_state %>% unique)
  time.chunks <- lapply(1:(nrow(x)-17), function(y){
    wide.confirmed.data <- dcast(x[y:(y+9),], province_state~id, value.var='confirmed')
    colnames(wide.confirmed.data) <- column.names
    wide.confirmed.data$pop_dense <- x[y:(y+9),]$pop_dense %>% unique
    if (ad[which(ad$'State' == x[y:(y+9),]$province_state  %>% unique),]$sh_date %>% as.Date >  
        x[(y+9),]$last_update %>% as.Date){
      wide.confirmed.data$stay_home <- 1
    }else{
      wide.confirmed.data$stay_home <- 0
    }
    wide.confirmed.data$deaths <- x[(y+9):(y+9+7),]$deaths %>% sum
    return(wide.confirmed.data)
  }) %>% do.call(rbind, .)
  return(time.chunks)
}) %>% rbindlist(.)
us_time_series.dt <- us_time_series.dt[sample(nrow(us_time_series.dt)),]

x_train <- us_time_series.dt[1:1368,-c('province_state', 'deaths')]
y_train <- us_time_series.dt[1:1368,]$deaths

x_test <- us_time_series.dt[1369:nrow(us_time_series.dt),-c('province_state', 'deaths')]
y_test <- us_time_series.dt[1369:nrow(us_time_series.dt),]$deaths


##---------------------------------------------------------------
## Linear Regression Model                                     --
##---------------------------------------------------------------

linear_data_train <- data.table(confirmed=x_train[,1:10] %>% rowSums, deaths=y_train)
linear_data_test <- data.table(confirmed=x_test[,1:10] %>% rowSums, deaths=y_test)

linear_model <- lm(deaths~confirmed, data=linear_data_train)
lm_predicts <- predict(linear_model, linear_data_test[,'confirmed']) %>% as.vector

rmse(linear_data_test$deaths, lm_predicts)
plot(linear_data_test$deaths)
plot(lm_predicts)

WTH_predicts <- (linear_model$coefficients[2] * linear_data_test[,]$confirmed)

rmse(linear_data_test$deaths, WTH_predicts %>% as.vector)
plot(linear_data_test$deaths)
plot(WTH_predicts)

plot(us_time_series.dt$deaths)

##---------------------------------------------------------------
## Random  Forest                                              --
##---------------------------------------------------------------

model <- randomForest(x_train, y_train, do.trace = TRUE)
rf_predicts <- predict(model, x_test) %>% as.vector

rmse(rf_predicts, y_test)

plot(rf_predicts)
plot(y_test)

##---------------------------------------------------------------
## LSTM Model using Keras                                      --
##---------------------------------------------------------------

X_train = array(x_train %>% as.matrix, dim=c(nrow(x_train), 10,1))
X_test = array(x_test %>% as.matrix, dim=c(nrow(x_test), 10,1))

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(10, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>%  
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary

model %>% fit(X_train, y_train, epochs=50, batch_size=32, shuffle = FALSE)
lstm_pred = model %>% predict(X_test)
plot(lstm_pred)
plot(y_test)
rmse(lstm_pred, y_test)

##---------------------------------------------------------------
## Plots                                                       --
##---------------------------------------------------------------

predict_data.dt <- data.table(y_value=y_test, rf_predicts=rf_predicts, lstm_pred=lstm_pred%>%as.vector, 
                              lm_pred=lm_predicts, wth_pred=WTH_predicts)

ggplot(predict_data.dt, aes(y_value, y=rf_predicts, color="Random Forest")) + 
  geom_point()+
  geom_point(data=predict_data.dt, aes(y_value, y=lstm_pred, color="LSTM"))


ggplot(predict_data.dt, aes(y_value, y=rf_predicts, color="Random Forest")) + 
  geom_point()+
  geom_point(data=predict_data.dt, aes(y_value, y=lstm_pred, color="LSTM"))+
  xlim(0,1000)+
  ylim(0,1000)

ggplot(predict_data.dt, aes(y_value, y=rf_predicts, color="Random Forest")) + 
  geom_point()+
  geom_point(data=predict_data.dt, aes(y_value, y=lstm_pred, color="LSTM"))+
  geom_point(data=predict_data.dt, aes(y_value, y=lm_pred, color="LM"))+
  xlim(0,1000)+
  ylim(-1000,1000)

ggplot(predict_data.dt, aes(y_value, y=rf_predicts, color="Random Forest")) + 
  geom_point()+
  geom_point(data=predict_data.dt, aes(y_value, y=lstm_pred, color="LSTM"))+
  geom_point(data=predict_data.dt, aes(y_value, y=lm_pred, color="LM"))+
  geom_point(data=predict_data.dt, aes(y_value, y=wth_pred, color="WTH"))+
  xlim(0,1000)+
  ylim(0,1000)
