library("leaps")
library("lmtest")
library("nlme")
library("ape")
library("broom")
library("FRK")
library("purrr")
library("lattice")
library("ggplot2")
library("RColorBrewer")
library("dplyr")
library("gstat")
library("sp")
library("tidyr")
library("Metrics")

data<-read.csv("st_data.csv")

data$date<-as.Date(data$date)

data<-filter(data, n_killed != 0)

data_test<-filter(data, incident_year < 2017)
data_valid<-filter(data, incident_year == 2017)

G<-auto_basis(data = data_test[,c('longitude','latitude')] %>% SpatialPoints(),
              nres = 1,
              type = "Gaussian")

S<-eval_basis(basis = G,
              s = data_test[,c('longitude','latitude')] %>%
                as.matrix()) %>% as.matrix()

colnames(S) <-paste0("B",1:ncol(S))

injury_data<-select(data_test,c('n_injured', 'date','longitude','latitude'))

injury2<-cbind(injury_data, S)

injury_ST_lm<-lm(n_injured ~ (longitude + latitude + date) ^ 2 + ., data = injury2)

fatality_data<-select(data_test,c('n_killed','date','longitude','latitude'))

fatal2<-cbind(fatality_data, S)

fatality_ST_lm<-lm(n_killed ~ (longitude + latitude + date) ^ 2 + ., data = fatal2)

summary(fatality_ST_lm)

valid_data<-select(data_valid,c('date','longitude','latitude'))

real_2017_injured<-select(data_valid,'n_injured')

real_2017_killed<-select(data_valid,'n_killed')

S<-eval_basis(basis = G,
              s = data_valid[,c('longitude','latitude')] %>%
                as.matrix()) %>% as.matrix()

colnames(S) <-paste0("B",1:ncol(S))

predict_data<-predict(fatality_ST_lm,cbind(valid_data,S))

rmse(t(as.vector(real_2017)),as.vector(predict_data))