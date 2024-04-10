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

# Basic data setup

df<-read.csv("st_data_pop.csv")
df$date<-as.Date(df$date)
df<-na.omit(df)
print("Injured (I), Killed (K), or Killed per capita (P)?")
case<-readline()
data_test<-filter(df, year < 2017)
data_valid<-filter(df, year == 2017)
G<-auto_basis(data = data_test[,c('longitude','latitude')] %>% SpatialPoints(),
              nres = 1,
              type = "Gaussian")
S<-eval_basis(basis = G,
              s = data_test[,c('longitude','latitude')] %>%
                as.matrix()) %>% as.matrix()
colnames(S) <-paste0("B",1:ncol(S))

# Setting up different cases for the models using injuries, deaths, and per capita

if(case == "I"){
  injury_data<-select(data_test,c('n_injured', 'date','longitude','latitude'))
  injury2<-cbind(injury_data, S)
  ST_lm<-lm(n_injured ~ (longitude + latitude + date) ^ 2 + ., data = injury2)
  real_2017<-select(data_valid,'n_injured')
} else if (case == "K") {
  fatality_data<-select(data_test,c('n_killed','date','longitude','latitude'))
  fatal2<-cbind(fatality_data, S)
  ST_lm<-lm(n_killed ~ (longitude + latitude + date) ^ 2 + ., data = fatal2)
  real_2017<-select(data_valid,'n_killed')
} else if (case == "P") {
  per_cap_data<-select(data_test,c('n_killed','date','longitude','latitude'))
  per_cap<-cbind(per_cap_data, S)
  ST_lm<-lm(n_killed ~ (longitude + latitude + date) ^ 2 + ., data = per_cap)
  real_2017<-select(data_valid,'per_hthous_killed')
}

#summary(ST_lm)

# Validation using 2017 data
valid_data<-select(data_valid,c('date','longitude','latitude'))
S<-eval_basis(basis = G,
              s = data_valid[,c('longitude','latitude')] %>%
                as.matrix()) %>% as.matrix()
colnames(S) <-paste0("B",1:ncol(S))
predict_data<-predict(ST_lm,cbind(valid_data,S))
rmse(t(as.vector(real_2017)),as.vector(predict_data))