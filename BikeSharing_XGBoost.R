outcome <- "cnt"
vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed")

library(vtreat)

treatplan <- designTreatmentsZ(bikesJuly,vars,verbose = FALSE)

library(magrittr)
newvars <- treatplan %>%
  use_series(scoreFrame) %>%
  filter(code %in% c("clean","lev")) %>%
  use_series(varName)

bikesJuly.treat <- prepare(treatplan,bikesJuly,varRestriction = newvars)
bikesAugust.treat <- prepare(treatplan,bikesAugust,varRestriction = newvars)

str(bikesAugust.treat)
str(bikesJuly.treat)

library(xgboost)

vc <- xgb.cv(data=as.matrix(bikesJuly.treat),
             label=bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective ="reg:linear",
             eta=0.3,
             max_depth=6,
             early_stopping_rounds= 10,
             verbose=0
)

elog <-vc$evaluation_log

elog %>%
  summarize(ntrees.train=which.min(train_rmse_mean),
            ntrees.test=which.min(test_rmse_mean))


bike_model_xgb <- xgboost(data = as.matrix(bikesJuly.treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)


ntrees <- 100
bikesAugust$pred <- predict(bike_model_xgb,as.matrix(bikesAugust.treat))
ggplot(bikesAugust,aes(pred,cnt))+
  geom_point()+
  geom_abline()

bikesAugust %>%
  mutate(resi =cnt-pred) %>%
  summarize(rmse=sqrt(mean(resi^2)))

bikesAugust %>% 
  mutate(instant = (instant - min(instant))/24) %>%  # set start to 0, convert unit to days
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Gradient Boosting model")

