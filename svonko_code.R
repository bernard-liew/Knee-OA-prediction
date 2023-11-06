data2 <- data %>%
  mutate (cycle = rep(1:100, 41)) %>%
  pivot_longer(-c(cycle, Gender, Name),
               names_to = "var",
               values_to = "val") %>%
  pivot_wider(names_from = "cycle",
              values_from = "val")


var_list <- data2 %>%
  group_by(var)%>%
  nest()


df_list <- var_list$data %>%
  map(~.x %>% select(-c(Gender, Name)) %>% as.matrix) %>%
  map (~apply(.x, 2, scale, center = TRUE, scale = FALSE))

names (df_list) <- var_list$var

df_list$id <- var_list$data[[1]]$Name
df_list$gender <- factor (var_list$data[[1]]$Gender)
df_list$cycle <- 1:100




fun_names <- grep("_deg", names (df_list), value = TRUE)

## Define the model formula
# response is group
lhs = "gender ~ "
# the right-hand side of the formula includes the scalar covariates
#rhs_scalar = "bols(sex, df = 1) + bols(age, df = 1) + bbs(ht, df = 1, center = TRUE) + bols(ht, df = 1) + bbs(wt, df = 1, center = TRUE) + bols(wt, df = 1)"
# as well as functional covariates:
# Here we have to options:
#
# 1) we include curves from all 3 tasks (CW, CCW and Rect) into the predictor
# but exclude the highly correlated ones
# correl_ones = apply(sapply(hc_fun, function(x) 
#   grepl(x,fun_names,fixed=T)),1,any)
# fun_names = fun_names[!correl_ones]
rhs = paste(paste0("bsignal(", fun_names, ", s = cycle, differences = 0, df = 1)"), collapse = " + ")

# hc_scalar = gsub("(?: |/)",".",hc_scalar)
# correl_ones_scal = apply(sapply(hc_scalar, function(x) 
#   grepl(x,scal_names,fixed=T)),1,any)
# scal_names = scal_names[!correl_ones_scal]
# rhs_scal_all_1 = paste(paste0("bols (", scal_names, ",  df = 1)"), collapse = " + ")
# rhs_scal_all_2 = paste(paste0("bbs (", scal_names, ",  df = 1, center = TRUE)"), collapse = " + ")



# transform the whole string to a formula
form = as.formula( paste0(lhs, rhs) )
# and pass it to FDboost for model fitting

mod <- FDboost(
  formula = form, 
  data = df_list, 
  timeformula = NULL, 
  family = Binomial(), 
  # define the boosting specific parameters
  # mstop:  set to a large number, such as 1000.
  #         We will choose the optimal stoppting
  #         iteration in the next step via cross-validation
  # nu:     learning rate: 0.1 is a good default, which 
  #         works for most cases
  control = boost_control(mstop = 100, 
                          nu = .01)
)

# having fitted the model, we search 
# through all 1000 iterations, whether we should have
# stopped early as the algorithm might already have
# overfitted
set.seed(1)
folds = cv(rep(1, length(unique(mod$id))), 
           type = "kfold", B = 4, strata = df_list$gender)
cvr = cvrisk(mod, folds = folds)
# plot the cross-validation search and 
# the best stopping iteration
plot(cvr)
# redefine the model and set it to the best stopping
# iteration (which is done inplace): 
(best_iteration = mstop(cvr))
mod[best_iteration]
# plot the important predictors
plot(mboost::varimp(mod))
# plot the estimated effects of the model
plot(mod)

o <- as.numeric (df_list$gender) -1
# plot the estimated probabilities for each class
pd <- predict(mod, type = "response")
boxplot(pd ~ df_list$gender)
# look at the inbag AUC
auc(actual = o, predicted = pd)
# and the OOB AUC:
(oob_auc <- unlist(cvrisk(mod, folds = folds, fun = 
                            function(object) auc(actual = o,
                                                 predict(object, type = "response")))))
mean(oob_auc)


# plot the estimated probabilities for each class
pd <- as.numeric (predict(mod, type = "class")) -1
# look at the inbag AUC
accuracy(actual = o, predicted = pd)



# and the OOB AUC:
(oob_auc <- unlist(cvrisk(mod, folds = folds, fun = 
                            function(object) accuracy(actual = o,
                                                      as.numeric (predict(object, type = "class")) -1))))
mean(oob_auc)