library (theft)
library (epiR)
library (mltest)
library (tidyverse)

#########################################
df <- readRDS ("merged_dat.RDS")
grps <- df$group

df <- df %>%
  map_if(is.matrix, ~.x[grps== "HOA",]) %>%
  map_at(c(1:10), ~.x[grps == "HOA"])

# Remove segment angles
df <- df[!grepl("Head|Thorax|Pelvis|Foot|Elbows_Y|Elbows_Z", names (df))]

# Keep only sagittal angles as PiG model bad for non-saggital
df <- df[!grepl("_Y|_Z", names (df))] %>%
  map_at(vars(starts_with("_X")), as.matrix)

df$cycle <- NULL

oa_side <- df$oa_side

df_Loa <- df %>%
  map_if(is.matrix, ~.x[oa_side == "L",]) %>%
  map_at(c(1:9), ~.x[oa_side == "L"])


df_Roa <- df %>%
  map_at(c(1:9), ~.x[oa_side == "R"]) %>%
  map_if(is.matrix, ~.x[oa_side == "R",])

#########################################


train <- data.frame( rbind(df_Roa$R_Hips_X,  df_Roa$R_Knees_X))
test <- data.frame( rbind(df_Loa$L_Hips_X, df_Loa$R_Knees_X, df_Roa$L_Knees_X))

train$oa <- c(rep("OA", length (df_Roa$oa_side)), rep("Con", length (df_Roa$oa_side)))
test$oa <- c(rep("OA", length (df_Loa$oa_side)),
             rep("Con", length (df_Loa$oa_side)),
             rep("Con", length (df_Roa$oa_side))
)

train <- train %>%
  rowid_to_column(var = "id") %>%
  pivot_longer(cols = - c(oa, id),
               names_to = "time",
               values_to = "angle")  %>%
  mutate(time = parse_number(time))

test <- test %>%
  rowid_to_column(var = "id") %>%
  pivot_longer(cols = - c(oa, id),
               names_to = "time",
               values_to = "angle")  %>%
  mutate(time = parse_number(time))

featMat_train <- calculate_features(data = train,
                              id_var = "id",
                              time_var = "time",
                              values_var = "angle",
                              group_var = "oa",
                              feature_set = c("catch22", "feasts"),
                              seed = 123)
featMat_test <- calculate_features(data = test,
                                    id_var = "id",
                                    time_var = "time",
                                    values_var = "angle",
                                    group_var = "oa",
                                    feature_set = c("catch22", "feasts"),
                                    seed = 123)


df_train <- featMat_train %>%
  ungroup()  %>%
  select (-c(feature_set))  %>%
  pivot_wider(names_from = "names",
              values_from = "values") %>%
  select (-c(id))  %>%
  mutate (group = factor (group, levels = c("Con", "OA"))) %>%
  as.data.frame() %>%
  janitor::remove_constant()

df_test <- featMat_test %>%
  ungroup()  %>%
  select (-c(feature_set))  %>%
  pivot_wider(names_from = "names",
              values_from = "values") %>%
  select (-c(id))  %>%
  mutate (group = factor (group, levels = c("Con", "OA"))) %>%
  as.data.frame() %>%
  janitor::remove_constant()


X_train <- apply (df_train[,-c(1)],2, scale, center = TRUE)
X_test <- apply (df_test[,-c(1)],2, scale, center = TRUE)
y_train <- as.numeric (df_train[,1])-1

o <-  as.numeric (df_test[,1])-1
cvfit <- cv.ncvreg(X_train, y_train,
                   penalty = "lasso", family='binomial')

par(mfrow=c(2,2))
plot(cvfit, type='all')

coef(cvfit, s = "lambda.min")
p <- predict(cvfit, X_test , type = "class",lambda = min(cvfit$lambda))
o
p

tab <- table(p, o)
a <- tab[2,2]
b <- tab[2,1]
c <- tab[1,2]
d <- tab[1,1]

data.frame (epi.tests(c(a, b, c, d), method = "exact", digits = 2, conf.level = 0.95)$detail[,c(1,2)] )%>%
  filter (statistic %in% c("se", "sp", "pv.pos", "pv.neg",
                           "lr.pos", "lr.neg", "diag.ac", "diag.or")) %>%
  mutate (est = round(est,2))

