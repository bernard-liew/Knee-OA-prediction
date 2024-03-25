# define a remove correlation function
# (already exists in caret and other packages,
# but actually do not yield satisfactory results)
remove_cor = function(cor_mat, cutoff = 0.8)
{

  cor_check = function(x, co = cutoff) any(x > co)
  eps = 1e-10

  cor_mat <- tmp <- abs(cor_mat)
  features_to_remove = c()
  tmp[upper.tri(tmp, diag = TRUE)] <- 0
  while(any(c(tmp) > cutoff))
  {

    # find highest correlated features
    current_max = max(tmp)
    ind_max = apply(tmp, 2, cor_check, co = current_max-eps)
    features = cor_mat[,names(ind_max)[ind_max]]
    max_var2 = names(features[features  > current_max-eps])
    max_var2 = max_var2[!max_var2 %in% features_to_remove]
    # check the mean correlation and number of highly correlated
    nrhighcor = apply(cor_mat[,max_var2],2,function(x) sum(x > cutoff))
    meancor = apply(cor_mat[,max_var2],2,mean)
    # remove the one with larger nrhighcor, and if equal, the one with
    # larger meancor
    if(diff(nrhighcor)!=0){
      rem_feat = names(nrhighcor[which.max(nrhighcor)])
    }else{
      rem_feat = names(nrhighcor[which.max(meancor)])
    }
    ind = which(colnames(tmp)==rem_feat)
    tmp = tmp[-ind,-ind]
    features_to_remove = c(features_to_remove, rem_feat)

  }

  return(features_to_remove)

}




### function to generate interpretation plots

make_interpret_plots <- function(mod, plot = TRUE,
                                 cuml = c("For each time-point", "Cumulative"),
                                 fun_var_name,
                                 which,
                                 data,
                                 eachTimePoint_logit = TRUE)
{

  # return list
  retList <- list()

  for(i in 1:length(fun_var_name))
  {

    for(j in cuml){

      var = fun_var_name[i]
      df_temp = data[var]
      df_temp <- df_temp[rep(1,ncol(df_temp[[1]]))]
      if(j=="Cumulative")
        df_temp <- list(do.call("rbind", lapply(1:length(df_temp), function(i){
          # create data sets with only the first i time points being non-zero
          ret = df_temp[[i]]
          ret[,pmin(ncol(ret),i+1):ncol(ret)] <- 0
          return(ret)
        }))) else df_temp <- list(do.call("rbind", lapply(1:length(df_temp), function(i){
          # create data sets with only the first i time points being non-zero
          ret = df_temp[[i]]
          ret[,-i] <- 0
          return(ret)
        })))

      names(df_temp)[1] <- var
      # make predictions
      type_link <- j == "For each time-point" & !eachTimePoint_logit
      pd = as.data.frame(
        matrix(predict(mod, newdata = df_temp, which = which [i],
                       type = if_else(type_link, "link", "response")),
               ncol = ncol(df_temp[[1]]), byrow = FALSE))
      pd$oa_side = data$oa_side
      pd$id <- 1:length (data$oa_side)

      gg <- pd %>% pivot_longer(cols = -c(oa_side, id),
                          names_to = "time",
                          values_to = "probability") %>%
        mutate(time = as.numeric(gsub("V","",time))) %>%
        ggplot(aes(x = time, y = probability, group = id, colour = oa_side)) +
        geom_line() +
        ggtitle(paste(var, j, sep = ": ")) +
        scale_color_manual(values =c ("blue", "red"),
                           labels = c("Left", "Right"))+
        ylab ("Cumulative probability") +
        xlab ("Walking stride (0-100%)") +
        labs (colour = "Side") +
        cowplot::theme_cowplot()

      if(type_link) gg <- gg + ylab("log odds")
      if(plot) print(gg)

      retList <- c(retList, list(gg))

    }
  }

  names(retList) <- paste0(rep(fun_var_name, each = length(cuml)), "__",
                           cuml)
  invisible(retList)

}

# Plots for multinomial data ---------------------------------------------------

###############################################################################
# function to create data for plots
#
# usage
#   which_feature: gives the number of the selected feature
#   aggregate_fun: takes a function to compute a "typical" curve, e.g.,
#                  the a certain quantile in the population, per default
#                  the mean curve (a theoretical person whose feature values
#                  are larger than 50% of all others); using mean doesnt work as
#                  features are zero mean scaled; another option would
#   type: when "response", will compute probabilites, otherwise ("link")
#         log-odds between all classes and the reference class
#
#   returns: returns a 101x4 (101x3 for log-odds) matrix describing the
#            the cumulative change in probability (log-odds) when looking at
#            the chosen feature across the cycle points
create_plotdata <- function(which_feature,
                            aggregate_fun = median,
                            type = "response")
{

  feature_name <- mod$baselearner[[which_feature]]$get_names()[1]
  aggreg_data <- apply(df[[feature_name]], 2, aggregate_fun)
  # create zero-padded data for cumulative plots
  padded_data <- matrix(aggreg_data, nrow=1)[rep(1,101),]
  padded_data[upper.tri(padded_data)] <- 0
  # create probabilities / log-odds
  newdata <- df
  newdata[[feature_name]] <- I(padded_data)
  pred <- predict(mod, newdata = newdata,
                  which = which_feature, type = type)
  if(type != "response") pred <- matrix(pred, ncol = 3)
  return(pred)

}

# function using the above function, compute for all features, and
# return the data in a ggplot-friendly format
#   all_selected: all selected numbers (which)
#   aggregate_fun: see above
#   type: see above
pred_for_all <- function(all_selected, aggregate_fun = median,
                         type = "response")
{

  which_feature_names <- sapply(all_selected, function(w)
    mod$baselearner[[w]]$get_names()[1])
  pp <- lapply(all_selected, function(w)
    create_plotdata(which_feature = w, aggregate_fun = aggregate_fun,
                    type = type))

  names(pp) <- which_feature_names
  names_colums <- levels(df$kl_severity)
  if(type != "response") names_colums <- names_colums[-1]

  ret_df <- do.call("rbind",
                    lapply(1:length(all_selected),
                           function(i) data.frame(
                             value = c(pp[[i]]),
                             cycle = rep(1:101, length(names_colums)),
                             class = rep(names_colums, each = 101),
                             feature = which_feature_names[i])))

  return(ret_df)

}
