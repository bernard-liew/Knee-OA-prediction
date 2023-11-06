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
        matrix(predict(mod, newdata = df_temp, which = i, type = if_else(type_link, "link", "response")),
               ncol = ncol(df_temp[[1]]), byrow = FALSE))
      pd$name = data$name
      pd$group = data$group
      
      pd %>% gather(key = "time", value = "probability", -name, -group) %>%
        mutate(time = as.numeric(gsub("V","",time))) %>%
        ggplot(aes(x = time, y = probability, group = name, colour = group)) + 
        geom_line() + 
        ggtitle(paste(var, j, sep = ": ")) + 
        scale_color_manual(values =c ("blue", "red"),
                           labels = c("control", "neck_pain"))+
        ylab ("Cumulative probability") +
        xlab ("Walking stride (0-100%)") +
        theme_bw()-> gg
      if(type_link) gg <- gg + ylab("log odds")
      if(plot) print(gg)
      
      retList <- c(retList, list(gg))
      
    }
  }
  
  names(retList) <- paste0(rep(fun_var_name, each = length(cuml)), "__", 
                           cuml)
  invisible(retList)
  
}