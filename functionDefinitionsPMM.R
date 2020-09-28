require(plyr)
require(distances)
require(caret) #for AUC Value calc
require(tidyverse)

generate_prediction_matrix <- function(ls_models,
                                        newdata,
                                        indiv_prediction_function,
                                        indiv_prediction_args,
                                        indpred_arg_name_modelobj,
                                        indpred_arg_name_newdata,
                                        .progress="none",
                                        .parallel=F){
  predict_function<-function(indiv_model,
                             wrappedfun=indiv_prediction_function,
                             prediction_args=indiv_prediction_args,
                             modelobj_argname=indiv_prediction_arg_name
  ){
    
    if (is.null(prediction_args)){
      prediction_args <- list()
    }
    #if no newdata argument name given assume first position in arguments (if also no modelobj_argname given, assume 2nd Position in arguments)
    if(is.null(indpred_arg_name_newdata)){
      prediction_args <- c(newdata,prediction_args)
    }else{
      #else give  newdata argument name according to indpred_arg_name_newdata
      prediction_args[[indpred_arg_name_newdata]] <- newdata
    }
    #if no modelobj_argname given,assume that arg one first position is modelobject argument
    if(is.null(indpred_arg_name_modelobj)){
      prediction_args <- c(indiv_model,prediction_args)
    }else{
    #else give modelobject the argumentname according to modelobj_argname
      prediction_args[[indpred_arg_name_modelobj]] <- indiv_model
    }
    #execute prediction function
    result <- do.call(wrappedfun,prediction_args)
    return(result)
  }#end predict_function
  
  df_predictions<-llply(ls_models,
                        .fun = function(x){predict_function(indiv_model=x)},
                        .progress = .progress,
                        .parallel = .parallel
                        )
  df_predictions <- Reduce(cbind,df_predictions)
  df_predictions <- data.frame(df_predictions)
  names(df_predictions)<- names(ls_models)
  return(df_predictions)
}

#calculate AUC, needs probability that label is true as well as the bool. True values
.auc<-function(p_label_IsTrue,y_label,.threshold=0.5){
  .levels<-c("F","T")
  #make observation and prediction class vecrots with unified levels
  obs<-factor(ifelse(y_label,"T","F"),levels=.levels)
  pred<- factor(ifelse(p_label_IsTrue>= .threshold,"T","F"),levels=.levels)
  
  
  df<-data.frame(obs=obs, pred=pred, "T"=p_label_IsTrue,"F"=1-p_label_IsTrue)
  result <- caret::twoClassSummary(data = df, lev=.levels)
  return(result[1]) # return first value of twoclassumary result which is ROC
}


.calculate_AUC_vector<-function(df_labels,
                                df_predictions,
                                .threshold=0.5
                                ){
  if(ncol(df_labels)!=ncol(df_predictions)){
    stop("Error: Labels dataframe and predictions dataframe do not have the same amount of columns")
  }# end if
  result <- sapply(c(1:ncol(df_labels)),
                   FUN= function(x){
                    return(.auc(df_predictions[[x]],df_labels[[x]]))
                   }
  )
  names(result)<-names(df_labels)
  return(result)
}

.df_prediction_to_knn <- function(df_knn_input,
                                  df_reference,
                                  vec_W,
                                  k,
                                  y_include_distances
                                  ){
  ###check input:
  #number of cols of input, reference and Weight vector have to match
  #if k is over the amount of reference rows (meaning more neigbours searched than points available),
  # distances::nearest_neighbor_search will crash R
  vec_inputLenghts <- c(ncol(df_knn_input),ncol(df_reference),length(vec_W))
  if(any(vec_inputLenghts!=vec_inputLenghts[1])){
    stop(sprintf("Stop: amount of columns of input table(%d), reference table(%d) and weight vector (%d) do not match.",
                 vec_inputLenghts[1],
                 vec_inputLenghts[2],
                 vec_inputLenghts[3])
         )
  }
  if(k>nrow(df_reference)){
    warning(sprintf("There are less points in the reference dataframe (%d) than Nearest Neighbours to analyse due to k (k= %d) to analyse(). k is therefore set to %d.",
                    nrow(df_reference),
                    k,
                    nrow(df_reference)
    )
    )
    k=nrow(df_reference)
  }
  
  ##calculate knn
  #only input columns where weight >0 as knn calculation cannot handle weight value of 0
  df_total_pred<-rbind(df_knn_input,df_reference)[,(vec_W!=0)]
  distance_object <- distances::distances(data=df_total_pred,
                                          weights =vec_W[(vec_W!=0)]
  )
  nn <- distances::nearest_neighbor_search(distance_object,
                                           query_indices=c((nrow(df_reference)+1):(nrow(df_reference)+nrow(df_knn_input))),
                                           search_indices=c(1:nrow(df_reference)),
                                           k=k
  )
  #make the nearest neighbour matrix to a dataframe in the long format
  nn<-as.data.frame(nn)
  nn$k <- c(1:nrow(nn)) #Index to column
  nn<-gather(nn, key="index_input",value="index_df_predictions",-k,convert = T)
  
  #include distances in the nearest neoghbour table if so requested
  if(y_include_distances){
    nn$distance <- as.vector(
      mapply(nn$index_input,
             nn$index_df_predictions,
             FUN=function(index_input,index_df_predictions){
               result <- distances::distance_matrix(distance_object,indices = c(index_input,index_df_predictions))
               result <- as.numeric(result) #result distance matrix has dim=(1,1), turn single value-Matrix to numeric
               return(result)
             }
      )
    )
  }
  nn$index_input<-nn$index_input-nrow(model$df_predictions)
  
  return(nn)
}

calculate_weighting_vector<-function(v_AUC,
                                     p=2
){
  return(pmax(0,(v_AUC-0.5)*2)^p)
}

pmmAggregation<- function(df_labels,
                          data,
                          p=2,
                          indiv_model_function,
                          indiv_model_function_args=NULL,
                          indiv_label_argName="label",
                          indiv_prediction_function=predict,
                          indiv_prediction_args=NULL,
                          indpred_arg_name_modelobj=NULL,
                          indpred_arg_name_newdata=NULL,
                          .progress="none",
                          .parallel=F
                          
){
  #check inputs
  if(!is.function(indiv_model_function)){
    stop("indiv_model_function not recognized as function. Did you write in the argument  accidentally function_name() (with brackets), instead of the correct: function_name ?")
  }
  if(!is.function(indiv_prediction_function)){
    stop("indiv_prediction_function not recognized as function. Did you write in the argument  accidentally function_name() (with brackets), instead of the correct: function_name ?")
  }
  
  train_function<-function(vec_label,
                           wrappedfun=indiv_model_function,
                           arguments=indiv_model_function_args,
                           label_argName=indiv_label_argName
                           ){
    if (is.null(arguments)){
      arguments <- list()
    }
    arguments[[label_argName]]<-vec_label
    result <- do.call(wrappedfun,arguments)
    
    ##if a treining result contains AUC, an AUC Value is present from training
    # if no AUC is given, the result is the model, often itself being a list. make this model to one explicit list element
    # If the AUC is given, the result is already a list, with the model being one element. it does not need to be further listed
    if(is.null(result$AUC)){
      return(list(result))
    }else{
      return(result)
    }
    
  }#end train_function
  
  
  if(.progress!="none"){print("Training models:")}
  ls_models <- llply(c(1:ncol(df_labels)),
                     .fun = function(x)(return(train_function(vec_label=df_labels[[x]]))),
                     .progress = .progress,
                     .parallel = .parallel
                     )
  names(ls_models)<-colnames(df_labels)
  
  #if the AUC is returned by the train function, a single element in ls_models will be (list(model=.model,AUC=.auc_value))
  vec_AUC<-rep_len(NA,ncol(df_labels)) #initialize empty vector
  vec_AUC[lengths(ls_models)==2] <- unlist(lapply(ls_models, `[[`, "AUC"))
  #if a model does not add its own AUC from its Training function, the rest of the AUC values are calculating the AUC using df_prediction
  y_AUC_IsMissing<-is.na(vec_AUC) # denot which AUC are missing
  #auc of the models in the models list is no more needed there, make ls_model list of models
  ls_models<-lapply(ls_models,'[[',1)
  # the dataframe of the predicted values of the labels is generated
  if(.progress!="none"){print("Generating prediction Dataframe:")}
  df_predictions<-generate_prediction_matrix (ls_models=ls_models,
                                              newdata=data,
                                              indiv_prediction_function,
                                              indiv_prediction_args,
                                              indpred_arg_name_modelobj,
                                              indpred_arg_name_newdata,
                                              .progress=.progress,
                                              .parallel=.parallel)
  
  vec_AUC_df_pred <- .calculate_AUC_vector(df_labels,
                                           df_predictions,
                                           .threshold=0.5)
  vec_AUC[y_AUC_IsMissing]<-vec_AUC_df_pred[y_AUC_IsMissing]
  
  
  #calculate weigth vector using the AUC values
  vec_W <- calculate_weighting_vector(vec_AUC,p)
  
  return(
    list(
      ls_models=ls_models,
      df_predictions=df_predictions,
      vec_AUC=vec_AUC,
      vec_W=vec_W,
      df_labels=df_labels,
      func_data=list(
        indiv_model_function=indiv_model_function,
        indiv_model_function_args=indiv_model_function_args,
        indiv_label_argName=indiv_label_argName,
        indiv_prediction_function=indiv_prediction_function,
        indiv_prediction_args=indiv_prediction_args,
        indpred_arg_name_modelobj=indpred_arg_name_modelobj,
        indpred_arg_name_newdata=indpred_arg_name_newdata
        ),
      p=p
      )
    )
}

predict.pmmAggregation<- function(model,
                                  data,
                                  k=10,
                                  str_returnType="knn",
                                  .progress="none",
                                  .parallel=F,
                                  y_include_distances=F
                                  ){
  #check inputs
  allowed_str_returnType <- c("knn","m_pred")
  if(!(str_returnType %in% allowed_str_returnType)){
    stop(
      sprintf("%s not in allowed return types: %s",str_returnType, paste(allowed_str_returnType,collapse = " , "))
    )
  }
  #model predictions
  result <- generate_prediction_matrix(model$ls_models,
                                        data,
                                        indiv_prediction_function=model$func_data$indiv_prediction_function,
                                        indiv_prediction_args=model$func_data$indiv_prediction_args,
                                        indpred_arg_name_modelobj=model$func_data$indpred_arg_name_modelobj,
                                        indpred_arg_name_newdata=model$func_data$indpred_arg_name_newdata,
                                        .progress=.progress,
                                        .parallel=.parallel
                                        )
  #if only prediction matrix is requested, stop here
  if(str_returnType=="m_pred"){return(result)}
  
  
  #determine Nearest neighbours
  result <- .df_prediction_to_knn(result,
                        model$df_predictions,
                        model$vec_W,
                        k=k,
                        y_include_distances = y_include_distances)
  return(result)
}#end predict.pmmAggregation

evaluate.pmmAggregation<- function(model,
                                   testing_predictions=NULL,
                                   testing_df_label=NULL,
                                   kmax=NULL,
                                   .progress="none",
                                   .parallel=F
){
  if(is.null(testing_predictions)){
    testing_predictions <- model$df_predictions
    warning(" no testing_df_predictions given, using the df_predictions of training data \n")
  }
  if(is.null(testing_df_label)){
    testing_df_label <- model$df_labels
    warning(" no testing_df_label given, using the df_labels of training data \n")
  }
  if(is.null(kmax)){
    kmax=nrow(testing_predictions)
  }
  if(nrow(testing_predictions)!=nrow(testing_df_label)){
    stop(sprintf("Number of rows feature data (%d) and number of rows on testing_df_label do not match(%d) \n ",nrow(testing_data),nrow(testing_df_label)))
  }
  
  df_reference<- .df_prediction_to_knn(testing_predictions,model$df_labels,model$vec_W,k=nrow(model$df_labels),y_include_distances=T)
  df_reference <-df_reference %>% arrange(index_input,index_df_predictions)
  
  df_reference_true<- .df_prediction_to_knn(testing_df_label,model$df_labels,rep_len(1,length(model$vec_W)),k=nrow(model$df_labels),y_include_distances=T)
  df_reference_true <- df_reference_true %>% arrange(index_input,index_df_predictions)
  
  
  df_reference$dist_true <- df_reference_true$distance
  
  
  df_reference <-  df_reference %>%
    dplyr::arrange(index_input,k) %>%
    dplyr::group_by(index_input) %>%
    dplyr::mutate(avg_nbhood_dist=cummean(dist_true),rand_knn=sample(k),
                  avg_perfect_nbhood_dist=cummean(sort(dist_true)),
                  avg_rand_nbhood_dist=cummean(dist_true[order(rand_knn)])
    )%>% 
    dplyr::ungroup() %>%
    dplyr::group_by(k) %>% 
    dplyr::summarise(t_value=t.test(avg_nbhood_dist,avg_rand_nbhood_dist, alternative = "less")$statistic,
                     t_value_perfect=t.test(avg_perfect_nbhood_dist,avg_rand_nbhood_dist, alternative = "less")$statistic
    )
  

  
  return(df_reference)
}

update_weights<- function(model,p=NULL,y_updateAUC=F,vec_AUC=NULL){
  if(is.null(p)){p=model$p}
  if(y_updateAUC){
    model$vec_AUC=.calculate_AUC_vector(model$df_labels,
                                        model$df_predictions,
                                        .threshold=0.5)
  }
  if(!is.null(vec_AUC)){
    if(length(vec_AUC)!=length(model$df_labels)){
      stop(sprintf("number of Labelcolumns(%d) and Number of new AUC(%d) do not match.",length(model$df_labels,length(vec_AUC))))
    }else{
      model$vec_AUC <- vec_AUC
    }
  }
  
  model$vec_W<-calculate_weighting_vector(model$vec_AUC,p)
  return(model)
}


