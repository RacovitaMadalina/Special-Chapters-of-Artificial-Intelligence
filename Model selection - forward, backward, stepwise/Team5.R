library(dplyr)

house_dataset = read.table("http://profs.info.uaic.ro/~cgatu/csia/res/house.dat", header=TRUE)

head(house_dataset)
summary(house_dataset)

print(paste("The number of columns is: ", length(colnames(house_dataset))))
print(paste("The number of observations is: ", length(house_dataset$PRICE)))

house_dataset


# FORWARD SELECTION - Steps:
# 1.Start with no variable.
# 2. For each variable NOT in the model, check the p-value if there are added in the model.
# 3. Choose the one with lowest p-value less than alpha.
# 4. Continue until no new variable can be added.

forward_model_selection <- function(X, Y){
  all_predictors = colnames(X)
  predictors_best_submodel_current_step = c()
  alpha = 0.05 # significance level
  
  while(TRUE){ 
    # iterate until there is no available variable significant enough
    predictors_to_be_checked = setdiff(all_predictors, predictors_best_submodel_current_step)
    
    lowest_p_value = NULL
    predictor_for_lowest_p_value = NULL
    
    for(current_variable in predictors_to_be_checked){
      predictors_submodel = c(predictors_best_submodel_current_step, current_variable)
      submodel_X = X %>% select(predictors_submodel)
      
      simple_regression_model = lm(Y ~ ., data = submodel_X)
      
      # print(summary(simple_regression_model))
      
      # the p value for the current variable is found in the column Pr(>|t|)
      # print(summary(simple_regression_model)$coefficients[,4])
      p_value_current_variable = summary(simple_regression_model)$coefficients[,4][current_variable]
      
      if(p_value_current_variable < alpha){
        if(is.null(predictor_for_lowest_p_value) || (lowest_p_value > p_value_current_variable)){
          lowest_p_value = p_value_current_variable
          predictor_for_lowest_p_value = current_variable
        }
      } 
    }
   
    if(is.null(predictor_for_lowest_p_value) || (length(predictors_to_be_checked)== 0)){
      print(paste("The best submodel for the forward model selection is formed of: ", paste(predictors_best_submodel_current_step, collapse=', ' )))
      return(predictors_best_submodel_current_step)
      break;
    }
    
    predictors_best_submodel_current_step = c(predictors_best_submodel_current_step, predictor_for_lowest_p_value)
    print(paste("The predictor with the lowest p-value chosen at the current step is: ", predictor_for_lowest_p_value))
  }
}



# BACKWARD SELECTION - Steps:
# 1. Start with all variables in the model.
# 2. Remove the variable with the highest p-value greater than alpha.
# 3. Refit the model and go to step 2.
# 4. Stop when ALL p-values are less than alpha.

backward_model_selection <- function(X, Y){
  all_predictors = colnames(X)
  predictors_best_submodel_current_step = all_predictors
  alpha = 0.05 # significance level
  
  while(TRUE){ 
    # iterate until there is no available variable significant enough
    
    predictors_to_be_checked = predictors_best_submodel_current_step
    
    biggest_p_value = NULL
    predictor_for_biggest_p_value = NULL
    
    for(current_variable in predictors_to_be_checked){
      predictors_submodel = predictors_to_be_checked
      submodel_X = X %>% select(predictors_submodel)
      
      simple_regression_model = lm(Y ~ ., data = submodel_X)
      
      # print(summary(simple_regression_model))
      
      # the p value for the current variable is found in the column Pr(>|t|)
      p_value_current_variable = summary(simple_regression_model)$coefficients[,4][current_variable]
      
      if(p_value_current_variable > alpha){
        if(is.null(predictor_for_biggest_p_value) || (biggest_p_value < p_value_current_variable)){
          biggest_p_value = p_value_current_variable
          predictor_for_biggest_p_value = current_variable
        }
      } 
    }
   
    if(is.null(predictor_for_biggest_p_value) || (length(predictors_to_be_checked) == 0)){
      print(paste("The best submodel for the backward model selection is formed of: ", paste(predictors_best_submodel_current_step, collapse=', ' )))
      return(predictors_best_submodel_current_step)
      break;
    }
    
    predictors_best_submodel_current_step = predictors_best_submodel_current_step[predictors_best_submodel_current_step != predictor_for_biggest_p_value]
    print(paste("The predictor with the biggest p-value chosen at the current step is: ", predictor_for_biggest_p_value))
  }
}



# STEPWISE MODEL SELECTION

stepwise_model_selection <- function(X, Y){
  all_predictors = colnames(X)
  predictors_best_submodel_current_step = c()
  alpha = 0.05 # significance level
  
  while(TRUE){ 
    # iterate until there is no available variable significant enough
    predictors_to_be_checked = setdiff(all_predictors, predictors_best_submodel_current_step)
    
    lowest_p_value = NULL
    predictor_for_lowest_p_value = NULL
    
    for(current_variable in predictors_to_be_checked){
      predictors_submodel = c(predictors_best_submodel_current_step, current_variable)
      submodel_X = X %>% select(predictors_submodel)
      
      simple_regression_model = lm(Y ~ ., data = submodel_X)
      
      # print(summary(simple_regression_model))
      
      # the p value for the current variable is found in the column Pr(>|t|)
      # print(summary(simple_regression_model)$coefficients[,4])
      p_value_current_variable = summary(simple_regression_model)$coefficients[,4][current_variable]
      
      if(p_value_current_variable < alpha){
        if(is.null(predictor_for_lowest_p_value) || (lowest_p_value > p_value_current_variable)){
          lowest_p_value = p_value_current_variable
          predictor_for_lowest_p_value = current_variable
        }
      } 
    }
    
    if(is.null(predictor_for_lowest_p_value) || (length(predictors_to_be_checked)== 0)){
      print(paste("The best submodel for the stepwise model selection is formed of: ", paste(predictors_best_submodel_current_step, collapse=', ' )))
      return(predictors_best_submodel_current_step)
      break;
    }
    
    predictors_best_submodel_current_step = c(predictors_best_submodel_current_step, predictor_for_lowest_p_value)
    print(paste("The predictor with the lowest p-value chosen at the current step is: ", predictor_for_lowest_p_value))
    
    # The backward call
    
    while(TRUE){ 
      # iterate until there is no available variable significant enough
      
      predictors_to_be_checked = predictors_best_submodel_current_step
      
      biggest_p_value = NULL
      predictor_for_biggest_p_value = NULL
      
      for(current_variable in predictors_to_be_checked){
        predictors_submodel = predictors_to_be_checked
        submodel_X = X %>% select(predictors_submodel)
        
        simple_regression_model = lm(Y ~ ., data = submodel_X)
        
        # print(summary(simple_regression_model))
        
        # the p value for the current variable is found in the column Pr(>|t|)
        p_value_current_variable = summary(simple_regression_model)$coefficients[,4][current_variable]
        
        if(p_value_current_variable > alpha){
          if(is.null(predictor_for_biggest_p_value) || (biggest_p_value < p_value_current_variable)){
            biggest_p_value = p_value_current_variable
            predictor_for_biggest_p_value = current_variable
          }
        } 
      }
      
      if(is.null(predictor_for_biggest_p_value) || (length(predictors_to_be_checked) == 0)){
        print(paste("The best submodel remained after the backward call is formed of: ", paste(predictors_best_submodel_current_step, collapse=', ' )))
        break;
      }
      
      predictors_best_submodel_current_step = predictors_best_submodel_current_step[predictors_best_submodel_current_step != predictor_for_biggest_p_value]
      print(paste("The predictor with the biggest p-value chosen at the current step is: ", predictor_for_biggest_p_value))
    }
  }
}

X_house = house_dataset[,names(house_dataset) != "PRICE"]
Y_house = house_dataset$PRICE

best_submodel_forward_selection = forward_model_selection(X_house, Y_house)
best_submodel_backward_selection = backward_model_selection(X_house, Y_house)
best_submodel_stepwise_selection = stepwise_model_selection(X_house, Y_house)
all_h4_submodel = c("FLR", "RMS", "ST", "LOT", "BDR", "CON", "GAR", "L2")

all_y_tilda = predict(lm(Y~.,data=X[,all_h4_submodel]), X[,all_h4_submodel])
forward_y_tilda = predict(lm(Y ~., data=X[,best_submodel_forward_selection]), X[,best_submodel_forward_selection])
backward_y_tilda = predict(lm(Y ~., data=X[,best_submodel_backward_selection]), X[,best_submodel_backward_selection])
stepwise_y_tilda = predict(lm(Y ~., data=X[,best_submodel_stepwise_selection]), X[,best_submodel_stepwise_selection])

plot(0, main="Variable selections methods", xlab="y ", ylab="y_tilda", pch=19, xlim=c(38, 90), ylim=c(38, 90))
abline(0,1, col='green', lwd=2)
points(Y, all_y_tilda, pch=20)  # black-best-h4

# the predictions of the backward methos is apx the same with the ones resulted from the exhaustive generation since the 
# model returned by the exhaustive generation has 2 more variables: "RMS" and "BDR"
points(Y, backward_y_tilda, col=rgb(0.18,0.545,0.3411), bg=rgb(0.18,0.545,0.3411), pch=20)  # green-backward!

# the predictions will be the same since the submodels are the same
points(Y, forward_y_tilda, col=rgb(1,0,0,0.5), bg=rgb(1,0,0,0.5), pch=23)  # red-forward
points(Y, stepwise_y_tilda, col=rgb(0,0,1,0.32), bg = rgb(0,0,1,0.32), pch=22) # blue-stepwise
