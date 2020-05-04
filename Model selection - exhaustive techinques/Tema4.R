# library(dplyr)
library(lmSubsets)

house_dataset = read.table("http://profs.info.uaic.ro/~cgatu/csia/res/house.dat", header=TRUE)

head(house_dataset)
summary(house_dataset)

print(paste("Numarul de coloane din house_dataset este:", length(colnames(house_dataset))))
print(paste("Numarul de inregistrari din dataset este:", length(house_dataset$PRICE)))

house_dataset

feature_names = colnames(house_dataset)

get_all_feature_combinations_specific_len <- function(features, of_length){
  return(combn(features, of_length))
}

compute_beta_by_normal_equation <- function(X, Y){
  X = cbind(Intercept=1, X)
  return(solve(t(X) %*% X) %*% t(X) %*% Y)
}

compute_rss <- function(Y, Y_tilda){
  return(sum((Y- Y_tilda) * (Y- Y_tilda)))
}

compute_tss <- function(Y){
  mean_price = mean(Y)
  return(sum((Y- mean_price) ** 2))
}

compute_r_squared <- function(Y, Y_tilda){
  return(1 - (compute_rss(Y, Y_tilda) / compute_tss(Y)))
}

compute_r_squared_adjusted <- function(Y, Y_tilda, features_number){
  return(1 - (1 - compute_r_squared(Y, Y_tilda)) * ((length(Y) - 1) / (length(Y) - features_number - 1)))
}

compute_mallows_cp <- function(Y, Y_tilda, Y_tilda_full_model, features_number){
  return((compute_rss(Y, Y_tilda) / compute_rss(Y, Y_tilda_full_model)) * (length(Y) - 13 - 1) - (length(Y) - 2 * (features_number+ 1)))
}

compute_how_much_bias = function(Y, Y_tilda, Y_tilda_full_model, features_number) {
  mallows_cp_coeff = compute_mallows_cp(Y, Y_tilda, Y_tilda_full_model, features_number)
  return(abs(mallows_cp_coeff-features_number-1))
}

compute_Y_tilda_for_current_submodel <- function(X, Y){
  return(cbind(Intercept=1, X) %*% compute_beta_by_normal_equation(X, Y))
}

compute_aic <- function(Y, Y_tilda, features_number){
  return(2*(features_number + 1) + length(Y)* log10(compute_rss(Y, Y_tilda) / length(Y)))
}


generate_regression_models_score_optim <- function(dataset, score_function, function_index){
  to_return_matrix = matrix(nrow = 13, ncol = 13)
  X = dataset[,names(dataset) != "PRICE"]
  Y = dataset$PRICE
  optimum_scores = c()
  y_tilda_full = compute_Y_tilda_for_current_submodel(as.matrix(X), Y)
  
  for(p in seq(1, 13)){
    current_combinations_list = get_all_feature_combinations_specific_len(colnames(X), p)
    score_vec = c()
    for(index in seq(choose(length(colnames(X)),p))){
      current_X <- X %>% select(current_combinations_list[, index])
      # print(colnames(current_X))
      
      current_Y_tilda <- compute_Y_tilda_for_current_submodel(as.matrix(current_X), Y)
      if(function_index == 1 || function_index == 2){
        score_vec = append(score_vec, score_function(Y, current_Y_tilda))
      }
      if(function_index == 4) {
        score_vec = append(score_vec, score_function(Y, current_Y_tilda, y_tilda_full, p))
      }
      if(function_index == 3 || function_index == 5) {
        score_vec = append(score_vec, score_function(Y, current_Y_tilda, p))
      }
    }
    if(function_index == 1 || function_index == 4){
      optimum_score_subset_index = which.min(score_vec)
      optimum_scores = append(optimum_scores, min(score_vec))
    }
    else {
      optimum_score_subset_index = which.max(score_vec)
      optimum_scores = append(optimum_scores, max(score_vec))
      print(max(score_vec))
    }

    features_combination = current_combinations_list[,optimum_score_subset_index]
    print(features_combination)
    for(i in seq(1, p)){
      to_return_matrix[p, i] = features_combination[i]
    }
  }
  return(c(optimum_scores, to_return_matrix))
}

result_rss = generate_regression_models_score_optim(house_dataset, compute_rss, 1)
result_rss = matrix(result_rss, 13, 14)
print(result_rss)
rss_scores = result_rss[,1]
plot(rss_scores, xlab="Number of features", ylab='RSS', main='Residual Sum of Squares', type='o', col='red', lwd=2.5)

result_r2 = generate_regression_models_score_optim(house_dataset, compute_r_squared, 2)
result_r2 = matrix(result_r2, 13, 14)
print(result_r2)
r_squared_scores = result_r2[,1]
plot(r_squared_scores, xlab="Number of features", ylab='Value R**2', main='R Squared', type='o', col='green', lwd=2.5)

result_r2_adj = generate_regression_models_score_optim(house_dataset, compute_r_squared_adjusted, 3)
result_r2_adj = matrix(result_r2_adj, 13, 14)
print(result_r2_adj)
r_squared_adj_scores = result_r2_adj[,1]
plot(r_squared_adj_scores, xlab="Number of features", ylab='Value R**2_adj', main='R Squared Adjusted', type='o', col='purple', lwd=2.5)

result_bias = generate_regression_models_score_optim(house_dataset, compute_how_much_bias, 4)
result_bias = matrix(result_bias, 13, 14)
print(result_bias)
bias_scores = result_bias[,1]
plot(bias_scores, xlab="Number of features", ylab='Value |Cp - p|', main='Absolute value of Mallows Cp - p', type='o', col='orange', lwd=2.5)
abline(0,0)

result_aic = generate_regression_models_score_optim(house_dataset, compute_aic, 5)
result_aic = matrix(result_aic, 13, 14)
print(result_aic)
aic_scores = result_aic[,1]

print(paste("Model with smallest bias has ", which.min(result_bias[1:10,1]), " features"))
print(paste("Model with smallest RSS has ", which.min(result_rss[,1]), " features"))
print(paste("Model with best R2 has ", which.max(result_r2[,1]), " features"))
print(paste("Model with best r2_adj has ", which.max(result_r2_adj[,1]), " features"))

print(paste("Best submodel is = "))
print(result_rss[which.max(result_r2_adj[,1]),])

print("Verification: ")
subset_selection_model = lmSelect(PRICE~., data=house_dataset)
print(subset_selection_model)
