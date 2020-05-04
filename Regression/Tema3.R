# Exercitiul 1

# install.packages("dplyr")
library(dplyr)

alcool_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/alcool.dat",header=TRUE)
head(alcool_df)
summary(alcool_df)

# Diagrama de imprastiere

plot(alcool_df%>%select(2:3), main="Diagrama de imprastiere ALCOOL DATASET",
     xlab="Consum mediu anual de vin in litri/persoana", 
     ylab="Nr. decese anuale la 100000/loc (boli cardiace)", type='p', col='darkgreen', pch=19)
abline(lm(alcool_df$Decese_datorate_afectiunilor_cardiace ~ alcool_df$Alcool_din_vin), col="red", lw=2) # regression line (y~x)
lines(lowess(alcool_df$Alcool_din_vin, alcool_df$Decese_datorate_afectiunilor_cardiace), col="blue") # lowess line (x,y)

# Corelatia celor doua variabile numerice
cor(alcool_df$Alcool_din_vin, alcool_df$Decese_datorate_afectiunilor_cardiace)

# corelatia negativa inseamna ca daca variabila y scade atunci x xa creste 



# --------------------------------------------------------------------------------------
# Exercitiul 2 

iq_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/iq.dat", header=TRUE)
head(iq_df)
colnames(iq_df)
length(iq_df$Student)

# Diagrama de imprastiere

plot(iq_df%>%select(2:3), main="Diagrama de imprastiere IQ DATASET",
     xlab="IQ student", ylab="Nota", type='p', col='darkgreen', pch=19)
abline(lm(iq_df$Nota ~ iq_df$IQ), col='red', lw=2)

model_simple_lm = lm(iq_df$Nota ~ iq_df$IQ)
summary(model_simple_lm)

# coeficienti pentru modelul de regresie simpla
beta = model_simple_lm$coefficients

# etapa de predictie
test_data <- data.frame("Intercept"= c(1, 1), "IQ"= c(115, 130))

y_tilda <- as.matrix(test_data) %*% as.matrix(beta)

nota_student1 = y_tilda[1]
nota_student1

nota_student2 = y_tilda[2]
nota_student2


# -----------------------------------------------------------------------------------------------
# Exercitiul 3                                          FORMULA: y[i] = a + b * x[i] + epsilon[i] 

gereate_dataframe_from_lm_model = function(m, a, b, xmin, xmax, sigma) {
  x = runif(m, min = xmin, max = xmax)
  epsilon = rnorm(m, mean = 0, sd = sigma)
  y = c(a + b * x + epsilon)
  return (data.frame(x=x, y=y))
}

gereate_dataframe_from_lm_model(m=10, a=1, b=2, xmin=0, xmax=10, sigma=1)

# -----------------------------------------------------------------------------------------------
# Exercitiul 4 

m = 10
a = 1
b = 2
xmin = 0
xmax = 10
sigma = 1

compute_ssr <- function(y, y_tilda){
  ssr = 0
  for(i in seq(length(y_tilda))){
    ssr = ssr + (y[i] - y_tilda[i])^2
  }
  ssr
}


get_confint_coeficients_lm <- function(m, a, b, xmin, xmax, sigma){
  dataset_for_lm = gereate_dataframe_from_lm_model(m, a, b, xmin, xmax, sigma)
  model_simple_regression = lm(dataset_for_lm$y ~ dataset_for_lm$x)
  
  print(summary(model_simple_regression))
  
  beta = model_simple_regression$coefficients
  print(beta)
  
  b_tilda = beta[2]
  
  b_tilda_2 = (dataset_for_lm$x - mean(dataset_for_lm$x)) %*% as.matrix(dataset_for_lm$y - mean(dataset_for_lm$y)) / 
              ((dataset_for_lm$x - mean(dataset_for_lm$x)) %*% as.matrix(dataset_for_lm$x - mean(dataset_for_lm$x)))
  print(paste("b_tilda lm = ", b_tilda))
  print(paste("b_tilda calcul direct = ", b_tilda_2))
  
  a_tilda = beta[1]
  a_tilda_2 = mean(dataset_for_lm$y) - b_tilda_2 * mean(dataset_for_lm$x)
  print(paste("a_tilda lm = ", a_tilda))
  print(paste("a_tilda calcul direct = ", a_tilda_2)) 
  
  print("-----------------------------------")
  degrees_of_freedom = m - 2
  x = dataset_for_lm$x
  y = dataset_for_lm$y
  
  y_tilda = a_tilda + b_tilda * x
  
  # ssr_2 = compute_ssr(y, y_tilda)
  ssr = sum(resid(model_simple_regression)^2) 
  sd_y = sqrt(ssr / degrees_of_freedom)
  
  standard_error_b = (sigma/sqrt(m)) * (1/sd(x))
  standard_error_a = sd(x)*(sqrt(1/m + (mean(x)*mean(x))/sum(x*x)))
  cat("SE a = ", standard_error_a,"\n")
  cat("SE b = ", standard_error_b,"\n")
  
  t_value = abs(qt(0.025, degrees_of_freedom))
  plus_minus = t_value * sd_y / sqrt(sum(x*x))
  
  cat("Confidence intervals for beta:", b_tilda - plus_minus, " ", b_tilda + plus_minus,"\n")
  
  return(c(a_tilda, b_tilda, dataset_for_lm))
}

ex_4 = get_confint_coeficients_lm(m, a, b, xmin, xmax, sigma)


# -----------------------------------------------------------------------------------------------
# Exercitiul 5

plot_true_and_predicted_regressions = function(x, y, coef_model, index, a, b) {
  pdf(paste("plot_regression", index, ".pdf"), width=5, height=5, paper='special')
  plot(y~x, col="grey") 
  abline(a, b, col='green', lw=6)
  a_tilda = coef_model$`(Intercept)`
  b_tilda = coef_model$`dataset_for_lm$x`
  y_tilda = a_tilda + b_tilda * x
  abline(a_tilda, b_tilda, col="red", lw=2)
  dev.off ();
  
  plot(y~x, col="grey") 
  abline(a_tilda, b_tilda, col='green', lw=2)
  abline(a, b, col="red", lw=1)
}

plot_and_regression = function(m, a, b, xmin, xmax, sigma, index) {
  coef_model = get_confint_coeficients_lm(m, a, b, xmin, xmax, sigma)
  x = coef_model$x
  y = coef_model$y
  plot_true_and_predicted_regressions(x, y, coef_model, index, a, b)
}

plot_and_regression(100, 3, 5, -200, 200, 1, 1)
plot_and_regression(10, 3, 5, -5, 5, 1, 2)
plot_and_regression(10000, 3, 5, -5, 5, 1, 3)
plot_and_regression(10, 3, 5, 5, 5.2, 1, 4)
plot_and_regression(10000, 3, 5, 5, 5.2, 1, 5)
plot_and_regression(10000, 3, 5, 5, 5.2, 0, 6)
plot_and_regression(10, 3, 5, 5, 5.2, 0.01, 7)
