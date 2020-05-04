library(factoextra)
library(FactoMineR)

setwd("C:\\Users\\mracovita\\OneDrive - ENDAVA\\Desktop\\Facultate\\csia\\")
swiss_dataset <- read.table("swiss.txt", header = TRUE)

head(swiss_dataset)
summary(swiss_dataset)
cor(swiss_dataset)

pca_result <- prcomp(swiss_dataset, scale = TRUE)

# Visualize the variance explained
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualize the variance explained
cummulative_varince = cumsum(pca_result$sdev^2/sum(pca_result$sdev^2))
plot(cummulative_varince, type='b')

# Graph of individuals. Visualize the results individuals and variables, respectively.
fviz_pca_ind(pca_result,col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)   

eigenvalues <- get_eigenvalue(pca_result)
eigenvalues

# Correlation circle.The correlation between a variable and a principal component
fviz_pca_var(pca_result, col.var = "black")
