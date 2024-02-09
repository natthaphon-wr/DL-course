library(dplyr)
library(caret)
library(ggplot2)
library(cowplot)


data("iris")
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(colour = factor(Species)))
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_point(aes(colour = factor(Species)))

dummy <- dummyVars(" ~ .", data=iris)
iris_encoded <- data.frame(predict(dummy, newdata=iris))


logistic_regression <- function(theta, x) {
  f = unlist(theta %*% x)
  return(1/(1+exp(-f)))
}

gradient_des <- function(species, coef){
  H_theta <- apply(iris_encoded %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                   1, logistic_regression, theta = coef)
  gradient <- (H_theta - iris_encoded[,species]) %*% as.matrix(iris_encoded[,1:4])
  coef <- coef - alpha*gradient
  
  return(list(H_theta=H_theta, loss=sum(gradient)))
}

learn <- function(alpha, lambda, sampling){
  coef_se <- rnorm(4)
  coef_ver <- rnorm(4)
  coef_vir <- rnorm(4)
  H_theta_se <- list()
  H_theta_ver <- list()
  H_theta_vir <- list()
  gradient_all = data.frame(matrix(nrow = 0, ncol=3))
  
  for(i in 1:100) {
    # Subsampling data
    iris_encoded <- iris_encoded[sample(1:nrow(iris_encoded), nrow(iris_encoded)*sampling, replace=FALSE),]
    
    # Setosa
    H_theta_se <- apply(iris_encoded %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                     1, logistic_regression, theta = coef_se)
    gradient_se <- (H_theta_se - iris_encoded[,'Species.setosa']) %*% as.matrix(iris_encoded[,1:4]) + lambda*coef_se
    coef_se <- coef_se - alpha*gradient_se
    
    # Versicolor
    H_theta_ver <- apply(iris_encoded %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                        1, logistic_regression, theta = coef_ver)
    gradient_ver <- (H_theta_ver - iris_encoded[,'Species.versicolor']) %*% as.matrix(iris_encoded[,1:4]) + lambda*coef_ver
    coef_ver <- coef_ver - alpha*gradient_ver
  
    # Virginica
    H_theta_vir <- apply(iris_encoded %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                        1, logistic_regression, theta = coef_vir)
    gradient_vir <- (H_theta_vir - iris_encoded[,'Species.virginica']) %*% as.matrix(iris_encoded[,1:4]) + lambda*coef_vir
    coef_vir <- coef_vir - alpha*gradient_vir
    
    # Find argmax
    Est_df <- data.frame(Setosa = unlist(H_theta_se), 
                         Versicolor = unlist(H_theta_ver), 
                         Virginica = unlist(H_theta_vir)) 
    Est_df$argmax <- colnames(Est_df)[max.col(Est_df)]
    Est_df$max <- apply(Est_df[,1:3], 1, max, na.rm=TRUE)
    
    # Return loss
    gradient <- data.frame(se=c(sum(gradient_se)), ver=c(sum(gradient_ver)), vir=c(sum(gradient_vir)))
    gradient_all <- rbind(gradient_all, gradient)
  }
  
  se_plt <- ggplot(gradient_all, aes(x=1:100, y=se)) +
    geom_point() +
    geom_line()  +
    labs(x="Iteration", y="Loss", title="Setosa")
  
  ver_plt <- ggplot(gradient_all, aes(x=1:100, y=ver)) +
    geom_point() +
    geom_line() +
    labs(x="Iteration", y="Loss", title="Versicolor")
  
  vir_plt <- ggplot(gradient_all, aes(x=1:100, y=vir)) +
    geom_point() +
    geom_line() +
    labs(x="Iteration", y="Loss", title="Virginica")
  
  plot_grid(se_plt, ver_plt, vir_plt, nrow=3, ncol=1)
}

alpha = 0.2    #learning rate

learn(alpha, lambda=0, sampling=1)
learn(alpha, lambda=0.5, sampling=1)
learn(alpha, lambda=1, sampling=1)
learn(alpha, lambda=2, sampling=1)

learn(alpha, lambda=0, sampling=0.25)
learn(alpha, lambda=0, sampling=0.5)
learn(alpha, lambda=0, sampling=0.75)
learn(alpha, lambda=0, sampling=1)

learn(alpha, lambda=0.5, sampling=0.25)