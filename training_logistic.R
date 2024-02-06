logistic_regression <- function(theta, x) {
  f = unlist(theta %*% x)
  return(1/(1+exp(-f)))
}

library(MASS)
library(dplyr)
library(ggplot2)
library(gganimate)
npoints = 30
alpha = 0.2
set.seed(1)
data_1 <- mvrnorm(n = npoints, mu=c(1,1),Sigma = matrix(c(0.5,0,0,0.5),nrow = 2, ncol = 2))
data_2 <- mvrnorm(n = npoints, mu=c(3,3),Sigma = matrix(c(0.5,0,0,0.5),nrow = 2, ncol = 2))

cdata <- data.frame(X0=1,rbind(data_1,data_2),Y=c(rep(0,npoints),rep(1,npoints)))

# initialize parameters
coef <- rnorm(3)

result <- data.frame(iteration=as.integer(),m=as.numeric(),b=as.numeric()) 
for(i in 1:100) {
  H_theta <- apply(cdata %>% select(X0,X1,X2), 1, logistic_regression, theta = coef)
  gradient <- (H_theta - cdata[,'Y']) %*% as.matrix(cdata[,1:3])
  coef <- coef - alpha*gradient
  
  result %>% add_row(iteration = i, m = -coef[2]/coef[3], b = -coef[1]/coef[3]) -> result
  print(sum(gradient))

}

a<-ggplot(cdata) + 
  geom_point(aes(x=X1,y=X2,color=as.character(Y), shape=as.character(Y)),size=3, ) +
  geom_abline(data = result, aes(intercept = b, slope = m), alpha = 0.3, size = 2) +
  theme_light() + theme(panel.grid.major = element_blank(), 
                        text = element_text(family = 'Arial', size = 16),
                        axis.text = element_text(family = 'Arial', size = 16),
                        legend.text = element_text(family = 'Arial', size = 16)) + 
  guides(color=guide_legend(title="Class"), shape=FALSE) + transition_time(iteration) +
  labs(title = "Iteration: {frame_time}")
animate(a, duration = 8, width=500, height=500)
# anim_save('logistic.gif')
