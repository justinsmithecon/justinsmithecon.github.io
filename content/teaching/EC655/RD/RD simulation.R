library(tidyverse)
library(magrittr)
library(ggthemes)
library(car)
library(lmtest)
library(sandwich)
library(vtable)
library(AER)

data <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 10,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)

funy0 <- function(s) 2 + 5*s
funy1 <- function(s) 12 + 5*s

ggplot(data = data, aes(x = s, y = y)) +
  geom_point(alpha = .2) +
  stat_function(fun = funy0, geom="line", xlim=c(0,5), size = 1.5) +
  stat_function(fun = funy1, geom="line", xlim=c(5,10), size = 1.5) +
  stat_function(fun = funy0, geom="line", xlim=c(5,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funy1, geom="line", xlim=c(0,5), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Sharp RD with Different Slopes") 
  
  
data1 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 3*s,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)  

funy0 <- function(s) 2 + 5*s
funy1 <- function(s) 2 + 8*s


ggplot(data = data1, aes(x = s, y = y)) +
  geom_point(alpha = .2) +
  stat_function(fun = funy0, geom="line", xlim=c(0,5), size = 1.5) +
  stat_function(fun = funy1, geom="line", xlim=c(5,10), size = 1.5) +
  stat_function(fun = funy0, geom="line", xlim=c(5,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funy1, geom="line", xlim=c(0,5), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
labs(title ="Regression Discontinuity with Constant Effects") 



data2 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 3*s - 0.5*s^2 + 0.1*s^3 + eta,
         y1 = y0 -s - .3*s^2 +0.2*s^3,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)  

funy0 <- function(s) 2 + 3*s - 0.5*s^2 + 0.1*s^3
funy1 <- function(s) 2 +2*s - .8*s^2 + .3*s^3

ggplot(data = data2, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  stat_function(fun = funy0, geom="line", xlim=c(0,5), size = 1.5) +
  stat_function(fun = funy1, geom="line", xlim=c(5,10), size = 1.5) +
  stat_function(fun = funy0, geom="line", xlim=c(5,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funy1, geom="line", xlim=c(0,5), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Sharp RD with Nonlinear Effects")
  
  
data3 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 10,
         w0 = rbinom(1000,1,0.01*s),
         w1 = rbinom(1000,1,0.01*s + 0.6),
         z = if_else( s >= 5,1,0),
         w = w0 + (w1 - w0)*z,
         y = y0 + (y1 - y0)*w) 
  
  
funfs0 <- function(s) 0.01*s
funfs1 <- function(s) 0.01*s+0.6

funy0 <- function(s) 2 + 5*s
funy1 <- function(s) 7 + 5*s

funobs0 <- function(s) 2 + 5.05*s
funobs1 <- function(s) 5 + 5.05*s

ggplot(data = data3, aes(x = s, y = w)) +
  geom_point(alpha = .2) + 
  stat_function(fun = funfs0, geom="line", xlim=c(0,5), size = 1.5) +
  stat_function(fun = funfs1, geom="line", xlim=c(5,10), size = 1.5) +
  stat_function(fun = funfs0, geom="line", xlim=c(5,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funfs1, geom="line", xlim=c(0,5), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD First Stage")


ggplot(data = data3, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  stat_function(fun = funy0, geom="line", xlim=c(0,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funy1, geom="line", xlim=c(0,10), size = 1.5, alpha = .2, color = 'red', linetype='longdash') +
  stat_function(fun = funobs0, geom="line", xlim=c(0,5), size = 1.5) +
  stat_function(fun = funobs1, geom="line", xlim=c(5,10), size = 1.5) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD with Constant Effect")

