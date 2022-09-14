## ---- echo=FALSE, include=FALSE---------------------------------------------------------------------------------

library(sf)
library(scales)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(viridis)
library(DT)
library(plotly)
library(readxl)
library(rgeos)
library(maptools)
library(directlabels)
library(ggrepel)
library(ggthemes)
library(cancensus)
library(bookdown)
library(geomtextpath)
library(egg)
library(ggtext)
library(ragg)
library(scico)
library(haven)
library(here)
library(knitr)
library(jpeg)
library(imager)
library(Statamarkdown)
library(ggdag)
library(dagitty)
library(geomtextpath)
library(gganimate)
library(gifski)
library(vtable)

knitr::opts_chunk$set(
 echo=FALSE, 
 message = FALSE,
 warning = FALSE,
 fig.retina = 3, 
 fig.align = "center"
 )

set.seed(9810078)


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
data <- data.frame(eta=rnorm(100000,0,1)) %>%
  mutate(y0 = 2 + eta, y1 = y0 + 5, 
         treat_eff = y1 - y0)

sumtable(data, summ=c('notNA(x)','mean(x)','sd(x)'))


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
data %<>% mutate(w = if_else(runif(100000) > .5,1,0), 
                 y = y0 + (y1-y0)*w) %>% 
  group_by(w)

head(data)


## ---- echo=TRUE, fig.height = 4.55------------------------------------------------------------------------------
ggplot(data, aes(x=y0, color=as.factor(w))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y0")


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
summarize(data, mean(y))
summarize(data,mean(y))$`mean(y)`[2] - 
  summarize(data,mean(y))$`mean(y)`[1]


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
lm(y ~ w, data)


## ---- echo=TRUE, results=FALSE----------------------------------------------------------------------------------
data2 <- data %>% 
  ungroup() %>% 
  select(eta, y0,y1) %>%
  mutate(w = if_else(eta + runif(100000,-1,1) > 0,1,0), 
         y = y0 + (y1-y0)*w) %>%
  group_by(w)

sumtable(data2, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- results=TRUE----------------------------------------------------------------------------------------------
sumtable(data2, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- echo=TRUE, fig.height = 4.55------------------------------------------------------------------------------
ggplot(data2, aes(x=y0, color=as.factor(w))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y0")


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
summarize(data2, mean(y))
summarize(data2,mean(y))$`mean(y)`[2] - 
  summarize(data2,mean(y))$`mean(y)`[1]


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
lm(y ~ w, data2)


## ---- echo=TRUE, results=FALSE----------------------------------------------------------------------------------
data3 <- data %>% 
  ungroup() %>% 
  select(eta, y0,y1) %>%
  mutate(w =  if_else(between(percent_rank(y0),.25,.75),0,1), 
         y = y0 + (y1-y0)*w) %>%
  group_by(w)

sumtable(data3, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- results=TRUE----------------------------------------------------------------------------------------------
sumtable(data3, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- echo=TRUE, fig.height = 4.55------------------------------------------------------------------------------
ggplot(data3, aes(x=y0, color=as.factor(w))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y0")


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
summarize(data2, mean(y))
summarize(data2,mean(y))$`mean(y)`[2] - 
  summarize(data2,mean(y))$`mean(y)`[1]


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
lm(y ~ w, data3)


## ---- echo=TRUE, results=FALSE----------------------------------------------------------------------------------
data4 <- data %>% 
  ungroup() %>% 
  select(eta) %>%
  mutate(x = if_else(runif(100000) > .5,1,0),
         w = if_else(x + runif(100000, -1,1) > .5,1,0),
         y0 = 2 + 3*x + eta,
         y1 = y0 + 5,
         y = y0 + (y1-y0)*w) %>%
  group_by(w)

sumtable(data4, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- results=TRUE----------------------------------------------------------------------------------------------
sumtable(data4, 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w",
         group.long = TRUE)


## ---- results=TRUE, echo= TRUE----------------------------------------------------------------------------------
sumtable(filter(data4, x==1), 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w")


## ---- results=TRUE, echo= TRUE----------------------------------------------------------------------------------
sumtable(filter(data4, x==0), 
         summ=c('notNA(x)','mean(x)','sd(x)'), 
         group="w")


## ---- echo=TRUE-------------------------------------------------------------------------------------------------
lm(y ~ w, data4)
lm(y ~ w + x, data4)

