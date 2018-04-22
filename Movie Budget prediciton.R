#setwd("C:/Users/mruna/Desktop/studyMaterial/semester-2/EDA/Assignment4")
library(ggplot2)
library(broom)
library(MASS)

#reading the data
movie_budgets <- read.table("movie_budgets.txt", sep =  " ", header = TRUE)

#trend line graph for log10(budget) VS year with commented parts for different curving fits
ggplot(movie_budgets, aes(y = log10(budget), x = year, col = length)) + 
geom_point() + 
  #geom_smooth(method = "lm", formula = y~x+I(x^2), aes(weight = length)) +
  #geom_smooth(method = "lm", formula = y~x+I(x^2), col = "orange", alpha = 0.10) + 
  #geom_smooth(span = 0.10, method = "loess", col = "red") +
  #geom_smooth(span = 0.3, method = "loess", col = "blue") 
  geom_smooth(span = 0.40, method = "loess", col = "green")
  #geom_smooth(span = 0.50, method = "loess", col = "green") +
  #geom_smooth(span = 0.60, method = "loess", col = "yellow") 
  #geom_smooth(span = 0.75, method = "loess", col = "yellow")
  #geom_smooth(method = "rlm", method.args = list(psi = psi.bisquare), col = "green")

#fitting the model
movie_budgets_lm <- loess(log10(budget)~year, movie_budgets)
movie_budgets_lm_df <- augment(movie_budgets_lm)
summary(movie_budgets_lm_df)

ggplot(movie_budgets_lm_df, aes(y = .resid, x = year)) +
geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, col = "orange")

#spread location plot
ggplot(movie_budgets_lm_df, aes(y = abs(.resid), x = .fitted)) +
geom_smooth(method = "loess")

#inteeraction graph
ggplot(movie_budgets, aes(y = log10(budget), x = year, col = length)) + 
geom_point() + geom_smooth(span = 0.40, method = "loess", col = "green") + 
facet_grid(~cut_number(length, n = 6))

# rsquare value
r_square <- var(log10(abs(movie_budgets_lm_df$.fitted)))/var(log10(movie_budgets$budget))

#trend line graph for log10(budget) VS length with commented parts for different curving fits
gg <- ggplot(movie_budgets, aes(y = log10(budget), x = length, col = year)) + 
  geom_point() + 
  #geom_smooth(method = "lm", formula = y~x+I(x^2)) +
  #geom_smooth(method = "lm", formula = y~x+I(x^2), col = "orange") +
  #geom_smooth(method = "loess", col = "red", span = 0.25) +
  #geom_smooth(method = "loess", col = "yellow", span = 0.750)
  geom_smooth(method = "loess", col = "green", span = 0.50) 
  #geom_smooth(method = "loess", col = "orange", span = 0.75) 
  #geom_smooth(method = "rlm", method.args = list(psi = psi.bisquare), col = "green")
gg

#loess model fitting
movie_budgets_len_lm <- lm(budget~length + I(length)^2, movie_budgets)
movie_budgets_len_lm_df <- augment(movie_budgets_len_lm)
summary(movie_budgets_len_lm_df)

ggplot(movie_budgets_len_lm_df, aes(y = .resid, x = length)) +
  geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, col = "orange")

#spread location plot
ggplot(movie_budgets_len_lm_df, aes(y = abs(.resid), x = .fitted)) +
  geom_smooth(method = "loess")

#interaction plot
gg + facet_grid(~cut_number(year, n = 4)) + ggtitle("Graph: Interaction between Year and Length")

#r square value
r_square <- var(log10(abs(movie_budgets_len_lm_df$.fitted)))/var(log10(movie_budgets$budget))

#QUESTION 2
# faceted plots to display the fit : (conditioned on year)
movie.loess = loess(log10(budget) ~ length + year, span = 1, family = "symmetric", data = movie_budgets)
movie.loess.df = augment(movie.loess)
movie.grid = expand.grid(year=c(1920,1970,1995,2004),length = c(50,90,150,200))
movie.predict = predict(movie.loess, newdata = movie.grid)

gg = ggplot(data.frame(movie.grid, fit =as.vector(movie.predict)),aes(x = length,y = fit))+ geom_line()+ facet_grid(~year)
gg + labs(title = "Budget conditioned on Year")

#Question 3

movie_budgets$budget<-log10(movie_budgets$budget)

movie.loess = loess(budget ~ length + year, data = movie_budgets)
movie.grid = expand.grid(year = seq(1906,2005), length = seq(1,200,2))
movie.predict = predict(movie.loess, movie.grid)
movie.plot.df =data.frame(movie.grid, fit =as.vector(movie.predict))

ggplot(movie.plot.df,aes(x = length, y = year, z = fit))+ 
  geom_raster(aes(fill = fit))+coord_fixed()+ scale_fill_distiller(palette = "RdYlBu")+
  geom_contour()+
  ggtitle("Raster_contour plot for Budget fitted by Length~Year")



##########################           Additional graphs          ###########################

ggplot(movie.plot.df,aes(x = length, y = year, fill = fit))+ 
  geom_raster()+coord_fixed()+ scale_fill_distiller(palette = "RdYlBu")
facet_wrap(~cut_number(fit,n = 16), ncol = 4)

library(lattice)
cloud(budget ~ year*length, data = movie_budgets)

wireframe(fit ~ year*length, data = movie.plot.df)

#Density analysis
library(MASS)
ggplot(movie_budgets,aes(x = year, y = length))+ stat_density_2d(aes(fill = ..level..),geom = "polygon")

