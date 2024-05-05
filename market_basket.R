install.packages('arules')
library(arules)
install.packages('arulesViz')
library(arulesViz)
gro<-read.transactions('groceries.csv',sep=",")
itemFrequencyPlot(gro, topN = 10, type="absolute")
grules<-apriori(gro, parameter = list(supp=0.001,conf=0.8))
summary(grules)
#show the top 5 rules, but only 2 digits
options(digits=2)
inspect(grules[1:5])
head(quality(grules),5)
grules<-sort(grules,by="confidence",decreasing = TRUE)
plot(grules, method="graph")
plot(grules,method = "grouped")
head(quality(grules))




library(arules)
library(arulesViz)
library(dplyr)
ratings <- read.csv("rating.csv")
movies <- read.csv("movie.csv")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId), title = as.character(title), genres = as.character(genres)) #add new variables

movie_subset <- left_join(ratings, movies, by = "movieId")
movie_subset %>% glimpse() #Viewing joined dataset
n_distinct(movie_subset$userId)
data_list = split(movie_subset$title,
                  movie_subset$userId)
movie_trx = as(data_list, "transactions") #Convert to transactions
str(movie_trx)
itemFrequencyPlot(movie_trx, topN = 10, type = "absolute")
mrules <- apriori(movie_trx, parameter = list(supp = 0.001, conf = 0.8,target = "frequent"))
summary(mrules)
options(digits=2)
inspect(mrules[1:10])
plot(mrules,method = "graph")

