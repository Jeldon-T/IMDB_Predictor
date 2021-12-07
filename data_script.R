library(readr)
library(sqldf)
library(ggplot2)
library(dplyr)
library(class)
title_basics_year<-title_basics_edited[which(title_basics_edited$startYear >= '2000'),]
title_basics_final<-title_basics_year[which(title_basics_year$isAdult == '0'),]
# Merge Database : 
temp <- merge(title_basics_final,title_ratings,by=c("tconst")) 
final_dataset<-temp[which(temp$numVotes >= 1000),]
final_dataset <- sqldf("select A.* from final_dataset as A where A.averageRating > 2")

principles<-sqldf('select A.* from title_principles as A, final_dataset as B where A.tconst == B.tconst')
directors <- sqldf('select A.* from principles as A where A.category == "director"')
actors <- sqldf('select A.* from principles as A where A.category == "actor" or A.category == "actress"')

final_dataset = select(final_dataset, -2,-3,-5,-7)
rm(title_basics)
rm(temp)
rm(title_basics_edited)
rm(title_basics_final)
rm(title_basics_year)
rm(title_ratings)
gc() 

#edited directors
director_cor <- sqldf("select A.*,B.* from final_dataset as A, directors as B where A.tconst == B.tconst")
director_cor$nconst <- gsub("^.{0,2}", "", director_cor$nconst)
director_cor$nconst <- as.numeric(director_cor$nconst)
director_cor = select(director_cor, -8)

#director model
director_cor_temp <- sqldf("select A.* from director_cor as A GROUP BY A.nconst HAVING COUNT(A.nconst) > 1")
director_cor_temp <- sqldf("select director_cor.nconst from director_cor GROUP BY director_cor.nconst HAVING COUNT(director_cor.nconst) > 1")
director_cor_temp <- sqldf("select * from director_cor where nconst IN director_cor_temp")
director_cor <- director_cor_temp

director_cor <- sqldf("select A.* from director_cor as A where A.averageRating > 5 and A.numVotes > 1000")



directorsModel<-lm(averageRating~nconst, data = director_cor)
director_cor_sample <- sample_n(director_cor, 1000)
#director_cor_sample <- select(director_cor_sample, -6)
director_cor_sample$predicted_averageRating <- predict.lm(directorsModel, newdata = director_cor_sample)
director_compare <- sqldf("select A.tconst, A.nconst, A.originalTitle, A.averageRating, B.predicted_averageRating from director_cor as A, director_cor_sample as B
              where A.tconst = B.tconst")

#edited Actors
actors_cor <- sqldf("select A.*,B.* from final_dataset as A, actors as B where A.tconst == B.tconst")
actors_cor$nconst <- gsub("^.{0,2}", "", actors_cor$nconst)
actors_cor$nconst <- as.numeric(actors_cor$nconst)
actors_cor = select(actors_cor, -8)

#actor model
actorsModel <- lm(averageRating~nconst, data = actors_cor)
actors_cor_sample <- sample_n(actors_cor, 1000)
actors_cor_sample <- select(actors_cor_sample, -6)
actors_cor_sample$predicted_averageRating <- predict.lm(actorsModel, newdata = actors_cor_sample)
actors_compare <- sqldf("select A.tconst, A.originalTitle, A.averageRating, B.predicted_averageRating from actors_cor as A, actors_cor_sample as B
              where A.tconst = B.tconst")

#actors_cor<-select(actors_cor,-1,-3,-9,-11,-12,-13)
avg_rating_actors<-sqldf('select B.nconst, avg(A.averageRating) as ActorRating from final_dataset as A, actors_cor as B where B.tconst == A.tconst group by B.nconst') 
avg_rating_actors <-merge(avg_rating_actors,actors_cor,by=c("nconst"))
#SQL Statement to Create the Model
Predicted_Rating_Actor<-sqldf('select A.*, C.Predicted_Rating from (select B.tconst, sum(B.ActorRating)/4 as Predicted_Rating from avg_rating_actors as B group by B.tconst) as C, avg_rating_actors as A where A.tconst == C.tconst')

avg_rating_actors<-sqldf('select B.nconst, avg(A.averageRating) as ActorRating from final_dataset as A, actors_cor as B where B.tconst == A.tconst group by B.nconst') 
avg_rating_actors <-merge(avg_rating_actors,actors_cor,by=c("nconst"))
#SQL Statement to Create the Model
Predicted_Rating_Actor<-sqldf('select A.*, C.Predicted_Rating from (select B.tconst, sum(B.ActorRating)/4 as Predicted_Rating from avg_rating_actors as B group by B.tconst) as C, avg_rating_actors as A where A.tconst == C.tconst') 

actors_cor_temp <- sqldf("select A.* from actors_cor as A GROUP BY A.nconst HAVING COUNT(A.nconst) > 5")
avg_rating_actors<-sqldf('select B.nconst, avg(A.averageRating) as ActorRating from final_dataset as A, actors_cor_temp as B where B.tconst == A.tconst group by B.nconst') 
avg_rating_actors <-merge(avg_rating_actors,actors_cor,by=c("nconst"))
Predicted_Rating_Actor<-sqldf('select distinct A.tconst,A.originalTitle, A.averageRating, C.Predicted_Rating from (select B.tconst, sum(B.ActorRating)/count(B.nconst) as Predicted_Rating from avg_rating_actors as B group by B.tconst) as C, avg_rating_actors as A where A.tconst == C.tconst') 

Predicted_Rating_Actor$diff <- abs(Predicted_Rating_Actor$Predicted_Rating-Predicted_Rating_Actor$averageRating)
mean(Predicted_Rating_Actor$diff)
hist(Predicted_Rating_Actor$diff,main="Difference between predicted rating based on actor and actual rating", xlab="AbsoluteValue(PredictedRating - ActualRating)")


TestSample<-sample_n(actors_cor_temp,2000)
avg_rating_actors<-sqldf('select B.nconst, avg(A.averageRating) as ActorRating from final_dataset as A, TestSample as B where B.tconst == A.tconst group by B.nconst') 
avg_rating_actors <-merge(avg_rating_actors,actors_cor,by=c("nconst"))
Predicted_Rating_Actor<-sqldf('select distinct A.tconst,A.originalTitle, A.averageRating, C.Predicted_Rating from (select B.tconst, sum(B.ActorRating)/count(B.nconst) as Predicted_Rating from avg_rating_actors as B group by B.tconst) as C, avg_rating_actors as A where A.tconst == C.tconst') 
Predicted_Rating_Actor$diff <- abs(Predicted_Rating_Actor$Predicted_Rating-Predicted_Rating_Actor$averageRating)
mean(Predicted_Rating_Actor$diff)


#KNN stuff
knntester <- as.data.frame((final_dataset[, c('runtimeMinutes', 'averageRating')]))
knntester <- knntester[which(knntester$runtimeMinutes > 0),]
n.points <- 1000
sampling.rate <- 0.8
num.test.set.labels <- n.points * (1 - sampling.rate)
training <- sample(1:n.points, sampling.rate * n.points,replace=FALSE)
train <- subset(knntester[training, ], select = c(runtimeMinutes, startYear))
testing <- setdiff(1:n.points, training)
test <- subset(knntester[testing, ], select = c(runtimeMinutes, startYear))
cl <- knntester$averageRating[training]
true.labels <- knntester$averageRating[testing]
for (k in 1:20) {
  print(k)
  predicted.labels <- knn(train, test, cl, k)
  # We're using the R function knn()
  num.incorrect.labels <-sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels / num.test.set.labels
  print(misclassification.rate)
}
example <- c(97,2010) # can change this to see what output would be
knn(train,example,cl, k = 1)

comedyDB <- filterByGenre(final_dataset,genre ="Comedy")
actionDB <- filterByGenre(final_dataset,genre ="Action")
dramaDB <- filterByGenre(final_dataset,genre ="Drama")
musicDB <- filterByGenre(final_dataset,genre ="Music")
horrorDB <- filterByGenre(final_dataset,genre ="Horror")
thrillerDB <- filterByGenre(final_dataset,genre ="Thriller")
fantasyDB <- filterByGenre(final_dataset,genre ="Fantasy")
adventureDB <- filterByGenre(final_dataset,genre ="Adventure")
animationDB <- filterByGenre(final_dataset,genre ="Animation")
biographyDB <- filterByGenre(final_dataset,genre ="Biography")
sportDB <- filterByGenre(final_dataset,genre ="Sport")
warDB <- filterByGenre(final_dataset,genre ="War")
westernDB <- filterByGenre(final_dataset,genre ="Western")
dramaDB <- filterByGenre(final_dataset,genre ="Drama")

#models
hist(Predicted_Rating_Actor$diff,ylim = c(0, 6000) ,main="Average difference between predicted rating based on actor and actual rating", xlab="Mean difference",ylab = "Num Movies")

# Directors chart
plot(director_compare$averageRating ~ director_compare$nconst, xlab="Director ID", ylab="Average Rating", main="Director ID vs Average Rating")
predictedPoints <- matrix(as.numeric(c(director_compare$nconst, director_compare$predicted_averageRating)), ncol=2)
points(predictedPoints, col="red")
# Actors chart
plot(actors_compare$averageRating ~ actors_compare$nconst, xlab="Actor ID", ylab="Average Rating", main="Actor ID vs Average Rating")
predictedPoints <- matrix(as.numeric(c(actors_compare$nconst, actors_compare$predicted_averageRating)), ncol=2)
points(predictedPoints, col="red")


# Render KNN Graph for Ratings
levels <-sort(unique(knntester$averageRating))
plot(knntester$runtimeMinutes, knntester$startYear, main="Average Rating (KNN)",xlab="Runtime (minutes)",ylab="Year")

# Colorize the graph
for(h in 1:length(levels)){
  rat = levels[h]
  pointsList <- matrix(as.numeric(c(knntester[knntester$averageRating==rat,]$runtimeMinutes, knntester[knntester$averageRating==rat,]$startYear)), ncol=2)
  greenVal = as.hexmode(round(255 * rat/10))
  greenVal = trimws(greenVal)
  if(nchar(greenVal) == 1){
    greenVal = paste("0",greenVal,sep="")
  }
  redVal = as.hexmode(round(255*(10-rat)/10))
  redVal = trimws(redVal)
  if(nchar(redVal) == 1){
    redVal = paste("0",redVal,sep="")
  }
  pointCol = paste("#",redVal,greenVal,"00", sep="")
  points(pointsList, col=pointCol)
  
  # Uncomment this line to make an animation
  #Sys.sleep(0.06)
} 
# KNN predicting Year instead of Rating
knntester <- as.data.frame((final_dataset[, c('runtimeMinutes', 'startYear' , 'averageRating')]))
knntester <- knntester[which(knntester$runtimeMinutes > 0),]
n.points <- 1000
sampling.rate <- 0.8
num.test.set.labels <- n.points * (1 - sampling.rate)
training <- sample(1:n.points, sampling.rate * n.points,replace=FALSE)
train <- subset(knntester[training, ], select = c(runtimeMinutes, averageRating))
testing <- setdiff(1:n.points, training)
test <- subset(knntester[testing, ], select = c(runtimeMinutes, averageRating))
cl <- knntester$startYear[training]
true.labels <- knntester$startYear[testing]
for (k in 1:20) {
  print(k)
  predicted.labels <- knn(train, test, cl, k)
  # We're using the R function knn()
  num.incorrect.labels <-sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels / num.test.set.labels
  print(misclassification.rate)
}
example <- c(97,2010) # can change this to see what output would be
knn(train,example,cl, k = 1)

# Render KNN Graph for Year
levels <-sort(as.numeric(unique(knntester$startYear)))
plot(knntester$runtimeMinutes, knntester$averageRating, main="Year (KNN)",xlab="Runtime (minutes)",ylab="Average Rating")

# Colorize the graph
for(h in 1:length(levels)){
  rat = levels[h]
  pointsList <- matrix(as.numeric(c(knntester[knntester$startYear==rat,]$runtimeMinutes, knntester[knntester$startYear==rat,]$averageRating)), ncol=2)
  greenVal = as.hexmode(round(255 * (rat-2000)/21))
  greenVal = trimws(greenVal)
  if(nchar(greenVal) == 1){
    greenVal = paste("0",greenVal,sep="")
  }
  redVal = as.hexmode(round(255*(21-(rat-2000))/21))
  redVal = trimws(redVal)
  if(nchar(redVal) == 1){
    redVal = paste("0",redVal,sep="")
  }
  pointCol = paste("#",redVal,greenVal,"00", sep="")
  points(pointsList, col=pointCol)
  
  # Uncomment this line to make an animation
  #Sys.sleep(0.06)
} 

#KNN variation
knntester <- as.data.frame((final_dataset[, c('runtimeMinutes', 'startYear' , 'averageRating')]))
knntester  <- knntester [which(knntester $runtimeMinutes > 0),]
knnpredicted <- knntester
knnpredicted['ratingPrediction'] = 0
knnpredicted$runtimeMinutes <- as.numeric(knnpredicted$runtimeMinutes)
knnpredicted$startYear <- as.numeric(knnpredicted$startYear)
n.points <- 1000
bestRate <- 0
bestPick <- 0
sampling.rate <- 0.8
num.test.set.labels <- n.points * (1 - sampling.rate)
training <- sample(1:n.points, sampling.rate * n.points,replace=FALSE)
train <- subset(knntester[training, ], select = c(runtimeMinutes, startYear))
testing <- setdiff(1:n.points, training)
test <- subset(knntester[testing, ], select = c(runtimeMinutes, startYear))
cl <- knntester$averageRating[training]
true.labels <- knntester$averageRating[testing]
k <- sqrt(200)
for(x in 1:nrow(knntester))
{
  example <- knntester[x,c("runtimeMinutes" , "startYear")]
  knnpredicted[x,c("ratingPrediction")] <- as.numeric(as.character(knn(train,example,cl,k)))
}
sample <- sample_n(knnpredicted,10)
sample = select(sample, -2)
sample <- melt(sample, id.vars="runtimeMinutes")
ggplot(sample, aes(runtimeMinutes ,value, col=variable)) + 
  geom_point() + 
  stat_smooth() (edited)
[10:22 PM]

