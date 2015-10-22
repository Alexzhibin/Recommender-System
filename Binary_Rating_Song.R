library(recommenderlab) # package being evaluated
library(ggplot2) # For plots
train = read.csv("train.csv")
rownames(train)=train[,1]
train = train[,-1]  
train1 =t(train[,-1])
train2 = t(train)
users=train["user"]

train_com = data.frame(users=rep(users[1,1],nrow(train1)),items=attr(train1,"dimnames")[1],rating=train1[,1])
for(i in 2:nrow(train)){
train_com = rbind(train_com,data.frame(users=rep(users[i,1],nrow(train1)),items=attr(train1,"dimnames")[1],rating=train1[,i]))
}
train_com$users = as.factor(train_com$users)
colnames(train_com)[2]="items"
head(train_com)

###############
#Turn them as realRating matrix 
##############
song_rating = as(train_com,"realRatingMatrix")

# Visualizing a sample of this
#image(sample(song_rating, 500), main = "Raw ratings")


# Visualizing ratings
qplot(getRatings(song_rating), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(song_rating)) # Skewed to the right

# How about after normalization?
qplot(getRatings(normalize(song_rating, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 
summary(getRatings(normalize(song_rating, method = "Z-score"))) # seems better
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4.8520 -0.6466  0.1084  0.0000  0.7506  4.1280

# How many movies did people rate on average
qplot(rowCounts(song_rating), binwidth = 10, 
      main = "Movies Rated on average", 
      xlab = "# of users", 
      ylab = "# of movies rated")
# What is the mean rating of each movie
qplot(colMeans(song_rating), binwidth = .1, 
      main = "Mean rating of Movies", 
      xlab = "Rating", 
      ylab = "# of movies")

recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
# We have a few options

# Let's check some algorithms against each other
scheme <- evaluationScheme(song_rating, method = "split", train = .7,
                           k = 1, given = 200, goodRating = 1)

scheme

algorithms <- list(
  "random items" = list(name="RANDOM" ),
  "popular items" = list(name="POPULAR"),
  "user-based CF" = list(name="UBCF", param=list(
                                                 method="Cosine",
                                                 nn=50, minRating=0)),
  "item-based CF" = list(name="IBCF")
  
)

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
avg(results)
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

# See precision / recall
plot(results, "prec/rec", annotate=3)

##########
#From the plot, we could know that the item-based CF looks good
#########
##Top 50
train_song = song_rating[1:95]
rec1 <- Recommender(train_song, method = "POPULAR")
rec1

## create top-N recommendations for new users
# test_song =song_rating[96:100]
# pre1 <- predict(rec1, test_song , n = 10)
# pre1
# as(pre1, "list")
# 
# ## predict ratings for new users
# pre1 <- predict(rec1, test_song, type="ratings")
# pre1
# as(pre1, "matrix")
# 
# ## create recommendations using user ids with ids 1..10 in the
# ## training data
# pre1 <- predict(rec1, 1:5 , data = test_song, n = 10)
# pre1
# as(pre1, "list")
