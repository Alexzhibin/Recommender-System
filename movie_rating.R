#MovieLense
data = as(MovieLense, "data.frame")
write.csv(data,"movie_rating.csv",row.names=FALSE)

data1 = read.csv("movie_rating.csv")

data1 = as(data1,"realRatingMatrix")
# Visualizing a sample of this
image(sample(data1, 500), main = "Raw ratings")

# Visualizing ratings
qplot(getRatings(data1), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(data1)) # Skewed to the right

## create a matrix with ratings
m <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))), 
            nrow=10, ncol=10, dimnames = list(
              user=paste('u', 1:10, sep=" "),
              item=paste('u', 1:10, sep=" ")    
            ))
m

## coerce into a realRatingMAtrix
r <- as(m, "realRatingMatrix")
r

## get some information
dimnames(r)
rowCounts(r)
colCounts(r)
rowMeans(r)

## histogram of ratings
hist(getRatings(r), breaks="FD")

## inspect a subset
image(r[1:5,1:5])

## coerce it back to see if it worked
as(r, "matrix")

## coerce to data.frame
m1 = as(r, "data.frame")

## binarize into a binaryRatingMatrix with all 4+ rating a 1
b <- binarize(r, minRating=4)
b
as(b, "matrix")
as(b, "data.frame")
m2 = as(m1, "realRatingMatrix")
hist(getRatings(m2), breaks="FD")
