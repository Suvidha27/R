library(recommenderlab)
library(dplyr)

# Loading data 
beer_data<-read.csv("beer_data.csv")

class(beer_data)
str(beer_data)

# check NA values in the data set
sum(is.na(beer_data))
# there are no NA values

# check Null values
nrow(beer_data[(beer_data$review_profilename==""), ])
# 100 rows have null values

#Removing the rows for which the user has null values

beer_data<-beer_data[!(beer_data$review_profilename==""),]

# Data preparation
# Find unique beer ids

length(unique(beer_data$beer_beerid)) 
# 40304 unique beer ids are present 

# Remove duplicated rows based on beer id and user as same user can give multiple ratings to same beer

beer_data_distinct<-beer_data[!duplicated(c("beer_beerid","review_profilename")), ]

beer_data<-distinct(beer_data_distinct,beer_beerid,review_profilename,.keep_all = TRUE)

length(unique(beer_data$beer_beerid))

length(unique(beer_data$review_overall))
# 10 unique ratings are present in the data set

# Find the frequency of the rating
hist(beer_data$review_overall)
# 4 is the most frequent rating given by users
unique(beer_data$review_overall)

# Choose only those beers that have at least N number of reviews

# Find the total reviews for disctint beer
beer_total_reviews<-aggregate(beer_data$review_overall,by=list(beer_data$beer_beerid), FUN=length)
colnames(beer_total_reviews)<-c("beerid","total_reviews")
beer_total_reviews<- arrange(beer_total_reviews,-total_reviews)

# beerid -2093 has max reviews

# statistics of total_reviews
summary(beer_total_reviews)

# from the stats, it is clear that the total reviews is skewed after 3rd quartile

beer_reviews<-aggregate(beer_total_reviews$beerid,by=list(beer_total_reviews$total_reviews), FUN=length)
colnames(beer_reviews)<-c("reviews_total","freq")
summary(beer_reviews)
# from the stats , it is clear that 18080 beers got 1 rating, which implies there are only few bears with many reviews

library(ggplot2)
ggplot(beer_reviews,aes(x=reviews_total,y=freq))+geom_point()

# subsetting the data without total reviews 1

beer_reviews_subset<-subset(beer_reviews,reviews_total!=1)
ggplot(beer_reviews_subset,aes(x=reviews_total,y=freq))+geom_point()
summary(beer_reviews_subset)

# just removing total reviews 1 is not sufficient hence computing ratio to decide N

# computing the ratio of total reviews and freq of those total reviews
beer_freq_ratio<-beer_reviews_subset$reviews_total/beer_reviews_subset$freq
summary(beer_freq_ratio)

# the above stats show that the ratio data varies from 0.0003 to 977
# which means the data is skewed and mean cannot be taken as a good statistic measure
# N should be lesser than median numbers of total reviews

# Choosing N as 70
# subsetting beer_reviews_subset where N is greater than or equal to 70

beer_reviews_final<-subset(beer_total_reviews,beer_total_reviews$total_reviews>=70)

ggplot(beer_reviews_final,aes(x=total_reviews))+geom_histogram()

# Grouping by user views-total reviews given by user
user_total_reviews<-aggregate(beer_data$review_overall,by=list(beer_data$review_profilename), FUN=length)
colnames(user_total_reviews)<-c("user","total_usr_reviews")
user_total_reviews<- arrange(user_total_reviews,-total_usr_reviews)

# northyorksammy has given the max no of ratings
summary(user_total_reviews)

# subsetting user preferences in such a way that each user has atleast reviewed 30 beers
user_reviews_final<-subset(user_total_reviews,user_total_reviews$total_usr_reviews>=30)

beers_final<-merge(beer_data,beer_reviews_final,by.x="beer_beerid",by.y="beerid")
beers_final<-merge(beers_final,user_reviews_final,by.x="review_profilename",by.y="user")

summary(beers_final)

# realratingmatrix

beer_rrm<- as(beers_final[,c(1,2,3)], "realRatingMatrix")
dimnames(beer_rrm)
rowCounts(beer_rrm)
colCounts(beer_rrm)
rowMeans(beer_rrm)

beer_df <- as(beer_rrm, "data.frame")
str(beer_df)

##  data exploration
#Determine how similar the first ten users are with each other and visualise it
similar_users <- similarity(beer_rrm[1:10, ],method = "cosine",which = "users")
image(as.matrix(similar_users), main = "User similarity")

#Compute and visualise the similarity between the first 10 beers
# beer(item) similarity
similar_beers <- similarity(beer_rrm[,1:10 ],method = "cosine",which = "items")
image(as.matrix(similar_beers), main = "Beer(item) similarity")

# What are the unique values of ratings?
unique(beer_df$rating)


#Visualise the rating values
# The average beer ratings
avg_beer_ratings<-aggregate(beer_df$rating,by=list(beer_df$item), FUN=mean)
colnames(avg_beer_ratings)<-c("item","avg_rating")

ggplot(avg_beer_ratings,aes(x=avg_rating))+geom_histogram()

mean(avg_beer_ratings$avg_rating)
# 3.827463 is the average beer ratings

# The average user ratings

avg_user_ratings<-aggregate(beer_df$rating,by=list(beer_df$user), FUN=mean)
colnames(avg_user_ratings)<-c("user","avg_rating")

ggplot(avg_user_ratings,aes(x=avg_rating))+geom_histogram()

mean(avg_user_ratings$avg_rating)
# 3.888536 is the average user rating

# The average number of ratings given to the beers

avg_beer_reviews<-aggregate(beers_final$total_reviews,by=list(beers_final$beer_beerid), FUN=mean)
colnames(avg_beer_reviews)<-c("beerid","avg_reviews")
ggplot(avg_beer_reviews,aes(x=avg_reviews)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers")

mean(avg_beer_reviews$avg_reviews)
# each beer on an average gets 173 number of ratings

# The average number of ratings given by the users

avg_usr_reviews<-aggregate(beers_final$total_usr_reviews,by=list(beers_final$review_profilename), FUN=mean)
colnames(avg_usr_reviews)<-c("usr","avg_usr_reviews")
ggplot(avg_usr_reviews,aes(x=avg_usr_reviews)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers")

mean(avg_usr_reviews$avg_usr_reviews)
# each user on an average gives 120 reviews/ratings


# Recommendation Models

scheme <- evaluationScheme(beer_rrm, method = "split", train = .9,
                           k = 1, given = 1, goodRating = 4)
scheme

# using cross validation
scheme2 <- evaluationScheme(beer_rrm, method = "cross-validation",k = 5, given = 1, goodRating = 4)
scheme2


# IBCF Aand UBCF
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

results1 <- evaluate(scheme2, algorithms, n=c(1, 3, 5, 10, 15, 20))


# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

plot(results1, annotate = 1:4, legend="topleft")

# UBCF is better than IBCF as n increases

# Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet

rec <- Recommender(beer_rrm, method = "UBCF")
rec

recomm_cokes <- predict(rec, beer_rrm['cokes'], n=5)
as(recomm_cokes, "list")

recomm_genog <- predict(rec, beer_rrm['genog'], n=5)
as(recomm_genog, "list")

recomm_giblet <- predict(rec, beer_rrm['giblet'], n=5)
as(recomm_giblet, "list")
