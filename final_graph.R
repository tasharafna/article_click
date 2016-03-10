
###################### CODE OUTLINE ##########################

# 1. MAIN SETUP AND CONFIGURATION
# 2. KEY METRIC EXPLORATION
# 3. KEY METRIC ANOVA
# 4. EXPLORATION OF GENDER AND AUTHORS
# 5. EXPLORATION OF 'ID' FIELDS (topic, type, user, email, article)
# 6. AUTO CORRELATION TESTS
# 7. LINEAR REGRESSION - BY DAY
# APPENDIX A: LINEAR REGRESSION - BY HOUR
# APPENDIX B: ML CLASSIFIERS - determining gender by topic clicks


###################### 1. MAIN SETUP AND CONFIGURATION ##########################

# Libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)
library(MASS)
library(car)
library(class)
library(data.table)

# set working drive
location = '/Users/nicholasculver/repos/Projects/correlation_one'
setwd(location)

# Import file
core_frame <- fread('core_frame.csv', stringsAsFactors=FALSE, 
                    data.table=TRUE)


# Set theme
theme_set(theme_gray(base_size = 18))

# Remove NAs - the bad article_ids from the log file
core_frame <- core_frame[!is.na(core_frame$click)]

# Make legitimate dates in R
core_frame$send_date <- as.Date(core_frame$send_date, '%Y-%m-%d')
core_frame$send_date_hour <- as.POSIXct(core_frame$send_date_hour, 
                                        format= '%Y-%m-%d %H:%M:%S')


############  2. KEY METRIC EXPLORATION  ##############

# Most basic stat - the probabilty an article is clicked over all three months of data
prob_click_total = sum(core_frame$click) / length(core_frame$click)

# Group by day frame
frame_by_day <- core_frame %>%
  group_by(send_date) %>%
  summarize(prob_click=mean(click),
            total_clicks = sum(click==TRUE),
            total_articles = n(),
            total_emails = n_distinct(email_id),
            unique_articles = n_distinct(article_id)) %>%
  arrange(send_date)

# Group by hour frame
frame_by_hour <- core_frame %>%
  group_by(send_date_hour) %>%
  summarize(prob_click=mean(click),
            total_clicks = sum(click==TRUE),
            total_articles = n(),
            total_emails = n_distinct(email_id),
            unique_articles = n_distinct(article_id)) %>%
  arrange(send_date_hour)

# Plot prob_click by day -no dominant trend, if anything drifts lower then rises later
ggplot(data=frame_by_day,
       aes(x=send_date,
           y=prob_click)) +
  geom_point() + 
  geom_smooth()

# Plot prob_click by day: with regression line
reg_day <- lm(prob_click ~ send_date,
              data=frame_by_day)
ggplot(data=frame_by_day,
       aes(x=send_date,
           y=prob_click)) +
  geom_point() + 
  geom_abline(slope=coef(reg_day)[2], intercept=coef(reg_day)[1],
              col='blue')

# Total clicks by day
ggplot(data=frame_by_day,
       aes(x=send_date,
           y=total_clicks)) +
  geom_point() + 
  geom_smooth()

# Plot prob_click by hour
# This offers more granularity than 'by day' - even less of an evident trend here
ggplot(data=frame_by_hour,
       aes(x=send_date_hour,
           y=prob_click)) +
  geom_point() +
  geom_smooth()

# Plot distribution of clicks per hour
ggplot(data=frame_by_hour,
       aes(x=prob_click)) +
  geom_histogram(binwidth=0.0025)

# Let's ditch the 5% hours with the fewest articles sent
reduced_hour_frame <- subset(frame_by_hour, total_articles > quantile(frame_by_hour$total_articles, 
                                                                      probs=0.05))

# Looks pretty close to normal distribution
qqnorm(reduced_hour_frame$prob_click)

# Plot distribution of total articles sent
ggplot(data=reduced_hour_frame,
       aes(x=total_articles)) +
  geom_histogram(binwidth=1000)

# Plot total articles over time
ggplot(data=reduced_hour_frame,
       aes(x=send_date_hour,y=total_articles)) +
  geom_point() + geom_smooth()



############  3. KEY METRIC ANOVA ##############

# Anova for daily click probabilities
frame_by_day$send_month <- as.factor(as.numeric(format(frame_by_day$send_date, "%m")))
anova <- aov(prob_click ~ send_month,
             data= frame_by_day)
summary(anova)
ggplot(aes(x=send_month, y=prob_click),
       data=frame_by_day) +
  geom_boxplot()
TukeyHSD(anova)

# Anova for hourly click probabilities
reduced_hour_frame$send_month <- as.factor(as.numeric(format(reduced_hour_frame$send_date_hour, '%m')))
anova_hour <- aov(prob_click ~ send_month,
                  data= reduced_hour_frame)
summary(anova_hour)
TukeyHSD(anova_hour)
ggplot(aes(x=send_month, y=prob_click),
       data=reduced_hour_frame) +
  geom_boxplot()

# Anova by hour of day
reduced_hour_frame$hour_of_day <- as.factor(as.numeric(as.POSIXlt(reduced_hour_frame$send_date_hour)$hour))
anova_hour_of_day <- aov(prob_click ~ hour_of_day,
                         data=reduced_hour_frame)
summary(anova_hour_of_day)
ggplot(aes(x=hour_of_day, y=prob_click),
       data=reduced_hour_frame) +
  geom_boxplot()
TukeyHSD(anova_hour_of_day)



######## 4. EXPLORATION OF GENDER AND AUTHORS #######

# Group by author 
by_author <- core_frame %>%
  group_by(author_id) %>%
  summarise(total_articles = n(),
            prob_click = mean(click),
            total_clicks=sum(click==TRUE))

# Add author column to core frame (TRUE means the user contributed at least one article)
core_frame$author <- core_frame$user_id %in% by_author$author_id

# Experiment with a 'hot author' column (TRUE means the user was a top 25% contributor of articles)
hot_author <- subset(by_author, prob_click > quantile(by_author$prob_click, 0.75))
core_frame$hot_author <- core_frame$user_id %in% hot_author$author_id

# Basic author split
author_click_rate <- with(subset(core_frame, author == TRUE), 
                          sum(click) / length(click))
hot_author_click_rate <- with(subset(core_frame, hot_author == TRUE), 
                              sum(click) / length(click))
non_author_click_rate <- with(subset(core_frame, author == FALSE), 
                              sum(click) / length(click))

# 'Hot authors' slightly more likely to be women
table(core_frame$gender[core_frame$hot_author == TRUE])

# Basic gender probability of click
core_frame$gender <- as.factor(core_frame$gender)
male_click_rate <- with(subset(core_frame, gender == 'male'),
                   sum(click) / length(click))
female_click_rate <- with(subset(core_frame, gender == 'female'),
                     sum(click) / length(click))

# Group by hour frame, men only
frame_by_hour_male <- subset(core_frame, gender == 'male') %>%
  group_by(send_date_hour) %>%
  summarize(prob_click_m=mean(click),
            total_clicks_m = sum(click==TRUE),
            total_articles_m = n(),
            total_emails_m = n_distinct(email_id),
            unique_articles_m = n_distinct(article_id)) %>%
  arrange(send_date_hour)

# Group by hour frame, women only
frame_by_hour_female <- subset(core_frame, gender == 'female') %>%
  group_by(send_date_hour) %>%
  summarize(prob_click_w=mean(click),
            total_clicks_w = sum(click==TRUE),
            total_articles_w = n(),
            total_emails_w = n_distinct(email_id),
            unique_articles_w = n_distinct(article_id)) %>%
  arrange(send_date_hour)

# Merge the men and women by hour frames 
gender_by_hour <- merge(frame_by_hour_male, frame_by_hour_female,by='send_date_hour')

# Graph probability click by hour by gender (prepare for disappointment though)
ggplot(gender_by_hour,
       aes(x=send_date_hour)) +
  geom_point(aes(y=prob_click_m), color = 'blue') +
  geom_point(aes(y=prob_click_w), color = 'purple') +
  geom_smooth(aes(y=prob_click_m), color = 'red') +
  geom_smooth(aes(y=prob_click_w), color = 'orange')
 
# Graph total clicks by hour by gender
ggplot(gender_by_hour,
       aes(x=send_date_hour)) +
  geom_point(aes(y=total_articles_m), color = 'blue') +
  geom_point(aes(y=total_articles_w), color = 'purple') +
  geom_smooth(aes(y=total_articles_m), color = 'red') +
  geom_smooth(aes(y=total_articles_w), color = 'orange')



########  5. EXPLORATION OF 'ID' FIELDS (topic, type, user, email, article) #######

# Group by the various ids - goal is to see if clicks vary by attributes such as topic, type
# Each frame will be arranged by click rate, so that we can rank the ids
frame_by_topic <- core_frame %>%
  group_by(topic_id) %>%
  summarize(prob_click = mean(click),
            total_clicks = sum(click==TRUE)) %>%
  arrange(desc(prob_click))

frame_by_type <- core_frame %>%
  group_by(type_id) %>%
  summarize(prob_click = mean(click),
            total_clicks = sum(click==TRUE)) %>%
  arrange(desc(prob_click))

frame_by_user <- core_frame %>%
  group_by(user_id) %>%
  summarize(prob_click = mean(click),
            total_clicks = sum(click==TRUE)) %>%
  arrange(desc(prob_click))

frame_by_email <- core_frame %>%
  group_by(email_id) %>%
  summarize(prob_click = mean(click),
            total_clicks = sum(click==TRUE)) %>%
  arrange(desc(prob_click))

frame_by_article <- core_frame %>%
  group_by(article_id) %>%
  summarize(prob_click = mean(click),
            total_clicks = sum(click==TRUE)) %>%
  arrange(desc(prob_click))

# Graph  user clicks -really tight distribution, it seems likely our users are robots
frame_by_user$gender <- core_frame$gender[match(frame_by_user$user_id, core_frame$user_id)]
ggplot(frame_by_user,
       aes(x=user_id,
           y=total_clicks, group=gender, col=gender)) +
  geom_point()

# Create 'rank' of popularity for topics - ie. #1 is the most clicked topic
frame_by_topic <- arrange(frame_by_topic, desc(prob_click))
frame_by_topic$topic_rank_prob <- seq(1,105,1)
frame_by_topic <- arrange(frame_by_topic, desc(total_clicks))
frame_by_topic$topic_rank_gross <- seq(1,105,1)

# Add ranks of topics to our core frame
index <- match(core_frame$topic_id, frame_by_topic$topic_id)
core_frame$topic_rank_prob <- frame_by_topic$topic_rank_prob[index]
core_frame$topic_rank_gross <- frame_by_topic$topic_rank_gross[index]
rm(index)

# Might as well incoporate ranks for type, user, and article ids
# These are already sorted by prob_click
core_frame$type_rank <- match(core_frame$type_id, frame_by_type$type_id)
core_frame$user_rank <- match(core_frame$user_id, frame_by_user$user_id)
core_frame$article_rank <- match(core_frame$article_id, frame_by_article$article_id)

# All together now - make a new frame by day to view our rankings over time
ranks_by_day <- core_frame %>%
  group_by(send_date) %>%
  summarise(prob_click = mean(click),
            total_clicks = sum(click==TRUE),
            topic_rank_prob_index = mean(topic_rank_prob),
            topic_rank_gross_index = mean(topic_rank_gross),
            type_rank_prob_index = mean(type_rank),
            user_rank_prob_index = mean(user_rank),
            article_rank_prob_index = mean(article_rank)) %>%
  arrange(send_date)

# One graph of all the rankings together - scaled for graphing
# Clearly, we are steadily serving up less popular topics 
ggplot(ranks_by_day,
       aes(x=send_date)) +
  geom_smooth(aes(y=scale(topic_rank_prob_index), color='topic_rank_prob'),
              method = 'loess') +
  geom_smooth(aes(y=scale(topic_rank_gross_index), color='topic_rank_gross'),
              method = 'loess') +
  geom_smooth(aes(y=scale(type_rank_prob_index), color='type_rank'),
              method = 'loess') +
  geom_smooth(aes(y=scale(user_rank_prob_index), color='user_rank'),
              method = 'loess') + 
  geom_smooth(aes(y=scale(article_rank_prob_index), color='article_rank'),
              method = 'loess') +
  theme(legend.title=element_blank()) +
  xlab('Date') +
  ylab('Scaled rank index') +
  ggtitle('Rank index for topic, type and user (Jan - March)')



############ 6. AUTO CORRELATION TESTS ###############

# We have 63 days over 3 months (no data for weekends and holidays)
# Appears to be very little auto-correlation, when examined by day
prob_click_ts <- ts(frame_by_day$prob_click)
acf(prob_click_ts)

# And we have 610 hours in total
# Some significant autocorrelation by hour, up to 6 or 7 hours behind
prob_click_ts_hour <- ts(frame_by_hour$prob_click)
acf(prob_click_ts_hour)



############ 7. LINEAR REGRESSION - BY DAY ###############

# Coefficient for day comes out slightly negative but close to 0
# This regression is certainly flawed - we have multicollearity and small sample size

# Labels
prob_click_label <- frame_by_day$prob_click

# Features
type_index_feature <- ranks_by_day$type_rank_prob_index
user_index_feature <- ranks_by_day$user_rank_prob_index
days_feature <- seq(1,63,1)
topic_index_feature <- ranks_by_day$topic_rank_prob_index

# Together in frame
reg_frame <- data.frame(prob_click_label, days_feature,topic_index_feature)

# Look at correlation matrix - heavy multicollinearity
cor(reg_frame)
ggpairs(reg_frame)

# Linear regression - two features we know are of interest
simple_reg <- lm(prob_click_label ~ days_feature + topic_index_feature,
                  data=reg_frame)
summary(simple_reg)
predictions <- predict(simple_reg)
residuals <- residuals(simple_reg)
vif(simple_reg)

# Graphs results and residuals
ggplot(aes(x=predictions, y=prob_click_label),
       data=reg_frame) +
  geom_point() +
  geom_smooth()
ggplot(aes(x=predictions, y=rstandard(simple_reg)),
       data=reg_frame) +
  geom_point() +
  geom_smooth()
ggplot(aes(x=predictions, y=residuals),
       data=reg_frame) +
  geom_point() +
  geom_smooth()



############ APPENDIX A: LINEAR REGRESSION - BY HOUR ###############

# No model or results in this section, code is included to show feature ideas that were tried

# Ranks by hour
ranks_by_hour <- core_frame %>%
  group_by(send_date_hour) %>%
  summarise(prob_click = mean(click),
            total_clicks = sum(click==TRUE),
            topic_rank_prob_index = mean(topic_rank_prob),
            topic_rank_gross_index = mean(topic_rank_gross),
            type_rank_prob_index = mean(type_rank),
            user_rank_prob_index = mean(user_rank),
            article_rank_prob_index = mean(article_rank),
            total_articles=n()) %>%
  arrange(send_date_hour)

# Dump the 5% of hours with the fewest articles (concerns those sample may be small)
ranks_by_hour <- subset(ranks_by_hour, total_articles > quantile(ranks_by_hour$total_articles, 
                                                                 probs=0.05))
# Labels
prob_click_label <- ranks_by_hour$prob_click

# 'classic' features
type_index_feature <- ranks_by_hour$type_rank_prob_index
user_index_feature <- ranks_by_hour$user_rank_prob_index
hours_feature <- ranks_by_hour$send_date_hour
topic_index_feature <- ranks_by_hour$topic_rank_prob_index
article_index_feature <- ranks_by_hour$article_rank_prob_index

# Together in frame
reg_frame <- data.frame(prob_click_label, hours_feature,topic_index_feature,article_index_feature,
                        article_index_diff_feature, article_index_diff_sq_feature)

# transformed features
article_index_diff_feature <- ranks_by_hour$article_rank_prob_index - mean(ranks_by_hour$article_rank_prob_index)
article_index_diff_sq_feature <- article_index_diff_feature^2

# lag features
reg_frame$prob_click_label_lag1 <- c(0,reg_frame$prob_click_label[1:nrow(reg_frame)-1])
reg_frame$prob_click_label_lag2 <- c(0,reg_frame$prob_click_label_lag1[1:nrow(reg_frame)-1])
reg_frame$prob_click_label_lag3 <- c(0,reg_frame$prob_click_label_lag2[1:nrow(reg_frame)-1])
reg_frame$topic_index_feature_lag1 <- c(0,reg_frame$topic_index_feature[1:nrow(reg_frame)-1])
reg_frame$topic_index_feature_lag2 <- c(0,reg_frame$topic_index_feature_lag1[1:nrow(reg_frame)-1])
reg_frame$topic_index_feature_lag3 <- c(0,reg_frame$topic_index_feature_lag2[1:nrow(reg_frame)-1])

# brand new features
reg_frame$hour_of_day <- as.POSIXlt(reg_frame$hours_feature)$hour
reg_frame$day_of_week <- as.POSIXlt(reg_frame$hours_feature)$wday



########## APPENDIX B: ML CLASSIFIERS - determining gender by topic clicks ########### 

# An experiment to see if genders click on topics in a predictable way

# Subset to just articles that were clicked
click_frame <- subset(core_frame, click ==TRUE)

# Counts for all 105 toipcs for each of 20,000 users
all_topics_frame <- click_frame %>%
  group_by(user_id, topic_id) %>%
  summarise(counts=n()) %>%
  arrange(user_id, topic_id)

# Long to wide - each topic is now a column with a count for each user (so 105 topic columns)
all_topics_wide <- dcast(all_topics_frame, user_id ~ topic_id, value.var = 'counts')

# Replace NA with zeros
all_topics_wide[is.na(all_topics_wide)] <- 0

# Add gender column
user_index <- match(all_topics_wide$user_id, core_frame$user_id)
all_topics_wide$gender <- core_frame$gender[user_index]

# Restrict to just 'male' and 'female'
all_topics_wide <-subset(all_topics_wide, gender %in% c('male','female'))
all_topics_wide$gender <- as.factor(all_topics_wide$gender)
all_topics_wide$gender <- droplevels(all_topics_wide$gender)
  
# Name columns properly
topic_columns <- 0
for(i in 1:105)
{
  topic_columns[i] <- paste('Topic',i,sep='')
}
colnames(all_topics_wide) <- c('user_id',topic_columns,'gender')

# Several learning approaches follow 
# Basic conclusion: men and women pick topics very, very similarly
# See the group means for QDA results for example

# LDA 
topic_lda <- lda(gender ~. -user_id,
                 all_topics_wide)
predictions <- predict(topic_lda)
table(predictions$class, all_topics_wide$gender)
mean(predictions$class == all_topics_wide$gender)

# QDA - solid 64% accuracy, but is it overfit....
topic_qda <- qda(gender ~. -user_id,
                 all_topics_wide)

test_features <- all_topics_wide
test_features$gender <- NULL
topic_qda
qda_predictions <- predict(topic_qda, test_features)
table(qda_predictions$class, all_topics_wide$gender)
mean(qda_predictions$class == all_topics_wide$gender) 

# QDA - .......yes it is, it completely fails at predicting a test set
qda_train <- (all_topics_wide$user_id > 4000)
topic_qda <- qda(gender ~. -user_id,
                 all_topics_wide, 
                 subset = qda_train)

test_features <- all_topics_wide[!qda_train,]
test_features$gender <- NULL
topic_qda
qda_predictions <- predict(topic_qda, test_features)
table(qda_predictions$class, all_topics_wide$gender[!qda_train])
mean(qda_predictions$class == all_topics_wide$gender[!qda_train]) 

# KNN 
train <- (all_topics_wide$user_id < 15000)
features_train <- all_topics_wide[,c(-107,-1)][train,]
features_test <- all_topics_wide[,c(-107,-1)][!train,]
labels_train <- all_topics_wide$gender[train]
labels_test <- all_topics_wide$gender[!train]
topic_knn <- knn(features_train, features_test, labels_train, k=10)
table(topic_knn, labels_test)
mean(topic_knn == labels_test)

# Logistic regression
logistic_topic <- glm(gender ~ . -user_id,
                      data=all_topics_wide,
                      family=binomial)
summary(logistic_topic)
logistic_preds <- predict(logistic_topic, type='response')
logistic_class <- rep('female',length(logistic_preds))
logistic_class[logistic_preds>0.5] <- 'male'
table(logistic_class, all_topics_wide$gender)
mean(logistic_class == all_topics_wide$gender)
