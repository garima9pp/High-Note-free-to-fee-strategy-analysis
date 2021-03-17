options(digits=2)
options(scipen=3)
setwd("E:/BANA277/Midterm")
library("tidyverse")
library(purrr)
highnote <-  read_csv("HighNote Data Midterm.csv") #will be used for EDA
highnote_1 <- read_csv("HighNote Data Midterm.csv") #will be used for regression
HighNote <- read_csv("HighNote Data Midterm.csv")#will be used for PSM
#Summary statistics:
library(psych)
#Overall summary statistics
describe(highnote[-1])

#Summary statistics for adopter
adopter <- describe(filter(highnote, adopter == 1)[-1])

adopter <- cbind("Variables" = rownames(adopter), adopter)
rownames(adopter) <- 1:nrow(adopter)
colnames(adopter)

#Summary statistics for non-adopter
non_adopter <- describe(filter(highnote, adopter == 0)[-1])
non_adopter <- cbind("Variables" = rownames(non_adopter), non_adopter)
rownames(non_adopter) <- 1:nrow(non_adopter)
colnames(non_adopter)

#mean of the data for adopter column
mean_stats <- highnote %>% group_by(adopter) %>% summarise_all(mean)

#joining the summary for adopter and non-adopter and selecting the relevant columns
summary_stats <- adopter %>% inner_join(non_adopter, by="Variables",suffix=c("_1","_0")) %>%
  select(Variables,n_1,mean_1,median_1,sd_1,min_1,max_1,n_0,mean_0,median_0,sd_0,min_0,max_0) %>% filter(Variables != 'adopter')
summary_stats$mean_ratio <- summary_stats$mean_1/summary_stats$mean_0

#t tests
#t test for age
print(t.test(highnote$age ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for male
print(t.test(highnote$male ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for friend_cnt
print(t.test(highnote$friend_cnt ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for avg_friend_age
print(t.test(highnote$avg_friend_age ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for avg_friend_male
print(t.test(highnote$avg_friend_male ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value = 0.000009 the difference in the means is statistically significant

#t test for friend_country_cnt
print(t.test(highnote$friend_country_cnt ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for subscriber_friend_cnt
print(t.test(highnote$subscriber_friend_cnt ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for songsListened
print(t.test(highnote$songsListened ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for lovedTracks
print(t.test(highnote$lovedTracks ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant

#t test for posts
print(t.test(highnote$posts ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value = 0.00003 the difference in the means is statistically significant

#t test for playlists
print(t.test(highnote$playlists ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value = 9e-16 the difference in the means is statistically significant

#t test for shouts
print(t.test(highnote$shouts ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value = 0.0004 the difference in the means is statistically significant

#t test for tenure
print(t.test(highnote$tenure ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value = 5e-07 the difference in the means is statistically significant

#t test for good_country
print(t.test(highnote$good_country ~ highnote$adopter,alternative="two.sided",var.equal=FALSE))
#p-value < 2.2e-16 the difference in the means is statistically significant


##Data Visualization

library(ggplot2)
library(gridExtra)
#Demographics
highnote$adopter = factor(highnote$adopter, labels = c("non-adopter","adopter"))
#age histogram
ggplot(highnote, aes(x = age))+
  geom_histogram(aes(fill=adopter), binwidth = 1)+
  labs(title="Age Histogram", 
       fill="") 

#age
ggplot(highnote,aes(x=age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#age density
ggplot(highnote, aes(x = age))+
  geom_density(aes(fill=adopter), alpha=0.6)+
  labs(title="Age Density", 
       fill="") 


#gender bar
#install.packages("sjPlot")
library(sjPlot)
sjPlot::plot_xtab(highnote$adopter, factor(highnote$male, labels = c("female","male")), margin = "row", bar.pos = "stack")
sjPlot::plot_xtab(highnote$adopter, factor(highnote$good_country, labels = c("no","yes")), margin = "row", bar.pos = "stack")


####Peer influence

#Friend_cnt vs Subscriber_friend_cnt
ggplot(highnote, aes(x = friend_cnt, y = subscriber_friend_cnt))+
  geom_point(aes(color = adopter), size = 1, alpha = 0.8)+
  geom_smooth(aes(color = adopter), se = F)+
  labs(title="Friend_cnt vs Subscriber_friend_cnt",
       color = " ")


#friend_cnt
friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(friend_cnt=mean(friend_cnt))
ggplot(friend_cnt,aes(x = adopter,y=friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()+
  labs(title="Average friend count")


#friend age
ggplot(highnote,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()
 

#subscriber friend count
subscriber_friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))
ggplot(subscriber_friend_cnt,aes(x = adopter,y=subscriber_friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#avg friend male
avg_friend_male<- highnote %>%
  group_by(adopter)%>%
  summarise(avg_friend_male=mean(avg_friend_male))
ggplot(avg_friend_male,aes(x = adopter,y=avg_friend_male)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()+
  labs(title="Average of avg friend male")

#subscription rate
highnote = highnote %>% mutate(subscription_rate = subscriber_friend_cnt/friend_cnt)
adopter_subscription_rate = mean(highnote$subscription_rate[highnote$adopter == "adopter"])
#0.05
nonadopter_subscription_rate = mean(highnote$subscription_rate[highnote$adopter == "non-adopter"])
#0.021

ggplot(highnote, aes(x = adopter, y = subscription_rate)) + 
  geom_bar(stat = "summary", fun.y = "mean", width = 0.3)+
  labs(title="mean subscription rate")

#overall correlation between the age and friend age
cor(highnote$age,highnote$avg_friend_age)
#0.69

#install.packages("corrplot")
library(corrplot)
#adopter correlation plot
corr_data <- highnote %>% select(age,male,friend_cnt,avg_friend_age,
                                 avg_friend_male,friend_country_cnt, subscriber_friend_cnt,adopter)
cor_adopter <- corr_data %>% filter(adopter == "adopter") %>% select(age,male,friend_cnt,avg_friend_age,
                                                  avg_friend_male,friend_country_cnt, subscriber_friend_cnt)

Corr_a <- round(cor(cor_adopter),2)
upper<-Corr_a
upper[upper.tri(Corr_a)]<-""
upper<-as.data.frame(upper)
upper
corrplot(Corr_a, method="circle")

#non adopter correlation plot
corr_data <- highnote %>% select(age,male,friend_cnt,avg_friend_age,
                                 avg_friend_male,friend_country_cnt, subscriber_friend_cnt,adopter)
cor_nadopter <- corr_data %>% filter(adopter == "non-adopter") %>% select(age,male,friend_cnt,avg_friend_age,
                                                                     avg_friend_male,friend_country_cnt, subscriber_friend_cnt)

Corr_na <- round(cor(cor_nadopter),2)
upper<-Corr_na
upper[upper.tri(Corr_na)]<-""
upper<-as.data.frame(upper)
upper
corrplot(Corr_na, method="circle")


####User engagement

#songs listened
g=songsListened<- highnote %>%
  group_by(adopter)%>%
  summarise(songsListened=mean(songsListened))
ggplot(songsListened,aes(x = adopter,y=songsListened)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()+
  labs(title="Avg song listened")

#loved tracks
h=lovedTracks<- highnote %>%
  group_by(adopter)%>%
  summarise(lovedTracks=mean(lovedTracks))
ggplot(lovedTracks,aes(x = adopter,y=lovedTracks)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#posts
i=posts<-highnote %>%
  group_by(adopter)%>%
  summarise(posts=mean(posts))
ggplot(posts,aes(x = adopter,y=posts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#playlists
j=playlists<-highnote %>%
  group_by(adopter)%>%
  summarise(playlists=mean(playlists))
ggplot(playlists,aes(x = adopter,y=playlists)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#shouts
k=shouts<-highnote %>%
  group_by(adopter)%>%
  summarise(shouts=mean(shouts))
ggplot(shouts,aes(x = adopter,y=shouts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#tenure
l=tenure<- highnote %>%
  group_by(adopter)%>%
  summarise(tenure=mean(tenure))
ggplot(tenure,aes(x = adopter,y=tenure)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


##Propensity Score Matching (PSM)
#create the treatment variable
HighNote$Treatment <- ifelse(HighNote$subscriber_friend_cnt >= 1,1,0)
#checking the proportions
HighNote %>%
  group_by(Treatment) %>%
  summarise(count = n(),
            proportion_ad = mean(adopter))

#Proportions
table(HighNote$Treatment,HighNote$adopter)
sjPlot::plot_xtab(HighNote$adopter, factor(HighNote$Treatment, labels = c("no","yes")), margin = "row", bar.pos = "stack")

#checking for significance
t.test(HighNote$adopter ~ HighNote$Treatment)
#p-value <2e-16,we see that there is a significant difference in means.

#Now, we test the difference in means on the pre-treatment covariates. As we don't know anything 
#about the significance of these covariates yet, we perform a t-test on all of them to check if 
#there is a significant difference in means in the Treatment and Control groups.


HN_cov <- c('age','male','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt','songsListened','lovedTracks',
            'playlists','posts','shouts','good_country', 'tenure')

means <- HighNote %>%
  group_by(Treatment) %>%
  select(one_of(HN_cov)) %>%
  summarise_all(mean)

#do the t test

lapply(HighNote[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
                   'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
                   'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country')], function(i) t.test(i ~ HighNote$Treatment))

#Propensity Score Estimation
logit_ps <- glm(Treatment ~ age +male+ friend_cnt + songsListened + playlists + posts + shouts +
                  lovedTracks + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + 
                  tenure, family = binomial(), data = HighNote)

summary(logit_ps)


prp_df <- data.frame(pr_score = predict(logit_ps, type = "response"),
                     has_subscriber_friends = logit_ps$model$Treatment)
head(prp_df)
#region of common support
labs <- paste("Actual treatment type :", c("having subscriber friends", "No subscriber friends"))
prp_df %>%
  mutate(has_subscriber_friends = ifelse(has_subscriber_friends == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~has_subscriber_friends) +
  xlab("Probability of having subscriber friends") +
  theme_bw()

#To get a visual idea, let us plot the propensity scores for the treatment and control group.
library(Hmisc)
#install.packages("Hmisc")
HighNote$prp_score <- prp_df$pr_score
HighNote$has_subscriber_friends <- prp_df$has_subscriber_friends
histbackback(split(HighNote$prp_score, HighNote$has_subscriber_friends))


#Running the Matching Algorithm

#We use the package MatchIt to run a matching algorithm on the covariates. The matched data is put into a 
#new dataframe called matched_data. This table has a column called "distance", which gives us the propensity 
#score.
library(MatchIt)
matching <- matchit(Treatment ~ log(age) +log(1+male)+ log(friend_cnt) + log(1+songsListened) + log(1+playlists) + log(1+posts) + log(1+shouts) + 
                      log(1+lovedTracks) + log(1+good_country) + log(avg_friend_age) + log(1+avg_friend_male) +
                      log(1+friend_country_cnt) + log(1+tenure), method = "nearest", data = HighNote, caliper=0.08)
summary(matching)

matched_data <- match.data(matching)

head(matched_data)
#Distribution of propensity score
plot(matching,	type	= "jitter")
plot(matching, type="hist")

#	2.	chi-square	test
#install.packages("RItools")
library(RItools)
xBalance(Treatment ~ age + male+friend_cnt + songsListened + playlists + posts + shouts + 
           lovedTracks + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + 
           tenure,	data	= matched_data,	report	= c("chisquare.test"))
#p value 0.091
#t-test
t.test(	matched_data$adopter,matched_data$Treatment)
# p-value <2e-16 the results are significant
#distribution plot
sjPlot::plot_xtab(matched_data$adopter, factor(matched_data$Treatment, labels = c("no","yes")), margin = "row", bar.pos = "stack")

dim(matched_data)
head(matched_data)
#Effects of Treatment
#Plotting the propensity scores in a similar pattern as before, so that we can see a significant difference.
histbackback(split(matched_data$distance, matched_data$has_subscriber_friends))

#T-Tests
matched_cov <- c('age','male','friend_cnt',"songsListened","playlists","posts",'shouts','lovedTracks',
                 'good_country','avg_friend_age','avg_friend_male','friend_country_cnt', 'tenure')

match_means <- matched_data %>%
  group_by(Treatment) %>%
  select(one_of(matched_cov)) %>%
  summarise_all(mean)

lapply(matched_data[,c('age','male','friend_cnt',"songsListened","playlists","posts",'shouts','lovedTracks',
                   'good_country','avg_friend_age','avg_friend_male','friend_country_cnt', 'tenure')], 
       function(i) t.test(i ~ matched_data$Treatment,paired=TRUE))


#Logistic Regression on matched data

logit_reg1 <- glm(adopter ~ age +male+ friend_cnt + songsListened + playlists + posts + shouts + 
                   lovedTracks + good_country + avg_friend_age + subscriber_friend_cnt + 
                   avg_friend_male + friend_country_cnt + tenure, family = binomial(), 
                 data = matched_data)

summary(logit_reg1)
#regression after removing the insignificant variables at 95% confidence
logit_reg2 <- glm(adopter ~ male +  songsListened + playlists + lovedTracks + good_country + avg_friend_age + subscriber_friend_cnt + tenure, family = binomial(), data = matched_data)

summary(logit_reg2)

Odds_Ratio <- exp(coefficients(logit_reg2))
Odds_Ratio

#Multicollinearity -> all the variables are smaller than 5, which is the rule of thumb
library(car)
vif(logit_reg2)
#install.packages("ROCR")
prediction <- predict(logit_reg2, matched_data, type="response")
prediction <- as.data.frame(prediction)
#install.packages("modelplotr")
library(modelplotr)
fitted.results <- ifelse(prediction > 0.5,1,0)
misClasificError <- mean(fitted.results != matched_data$adopter)
print(paste('Accuracy',1-misClasificError))
#Accuracy 0.874203144921377

##Regression Analyses on the full data highnote_1 
#using the original data 
####First attempt
#Use all variables in the regression model. 
#put all variables into the regression
fit1 = glm(adopter ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + 
             subscriber_friend_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure + 
             good_country,data = highnote_1, family = binomial())
summary(fit1)
#delete non-significant variables
fit2 = glm(adopter ~ age + male + friend_cnt + avg_friend_age + friend_country_cnt + 
             subscriber_friend_cnt + songsListened + lovedTracks + playlists + tenure + good_country, 
           data = highnote_1, family = binomial())
summary(fit2)

#Multicollinearity -> all the variables are smaller than 5, which is the rule of thumb
library(car)
vif(fit2)
p1_5 <- predict(fit2, highnote_1, type="response")
p1_5 <- as.data.frame(p1_5)
#install.packages("modelplotr")
fitted.results <- ifelse(p1_5 > 0.5,1,0)
misClasificError <- mean(fitted.results != highnote_1$adopter)
print(paste('Accuracy',1-misClasificError))
#"Accuracy 0.918338010815251"
Odds_Ratio_full <- exp(coefficients(fit2))
Odds_Ratio_full














