# authors: Petrakov Sergey and Semeneev Danat

# Import libraries
library(readr)

#college_data <- read_csv("college_data.csv")
#covers_data <- read_csv("covers_data.csv")

# Add data
college_data <- read_csv("Documents/Микроэконометрика/HW2/college_data.csv")
#View(college_data)
covers_data <- read_csv("Documents/Микроэконометрика/HW2/covers_data.csv")
#View(covers_data)

############ Step 1: Make prepensity score ###############
pscores.model <- glm(win ~ line + I(line**2) + I(line**3) + I(line**4) + I(line**5) ,family = binomial("logit"),data = covers_data)
summary(pscores.model)
sum(pscores.model$residuals)

prs_df <- data.frame(pr_score = predict(pscores.model, type = "response"))

# Add propensity score to covers_data
data <- cbind(covers_data, prs_df)
#data

summary(data$pr_score)
# this is corresponds to author results The minimum and maximum estimated propensity scores are
# 0.005 and 0.994 respectively.
# Our result: min = 0.004492 ~ 0.005, max = 0.993637 ~ 0.994

library(lmtest)
nested <- glm(win~0,family = binomial("logit"),data = covers_data)
complex <- glm(win ~ line + I(line**2) + I(line**3) + I(line**4) + I(line**5) ,family = binomial("logit"),data = covers_data)

lrtest(nested,complex)
# The results are
# Authors result = highly significant; a likelihood ratio test of the hypothesis that all five coefficients equal
# zero produces a χ statistic of 11,333.
# Our result = -//- χ statistic of 11,427.

########### Step 2: Make dependent variable for college_data ###################
library(dplyr)

# basic princip how to make it
result <- college_data %>%
  group_by(teamname) %>%
  mutate(y = alumni_total_giving - lag(alumni_total_giving, n = 2L, default = NA, order_by = NULL)) %>%
  mutate(laged_alumni_ops_athletics = alumni_ops_athletics - lag(alumni_ops_athletics, n = 2L, default = NA, order_by = NULL)) %>% 
  select(X1, teamname, alumni_total_giving, laged_alumni_ops_athletics, y)

result <- data.frame(result)
result <- result %>% 
  select(-teamname)
# get column y and add it to college_data
data_1 <- merge(college_data, result, by.x = "X1", by.y = "X1")

###### Step 3: merging developed covers_data and developed college_data ########
df <- merge(data, data_1, by=c("teamname","year"))

res_2 <- df %>% 
  group_by(teamname, year) %>% 
  mutate(pr_score_plus = lead(rev(cumsum(rev(pr_score))), 1L))

pr_score_plus <- res_2$pr_score_plus
###### Step 4: Reduced form regression #############

# add pr_score_plus
df = cbind(df, pr_score_plus)
# drop 5% of lowest and highest values

new_df <- df %>% filter((pr_score >= quantile(df$pr_score, 1/20)[1])&(pr_score < quantile(data$pr_score, 19/20)[1]))

#5%
quantile(df$pr_score, 1/20)[1]
#95%
quantile(data$pr_score, 19/20)[1]
# assess the model
model_1 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week), weights = 1/pr_score,data = new_df)
model_2 <- lm(pr_score_plus ~ win*as.factor(week), data = new_df)
#colnames(new_df)
summary(model_1)
summary(model_2)

#new_df$athletics_donors

pi <- model_1$coefficients[c(2,14:24)]
gamma <- model_2$coefficients[c(2,14:24)]
beta <- pi/(1+gamma)
beta
# Подход по которому мы строили выборку, откуда рассчитываются коэффициенты для модели отличается от авторского:
# Авторы разделяют выборку на бины по propensity score, в которых делается взвешивание (весам соответствует коэффициент r)
# Мы изначально откидываем по 5% наблюдений, которые соответствуют 5% наибольших и наименьших значений pr_score,
# поэтому нам не надо в сумме всзвешивать на r
final_beta <- sum(beta)
final_beta  


############ naive blocking #############
model_3 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week) + pr_score, data = new_df)
summary(model_3)

pi_1 <- model_3$coefficients[c(2,15:25)]
gamma_1 <- model_2$coefficients[c(2,14:24)]

beta_1 <- pi_1/(1+gamma_1)
beta_1
final_beta_1 <- sum(beta_1)
final_beta_1




##### to be continued...





########### real blocking ################

one_interval_data <- new_df %>% filter(pr_score <= quantile(new_df$pr_score, 1/5)[1])
two_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 1/5)[1])&(pr_score < quantile(new_df$pr_score, 2/5)[1]))
three_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 2/5)[1])&(pr_score < quantile(new_df$pr_score, 3/5)[1]))
four_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 3/5)[1])&(pr_score < quantile(new_df$pr_score, 4/5)[1]))
five_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 4/5)[1])&(pr_score <= quantile(new_df$pr_score, 1)[1]))

# number of observations
nrow(one_interval_data)
nrow(two_interval_data)
nrow(three_interval_data)
nrow(four_interval_data)
nrow(five_interval_data)



quatiled_data <- list(one_interval_data, two_interval_data, three_interval_data, four_interval_data, five_interval_data)

gammas <- c(c(), c(), c(), c(), c())
pis <- c(c(), c(), c(), c(), c())
betas <- c(c(), c(), c(), c(), c())
i = 1
for (data in quatiled_data){
  model_11 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week) + pr_score, data = data)
  model_12 <- lm(pr_score_plus ~ win*as.factor(week), data = data)
  
  pi_1 <- model_11$coefficients[c(2,15:25)]
  gamma_1 <- model_12$coefficients[c(2,14:24)]
  print(pi_1)
  append(x = pis[i], pi_1)
  append(x = gammas[i], gamma_1)
  betas[i] <- pis[i]/(1+gammas[i]) 
  
  i = i+1
}

#########################

one_interval_data <- new_df %>% filter(pr_score <= quantile(new_df$pr_score, 1/3)[1])
two_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 1/3)[1])&(pr_score < quantile(new_df$pr_score, 2/3)[1]))
three_interval_data <- new_df %>% filter((pr_score >= quantile(new_df$pr_score, 2/3)[1])&(pr_score < quantile(new_df$pr_score, 1)[1]))
####

model_11 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week) + pr_score, data = one_interval_data)
model_12 <- lm(pr_score_plus ~ win*as.factor(week), data = one_interval_data)

pi_1 <- model_11$coefficients[c(2,15:25)]
gamma_1 <- model_12$coefficients[c(2,14:24)]

beta_1 <- pi_1/(1+gamma_1)
beta_1
final_beta_1 <- sum(beta_1)
final_beta_1
####

model_21 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week) + pr_score, data = two_interval_data)
model_22 <- lm(pr_score_plus ~ win*as.factor(week), data = two_interval_data)

pi_2 <- model_21$coefficients[c(2,15:25)]
gamma_2 <- model_22$coefficients[c(2,14:24)]

beta_2 <- pi_2/(1+gamma_2)
beta_2
final_beta_2 <- sum(beta_2)
final_beta_2


####
model_31 <- lm(laged_alumni_ops_athletics ~ win*as.factor(week) + as.factor(week) + pr_score, data = three_interval_data)
model_32 <- lm(pr_score_plus ~ win*as.factor(week), data = three_interval_data)

pi_3 <- model_31$coefficients[c(2,15:25)]
gamma_3 <- model_32$coefficients[c(2,14:24)]

beta_3 <- pi_3/(1+gamma_3)
beta_3
final_beta_3 <- sum(beta_3)
final_beta_3


(final_beta_3 + final_beta_2 + final_beta_1)/3




