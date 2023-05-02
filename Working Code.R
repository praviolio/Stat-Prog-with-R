install.packages("tidyverse")
install.packages("plotly")
install.packages("leaps")
install.packages("randomForest")
library(dplyr)
library(readr)
library(caret) 
library("tidyverse")
library ("plotly")
library("datasets")
library("forecast")
library(ggplot2) 
library('leaps')
library(car)
library(e1071)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(C50)

data <- read_csv('updated.csv')

# Understanding the data more through summary statistics and visualization
summary(data)

# Visualization
# Number of companies in different industries across the years
ggplot(data, aes(x = fyear, fill = FFI12_desc, na.rm=TRUE)) + geom_bar()

# Checking for correlation between ghg and e_score
options(scipen = 999, digits = 4)
ggplot(data, aes(x = e_score, y = ghg,na.rm=TRUE)) + geom_point()

# Which industry produce the most greenhouse gas?

#Understanding number of companies in each industry in our dataset
ind_count<- data %>% group_by(FFI12_desc) %>% summarise(total_count=n())

ggplot(ind_count, aes(x = reorder(FFI12_desc, -total_count), y = total_count))  +geom_bar(stat = "identity", fill = 'skyblue') +theme(axis.text.x=element_text(angle=45, hjust=0.9))

# Industries ranking in ghg levels using med_ghg 
median_ghg_ind <- data %>% group_by(FFI12_desc) %>% 
                          summarise(med_ghg = median(ghg)) 


ggplot(median_ghg_ind, aes(x = reorder(FFI12_desc, -med_ghg), y= med_ghg)) +geom_bar(stat = "identity", fill = 'skyblue') +theme(axis.text.x=element_text(angle=45, hjust=0.9))


# Is the greenhouse gas emission levels increasing or decreasing over the years
med_ghg_fyear <- data %>% group_by(fyear) %>% 
                          summarise(med_ghg = median(ghg)) 


ggplot(med_ghg_fyear, aes(x = fyear, y= med_ghg)) +geom_bar(stat = "identity", fill = 'skyblue') 

# e_score ranking for differnt industries

median_e_score_ind <- data %>% group_by(FFI12_desc) %>% 
  summarise(med_e_score = median(e_score)) 

ggplot(median_e_score_ind, aes(x = reorder(FFI12_desc, -med_e_score), y= med_e_score)) +geom_bar(stat = "identity", fill = 'skyblue') +theme(axis.text.x=element_text(angle=45, hjust=0.9))

# e_score over the years
median_e_score_fyear <- data %>% group_by(fyear) %>% 
  summarise(med_e_score = median(e_score)) 

ggplot(median_e_score_fyear, aes(x = fyear, y= med_e_score)) +geom_bar(stat = "identity", fill = 'skyblue') 


# Socka Visualizations 
# Cleaning for visualization for ghg 
median = data %>% group_by(FFI12_desc,fyear) %>% summarise(median(ghg))
average = data %>% group_by(FFI12_desc,fyear) %>% summarise(mean(ghg))
# Cleaning for visualization for e_score 
data1 = data %>% group_by(FFI12_desc,fyear) %>% mutate(e_score = ifelse(is.na(e_score),median(e_score, na.rm = TRUE),e_score))
median1 = data1 %>% group_by(FFI12_desc,fyear) %>% summarise(median(e_score,na.rm = TRUE))
average1 = data1 %>% group_by(FFI12_desc,fyear) %>% summarise(mean(e_score,na.rm = TRUE))

#Merge ghg data
A = merge(median, average)
A = rename(A, med_ghg = "median(ghg)")
A = rename(A, avg_ghg = "mean(ghg)")
B = merge(median1, average1)
B = rename(B, med_escore = "median(e_score, na.rm = TRUE)", avg_escore = "mean(e_score, na.rm = TRUE)")

#Visualising ghg 
p = ggplot(A, aes(x = fyear, y = med_ghg/1000000, group = FFI12_desc, color = FFI12_desc)) +
  geom_line() + 
  labs(title = "Median Greenhouse Gasses emitted", x = "Year", y = "Median Greenhouse Gasses emitted (million metric tons)")

ggplotly(p)

#Visualising e_score
q =   ggplot(B, aes(x = fyear, y = med_escore, group = FFI12_desc, color = FFI12_desc)) +
  geom_line() + 
  labs(title = "Median Environmental Score", x = "Year", y = "Median Environmental Score (%)")

ggplotly(q)

#Merge A and B
C = merge(A,B)
write_csv(C, "summary.csv")

#Correlation between e-score and greenhouse gas emissions
r = ggplot(C, aes(x = med_ghg/1000000, y = med_escore, group = fyear, color = FFI12_desc))+geom_point()+
  labs(title = "Relationship between Greenhouse Gas Emissions and E-Score", 
       x = "Greenhouse Gas Emissions (million metric tons)", y = "E-Score")

ggplotly(r)


# Visualization
# R&D
# Capex
# restate
# debt ratio
# roe 
# fortune 1000

# Regression Analysis

# Constructing of necessary variables for hypothesis testing/regression


data <- data %>%mutate(rd_com = xrd/sale)

data <- data %>%mutate(capex_com = capx/sale)

data <- data %>%mutate(debt_ratio = (dltt+dlc)/at)

data<- data %>%mutate(roe = ni/ceq)

summary(data)

#Hypothesis 1 : Companies that have spend more on R&D produce lesser GHG

rd_reg <- lm(formula = log(ghg) ~ rd_com, data = data )
summary(rd_reg)
# Interpretation: A one unit decrease in R&D expense is associated with lower ghg by 8.85
# Interpretation is correct as p value <0.05, statistically significant

ggplot(data , aes(x = rd_com , y = log(ghg))) +geom_point(na.rm=TRUE)+geom_smooth(method = "lm", se = FALSE, na.rm=TRUE)

# Hypothesis 2: Companies that spend more on capex have higher ghg
capex_reg <- lm(formula = log(ghg) ~ capex_com, data = data)
summary(capex_reg)
# Interpretation: A one unit increase in CApex is associated with an increase of ghg by 17.9
# Interpretation is correct as p value <0.05, statistically significant

ggplot(data , aes(x = capex_com , y = log(ghg))) +geom_point()+geom_smooth(method = "lm", se = FALSE)

# Hypothesis 3 : Companies who restates more are likely to have lower ghg 
mis_reg <- lm(formula = log(ghg) ~ restate, data = data)
summary(mis_reg)
# Interpretation: Not statistically significant as p-value >0.05, suggesting that restatements is irrelevant to ghg levels


# Hypothesis 4: Companies with higher debt ratio tends to produce lower ghg 
debt_ratio_reg <- lm(formula = log(ghg) ~ debt_ratio, data = data)
summary(debt_ratio_reg)
# Interpretation: A one unit increase in debt ratio is associated with an increase of ghg by 2.4
# Interpretation is correct as p value <0.05, statistically significant

ggplot(data , aes(x = debt_ratio , y = log(ghg))) +geom_point()+geom_smooth(method = "lm", se = FALSE)


# Hypothesis 5: Companies with higher roe tend to produce lesser ghg
roe_reg <- lm(formula = log(ghg) ~ roe, data = data)
summary(roe_reg)
# Interpretation: Not statistically significant as p-value >0.05, suggesting that roe is irrelevant to ghg levels


# Hypothesis 6: Fortune 1000 companies produce more ghg
fortune_reg <- lm(formula = log(ghg) ~ d_fortune, data = data)
summary(fortune_reg)
# Interpretation: This means that fortune 1000 companies produce more ghg, since p value<0.05, it is statistically significant

# Hypothesis 7 : Tech companies produce lesser ghg
# Filter number of companies in 'tech industry' using keyword tech
tech_count <- data %>% group_by(FFI12_desc) %>% filter(grepl("tech",busdesc))

# Visualization of the top3 tech industries
ind_count1<- tech_count %>% group_by(FFI12_desc) %>% summarise(total_count=n())

ggplot(ind_count1, aes(x = reorder(FFI12_desc, -total_count), y = total_count))  +geom_bar(stat = "identity", fill = 'skyblue') +theme(axis.text.x=element_text(angle=45, hjust=0.9))

# Creating a dummy variable for tech companies 
data <- data %>% mutate(d_tech = ifelse(grepl("tech",busdesc) ,1,0))
sum(data$d_tech) # To check if its correct with the tech_count dataset


tech_reg <- lm(formula = log(ghg) ~ d_tech, data = data)
summary(tech_reg)
# Interpretation: Tech companies produce lesser ghg, since coefficient of d_tech is negative and statistically significant

# Hypothesis 8 : Lower escore motivates companies to reduce ghg emissions, as companies are likely to decrease their ghg emissions YoY( negative change in ghg) when they see their low e_score
data2 = read_csv("summary.csv")
data2 = data2 %>% filter(!fyear == 2020)
data2 = data2 %>% select (-avg_ghg, -avg_escore)
data2 = data2 %>% group_by(FFI12_desc) %>%  mutate(lg_ghg = log(med_ghg,base = 10)) %>% mutate(lag_lg_ghg = lag(lg_ghg,1)) %>% mutate(lag_escore = lag(med_escore,1))

data_final = data2 %>% filter(!is.na(lag_lg_ghg), !is.na(lag_escore))

data_final = data_final %>% mutate(ch_lag_ghg = lg_ghg - lag_lg_ghg)

A = lm(formula = ch_lag_ghg ~ lag_escore, data = data_final)
summary(A)

ggplot(data_final, aes(x = lag_escore, y = ch_lag_ghg, color = FFI12_desc))+geom_point()+labs(title = "Change in GHG emissions against E-score", x = "E-score (preceding year)", y = "Change in GHG emissions (log)")



# Multi-regresion 1 (R&D and CAPEX)
multi_reg1 <- lm(formula = log(ghg) ~ rd_com + capex_com , data = data)
summary(multi_reg1)
# Results is consistent with Hypothesis 2

# Checking for multi-collinearity using VIF
vif(multi_reg1)

# Multi-regresion 2 (R&D + CAPEX + restate)
multi_reg2 <- lm(formula = log(ghg) ~ rd_com + capex_com +restate, data = data)
summary(multi_reg2)
# Results is consistent with Hypothesis 3

# Checking for multi-collinearity using VIF
vif(multi_reg2)

# Multi-regresion 3 (R&D + CAPEX + restate +debt_ratio)
multi_reg3 <- lm(formula = log(ghg) ~ rd_com + capex_com +restate +debt_ratio, data = data)
summary(multi_reg3)
# Results is consistent with Hypothesis 4

# Checking for multi-collinearity using VIF
vif(multi_reg3)

# Multi-regresion 4 (R&D + CAPEX + restate + debt_ratio + roe)
multi_reg4 <- lm(formula = log(ghg) ~ rd_com + capex_com +restate +debt_ratio +roe, data = data)
summary(multi_reg4)
# Results is consistent with Hypothesis 5

# Checking for multi-collinearity using VIF
vif(multi_reg4)

# Multi-regresion 5 (R&D + CAPEX + restate + debt_ratio + roe + d_fortune)
multi_reg5 <- lm(formula = log(ghg) ~ rd_com + capex_com +restate +debt_ratio +roe + d_fortune, data = data)
summary(multi_reg5)
# Results is consistent with Hypothesis 6

# Checking for multi-collinearity using VIF
vif(multi_reg5)


# Multi-regresion 6 (R&D + CAPEX + restate + debt_ratio + roe + d_fortune +d_tech)
multi_reg6 <- lm(formula = log(ghg) ~ rd_com + capex_com +restate +debt_ratio +roe + d_fortune +d_tech, data = data)
summary(multi_reg6)
# Results is not consistent with Hypothesis 8 anymore
# Interpretation: restate IV becomes significant now while d_tech becomes insignificant, the others stil same no sign change

# Checking for multi-collinearity using VIF
vif(multi_reg6)

data <- data%>% mutate(log_ghg = log(ghg))
ghg_pred <- predict(multi_reg6, data)
ghg_error <- data$log_ghg - ghg_pred
ghg_final <- data.frame(cbind(data,
                              ghg_pred, 
                              ghg_error))

accuracy(ghg_final$ghg_pred, ghg_final$log_ghg)
# Performing Variable Selection Algorithm

# 1. Forward Selection
ghg_forward <- step(multi_reg6, direction = "forward")
summary(ghg_forward)

ghg_forward_pred <- predict(ghg_forward, data)
accuracy(ghg_forward_pred, data$log_ghg)

# 2. Backward Selection
ghg_backward <- step(multi_reg6, direction = "backward")
summary(ghg_backward)

ghg_backward_pred <- predict(ghg_backward, data)
accuracy(ghg_backward_pred, data$log_ghg)
# 3. Stepwise Selection
ghg_stepwise <- step(multi_reg6, direction = "both")
summary(ghg_stepwise)

ghg_stepwise_pred <- predict(ghg_stepwise, data)
accuracy(ghg_stepwise_pred, data$log_ghg)


# Classification Tree

# If ghg for a company is lower than the global med ghg, it is deemed as healthy range 
ghg_data <- data %>% mutate(healthy = ifelse(ghg > quantile(ghg, probs=0.2 , na.rm = TRUE), 1, 0))
table(ghg_data$healthy) # Roughly 20%(2130) is healthy and 80%(8519) healthy(very even)

# Pareto Principle to check if the top 20% of companies in dataset contribtutes to almost 80% of ghg(Justify why we use quantile probs=20)
top20 <- round(0.2 *10649) 
pareto <- data %>% arrange(desc(ghg)) %>% top_n(top20)
pareto1 <- sum(pareto$ghg)
proportion_top_20 <- pareto1/sum(data$ghg) # Companies that belong in the top 20% of ghg emissions contribute to around 95% of total ghg emissions, proving Paretos Principle
# Thus using pareto principle , we determine that the quantile 20% and above to be unhealthy range

# WIthout Prunning
ghg_tree <- rpart(formula = healthy ~ rd_com + capex_com + restate + debt_ratio + roe + d_fortune + d_tech,
                  data = ghg_data, 
                  method = "class")

# With Prunning using cp 
ghg_tree1 <- rpart(formula = healthy ~ rd_com + capex_com + restate + debt_ratio + roe + d_fortune + d_tech,
                  data = ghg_data, 
                  method = "class" , cp=0.02) # Removes unncessary IVs so we should not prune instead

# Pruning using minsplit
ghg_tree2 <- rpart(formula = healthy ~ rd_com + capex_com + restate + debt_ratio + roe + d_fortune + d_tech,
                  data = ghg_data, 
                  method = "class" , minsplit = 600) # Removes unncessary IVs so we should not prune instead

# Showing it as a plot
prp(ghg_tree, type = 1, extra = 1)
prp(ghg_tree1, type = 1, extra = 1)
prp(ghg_tree2, type = 1, extra = 1)

# Printing the results as a text
rpart.rules(ghg_tree)
rpart.rules(ghg_tree1)
rpart.rules(ghg_tree2)

# Confusion Matrix

# Without Prunning
ghg_tree_pred <- predict(ghg_tree,
                           ghg_data, type = "class")
confusionMatrix(table(ghg_tree_pred, 
                      ghg_data$healthy), 
                positive = "1")

# With Prunning (cp=0.02)
ghg_tree_pred1 <- predict(ghg_tree1,
                         ghg_data, type = "class")
confusionMatrix(table(ghg_tree_pred1, 
                      ghg_data$healthy), 
                positive = "1")

# With Prunning (minsplit =600)
ghg_tree_pred2 <- predict(ghg_tree2,
                          ghg_data, type = "class")
confusionMatrix(table(ghg_tree_pred2, 
                      ghg_data$healthy), 
                positive = "1")
# ROC

# Without Prunning
ghg_prob_pred <- predict(ghg_tree, 
                           ghg_data, 
                           type = "prob")

ghg_tree_combined <- cbind(ghg_data, 
                           ghg_prob_pred)

ROC <- plot.roc(ghg_tree_combined$healthy, 
                ghg_tree_combined$`1`)

auc(ROC) 

# With Prunning (cp =0.02)
ghg_prob_pred1 <- predict(ghg_tree1, 
                         ghg_data, 
                         type = "prob")

ghg_tree_combined1 <- cbind(ghg_data, 
                           ghg_prob_pred1)

ROC <- plot.roc(ghg_tree_combined1$healthy, 
                ghg_tree_combined1$`1`)

# With Prunning (minsplit =600)
ghg_prob_pred2 <- predict(ghg_tree2, 
                          ghg_data, 
                          type = "prob")

ghg_tree_combined2 <- cbind(ghg_data, 
                            ghg_prob_pred2)

ROC <- plot.roc(ghg_tree_combined2$healthy, 
                ghg_tree_combined2$`1`)

auc(ROC) 

# Bagging

# Removing NA values because random forest dont handle missing values in predictors
ghg_data1 <- ghg_data %>% filter(!is.na(ind_xad) , !is.na(ind_xlr) , !is.na(ind_xrd), !is.na(rd_com))
summary(ghg_data1)

ghg_randf<-randomForest(as.factor(healthy)~ .,
                          data = ghg_data1,
                          ntree=500)

# generate confusion matrix for random forest
ghg_randf_pred <- predict(ghg_randf,ghg_data1,type="class")
confusionMatrix(table(ghg_randf_pred, ghg_data1$healthy), positive = "1")

# generate ROC for random forest
ghg_prob_pred <- predict(ghg_randf, ghg_data1, type="prob")
ghg_tree_combined <- cbind(ghg_data1, ghg_prob_pred)

ROC <- plot.roc(ghg_tree_combined$healthy, ghg_tree_combined$`1`)
auc(ROC)

