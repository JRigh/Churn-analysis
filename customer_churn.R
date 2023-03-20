#-----------------------------
# Customer churn analysis in R
#-----------------------------

library(tidyverse)
library(xtable)
library(gridExtra)
library(cowplot)
library(recipes)
library(corrr)
library(sjPlot)
library(MASS)

# 1. Download dataset
dataset <- read.csv('C:/Users/julia/OneDrive/Desktop/github/11. Customer_churn_analysis/Telco-Customer-Churn.csv', na.strings = c('','?'))
dataset$ChurnDummy <- as.factor(ifelse(dataset$Churn == 'Yes', 1, 0))
dataset = na.omit(dataset) # removing missing values

# 2. churn rate baseline
churn.base <- dataset %>% 
    group_by(Churn) %>% 
    count(Churn) %>% 
    mutate(percentage = n/nrow(dataset) * 100) %>% 
    rename(customers = n)

# 3. Data visualization

churn.base.percentage.barpot <- ggplot(as.data.frame(churn.base), aes(x = Churn, y=percentage, 
                                                           fill = percentage)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  annotate('text', label = paste("Yes =", round(as.data.frame(churn.base)[1,3]), 3), x = 'Yes' , y = 100/2, size = 3) +
  annotate('text', label = paste("No =", round(as.data.frame(churn.base)[2,3]), 3), x = 'No' , y = 100/2, size = 3) +
  labs(title = 'Customer churn in percentage - Bar plot',
       subtitle = 'Telco churn dataset',
       y="percentage of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

churn.base.count.barpot <- ggplot(as.data.frame(churn.base), aes(x = Churn, y=customers, 
                                                           fill = customers)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  annotate('text', label = paste("Yes =", as.data.frame(churn.base)[1,2]), x = 'Yes' , y = nrow(dataset)/2, size = 3) +
  annotate('text', label = paste("No =", as.data.frame(churn.base)[2,2]), x = 'No' , y = nrow(dataset)/2, size = 3) +
  labs(title = 'Customer churn count - Bar plots',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(churn.base.percentage.barpot, churn.base.count.barpot, 
             nrow = 2)

# churn by type of contract

churn.contract = dataset %>%
  group_by(Contract) %>% 
  count(Churn) %>% 
  mutate(percentage = n/nrow(dataset) * 100) %>% 
  rename(customers = n)

churn.contract.percentage.barpot <- ggplot(as.data.frame(churn.contract), aes(x = Contract, y=percentage, 
                                                                      fill = Churn)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
 labs(title = 'Customer churn by type of contract in percentage - Bar plot',
       subtitle = 'Telco churn dataset',
       y="percentage of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

churn.contract.count.barpot <- ggplot(as.data.frame(churn.contract), aes(x = Contract, y=customers, 
                                                                 fill = Churn)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Customer churn by type of contract count - Bar plot',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(churn.contract.percentage.barpot, churn.contract.count.barpot, 
             nrow = 2)


# churn by gender

churn.gender = dataset %>%
  group_by(gender) %>% 
  count(Churn) %>% 
  mutate(percentage = n/nrow(dataset) * 100) %>% 
  rename(customers = n)

churn.gender.percentage.barpot <- ggplot(as.data.frame(churn.gender), aes(x = gender, y=percentage, 
                                                                              fill = Churn)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Customer churn by gender in percentage - Bar plot',
       subtitle = 'Telco churn dataset',
       y="percentage of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

churn.gender.count.barpot <- ggplot(as.data.frame(churn.gender), aes(x = gender, y=customers, 
                                                                         fill = Churn)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Customer churn by gender count - Bar plot',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(churn.gender.percentage.barpot, churn.gender.count.barpot, 
             nrow = 2)


# churn

# 1. Create initial scatterplot
p1 <- ggplot(dataset, aes(x = tenure, fill = Churn)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Tenure',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p2 <- ggplot(dataset, aes(x = MonthlyCharges, fill = Churn)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Monthly Charges',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p3 <- ggplot(dataset, aes(x = TotalCharges, fill = Churn)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Total Charges',
       subtitle = 'Telco churn dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

final.plot <- grid.arrange(p1, p2, p3, nrow = 1)

#---------------------
# end of visualization
#---------------------

# correlation analysis

# recapitulation of training set with separation between predictors and outcomes
rec_obj <- training %>%
  recipe(Churn ~ .) %>%
  step_rm(customerID) %>%
  step_naomit(all_outcomes(), all_predictors()) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

summary(rec_obj)
print(summary(rec_obj), n = 36)

# design matrix of predictors and vector of outcomes as numeric
x_train_tbl <- juice(rec_obj, all_predictors(), composition = "matrix") 
y_train_vec <- juice(rec_obj, all_outcomes()) %>% pull()

# analysis of correlations
corrr_analysis <- x_train_tbl %>%
  as_tibble() %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) 

# positive and negative correlated predictors
pos <- corrr_analysis %>%
  filter(Churn > 0)

neg <- corrr_analysis %>%
  filter(Churn < 0)

# plot
ggplot(corrr_analysis, aes(x = Churn, y = fct_reorder(term, desc(Churn)))) +
  geom_point() +
  geom_segment(aes(xend = 0, yend = term), data = under, color = 'darkred') +
  geom_point(data = neg, color = 'darkred') +
  geom_segment(aes(xend = 0, yend = term), data = over, color = "darkblue") +
  geom_point(data = pos, color = "darkblue") +
  labs(title = 'Churn correlations',
       subtitle = 'Telco dataset',
       y="features", x="correlation") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#---------------
# Churn modeling 
#---------------

# split dataset into training and testing sets
set.seed(2023)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
training <- dataset[ind==1,]
testing <- dataset[ind==2,]

#-------------------------------------------------
# Binary logistic regression modeling - full model
#-------------------------------------------------

model.1.lr <- glm(formula = ChurnDummy  ~ gender + SeniorCitizen + Partner + Dependents + 
                                          tenure + PhoneService +  MultipleLines +
                                          InternetService +OnlineSecurity + OnlineBackup + 
                                          DeviceProtection + TechSupport + StreamingTV +
                                          StreamingMovies + Contract + PaperlessBilling + 
                                          PaymentMethod + MonthlyCharges + TotalCharges,               
                  data = training, family = "binomial")

summary <- summary(model.1.lr)

# export the results in LaTex document
print(xtable(summary$coefficients, type = "latex"), file = "Customer_churn_analysis_tables.tex")

# Confidence intervals
exp(confint(model.1.lr))

# plot coefficients odds ratio 
plot_model(model.1.lr, vline.color = "red",
           sort.est = TRUE, show.values = TRUE) +
  labs(title = 'Odds ratios of churn - Binary Logistic regression',
       subtitle = 'Telco dataset',
       y="features", x="correlation") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90")) 

# plot the predictions and confusion matrix
pred_lr<- model.1.lr %>% predict(testing, type = "response")
predictions <- ifelse(pred_lr > 0.5, 1, 0)

# 2. Create a Confusion Matrix
tab <- table(predictions, testing$ChurnDummy)

result_lr <- caret::confusionMatrix(tab)
round(result_lr$byClass,3)

# export the results in LaTeX document
print(xtable(result_lr$table, type = "latex"), file = "Customer_churn_analysis_tables.tex")

# New model using stepwise selection of relevant covariates base on minimization of BIC
model.2.lr <- stepAIC(model.1.lr, direction = 'both', 
                               k = log(dim(training)[1]))

summary2 <- summary(model.2.lr)

# export the results in LaTex document
print(xtable(summary2$coefficients, type = "latex"), file = "Customer_churn_analysis_tables.tex")

# Confidence intervals
exp(confint(model.2.lr))

# plot coefficients odds ratio 
plot_model(model.2.lr, vline.color = "red",
           sort.est = TRUE, show.values = TRUE) +
  labs(title = 'Odds ratios of churn - Binary Logistic regression (second model)',
       subtitle = 'Telco dataset',
       y="features", x="correlation") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90")) 

# plot the predictions and confusion matrix
pred_lr2<- model.2.lr %>% predict(testing, type = "response")
predictions2 <- ifelse(pred_lr2 > 0.5, 1, 0)

# 2. Create a Confusion Matrix
tab2 <- table(predictions2, testing$ChurnDummy)

result_lr2 <- caret::confusionMatrix(tab2)
round(result_lr2$byClass,3)

# export the results in LaTeX document
print(xtable(result_lr2$table, type = "latex"), file = "Customer_churn_analysis_tables.tex")

#---------------------------
# Random forest - full model
#---------------------------

library(randomForest)

# 1. Build a Random Forest learning tree Classifier
set.seed(2023)

model.1.rf <- randomForest(formula = ChurnDummy  ~ gender + SeniorCitizen + Partner + Dependents + 
                             tenure + PhoneService +  MultipleLines +
                             InternetService +OnlineSecurity + OnlineBackup + 
                             DeviceProtection + TechSupport + StreamingTV +
                             StreamingMovies + Contract + PaperlessBilling + 
                             PaymentMethod + MonthlyCharges + TotalCharges
                           , data=training, ntree=100,proximity=TRUE)

table(predict(model.1.rf),training$ChurnDummy)
pred_rf <- predict(model.1.rf, testing)

# 2. Create a Confusion Matrix
tab <- table(predict(model.1.rf),training$ChurnDummy)
result_rf <- caret::confusionMatrix(tab)

# export the results in LaTeX document
print(xtable(result_rf$table, type = "latex"), file = "Customer_churn_analysis_tables.tex")

result_rf <- caret::confusionMatrix(pred_rf, testing$ChurnDummy)
sensitivity_rf <- round(result_rf$byClass[1], 4)
specificity_rf <- round(result_rf$byClass[2], 4)
accuracy_rf <- round(result_rf$overall[1], 4)

# 3. Plot the Confusion Matrix

testing$pred_rf <- pred_rf
ggplot(testing, aes(ChurnDummy, pred_rf, color = ChurnDummy)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Random Forest',
       subtitle = paste('Predicted vs. Observed from Telco dataset. Accuracy: ', accuracy_rf),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----