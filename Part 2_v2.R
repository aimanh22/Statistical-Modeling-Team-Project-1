###### Clear environment and load libraries
rm(list = ls())
library(ggplot2)
library(rms)
library(MASS)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(dplyr)
library(tidyr)
require(gridExtra)

###### Load the data
lalondedata <-
  read.table(
    "lalondedata.txt",
    header = TRUE,
    sep = ",",
    colClasses = c(
      "factor",
      "factor",
      "numeric",
      "numeric",
      "factor",
      "factor",
      "factor",
      "factor",
      "numeric",
      "numeric",
      "numeric"
    )
  )

lalondedata$earn <- ifelse(lalondedata$re78 > 0, 1, 0)

lalondedata$earnf <-
  factor(
    ifelse(lalondedata$re78 > 0, 1, 0),
    levels = c(0, 1),
    labels = c("Zero", "Positive")
  )

# lalondedata$treat <-
#   factor(
#     lalondedata$treat,
#     levels = c(0, 1),
#     labels = c("No job training", "Received job training")
#   )
# lalondedata$black <-
#   factor(
#     lalondedata$black,
#     levels = c(0, 1),
#     labels = c("Otherwise", "Black")
#   )
# lalondedata$hispan <-
#   factor(
#     lalondedata$hispan,
#     levels = c(0, 1),
#     labels = c("Otherwise", "Hispanic")
#   )
# lalondedata$married <-
#   factor(
#     lalondedata$married,
#     levels = c(0, 1),
#     labels = c("Otherwise", "Married")
#   )
# lalondedata$nodegree <-
#   factor(
#     lalondedata$nodegree,
#     levels = c(0, 1),
#     labels = c("Otherwise", "No high school degree")
#   )

###### Exploratory data analysis
# earn vs age
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none")

# earn vs age by treat
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age by Treat",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ treat)

# earn vs age by black
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age by Black Race",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ black)

# earn vs age by hispan
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age by Hispanic Ethinicity",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ hispan)

# earn vs age by married
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age by Marital Status",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ married)

# earn vs age by nodegree
ggplot(lalondedata, aes(x = earnf, y = age, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Age by High school degree",
       x = "Had salaries or no?", y = "Age") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ nodegree)

# earn vs educ
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none")

# earn vs educ by treat
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education by Treat",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ treat)

# earn vs educ by black
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education by Black Race",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ black)

# earn vs educ by hispan
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education by Hispanic Ethinicity",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ hispan)

# earn vs educ by married
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education by Marital Status",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ married)

# earn vs educ by nodegree
ggplot(lalondedata, aes(x = earnf, y = educ, fill = earnf)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Had salaries or not vs Education by High school degree",
       x = "Had salaries or no?", y = "Education") +
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~ nodegree)

# earn vs treat
t1 <-
  round(apply(table(lalondedata[, c("earnf", "treat")]) / sum(table(lalondedata[, c("earnf", "treat")])),
              2, function(x)
                x / sum(x)), 4)
t2 <-
  round(apply(table(lalondedata[, c("earnf", "black")]) / sum(table(lalondedata[, c("earnf", "black")])),
              2, function(x)
                x / sum(x)), 4)
t3 <-
  round(apply(table(lalondedata[, c("earnf", "hispan")]) / sum(table(lalondedata[, c("earnf", "hispan")])),
              2, function(x)
                x / sum(x)), 4)
t4 <-
  round(apply(table(lalondedata[, c("earnf", "married")]) / sum(table(lalondedata[, c("earnf", "married")])),
              2, function(x)
                x / sum(x)), 4)
t5 <-
  round(apply(table(lalondedata[, c("earnf", "nodegree")]) / sum(table(lalondedata[, c("earnf", "nodegree")])),
              2, function(x)
                x / sum(x)), 4)

chitest1 <- chisq.test(table(lalondedata[, c("earnf", "treat")]))
chitest2 <- chisq.test(table(lalondedata[, c("earnf", "black")]))
chitest3 <- chisq.test(table(lalondedata[, c("earnf", "hispan")]))
chitest4 <- chisq.test(table(lalondedata[, c("earnf", "married")]))
chitest5 <- chisq.test(table(lalondedata[, c("earnf", "nodegree")]))

#binned plots
par(mfcol = c(1, 1))
binnedplot(
  x = lalondedata$age,
  y = lalondedata$earn,
  xlab = "Age",
  ylim = c(0, 1),
  col.pts = "navy",
  ylab = "Had salaries or not ",
  main = "Binned Plot for Had salaries or not  w.r.t \nAge",
  col.int = "white"
)

binnedplot(
  x = lalondedata$educ,
  y = lalondedata$earn,
  xlab = "Education",
  ylim = c(0, 1),
  col.pts = "navy",
  ylab = "Had salaries or not ",
  main = "Binned Plot for Had salaries or not  w.r.t \nEducation",
  col.int = "white"
)

###### Model fitting
lalondedata$agec <- lalondedata$age - mean(lalondedata$age)
lalondedata$agec2 <- lalondedata$agec ^ 2
lalondedata$educc <- lalondedata$educ - mean(lalondedata$educ)

ModelNull <-
  glm(earn ~ treat + black + agec + agec2,
      data = lalondedata,
      family = binomial)
summary(ModelNull)

ModelFull <-
  glm(
    earn ~ (agec + educc + treat + black + hispan + married + nodegree) ^ 2 + agec2,
    data = lalondedata,
    family = binomial
  )
summary(ModelFull)

Model_stepwise_aic <- step(ModelNull,
                           scope = ModelFull,
                           direction = "both",
                           trace = 0)
summary(Model_stepwise_aic)

Model_forward_aic <- step(ModelNull,
                          scope = ModelFull,
                          direction = "forward",
                          trace = 0)
summary(Model_forward_aic)

Model_backward_aic <- step(ModelNull,
                           scope = ModelFull,
                           direction = "backward",
                           trace = 0)
summary(Model_backward_aic)

Model1 <-
  glm(earn ~ treat + black + agec + agec2 + agec:treat,
      data = lalondedata,
      family = binomial)
summary(Model1)
anova(ModelNull, Model1, test = "Chisq")

Model2 <-
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat + educ + educ:black,
    data = lalondedata,
    family = binomial
  )
summary(Model2)
anova(Model1, Model2, test = "Chisq")

Model3 <-
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat + hispan + hispan:educ,
    data = lalondedata,
    family = binomial
  )
summary(Model3)
anova(Model1, Model3, test = "Chisq")

Model4 <-
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat +  married + married:educc,
    data = lalondedata,
    family = binomial
  )
summary(Model4)
anova(Model1, Model4, test = "Chisq")

Model5 <-
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat +  nodegree + nodegree:agec,
    data = lalondedata,
    family = binomial
  )
summary(Model5)
anova(Model1, Model5, test = "Chisq")

Model6 <-
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat +  nodegree + nodegree:agec + nodegree:educ,
    data = lalondedata,
    family = binomial
  )
summary(Model6)
anova(Model1, Model6, test = "Chisq")

Model7 <-
  glm(
    earn ~ earn ~ treat + black + agec + agec2 + agec:treat +  nodegree + nodegree:agec +
      treat:black + treat:hispan + treat:married + treat:nodegree +
      black:hispan + black:married + black:nodegree +
      hispan:married + hispan:nodegree +
      nodegree:married + hispan + married,
    data = lalondedata,
    family = binomial
  )
summary(Model7)
anova(Model6, Model7, test = "Chisq")

Model8 <- step(Model5,
               scope = Model7,
               direction = "both",
               trace = 0)
summary(Model8)

Model9 <- step(Model5,
               scope = Model7,
               direction = "forward",
               trace = 0)
summary(Model9)

Model10 <- step(Model5,
                scope = Model7,
                direction = "backward",
                trace = 0)
summary(Model10)

Model11 <- 
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat +  nodegree + nodegree:agec +
      hispan + married + hispan:married,
    data = lalondedata,
    family = binomial
  )
summary(Model11)
anova(Model5, Model11, test = "Chisq")

Model12 <- 
  glm(
    earn ~ treat + black + agec + agec2 + agec:treat +
      hispan + married + hispan:married,
    data = lalondedata,
    family = binomial
  )
summary(Model12)
anova(Model12, Model11, test = "Chisq")

FinalModel <- Model11
summary(FinalModel)

###### Model fitting
rawresid <- residuals(FinalModel, "resp")

#binned residual plots
par(mfrow = c(1, 1))
binnedplot(
  x = fitted(FinalModel),
  y = rawresid,
  xlab = "Pred. probabilities",
  col.int = "red4",
  ylab = "Avg. residuals",
  main = "Binned residual plot",
  col.pts = "navy"
)

binnedplot(
  x = lalondedata$agec,
  y = rawresid,
  xlab = "Age (Centered)",
  col.int = "red4",
  ylab = "Avg. residuals",
  main = "Binned residual plot",
  col.pts = "navy"
)

binnedplot(
  x = lalondedata$educc,
  y = rawresid,
  xlab = "Education (Centered)",
  col.int = "red4",
  ylab = "Avg. residuals",
  main = "Binned residual plot",
  col.pts = "navy"
)


######## Model Validation
Conf_mat <-
  confusionMatrix(as.factor(ifelse(
    fitted(FinalModel) >= mean(lalondedata$earn), "1", "0"
  )),
  as.factor(lalondedata$earn), positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity", "Specificity")]

roc(
  lalondedata$earn,
  fitted(FinalModel),
  plot = T,
  print.thres = "best",
  legacy.axes = T,
  print.auc = T,
  col = "red3",
  quiet = TRUE
)

###### Confidence Interval
confint(FinalModel)
