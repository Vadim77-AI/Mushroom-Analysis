#############################################
# Author: Soldatov Vadim
# Mushroom Analysis
#############################################
# Loading libraries
#############################################

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(dplyr)
library(vcd)
library(psych) 
library(gridExtra)
library(corrplot)
library(MASS)

#############################################
# Mushroom dataset
#############################################

mush <- readr::read_csv("https://raw.githubusercontent.com/Vadim77-AI/Mushroom-Analysis/refs/heads/main/mushrooms.csv")
head(mush)

#############################################
# Data Exploration
#############################################

class(mush)
str(mush)
head(mush)
glimpse(mush)
dim(mush)
summary(mush)

# plot on how many edible  and poisonous
ggplot(mush, aes(x = class, fill = class)) +
  theme_bw()+
  geom_bar()+
  labs(x = "edible  and poisonous",
       y = "count",
       title = "rates of edible  and poisonous")

# Create a bar graph of Mushroom Cap color Attribute
ggplot(mush, aes(x = `cap-color`, fill = `cap-color`)) +
  theme_bw()+
  geom_bar()+
  labs(x = "Cap color",
       y = "count",
       title = "Mushroom Cap color Attribute")

mush1 <- ggplot(aes(x = `cap-shape`, fill = `cap-shape`), data = mush) +
  geom_bar(stat="count") +
  facet_wrap(~class) +
  xlab("Cap Shape")

mush2 <- ggplot(aes(x = `cap-surface`, fill = `cap-surface`), data = mush) +
  geom_bar(stat="count") +
  facet_wrap(~class) +
  xlab("Cap Surface")

mush3 <- ggplot(aes(x = `cap-color`, fill = `cap-color`), data = mush) +
  geom_bar(stat="count") +
  facet_wrap(~class) +
  xlab("Cap Color")

grid.arrange(mush1, mush2, mush3, ncol = 2)

mush4 <- ggplot(aes(x = bruises, fill = bruises), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Bruises")

mush5 <- ggplot(aes(x = odor, fill = odor), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Odor")

grid.arrange(mush4, mush5, ncol = 2)

mush6 <- ggplot(aes(x = `gill-attachment`, fill = `gill-attachment`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Gill Attachemnt")

mush7 <- ggplot(aes(x = `gill-spacing`, fill = `gill-spacing`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Gill Spacing")

mush8 <- ggplot(aes(x = `gill-size`, fill = `gill-size`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Gill Size")

grid.arrange(mush6, mush7, mush8, ncol = 2)

mush9 <- ggplot(aes(x = `stalk-shape`, fill = `stalk-shape`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Stalk  Shape")

mush10 <- ggplot(aes(x = `stalk-root`, fill = `stalk-root`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Stalk Root")

grid.arrange(mush9, mush10, ncol = 2)

mush11 <- ggplot(aes(x = `stalk-surface-above-ring`, fill = `stalk-surface-above-ring`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Stalk Surface Above Ring")

mush12 <- ggplot(aes(x = `stalk-surface-below-ring`, fill = `stalk-surface-below-ring`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Stalk Surface Below Ring")

grid.arrange(mush11, mush12, ncol = 2)

mush13 <- ggplot(aes(x = `veil-type`, fill = `veil-type`), data = mush) +
  geom_bar(stat = "count") +
  xlab("Veil Type")

mush14 <- ggplot(aes(x = `veil-color`, fill = `veil-color`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Veil Color")

mush15 <- ggplot(aes(x = `ring-number`, fill = `ring-number`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Ring Number")

mush16 <- ggplot(aes(x = `ring-type`, fill = `ring-type`), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Ring Type")

grid.arrange(mush13, mush14, mush15, mush16, ncol = 2)

mush17 <- ggplot(aes(x = population, fill = population), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Population")

mush18 <- ggplot(aes(x = habitat, fill = habitat), data = mush) +
  geom_bar(stat = "count") +
  facet_wrap(~class) +
  xlab("Habitat")

grid.arrange(mush17, mush18, ncol = 2)

#############################################
# Finding relationship
#############################################

tbl1 <- table(mush$class, mush$`cap-shape`)
chisq.test(tbl1)

tbl2 <- table(mush$class, mush$`cap-surface`)
chisq.test(tbl2)

tbl3 <- table(mush$class, mush$`cap-color`)
chisq.test(tbl3)

tbl4 <- table(mush$class, mush$bruises)
chisq.test(tbl4)

tbl5 <- table(mush$class, mush$odor)
chisq.test(tbl5)

tbl6 <- table(mush$class, mush$`gill-attachment`)
chisq.test(tbl6)

tbl7 <- table(mush$class, mush$`gill-spacing`)
chisq.test(tbl7)

tbl8 <- table(mush$class, mush$`gill-size`)
chisq.test(tbl8)

tbl9 <- table(mush$class, mush$`stalk-shape`)
chisq.test(tbl19)

tbl10 <- table(mush$class, mush$`stalk-root`)
chisq.test(tbl10)

tbl11 <- table(mush$class, mush$`stalk-surface-above-ring`)
chisq.test(tbl11)

tbl12 <- table(mush$class, mush$`stalk-surface-below-ring`)
chisq.test(tbl12)

tbl13 <- table(mush$class, mush$`veil-type`)
chisq.test(tbl13)

tbl14 <- table(mush$class, mush$`veil-color`)
chisq.test(tbl14)

tbl15 <- table(mush$class, mush$`ring-number`)
chisq.test(tbl15)

tbl16 <- table(mush$class, mush$`ring-type`)
chisq.test(tbl16)

tbl17 <- table(mush$class, mush$population)
chisq.test(tbl17)

tbl18 <- table(mush$class, mush$habitat)
chisq.test(tbl18)

#############################################
# Multivariate analysis
#############################################

ggplot(mush, aes(bruises, `gill-size`, col = population, size = class)) +
  geom_point()

ggplot(mush, aes(`stalk-surface-above-ring`, `stalk-surface-below-ring`, col = population, size = class)) +
  geom_point()

#############################################
# Logistic regression
#############################################

mush_train <- mutate(mush, 
                     class = factor(class, labels = c("e", "p")),
                     `stalk-surface-above-ring` = factor(`stalk-surface-above-ring`, labels = c("f","y","k","s")),
                     `stalk-surface-below-ring` = factor(`stalk-surface-below-ring`, labels = c("f","y","k","s")),
                     `gill-size` = factor(`gill-size`, labels = c("b","n")),
                     bruises = factor(bruises, labels = c("t","f")))

# Let's build a mosaic graph
mosaic(~ class + `stalk-surface-above-ring`, data=mush_train)
mosaic(~ class + `stalk-surface-below-ring`, data=mush_train)
mosaic(~ class + `gill-size`, data=mush_train)
mosaic(~ class + bruises, data=mush_train)

# Intercept only model
mush_simple <- glm(class ~ 1, mush_train,  family = "binomial")
coef(mush_simple)
table(mush_train$class)

# A model with a single nominative predictor
mush_fit1 <- glm(class ~ `stalk-surface-above-ring`, mush_train, family = "binomial")
coef(mush_fit1)
summary(mush_fit1)
table(mush_train$class, mush_train$`stalk-surface-above-ring`)

# A model with two categorical predictors
mush_fit2 <- glm(class ~ `stalk-surface-above-ring` * `gill-size`, mush_train, family = "binomial")
coef(mush_fit2)
summary(mush_fit2)
table(mush_train$class, mush_train$`stalk-surface-above-ring`, mush_train$`gill-size`)

# model comparison
anova(mush_fit1, mush_fit2, test="Chisq")

# predicting new data
mush_fit3 <- glm(class ~ bruises, mush_train, family = "binomial")
coef(mush_fit2)
summary(mush_fit3)
new_mush <- data.frame(class = "e", bruises = "t")
predict(mush_fit3, newdata = new_mush)

# Check a model with different variables
mush_fit4 <- glm(class ~ `stalk-surface-above-ring` + `gill-size` + bruises, mush_train, family = "binomial")
summary(mush_fit4)

anova(mush_fit4, test="Chisq")
