### Week 13 B Notes 
## Created by: Claudia Rea
## Created on: 20210428

## Load Libraries
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom)
library(performance) # if you didn't already install from group projects install it
library(modelsummary)
library(tidymodels)
library(see)
library(qqplotr)

########### Model the penguin dataset #########################
# Linear model of Bill depth ~ Bill length by species
Peng_mod<-lm(bill_length_mm ~ bill_depth_mm*species, data = penguins)

#Check model assumptions with performace
check_model(Peng_mod) # check assumptions of an lm model

### View results: base R ###
#ANOVA Table
anova(Peng_mod)

#Coefficients (effect size) with error
summary(Peng_mod)

### View results with broom ###
# Tidy coefficients
coeffs<-tidy(Peng_mod) # just put tidy() around it
coeffs
#glance extracts R-squared, AICs, etc of the model
# tidy r2, etc
results<-glance(Peng_mod) 
results
#augment add residuals and predicted values to your original data and requires that you put both the model and data
# tidy residuals, etc
resid_fitted<-augment(Peng_mod)
resid_fitted

### Results in {modelsummary} ###
# New model
Peng_mod_noX<-lm(bill_length_mm ~ bill_depth_mm, data = penguins)
#Make a list of models and name them
models<-list("Model with interaction" = Peng_mod,
             "Model with no interaction" = Peng_mod_noX)
#Save the results as a .docx
modelsummary(models, output = here("Week_13","Output","table.docx"))

### Modelplot
#Canned coefficient modelplots
library(wesanderson)
modelplot(models) +
  labs(x = 'Coefficients', 
       y = 'Term names') +
  scale_color_manual(values = wes_palette('Darjeeling1'))
