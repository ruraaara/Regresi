# Load packages
library(tidyverse)
library(MASS)      # untuk stepAIC
library(readr)

# Load data
fish_data <- read_csv("Fish.csv")

# Filter hanya ikan Pike
pike_data <- fish_data %>%
  filter(Species == "Pike") %>%
  select(-Species)  # drop kolom Species

# Model awal (intercept only)
model_null <- lm(Weight ~ 1, data = pike_data)

# Model penuh dengan semua prediktor
model_full <- lm(Weight ~ ., data = pike_data)

# Seleksi model dengan metode forward dan BIC
model_forward_bic <- stepAIC(model_null, 
                             scope = list(lower = model_null, upper = model_full), 
                             direction = "forward", 
                             k = log(nrow(pike_data)))  # BIC uses k = log(n)

# Ringkasan model terbaik
summary(model_forward_bic)
