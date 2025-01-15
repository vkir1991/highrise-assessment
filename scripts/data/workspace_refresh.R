# data refresh script

suppressMessages(suppressWarnings(source('../r-helper-functions/kirillov_lib.R')))

library(uuid)
library(mltools)
library(glmnet)
library(pROC)

options(useFancyQuotes = FALSE)

# source function scripts
source('scripts/functions/functions_util.R')
source('scripts/functions/functions_model.R')

# SQL
refreshSQL()
