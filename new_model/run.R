library(simmer)

set.seed(123456)

# Set Directory #

model_dir = "C:/Users/christopher.e.marks/My Documents/new_model"

# Import Model #

source(paste(model_dir,"migrant_model.R",sep="/"))

# Data input #

## Set Input files ##

migrant.datafile <- paste(model_dir,"migrant_inputs.xlsx",sep="/")
areas.datafile <- paste(model_dir,"ship_info.xlsx",sep="/")
processing.datafile <- paste(model_dir,"processing_inputs.xlsx",sep="/")

## Source Data File ##

source(paste(model_dir,"migrant_data_input.R",sep="/"))

# Build model with data (produces "env",total.days) #

## Set Build Parameters ##

total.days <- 365*2
repat.at.sea.prob <- 0.01

## Source Build Script ##

source(paste(model_dir,"migrant_model_build.R",sep="/"))


# Run and analyze #

env %>% run(
  until <- total.days*24
)



