library(simmer)
model_dir = "/home/cemarks/Projects/migration/new_model"
source(paste(model_dir,"migrant_model.R",sep="/"))

# Data input
# Input files #

migrant.datafile <- "/home/cemarks/Projects/migration/new_model/migrant_inputs.xlsx"
areas.datafile <- "/home/cemarks/Projects/migration/new_model/ship_info.xlsx"
processing.datafile <- "/home/cemarks/Projects/migration/new_model/processing_inputs.xlsx"
source(paste(model_dir,"migrant_data_input.R",sep="/"))

# Model build (produces "env",total.days)
source(paste(model_dir,"migrant_model_build.R",sep="/"))


# Run and analyze

env %>% run(
  until <- total.days*24
)

g <- get_mon_attributes(env)
w <- which(g$key=="windward_transit_queue")
plot(g$time[w],g$value[w],type = "l",xlab = "Hours",ylab="Queue Length",main="Windward Transit Queue")

