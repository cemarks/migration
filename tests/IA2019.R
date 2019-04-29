# library(migration)
library(simmer)
library(readxl)
library(ggplot2)
library(reshape)


# migrant.input.file <- system.file(
#   "extdata",
#   "migrant_arrivals_inputs.xlsx",
#   package="migration",
#   mustWork=TRUE
# )
# ship.input.file <- system.file(
#   "extdata",
#   "ship_transit_inputs.xlsx",
#   package="migration",
#   mustWork=TRUE
# )
# camp.input.file <- system.file(
#   "extdata",
#   "camp_processing_inputs.xlsx",
#   package="migration",
#   mustWork=TRUE
# )

############ REPLACEMENT FOR ABOVE FOR DEBUGGING #####

source.dir <- "/home/cemarks/Projects/migration/R"
source.files <- dir(source.dir)
for(f in source.files){
  if(substr(f,nchar(f)-1,nchar(f)) == ".R"){
    source(
      paste(
        source.dir,
        f,
        sep="/"
      )
    )
  }
}

migrant.input.file <- "/home/cemarks/Projects/migration/inst/extdata/migrant_arrival_inputs.xlsx"
ship.input.file <- "/home/cemarks/Projects/migration/inst/extdata/ship_transit_inputs.xlsx"
camp.input.file <- "/home/cemarks/Projects/migration/inst/extdata/camp_processing_inputs.xlsx"
initial.conditions.datafile <- "/home/cemarks/Projects/migration/inst/extdata/initialization.xlsx"

######################################################

migrant.inputs <- migrant_inputs(migrant.input.file)
ship.inputs <- ship_area_inputs(
  ship.input.file,
  migrant.inputs$migrant.sources
)
processing.inputs <- processing_center_inputs(
  camp.input.file,
  migrant.inputs$nationality.probs
)


camp.trajectory <- trajectory() %>%
  seize("nsgb.counter") %>%
  health_branch(
    migrant.inputs$health.probs,
    processing.inputs$health.recovery
  ) %>%
  ferry_to_leeward(
    processing.inputs$proc.params
  ) %>%
  ICE_screening(
    migrant.inputs$family.status.probs,
    processing.inputs$inprocessing.schedule
  ) %>%
  ferry_to_windward(
    processing.inputs$proc.params
  ) %>%
  security_risk_branch(
    processing.inputs$sec.repat
  ) %>%
  timeout_to_boat_op(
    processing.inputs$proc.params
  ) %>%
  ferry_to_leeward(
    processing.inputs$proc.params
  ) %>%
  CIS_screening(
    migrant.inputs$family.status.probs,
    processing.inputs$cis.schedule,
    processing.inputs$cis.rescreening
  ) %>%
  ferry_to_windward(
    processing.inputs$proc.params
  ) %>%
  repat(
    migrant.inputs$nationality.probs,
    migrant.inputs$family.status.probs
  )

migrant_processing <- function(
  .trj,
  camp.subtrajectory
){
  o <- join(
    .trj,
    camp.subtrajectory
  )
  return(o)
}

ship.trajectories <- boat_trajectory_list(
  ship.inputs$pickup.areas,
  ship.inputs$ship.attributes,
  ship.inputs$ship.allocation
)

ferry.trajectory <- ferry_trajectory(
  processing.inputs$proc.params
)

# initial.conditions.datafile <- system.file(
#   "extdata",
#   "initialization.xlsx",
#   package="migration",
#   mustWork=TRUE
# )

initial.camp <- readxl::read_excel(
  initial.conditions.datafile,
  sheet = "camp"
)
initial.inbound <- readxl::read_excel(
  initial.conditions.datafile,
  sheet = "afloat"
)
initial.afloat <- readxl::read_excel(
  initial.conditions.datafile,
  sheet = "afloat_remaining"
)

camp.initialization <- list()
cnt <- 1
for(i in 1:nrow(initial.camp)){
  if(initial.camp$count[i] > 0){
    camp.initialization[[cnt]] <- list(
      trajectory = trajectory() %>%
        set_all_attributes(
          migrant.inputs$migrant.sources,
          migrant.inputs$source.rates,
          migrant.inputs$family.status.probs,
          migrant.inputs$health.probs,
          migrant.inputs$nationality.probs,
          migrant.inputs$protected.probs,
          migrant.inputs$security.risk.probs,
          nat = initial.camp$nationality[i],
          prot = initial.camp$protected[i],
          fs = initial.camp$family.status[i]
        ) %>%
        seize("nsgb.counter") %>%
        repat(
          migrant.inputs$nationality.probs,
          migrant.inputs$family.status.probs
        ),
      count = initial.camp$count[i]
    )
    cnt <- cnt + 1
  }
}

proc.dropoff.initialization <- list()
w <- grep("day",names(initial.inbound),ignore.case=TRUE)
cnt <- 1
for(i in 1:nrow(initial.inbound)){
  for(j in w){
    if(initial.inbound[i,j] > 0){
      proc.dropoff.initialization[[cnt]] <- list(
        trajectory = trajectory() %>%
          set_all_attributes(
            migrant.inputs$migrant.sources,
            migrant.inputs$source.rates,
            migrant.inputs$family.status.probs,
            migrant.inputs$health.probs,
            migrant.inputs$nationality.probs,
            migrant.inputs$protected.probs,
            migrant.inputs$security.risk.probs,
            nat = initial.inbound$nationality[i],
            fs = initial.inbound$family.status[i]
          ) %>%
          migrant_processing(camp.trajectory),
        count = initial.inbound[i,j],
        day = as.numeric(
          strsplit(
            names(initial.inbound)[j],
            ".",
            fixed=TRUE
          )[[1]][2]
        )
      )
      cnt <- cnt + 1
    }
  }
}

afloat.initialization <- list()
cnt <- 1
for(i in 1:nrow(initial.afloat)){
  if(initial.afloat$count[i] > 0){
    afloat.initialization[[cnt]] <- list(
      trajectory = trajectory() %>%
        set_all_attributes(
          migrant.inputs$migrant.sources,
          migrant.inputs$source.rates,
          migrant.inputs$family.status.probs,
          migrant.inputs$health.probs,
          migrant.inputs$nationality.probs,
          migrant.inputs$protected.probs,
          migrant.inputs$security.risk.probs,
          nat = initial.afloat$nationality[i]
        ) %>%
        set_pickup_area(
          migrant.inputs$migrant.sources,
          ship.inputs$pickup.areas
        ) %>%
        pickup_area(
          ship.inputs$pickup.areas,
          ship.inputs$ship.attributes
        ) %>%
        migrant_processing(
          camp.trajectory
        ),
      count = initial.afloat$count[i]
    )
    cnt <- cnt + 1
  }
}

env <- simmer()

for(i in 1:nrow(migrant.inputs$migrant.sources)){
  env <- add_generator(
    env,
    name_prefix = paste(
      "Migrant",
      migrant.inputs$migrant.sources$Source[i],
      sep="_"
    ),
    trajectory = trajectory() %>%
      source_trajectory(
        migrant.inputs$migrant.sources,
        migrant.inputs$source.rates,
        migrant.inputs$family.status.probs,
        migrant.inputs$health.probs,
        migrant.inputs$nationality.probs,
        migrant.inputs$protected.probs,
        migrant.inputs$security.risk.probs,
        ship.inputs$pickup.areas,
        src = migrant.inputs$migrant.sources$Source[i]
      ) %>%
      pickup_area(
        ship.inputs$pickup.areas,
        ship.inputs$ship.attributes
      ) %>%
      migrant_processing(camp.trajectory),
    distribution = migrant_function_generator(
      migrant.inputs$migrant.sources$Source[i],
      migrant.inputs$source.rates
    )
  )
}

for(i in 1:length(ship.trajectories)){
  env <- add_generator(
    env,
    name_prefix = paste(
      ship.trajectories[[i]][['boat.type']],
      "Area",
      i,
      sep="_"
    ),
    trajectory = ship.trajectories[[i]][['trajectory']],
    distribution = at(
      boat_first_arrivals(
        ship.trajectories[[i]][['boat.count']]
      )
    )
  )
}

env <- add_generator(
  env,
  name_prefix = "ferry",
  trajectory = ferry.trajectory,
  distribution = at(
    ferry_start_times(
      processing.inputs$proc.params
    )
  )
)

for(i in 1:length(camp.initialization)){
  env <- add_generator(
    env,
    name_prefix = paste(
      "Initial_Camp",
      i,
      sep="_"
    ),
    trajectory = camp.initialization[[i]]$trajectory,
    distribution = at(
      rep(
        0,
        camp.initialization[[i]]$count
      )
    )
  )
}


for(i in 1:length(proc.dropoff.initialization)){
  env <- add_generator(
    env,
    name_prefix = paste(
      "Initial_Dropoff",
      i,
      sep="_"
    ),
    trajectory = proc.dropoff.initialization[[i]]$trajectory,
    distribution = at(
      rep(
        proc.dropoff.initialization[[i]]$day*24+stats::runif(1,8,12),
        proc.dropoff.initialization[[i]]$count
      )
    )
  )
}

for(i in 1:length(afloat.initialization)){
  env <- add_generator(
    env,
    name_prefix = paste(
      "Initial_Afloat",
      i,
      sep="_"
    ),
    trajectory = afloat.initialization[[i]]$trajectory,
    distribution = at(
      rep(
        0,
        afloat.initialization[[i]]$count
      )
    )
  )
}


total.days <- 45

env <- add_all_resources_globals(
  env,
  total.days,
  migrant.inputs$nationality.probs,
  migrant.inputs$family.status.probs,
  ship.inputs$pickup.areas,
  processing.inputs$proc.params,
  processing.inputs$inprocessing.schedule,
  processing.inputs$cis.schedule,
  processing.inputs$move.outs
)


# Run the Simulation

env <- run(
  env,
  until = total.days*24
)


## Analysis

create_steps <- function(df,x.col,y.col,group.col){
  if(is.numeric(x.col)){
    x.int <- x.col
  } else {
    x.int <- which(names(df)==x.col)
  }
  if(is.numeric(y.col)){
    y.int <- y.col
  } else {
    y.int <- which(names(df)==y.col)
  }
  if(is.numeric(group.col)){
    group.int <- group.col
  } else {
    group.int <- which(names(df)==group.col)
  }
  x <- NULL
  y <- NULL
  g <- NULL
  for(u in unique(df[,group.int])){
    w <- which(df[,group.int]==u)
    d <- df[w,]
    d <- d[order(d[,x.int]),]
    vec.x <- d[,x.int]
    vec.y <- d[,y.int]
    x <- c(
      x,
      rbind(
        vec.x[1:(length(vec.x)-1)],
        vec.x[1:(length(vec.x)-1)]
      ),
      vec.x[length(vec.x)]
    )
    y <- c(
      y,
      vec.y[1],
      rbind(
        vec.y[2:(length(vec.y))],
        vec.y[2:(length(vec.y))]
      )
    )
    g <- c(
      g,
      rep(u,length(vec.x)*2-1)
    )
  }
  df.out <- data.frame(
    x,
    y,
    g,
    stringsAsFactors=FALSE
  )
  names(df.out) <- names(df)[c(x.int,y.int,group.int)]
  return(df.out)
}

resources <- get_mon_resources(env)
resources$time <- resources$time/24
resources <- resources[order(resources$time),]
w <- which(resources$resource %in% c("nsgb.counter","afloat.counter"))
df <- create_steps(
  resources[w,],
  "time",
  "server",
  "resource"
)

g <- ggplot(
  data = df,
  mapping = aes(
    x = time,
    y = server,
    color=resource,
    group=resource
  )
) + 
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Migrants Afloat",
    title = "Migrant Locations Over Time"
  ) +
  scale_color_discrete(
    breaks = c(
      "afloat.counter",
      "nsgb.counter"
    ),
    labels = c(
      "Afloat",
      "GTMO"
    )
  ) +
  scale_x_continuous(
    labels = function(x) return(x+15)
  ) +
  theme(
    legend.text = element_text(size=15),
    legend.title = element_text(size=16),
    axis.text = element_text(size = 15),
    title = element_text(size = 22)
  )

plot(g)
