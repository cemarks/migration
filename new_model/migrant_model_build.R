library(simmer)


# Build Trajectories #

processing.trajectory <- processing_trajectory()
area.trajectories <- lapply(
  1:nrow(pickup.areas),
  function(x)
    return(
      join(
        ca_migrant_trajectory(x),
        processing.trajectory
      )
    )
)
names(area.trajectories) <- pickup.areas$pickup.area
migrant.trajectories <- source_trajectories(area.trajectories)

boat.trajectories <- list()
count <- 0
for(i in 1:nrow(pickup.areas)){
  for(j in 1:nrow(ship.attributes)){
    boat.type <- ship.attributes[j,1]
    pickup.area <- pickup.areas[i,1]
    w <- which(ship.allocation[,1]==pickup.area)
    v <- ship.allocation[w,boat.type]
    if(v > 0){
      count <- count + 1
      boat.trajectories[[count]] <- list(
        trajectory = ca_boat_trajectory(i,j),
        boat.count = v,
        pickup.area = pickup.areas[i,1],
        boat.type = ship.attributes[j,1]
      )
    }
  }
}

ferry.trajectory <- ferry_trajectory(proc.params)

# Build environment

env <- simmer()


## Add generators ## 

# Add migrant generators
for(i in 1:length(migrant.sources$Source)){
  env <- add_generator(
    env,
    name_prefix = paste(migrant.sources$Source[i],"migrant",sep="-"),
    trajectory = migrant.trajectories[[migrant.sources$Source[i]]],
    distribution = migrant_function_generator(migrant.sources$Source[i])
    )
}

# Add boat generators
for(i in 1:length(boat.trajectories)){
  env <- add_generator(
    env,
    name_prefix = paste(
      boat.trajectories[[i]][["pickup.area"]],
      boat.trajectories[[i]][["boat.type"]],
      sep="_"
    ),
    trajectory = boat.trajectories[[i]][['trajectory']],
    distribution = at(
      boat_first_arrivals(
        boat.trajectories[[i]][["boat.count"]]
      )
    )
  )
}

# Add ferry generator
env <- add_generator(
  env,
  name_prefix = "ferry",
  trajectory = ferry.trajectory,
  distribution = at(ferry_start_times(proc.params))
)

## Add resources ##

# boats and areas
for(i in 1:nrow(pickup.areas)){
  env <- add_resource(
    env,
    name = paste("area",i,sep="_"),
    capacity = 1,
    queue_size=Inf
  ) %>%
    add_resource(
      name = paste("boat.area",i,sep="_"),
      capacity = 0,
      queue_size = Inf
    )
}

# berths

w <- which(proc.params$Parameter=='big.boat.berth.capacity')

env <- add_resource(
  env,
  name = "berth",
  capacity = proc.params$Value[w],
  queue_size = Inf
)

# transit capacities
env <- add_resource(
  env,
  name = "transit-to-leeward",
  capacity = 0,
  queue_size = Inf

) %>%
  add_resource(
    name="transit-to-windward",
    capacity = 0,
    queue_size = Inf
  )

# ICE.agent

env <- add_resource(
  env,
  name = "ICE.agent",
  capacity = create_schedule(inprocessing.schedule,total.days),
  queue_size = Inf
)

# CIS.screener

env <- add_resource(
  env,
  name = "CIS.screener",
  capacity = create_schedule(cis.schedule,total.days),
  queue_size = Inf
)

# Repatriation/resettlement servers

for(nat in nationality.probs[1:2,1]){
  for(protected in c("repat","resettle")){
    env <- add_resource(
      env,
      name = paste(protected,nat,sep="-"),
      capacity = repat_resettle_schedule(
        move.outs,
        nat,
        protected,
        total.days
      ),
      queue_size = Inf
    )
  }
}

## Add globals ##

# Control globals

for(i in 1:nrow(pickup.areas)){
  env <- add_global(
    env,
    key=paste("boat.type",i,sep="_"),
    value = 0
    ) %>%
    add_global(
      key=paste("boat.count",i,sep="_"),
      value = 0
    ) %>%
    add_global(
      key=paste("boat.occuppied",i,sep="_"),
      value = 0
    ) %>%
    add_global(
      key=paste("boat.saturated",i,sep="_"),
      value = 0
    ) %>%
    add_global(
      key=paste("boat.full",i,sep="_"),
      value = 0
    )
}

# Analysis globals
env <- add_global(
  env,
  key = "windward_transit_queue",
  value = 0
)

