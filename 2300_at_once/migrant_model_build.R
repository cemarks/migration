library(simmer)


# Build Trajectories #


ferry.trajectory <- ferry_trajectory(proc.params)
init.trajectories <- initialization_trajectories(initial.camp)
impulse.trajectories <- impulse_trajectories(initial.boat)

# Build environment

env <- simmer(verbose = verbose, log_level = log_level)


## Add generators ## 

# Add ferry generator
env <- add_generator(
  env,
  name_prefix = "ferry",
  trajectory = ferry.trajectory,
  distribution = at(ferry_start_times(proc.params))
)

# Add migrant generators
for(i in 1:length(init.trajectories)){
  if(initial.camp$number[i] > 0){
    n <- names(init.trajectories)[i]
    env <- add_generator(
      env,
      name_prefix = n,
      trajectory = init.trajectories[[i]],
      distribution = at(rep(0,initial.camp$number[i]))
    )
  }
}

for(i in 1:length(impulse.trajectories)){
  if(initial.boat$number[i] > 0){
    n <- names(impulse.trajectories)[i]
    env <- add_generator(
      env,
      name_prefix = n,
      trajectory = impulse.trajectories[[i]],
      distribution = at(rep(migrant.arrival.time,initial.boat$number[i]))
    )
  }
}



## Add resources ##

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


#### COUNTER RESOURCES  #### added to provide migrant counts

env <- add_resource(
  env,
  name = "nsgb.counter",
  capacity = Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name = "security.counter",
  capacity = Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name="inprocessed.counter",
  capacity=Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name="wait.ferry.to.leeward",
  capacity=Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name="repat.security",
  capacity = Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name = "wait.ferry.to.windward",
  capacity = Inf,
  queue_size = Inf
)

env <- add_resource(
  env,
  name = "leeward.counter",
  capacity = Inf,
  queue_size = Inf
)

# add capacity = 1 

# Repatriation/resettlement servers

for(nat in nationality.probs[,1]){
  for(protected in c("repat","resettle")){
    env <- add_resource(
      env,
      name = paste(protected,nat,sep="-"),
      capacity = Inf,
      queue_size = Inf
    )
  }
}

for(fs in family.status.probs[,1]){
  env <- add_resource(
    env,
    name = paste(
      "Inprocessing",
      fs,
      sep = "_"
    ),
    capacity = Inf,
    queue_size = Inf
  ) %>%
    add_resource(
      name = paste(
        "CIS",
        fs,
        sep = "_"
      ),
      capacity = Inf,
      queue_size = Inf
    )
  for(n in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      env <- add_resource(
        env,
        name = paste(
          paste(protected,n,sep="-"),
          fs,
          sep = "_"
        ),
        capacity = Inf,
        queue_size = Inf
      )
    }
  }
}
