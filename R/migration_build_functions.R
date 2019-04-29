## Model Build functions

## Migrant Arrivals

migrant_function_generator <- function(mig.source, source.rates){
  d <- source.rates[which(source.rates$source==mig.source),]
  d <- d[order(d$time),]
  d$time <- d$time
  return(
    function(){
      time.now <- simmer::now(env)
      w <- which(d$time <= time.now)
      if(length(w) == 0){
        p <- 1/100
      } else {
        p <- d$rate[max(w)]
      }
      if(p<=0){
        return(-1)
      } else {
        return(stats::rexp(1,p))
      }
    }
  )
}


## Boat first arrivals ##

boat_first_arrivals <- function(boat.count){
  return(sort(stats::runif(boat.count,1,24)))
}

## Ferry start times ##

ferry_start_times <- function(proc.params){
  ferry.count <- proc.params$Value[which(proc.params$Parameter == "ferry.count")]
  ferry.roundtrip <- 2*proc.params$Value[which(proc.params$Parameter == "ferry.transit.time")]
  ferry.service.hours <- proc.params$Value[which(proc.params$Parameter == "ferry.service.hours")]
  ferry.interval <- ferry.roundtrip/ferry.count
  ferry.start.times <- seq(0,(ferry.count-1)*ferry.interval,ferry.interval)
  ferry.start.times <- ferry.start.times + (24-ferry.service.hours)/2
  return(ferry.start.times)
}

## Schedule creators ##


create_schedule <- function(schedule.data.frame,total.days){
  n <- grep("count",names(schedule.data.frame))
  h <- grep("hours",names(schedule.data.frame))
  sdf <- schedule.data.frame[order(schedule.data.frame$time),]
  out.times <- sapply(
    0:total.days,
    function(x){
      w <- max(which(sdf$time <= x))
      if(length(w)==0){
        return(NULL)
      } else {
        hrs <- sdf[w,h]
        return(
          c(
            x*24+((24-hrs)/2),
            x*24+((24-hrs)/2)+hrs
          )
        )
      }
    }
  )
  times <- sort(c(out.times))
  v <- sapply(
    times[seq(1,(length(times)-1),2)],
    function(x){
      w <- max(which(sdf$time <= x))
      if(length(w)==0){
        return(0)
      } else {
        return(sdf[w,n])
      }
    }
  )
  values <- rep(0,length(times))
  values[seq(1,(length(times)-1),2)] <- v
  return(
    simmer::schedule(
      timetable = times,
      values = values
    )
  )
  return(
    simmer::schedule(
      timetable = times,
      values = servers
    )
  )
}

repat_resettle_schedule <- function(sched.data.frame,nationality,protected,total.days){
  if(!(protected %in% c("resettle","repat"))){
    stop("Invalid value in repatriation/resettlement scheduler.")
  }
  w <- which(
    sched.data.frame$nationality==nationality &
    sched.data.frame$resettle.repatriate==protected
  )
  sdf <- sched.data.frame[w,]
  sdf <- sdf[order(sdf$time),]
  times <- sort(c((0:total.days)*24,(0:total.days)*24+small.epsilon()))
  v <- sapply(
    times[seq(1,(length(times)-1),2)],
    function(x){
      w <- max(which(sdf$time <= x))
      if(length(w)==0){
        return(0)
      } else {
        s <- rtriang(
          sdf$mode[w],
          sdf$min[w],
          sdf$max[w]
        )
        return(round(s))
      }
    }
  )
  values <- rep(0,length(times))
  values[seq(1,(length(times)-1),2)] <- v
  return(
    simmer::schedule(
      timetable = times,
      values = values
    )
  )
}

### Need functions to generate globals, resources

add_pickup_area_resources <- function(.env,pickup.areas){
  o <- .env
  for(i in 1:nrow(pickup.areas)){
    o <- simmer::add_resource(
      o,
      name = paste("area",i,sep="_"),
      capacity = 1,
      queue_size=Inf
    ) %>%
      simmer::add_resource(
        name = paste("boat.area",i,sep="_"),
        capacity = 0,
        queue_size = Inf
      )
  }
  return(o)
}

add_berth_resources <- function(.env,proc.params){
  w <- which(proc.params$Parameter=='big.boat.berth.capacity')
  o <- simmer::add_resource(
    .env,
    name = "berth",
    capacity = proc.params$Value[w],
    queue_size = Inf
  )
  return(o)
}

add_ferry_transit_resources <- function(.env){
  o <- simmer::add_resource(
    .env,
    name = "transit-to-leeward",
    capacity = 0,
    queue_size = Inf

  ) %>%
    simmer::add_resource(
      name="transit-to-windward",
      capacity = 0,
      queue_size = Inf
    )
  return(o)
}

add_ICE_agents <- function(.env,inprocessing.schedule,total.days){
  o <- simmer::add_resource(
    .env,
    name = "ICE.agent",
    capacity = create_schedule(inprocessing.schedule,total.days),
    queue_size = Inf
  )
  return(o)
}

# CIS.screener

add_CIS_screeners <- function(.env,cis.schedule,total.days){
  o <- simmer::add_resource(
    .env,
    name = "CIS.screener",
    capacity = create_schedule(cis.schedule,total.days),
    queue_size = Inf
  )
  return(o)
}

#### COUNTER RESOURCES  #### added to provide migrant counts

add_counters <- function(.env){
  o <- simmer::add_resource(
    .env,
    name = "repat_afloat",
    capacity = Inf,
    queue_size = Inf
  ) %>% 
    simmer::add_resource(
      name = "afloat.counter",
      capacity = Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name = "nsgb.counter",
      capacity = Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name = "usa.counter",
      capacity = Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name = "security.counter",
      capacity = Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name="inprocessed.counter",
      capacity=Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name="wait.ferry.to.leeward",
      capacity=Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name="repat.security",
      capacity = Inf,
      queue_size = Inf
    ) %>% 
    simmer::add_resource(
      name = "wait.ferry.to.windward",
      capacity = Inf,
      queue_size = Inf
    )
  return(o)
}

add_repat_counters <- function(
  .env,
  nationality.probs,
  move.outs,
  total.days
){
  o <- .env
  for(nat in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      o <- simmer::add_resource(
        o,
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
  return(o)
}

add_family_status_counters <- function(
  .env,
  family.status.probs,
  nationality.probs
){
  o <- .env
  for(fs in family.status.probs[,1]){
    o <- simmer::add_resource(
      o,
      name = paste(
        "Inprocessing",
        fs,
        sep = "_"
      ),
      capacity = Inf,
      queue_size = Inf
    ) %>%
    simmer::add_resource(
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
        o <- simmer::add_resource(
          o,
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
  return(o)
}


## Add globals ##

add_pickup_area_globals <- function(.env,pickup.areas){
  o <- .env
  for(i in 1:nrow(pickup.areas)){
    o <- simmer::add_global(
      o,
      key=paste("boat.type",i,sep="_"),
      value = 0
      ) %>%
      simmer::add_global(
        key=paste("boat.count",i,sep="_"),
        value = 0
      ) %>%
      simmer::add_global(
        key=paste("boat.occuppied",i,sep="_"),
        value = 0
      ) %>%
      simmer::add_global(
        key=paste("boat.saturated",i,sep="_"),
        value = 0
      ) %>%
      simmer::add_global(
        key=paste("boat.full",i,sep="_"),
        value = 0
      )
  }
  return(o)
}

add_all_resources_globals <- function(
  .env,
  total.days,
  nationality.probs,
  family.status.probs,
  pickup.areas,
  proc.params,
  inprocessing.schedule,
  cis.schedule,
  move.outs
){
  o <- add_pickup_area_resources(
    .env,
    pickup.areas
  ) %>%
    add_berth_resources(
      proc.params
    ) %>%
    add_ferry_transit_resources() %>%
    add_ICE_agents(
      inprocessing.schedule,
      total.days
    ) %>%
    add_CIS_screeners(
      cis.schedule,
      total.days
    ) %>% 
    add_counters() %>%
    add_repat_counters(
      nationality.probs,
      move.outs,
      total.days
    ) %>%
    add_family_status_counters(
      family.status.probs,
      nationality.probs
    ) %>%
    add_pickup_area_globals(
      pickup.areas
    )
  return(o)
}


