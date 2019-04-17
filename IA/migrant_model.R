library(simmer)

# Helper Functions & Data Format Parameters#

## Migrant Attributes ##

migrant.attributes <- list(
  "family.status" = c(
    "M/Accompanied",
    "F/Accompanied",
    "Ch/Accompanied",
    "M/Unaccompanied",
    "F/Unaccompanied",
    "Ch/Unaccompanied"
  ),
  "health" = c(
    "Disease",
    "No Disease"
  ),
  "nationality" = c(
    "Haitian",
    "Cuban",
    "Other"
  ),
  "security.risk" = c(
    "Security Risk",
    "No Security Risk"
  ),
  "protected" = c(
    "Protected",
    "Not Protected"
  )
)


## Probability Distribution functions & parameters ##

epsilon <- function(){
  return(runif(1,0.05,0.2))
}

small.epsilon <- 0.001

triangle_quantile <- function(r,t.mode,t.min,t.max){
  ymax = 2/(t.max-t.min)
  s <- as.numeric(r > (t.mode-t.min)*ymax/2)
  x.upper <- t.max - sqrt(
    t.max^2 - (t.max*t.min-t.min*t.mode+t.max*t.mode+2*r*t.max/ymax-2*r*t.mode/ymax)
  )
  x.lower <- t.min + sqrt(
    t.min^2 + (2*r*(t.mode-t.min)/ymax - t.min^2)
  )
  return((1-s)*x.lower+s*x.upper)
}

rtriang <- function(t.mode,t.min,t.max){
  if((t.min > t.max) || (t.min > t.mode) || (t.mode > t.max)){
    stop("Triangle distribution parameters incorrect ordering")
  }
  if((t.min == t.mode) && (t.mode == t.max)){
    return(t.mode)
  } else {
    r <- runif(1)
    return(triangle_quantile(r,t.mode,t.min,t.max))
  }
}

## Migrant attribute generator functions ##

prob_generator_int <- function(prob.vector){
  # Renormalize
  prob.vector <- prob.vector/sum(prob.vector)
  cum.prob <- cumsum(prob.vector)
  r <- runif(1)
  w <- which(cum.prob > r)[1]
  return(w)
}
prob_generator <- function(input.table,mig.source){
  # Renormalize
  w <- prob_generator_int(input.table[,mig.source])
  return(
    list(
      key = input.table[w,1],
      value = w
    )
  )
}
security_risk_prob_generator <- function(family.status.value,mig.source){
  p <- security.risk.probs[family.status.value,mig.source]
  r <- runif(1)
  if(r <= p){
    return(1) # Security Risk
  } else {
    return(2)
  }
}

migrant_attributes <- function(mig.source){
  family.status <- prob_generator(family.status.probs,mig.source)$value
  migrant.health <- prob_generator(health.probs,mig.source)$value
  migrant.nationality <- prob_generator(nationality.probs,mig.source)$value
  security.risk <- security_risk_prob_generator(family.status,mig.source)
  if(security.risk == 2){
    migrant.protected <- prob_generator(protected.probs,mig.source)$value
  } else {
    migrant.protected <- 2
  }
  return(
    c(
      family.status = family.status,
      health = migrant.health,
      nationality = migrant.nationality,
      security.risk = security.risk,
      protected = migrant.protected
    )
  )
}


## Migrant Arrival Generator ##

migrant_function_generator <- function(mig.source){
  d <- source.rates[which(source.rates$source==mig.source),]
  d <- d[order(d$time),]
  return(
    function(){
      time.now <- now(env)
      w <- which(d$time <= time.now)
      if(length(w) == 0){
        p <- 1/100
      } else {
        p <- d$rate[max(w)]
      }
      if(p<=0){
        return(-1)
      } else {
        return(rexp(1,p))
      }
    }
  )
}

migrant_arrival_time <- function(day){
  return(day*24+runif(1,8,12))
}


## Boat first arrivals ##

boat_first_arrivals <- function(boat.count){
  return(sort(runif(boat.count,1,24)))
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
    schedule(
      timetable = times,
      values = values
    )
  )
  return(
    schedule(
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
  times <- sort(c((0:total.days)*24,(0:total.days)*24+small.epsilon))
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
    schedule(
      timetable = times,
      values = values
    )
  )
}

# Trajectory Generators #

## Source Trajectory Generator ##

check_area_trajectory_list <- function(atl){
  if(!all(names(atl) %in% as.character(pickup.areas[,1]))){
    stop("Pickup Area Trajectory Name Mismatch!")
  }
  s <- match(
    as.character(pickup.areas[,1]),
    names(atl)
  )
  return(atl[s])
}
source_trajectories <- function(area.trajectory.list){
  atl <- check_area_trajectory_list(area.trajectory.list)
  source.cols <- which(names(pickup.areas) %in% as.character(migrant.sources$Source))
  mig.sources <- names(pickup.areas)[source.cols]
  if(!all(as.character(migrant.sources$Source) %in% as.character(mig.sources))){
    warning("Pickup data does not contain columns for all sources.")
  }
  names(atl) <- NULL
  output <- list()
  pgen <- function(m){
    p <- pickup.areas[,m]
    f <- function(){
      route <- prob_generator_int(p)
      return(route)
    }
    return(f)
  }
  agen <- function(m){
    m.source <- m
    f <- function(){
      return(
        as.numeric(
          migrant_attributes(
            m.source
          )
        )
      )
    }
    return(f)
  }
  for(i in 1:length(mig.sources)){
    mig.source <- mig.sources[i]
    f <- pgen(mig.source)
    g <- agen(mig.source)
    output[[mig.source]] <- trajectory(name=mig.source) %>%
      set_attribute(
        keys = c(
          "family.status",
          "health",
          "nationality",
          "security.risk",
          "protected"
        ),
        values = g
      ) %>%
      branch(
        option = f,
        continue = FALSE,
        atl
      )
  }
  return(output)
}

## Pick-up area migrant trajectory generator ##

ca_migrant_trajectory <- function(area.index,follow.on.trajectory = trajectory()){
  journey.time <- pickup.areas$transit.time[area.index]
  timeout.action <- pickup.areas$timeout.action[area.index]
  if(timeout.action == 'depart'){
    out.trajectory <- trajectory() %>%
    seize("usa.counter",1) %>%                                                           # add USA counter
    release("usa.counter",1) 
  } else {
    w <- which(pickup.areas[,1]==timeout.action)
    # The following line could result in an infinite loop
    # Add check that there is no circular logic in migrant flow.
    out.trajectory <- ca_migrant_trajectory(w,follow.on.trajectory)
  } 
  area.migrant <- trajectory(paste("Migrant",area.index,sep="_")) %>%
    set_attribute(keys = "area.journey.time", values=journey.time) %>%
    renege_in(
      t = pickup.areas$migrant.timeout[area.index],                                      # add branches here? depart = count
      out = out.trajectory
    ) %>%
    seize(paste("boat.area",area.index,sep="_")) %>%
    renege_abort() %>% 
    branch(
      option = function(){
        s <- sample(c(0,1),1,prob = c(1-repat.at.sea.prob,repat.at.sea.prob))
      },
      continue = FALSE,
      trajectory() %>%
        release(paste("boat.area",area.index,sep="_")) %>%
        seize("repat_afloat",1) %>%                                                     # repat afloat counter
        release("repat_afloat", 1)   ## 
    ) %>%
    seize("afloat.counter") %>%                                                         # afloat.counter
    set_global(
      keys = paste("boat.count",area.index,sep="_"),
      values = 1,
      mod = "+"
    ) %>% 
    branch(
      option = function(){
        boat.count <- get_global(env,paste("boat.count",area.index,sep="_"))
        boat.type <- get_global(env,paste("boat.type",area.index,sep="_"))
        n <- ship.attributes$sat.capacity[boat.type]
        m <- ship.attributes$supersat.capacity[boat.type]
        if(boat.count == 1){
          return(1)
        } else if(boat.count==n){
          return(2)
        } else if(boat.count==m){
          return(3)
        } else {
          return(0)
        }
      },
      continue = TRUE,
      list(
        "1" = set_global(
          trajectory(),
          keys = paste("boat.occupied",area.index,sep = "_"),
          values = function() return(now(env))
        ) %>%
          send(
            signals = paste("boat.depart",area.index,sep="_"),
            delay = function(){
              boat.type <- get_global(env,paste("boat.type",area.index,sep="_"))
              t.1 <- ship.attributes$occupied.time[boat.type]
              jt <- get_attribute(env,"area.journey.time")
              return(max(0,t.1 - jt))
            }
          ),
        "2" = set_global(
          trajectory(),
          keys = paste("boat.saturated",area.index,sep = "_"),
          values = function() return(now(env))
        ) %>%
          send(
            signals = paste("boat.depart",area.index,sep="_"),
            delay = function(){
              boat.type <- get_global(env,paste("boat.type",area.index,sep="_"))
              t.n <- ship.attributes$sat.time[boat.type]
              jt <- get_attribute(env,"area.journey.time")
              return(max(t.n - jt,0))
            }
          ),
        "3" = set_global(
          trajectory(),
          keys = paste("boat.supersaturated",area.index,sep = "_"),
          values = function() return(now(env))
        ) %>%
          send(
            signals = paste("boat.depart",area.index,sep="_"),
            delay = 0
          )
      )
    ) %>%
    trap(paste("boat.departed",area.index,sep="_")) %>%
    wait() %>%
    release(paste("boat.area",area.index,sep="_")) %>%
    timeout(journey.time) %>%
    trap(
      signals = function(){
        return(
          sprintf("debark_%1.2f",now(env))
        )
      }
    )%>%
    wait() %>%
    release("afloat.counter")                                          # release afloat counter

  return(join(area.migrant,follow.on.trajectory))
}

## Pickup area boat trajectory generator ##

ca_boat_trajectory <- function(area.index,boat.type.index){
  journey.time <- pickup.areas$transit.time[area.index]
  boat.trajectory <- trajectory(paste("Boat",area.index,boat.type.index,sep="_")) %>%
    seize(paste("area",area.index,sep="_")) %>%
    timeout(epsilon) %>%
    set_capacity(
      resource=paste("boat.area",area.index,sep="_"),
      value = ship.attributes$supersat.capacity[boat.type.index],
      mod = "+"
    ) %>%
    set_global(
      keys = paste("boat.type",area.index,sep="_"),
      values = boat.type.index
    ) %>%
    trap(
      paste("boat.depart",area.index,sep="_"),
      handler = trajectory() %>%
        rollback(
          amount = 2,
          check = function(){
            count <- get_global(env,paste("boat.count",area.index,sep="_"))
            occ <- get_global(env,paste("boat.occupied",area.index,sep="_"))
            sat <- get_global(env,paste("boat.saturated",area.index,sep="_"))
            full <- get_global(env,paste("boat.supersaturated",area.index,sep="_"))
            time.now <- now(env)
            if((count >=1) && (time.now - occ >= ship.attributes$occupied.time[boat.type.index])) return(FALSE)
            else if((count >= ship.attributes$sat.capacity[boat.type.index]) && (time.now - sat) >= ship.attributes$sat.time[boat.type.index]) return(FALSE)
            else if(count >= ship.attributes$supersat.capacity[boat.type.index]) return(FALSE)
            else return(TRUE)
          }
        )
    ) %>%
    wait() %>%
    set_capacity(
      resource = paste("boat.area",area.index,sep="_"),
      value = -ship.attributes$supersat.capacity[boat.type.index],
      mod = "+"
    ) %>%
    timeout(epsilon) %>%
    send(paste("boat.departed",area.index,sep="_")) %>%
    timeout(small.epsilon) %>%
    untrap(paste("boat.depart",area.index.sep="_")) %>% 
    set_global(
      paste("boat.count",area.index,sep="_"),
      0
    ) %>%
    timeout(small.epsilon) %>% 
    release(paste("area",area.index,sep="_")) %>%
    timeout(journey.time) %>%
    set_attribute(
      keys = "debark.signal",
      values = function() return(now(env))
    ) %>%
    seize(
      "berth",
      amount = ship.attributes$berth.occupation[boat.type.index]
    ) %>%
    timeout(ship.attributes$offload.time[boat.type.index]) %>%
    send(
      signals = function(){
        return(sprintf("debark_%1.2f",get_attribute(env,"debark.signal")))
      }
    ) %>%
    release_all("berth") %>%
    timeout(ship.attributes$recovery.time[boat.type.index]) %>%
    timeout(journey.time) %>%
    rollback(22)
  return(boat.trajectory)
}

## Processing Center Transit Trajectory Generator ##

ferry_trajectory <- function(proc.params){
  ferry.transit.time <- proc.params$Value[which(
    proc.params$Parameter == "ferry.transit.time"
  )]
  ferry.reset <- proc.params$Value[which(
    proc.params$Parameter == "ferry.reset"
  )]
  ferry.capacity <- proc.params$Value[which(
    proc.params$Parameter == "ferry.capacity"
  )]
  ferry.service.hours <- proc.params$Value[which(
    proc.params$Parameter == "ferry.service.hours"
  )]
  ferry.trajectory <- trajectory() %>%
    set_attribute(keys = "start.time",values = function() return(now(env))) %>%
    set_capacity("transit-to-leeward",ferry.capacity,mod="+") %>%
    timeout(small.epsilon) %>%
    set_capacity("transit-to-leeward",-ferry.capacity,mod="+") %>%
    timeout(ferry.transit.time) %>%
    set_capacity("transit-to-windward",ferry.capacity,mod="+") %>%
    timeout(small.epsilon) %>%
    set_capacity("transit-to-windward",-ferry.capacity,mod="+") %>%
    timeout(ferry.transit.time) %>% 
    branch(
      option = function(){
        time.now <- now(env)
        start.time <- get_attribute(env,"start.time")
        if(time.now + ferry.transit.time + ferry.reset - start.time > ferry.service.hours){
          return(1)
        } else {
          return(2)
        }
      },
      continue = TRUE,
      trajectory() %>%
        timeout(
          function(){
            start.time <- get_attribute(env,"start.time")
            time.now <- now(env)
            return(start.time + 24 - time.now)
          }
        ) %>%
        set_attribute(keys = "start.time",values = function() return(now(env))),
      trajectory() %>%
        timeout(ferry.reset)
    ) %>%
    rollback(9)
  return(ferry.trajectory)
}



## Initialization Trajectory ##


nat_convert <- function(nat){
  if(grepl("cuba",tolower(nat))){
    return("Cuban")
  } else if(grepl("haiti",tolower(nat))){
    return("Haitian")
  } else {
    return("Other")
  }
}

nat_convert_int <- function(nat){
  n <- nat_convert(nat)
  w <- which(nationality.probs[,1]==n)
  return(w)
}

protected_convert <- function(prot){
  if(prot=="PROTECTED"){
    return(1)
  } else {
    return(2)
  }
}

fs_convert <- function(fs){
  w <- which(tolower(family.status.probs[,1])==tolower(fs))
  if(length(w)==0){
    stop("Error interpreting family status in initial conditions")
  }
  return(w)
}

## Processing Center Migrant Trajectory Generator ##

### Family status counter sub-trajectory ###

family_status_counter_trajectory <- function(
  family.status.int,
  queue.name,
  queue.resource = NULL
){
  if(is.null(queue.resource)){
    traj <- trajectory() %>%
	  seize(paste(queue.name,family.status.probs[family.status.int,1],sep="_")) %>%
	  release(paste(queue.name,family.status.probs[family.status.int,1],sep="_"))
  } else {
    traj <- trajectory() %>%
	  seize(paste(queue.name,family.status.probs[family.status.int,1],sep="_")) %>%
        seize(queue.resource) %>%
	  release(paste(queue.name,family.status.probs[family.status.int,1],sep="_"))
  }
  return(traj)
}


processing_trajectory <- function(){
  repat.trajectory.list <- list()
  for(n in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      family.trajectory.list <- list()
      for(i in 1:length(family.status.probs[,1])){
        family.trajectory.list <- append(
          family.trajectory.list,
          family_status_counter_trajectory(
            i,
            paste(protected,n,sep="-"),
            paste(protected,n,sep="-")
          )
        )
      }
      repat.trajectory.list <- append(
        repat.trajectory.list,
        trajectory() %>%
          branch(
            option = function(){
              fs <- get_attribute(env,"family.status")
              return(fs)
            },
            continue = TRUE,
            family.trajectory.list
          ) %>%
          timeout(
            function() return(runif(1,0.25,23.99))
          )%>%
          release("nsgb.counter") %>%                          # release NSGB counter upon repat-resettle
          release(paste(protected,n,sep="-")) 
      )
    }
  }
  inprocess.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    inprocess.trajectory.list <- append(
      inprocess.trajectory.list,
      family_status_counter_trajectory(
        i,
        "Inprocessing",
        "ICE.agent"
      )
    )
  }
  cis.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    cis.trajectory.list <- append(
      cis.trajectory.list,
      family_status_counter_trajectory(
        i,
        "CIS",
        "CIS.screener"
      )
    )
  }
  welcome.to.GTMO <- trajectory(name = "GTMO") %>%
    seize("nsgb.counter") %>%                                  # initiate NSGB counter
    # Initial health separation
    branch(
      option = function(){
        variable <- "health"
        key <- "Disease"
        df <- health.probs
        value <- get_attribute(env,variable)
        key.index <- which(df[,1]==key)
        if(value == key.index){
          return(1)
        } else {
          return(0)
        }
      },
      continue = TRUE,                                       #This guy jumps right back in line once he feels better
      trajectory(name="Sick")  %>%
        timeout(
          function(){
            time.now <- now(env)
            w <- max(which(health.recovery$time <= time.now))
            return(
              rtriang(
                health.recovery$recovery.mode[w],
                health.recovery$recovery.min[w],
                health.recovery$recovery.max[w]
              )
            )
          }
        )
    ) %>% 
    seize("wait.ferry.to.leeward")%>%                          # initiate leeward dock counter
    seize(
      "transit-to-leeward"
    ) %>%
    release("wait.ferry.to.leeward") %>%                       # counter for dock waiting time
    timeout(2*small.epsilon) %>%
    release("transit-to-leeward") %>%
    timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    ) %>%
    branch(
      option = function(){
        fs <- get_attribute(env,"family.status")
        return(fs)
      },
      continue = TRUE,
      inprocess.trajectory.list
    ) %>%
    timeout(
      function(){
        time.now <- now(env)
        w <- max(which(inprocessing.schedule$time <= time.now))
        return(
          rtriang(
            inprocessing.schedule$inprocessing.mode[w],
            inprocessing.schedule$inprocessing.min[w],
            inprocessing.schedule$inprocessing.max[w]
          )
        )
      }
    ) %>%
    release("ICE.agent") %>%
    seize("wait.ferry.to.windward") %>%
    seize(
      "transit-to-windward"
    ) %>%
    release("wait.ferry.to.windward") %>%
    timeout(2*small.epsilon) %>%
    release("transit-to-windward") %>%
    timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    ) %>%
    branch(
      option = function() {
        variable <- "security.risk"
        # key <- "Security Risk"                          # Not needed because security risk table is formatted differently.
        value <- get_attribute(env,variable)
        key.index <- 1                                     # Need this because security risks are handled differently.
        if(value == key.index){
          return(1)
        } else {
          return(0)
        }
      },
      continue = FALSE,                                  # security folks timeout and do not use the migrant repat/resettle path
      trajectory(name="security.risk") %>%
        seize("security.counter") %>%
        timeout(
          function(){
            time.now <- now(env)
            w <- max(which(sec.repat$time <= time.now))
            return(
              rtriang(
                sec.repat$sec.risk.repat.mode[w],
                sec.repat$sec.risk.repat.min[w],
                sec.repat$sec.risk.repat.max[w]
              )
            )
          }
        ) %>%
        release("security.counter") %>%
        release("nsgb.counter")                           # security folks timeout and do not use the migrant repat/resettle path
    ) %>%
    timeout(
      function(){
        time.now <- now(env)
        boat.op.hours <- proc.params$Value[which(proc.params$Parameter=="ferry.service.hours")]
        report.time <- (floor(time.now/24) + 1)*24 + (12-boat.op.hours/2)
        delay <- report.time-time.now
        return(delay)
      }
    ) %>%
    seize("wait.ferry.to.leeward")%>%                          # initiate leeward dock counter
    seize(
      "transit-to-leeward"
    ) %>%
    release("wait.ferry.to.leeward") %>%                       # counter for dock waiting time
    timeout(2*small.epsilon) %>%
    release("transit-to-leeward") %>%
    timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    ) %>%
    branch(
      option = function(){
        fs <- get_attribute(env,"family.status")
        return(fs)
      },
      continue = TRUE,
      cis.trajectory.list
    ) %>% 
    timeout(
      function(){
        time.now <- now(env)
        w <- max(which(cis.schedule$time <= time.now))
        return(
          rtriang(
            cis.schedule$cis.screening.mode[w],
            cis.schedule$cis.screening.min[w],
            cis.schedule$cis.screening.max[w]
          )
        )
      }
    ) %>% 
    release("CIS.screener") %>%
    branch(
      option = function(){
        time.now <- now(env)
        w <- max(which(cis.rescreening$time <= time.now))
        cis.rescreen.rate <- cis.rescreening$rescreen.rate[w]
        if(runif(1) < cis.rescreen.rate){
          return(1)
        } else {
          return(0)
        }
      },
      continue = TRUE,
      trajectory(name="re-screen")  %>%
        timeout(
          function(){
            time.now <- now(env)
            w <- max(which(cis.rescreening$time <= time.now))
            return(
              rtriang(
                cis.rescreening$rescreen.wait.mode[w],
                cis.rescreening$rescreen.wait.min[w],
                cis.rescreening$rescreen.wait.max[w]
              )
            )
          }
        )
    ) %>%
    seize("wait.ferry.to.windward") %>%
    seize(
      "transit-to-windward"
    ) %>%
    release("wait.ferry.to.windward") %>%
    timeout(2*small.epsilon) %>%
    release("transit-to-windward") %>%
    timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    ) %>%
    branch(
      option = function() {
        protected <- get_attribute(env,"protected")
        n <- get_attribute(env,"nationality")
        return(2 * n + 1 - protected)
      },
      continue = TRUE,
      repat.trajectory.list
    )
  return(welcome.to.GTMO)
}


## Camp initialization trajectories ##

initialization_trajectories <- function(init.data){
  output <- list()
  agen <- function(nat,fs,ps){
    output <- c(
      fs,
      2,
      nat,
      2,
      ps
    )
    f <- function(){
      return(
        output
      )
    }
    return(f)
  }
  repat.trajectory.list <- list()
  for(n in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      family.trajectory.list <- list()
      for(i in 1:length(family.status.probs[,1])){
        family.trajectory.list <- append(
          family.trajectory.list,
          family_status_counter_trajectory(
            i,
            paste(protected,n,sep="-"),
            paste(protected,n,sep="-")
          )
        )
      }
      repat.trajectory.list <- append(
        repat.trajectory.list,
        trajectory() %>%
          branch(
            option = function(){
              fs <- get_attribute(env,"family.status")
              return(fs)
            },
            continue = TRUE,
            family.trajectory.list
          ) %>%
          timeout(
            function() return(runif(1,0.25,23.99)) 
          )%>%
          release("nsgb.counter") %>%                          # release NSGB counter upon repat-resettle
          release(paste(protected,n,sep="-")) 
      )
    }
  }
  for(i in 1:nrow(init.data)){
    nat.str <- nat_convert(init.data$Nationality[i])
    nat.int <- nat_convert_int(init.data$Nationality[i])
    prot.int <- protected_convert(init.data$Protected[i])
    fs.int <- fs_convert(init.data$family_status[i])
    fs.str <- init.data$family_status[i]
    if(prot.int==1){
      prot.str <- "Protected"
    } else {
      prot.str <- "Not Protected"
    }
    g <- agen(
      nat.int,
      fs.int,
      prot.int
    )
    output[[paste(nat.str,fs.str,prot.str,"init",sep="-")]] <- trajectory(name=paste(nat.str,fs.str,prot.str,"init",sep="-")) %>%
      set_attribute(
        keys = c(
          "family.status",
          "health",
          "nationality",
          "security.risk",
          "protected"
        ),
        values = g
      ) %>%
      seize("nsgb.counter") %>%
      branch(
        option = function() {
          protected <- get_attribute(env,"protected")
          n <- get_attribute(env,"nationality")
          return(2 * n + 1 - protected)
        },
        continue = TRUE,
        repat.trajectory.list
      )
  }
  return(output)
}


## Impulse trajectories

haiti_source_prob <- function(nat){
  if(grepl("hait",tolower(nat))){
    n <- "Haitian"
  } else if(grepl("cuba",tolower(nat))) {
    n <- "Cuban"
  } else {
    n <- "Other"
  }
  rnds <- runif(1)
  w <- which(nationality.probs$nationality==n)
  s <- source.rates[order(source.rates$time),]
  w.cuba <- min(which(s$source=="Cuba"))
  w.haiti <- min(which(s$source=="Haiti"))
  s.haiti <- s$rate[w.haiti]/(s$rate[w.haiti]+s$rate[w.cuba])
  p.haitian <- (nationality.probs$Haiti[w]*(s.haiti))/(nationality.probs$Haiti[w]*(s.haiti) + nationality.probs$Cuba[w]*(1-s.haiti))
  return(p.haitian)
}

impulse_attributes <- function(nat,family.stat){
  p.haitian <- haiti_source_prob(nat)
  rnds <- runif(3)
  p.disease <- health.probs$Haiti[1]*p.haitian + health.probs$Cuba[1]*(1-p.haitian)
  w <- which(security.risk.probs$family.status==family.stat)
  p.sr <- security.risk.probs$Haiti[w]*p.haitian + security.risk.probs$Cuba[w]*(1-p.haitian)
  p.prot <- protected.probs$Haiti[1]*p.haitian + protected.probs$Cuba[1]*(1-p.haitian)
  if(rnds[1] < p.disease){
    disease <- 1
  } else {
    disease <- 2
  }
  if(rnds[2] < p.sr){
    sr <- 1
    pr <- 2
  } else {
    sr <- 2
  }
  if((rnds[3] < p.disease) && (rnds[2] >= p.sr)){
    pr <- 1
  } else {
    pr <- 2
  }
  return(
    c(
      Health = disease,
      Security.Risk = sr,
      Protected = pr
    )
  )
}


impulse_trajectories <- function(impulse.data){
  output <- list()
  agen <- function(nat,fs){
    atts <- impulse_attributes(nat,fs)
    fs.int <- which(family.status.probs$family.status==fs)
    nat.int <- nat_convert_int(nat)
    output <- c(
      fs.int,
      atts[1],
      nat.int,
      atts[2],
      atts[3]
    )
    f <- function(){
      return(
        output
      )
    }
    return(f)
  }
  repat.trajectory.list <- list()
  for(n in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      family.trajectory.list <- list()
      for(i in 1:length(family.status.probs[,1])){
        family.trajectory.list <- append(
          family.trajectory.list,
          family_status_counter_trajectory(
            i,
            paste(protected,n,sep="-"),
            paste(protected,n,sep="-")
          )
        )
      }
      repat.trajectory.list <- append(
        repat.trajectory.list,
        trajectory() %>%
          branch(
            option = function(){
              fs <- get_attribute(env,"family.status")
              return(fs)
            },
            continue = TRUE,
            family.trajectory.list
          ) %>%
          timeout(
            function() return(runif(1,0.25,23.99))
          )%>%
          release("nsgb.counter") %>%                          # release NSGB counter upon repat-resettle
          release(paste(protected,n,sep="-")) 
      )
    }
  }
  inprocess.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    inprocess.trajectory.list <- append(
      inprocess.trajectory.list,
      family_status_counter_trajectory(
        i,
        "Inprocessing",
        "ICE.agent"
      )
    )
  }
  cis.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    cis.trajectory.list <- append(
      cis.trajectory.list,
      family_status_counter_trajectory(
        i,
        "CIS",
        "CIS.screener"
      )
    )
  }
  for(j in grep("Day",names(impulse.data))){
    for(i in 1:nrow(impulse.data)){
      nat.str <- nat_convert(impulse.data$Origin[i])
      nat.int <- nat_convert_int(impulse.data$Origin[i])
      fs.int <- fs_convert(impulse.data$family_status[i])
      fs.str <- impulse.data$family_status[i]
      rnd <- runif(2)
      g <- agen(
        nat.str,
        fs.str
      )
      output[[paste(nat.str,fs.str,sprintf("day%i",j),"impulse",sep="-")]] <- list()
      output[[paste(nat.str,fs.str,sprintf("day%i",j),"impulse",sep="-")]][[1]] <- trajectory(name=paste(nat.str,fs.str,sprintf("day%i",j),"impulse",sep="-")) %>%
        set_attribute(
          keys = c(
            "family.status",
            "health",
            "nationality",
            "security.risk",
            "protected"
          ),
          values = g
        ) %>%
        seize("nsgb.counter") %>%                                  # initiate NSGB counter
        # Initial health separation
        branch(
          option = function(){
            variable <- "health"
            key <- "Disease"
            df <- health.probs
            value <- get_attribute(env,variable)
            key.index <- which(df[,1]==key)
            if(value == key.index){
              return(1)
            } else {
              return(0)
            }
          },
          continue = TRUE,                                       #This guy jumps right back in line once he feels better
          trajectory(name="Sick")  %>%
            timeout(
              function(){
                time.now <- now(env)
                w <- max(which(health.recovery$time <= time.now))
                return(
                  rtriang(
                    health.recovery$recovery.mode[w],
                    health.recovery$recovery.min[w],
                    health.recovery$recovery.max[w]
                  )
                )
              }
            )
        ) %>% 
        seize("wait.ferry.to.leeward")%>%                          # initiate leeward dock counter
        seize(
          "transit-to-leeward"
        ) %>%
        release("wait.ferry.to.leeward") %>%                       # counter for dock waiting time
        timeout(2*small.epsilon) %>%
        release("transit-to-leeward") %>%
        timeout(
          function(){
            w <- which(proc.params$Parameter=="ferry.transit.time")
            return(proc.params$Value[w])
          }
        ) %>%
        branch(
          option = function(){
            fs <- get_attribute(env,"family.status")
            return(fs)
          },
          continue = TRUE,
          inprocess.trajectory.list
        ) %>%
        timeout(
          function(){
            time.now <- now(env)
            w <- max(which(inprocessing.schedule$time <= time.now))
            return(
              rtriang(
                inprocessing.schedule$inprocessing.mode[w],
                inprocessing.schedule$inprocessing.min[w],
                inprocessing.schedule$inprocessing.max[w]
              )
            )
          }
        ) %>%
        release("ICE.agent") %>%
        seize("wait.ferry.to.windward") %>%
        seize(
          "transit-to-windward"
        ) %>%
        release("wait.ferry.to.windward") %>%
        timeout(2*small.epsilon) %>%
        release("transit-to-windward") %>%
        timeout(
          function(){
            w <- which(proc.params$Parameter=="ferry.transit.time")
            return(proc.params$Value[w])
          }
        ) %>%
        branch(
          option = function() {
            variable <- "security.risk"
            # key <- "Security Risk"                          # Not needed because security risk table is formatted differently.
            value <- get_attribute(env,variable)
            key.index <- 1                                     # Need this because security risks are handled differently.
            if(value == key.index){
              return(1)
            } else {
              return(0)
            }
          },
          continue = FALSE,                                  # security folks timeout and do not use the migrant repat/resettle path
          trajectory(name="security.risk") %>%
            seize("security.counter") %>%
            timeout(
              function(){
                time.now <- now(env)
                w <- max(which(sec.repat$time <= time.now))
                return(
                  rtriang(
                    sec.repat$sec.risk.repat.mode[w],
                    sec.repat$sec.risk.repat.min[w],
                    sec.repat$sec.risk.repat.max[w]
                  )
                )
              }
            ) %>%
            release("security.counter") %>%
            release("nsgb.counter")                           # security folks timeout and do not use the migrant repat/resettle path
        ) %>%
        timeout(
          function(){
            time.now <- now(env)
            boat.op.hours <- proc.params$Value[which(proc.params$Parameter=="ferry.service.hours")]
            report.time <- (floor(time.now/24) + 1)*24 + (12-boat.op.hours/2)
            delay <- report.time-time.now
            return(delay)
          }
        ) %>%
        seize("wait.ferry.to.leeward")%>%                          # initiate leeward dock counter
        seize(
          "transit-to-leeward"
        ) %>%
        release("wait.ferry.to.leeward") %>%                       # counter for dock waiting time
        timeout(2*small.epsilon) %>%
        release("transit-to-leeward") %>%
        timeout(
          function(){
            w <- which(proc.params$Parameter=="ferry.transit.time")
            return(proc.params$Value[w])
          }
        ) %>%
        branch(
          option = function(){
            fs <- get_attribute(env,"family.status")
            return(fs)
          },
          continue = TRUE,
          cis.trajectory.list
        ) %>% 
        timeout(
          function(){
            time.now <- now(env)
            w <- max(which(cis.schedule$time <= time.now))
            return(
              rtriang(
                cis.schedule$cis.screening.mode[w],
                cis.schedule$cis.screening.min[w],
                cis.schedule$cis.screening.max[w]
              )
            )
          }
        ) %>% 
        release("CIS.screener") %>%
        branch(
          option = function(){
            time.now <- now(env)
            w <- max(which(cis.rescreening$time <= time.now))
            cis.rescreen.rate <- cis.rescreening$rescreen.rate[w]
            if(runif(1) < cis.rescreen.rate){
              return(1)
            } else {
              return(0)
            }
          },
          continue = TRUE,
          trajectory(name="re-screen")  %>%
            timeout(
              function(){
                time.now <- now(env)
                w <- max(which(cis.rescreening$time <= time.now))
                return(
                  rtriang(
                    cis.rescreening$rescreen.wait.mode[w],
                    cis.rescreening$rescreen.wait.min[w],
                    cis.rescreening$rescreen.wait.max[w]
                  )
                )
              }
            )
        ) %>%
        seize("wait.ferry.to.windward") %>%
        seize(
          "transit-to-windward"
        ) %>%
        release("wait.ferry.to.windward") %>%
        timeout(2*small.epsilon) %>%
        release("transit-to-windward") %>%
        timeout(
          function(){
            w <- which(proc.params$Parameter=="ferry.transit.time")
            return(proc.params$Value[w])
          }
        ) %>%
        branch(
          option = function() {
            protected <- get_attribute(env,"protected")
            n <- get_attribute(env,"nationality")
            return(2 * n + 1 - protected)
          },
          continue = TRUE,
          repat.trajectory.list
        )
      output[[paste(nat.str,fs.str,sprintf("day%i",j),"impulse",sep="-")]][[2]] <- as.numeric(gsub("Day","",gsub(" ","",names(impulse.data)[j])))
      output[[paste(nat.str,fs.str,sprintf("day%i",j),"impulse",sep="-")]][[3]] <- as.numeric(impulse.data[i,j])
    }
  }
  return(output)
}



## Afloat trajectories


afloat_attributes <- function(nat){
  p.haitian <- haiti_source_prob(nat)
  rnd1 <- runif(1)
  if(rnd1 < p.haitian){
    src <- "Haiti"
    src.int <- 1
  } else {
    src <- "Cuba"
    src.int <- 2
  }
  rnds <- runif(4)
  fs <- min(which(cumsum(family.status.probs[,src]) > rnds[1]))
  family.stat <- family.status.probs$family.status[fs]
  p.disease <- health.probs$Haiti[1]*p.haitian + health.probs$Cuba[1]*(1-p.haitian)
  w <- which(security.risk.probs$family.status==family.stat)
  p.sr <- security.risk.probs$Haiti[w]*p.haitian + security.risk.probs$Cuba[w]*(1-p.haitian)
  p.prot <- protected.probs$Haiti[1]*p.haitian + protected.probs$Cuba[1]*(1-p.haitian)
  if(rnds[2] < p.disease){
    disease <- 1
  } else {
    disease <- 2
  }
  if(rnds[3] < p.sr){
    sr <- 1
    pr <- 2
  } else {
    sr <- 2
  }
  if((rnds[4] < p.disease) && (rnds[2] >= p.sr)){
    pr <- 1
  } else {
    pr <- 2
  }
  return(
    c(
      Source = src.int,
      Family.Status = fs,
      Health = disease,
      Security.Risk = sr,
      Protected = pr
    )
  )
}

afloat_trajectories <- function(afloat.data,area.trajectory.list){
  atl <- check_area_trajectory_list(area.trajectory.list)
  output <- list()
  agen <- function(nat){
    atts <- afloat_attributes(nat)    
    nat.int <- nat_convert_int(nat)
    output <- c(
      atts[1],
      atts[2],
      atts[3],
      nat.int,
      atts[4],
      atts[5]
    )
    f <- function(){
      return(
        output
      )
    }
    return(f)
  }
  for(i in 1:nrow(afloat.data)){
    nat.str <- nat_convert(afloat.data$Origin[i])
    g <- agen(
      nat.str
    )
    output[[paste(nat.str,"afloat",sep="-")]] <- trajectory(name=paste(nat.str,"afloat",sep="-")) %>%
      set_attribute(
        keys = c(
          "Source",
          "family.status",
          "health",
          "nationality",
          "security.risk",
          "protected"
        ),
        values = g
      ) %>%
      branch(
        option =  function(){
          m.int <- get_attribute(env,"Source")
          m <- migrant.sources[m.int,1]
          p <- pickup.areas[,m]
          route <- prob_generator_int(p)
          return(route)
        },
        continue = FALSE,
        atl
      )
  }
  return(output)
}








