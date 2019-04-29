family_status_counter_trajectory <- function(
  family.status.probs,
  family.status.int,
  queue.name,
  queue.resource = NULL
){
  if(is.null(queue.resource)){
    traj <- simmer::trajectory() %>%
    simmer::seize(paste(queue.name,family.status.probs[family.status.int,1],sep="_")) %>%
    simmer::release(paste(queue.name,family.status.probs[family.status.int,1],sep="_"))
  } else {
    traj <- simmer::trajectory() %>%
    simmer::seize(paste(queue.name,family.status.probs[family.status.int,1],sep="_")) %>%
        simmer::seize(queue.resource) %>%
    simmer::release(paste(queue.name,family.status.probs[family.status.int,1],sep="_"))
  }
  return(traj)
}

ferry_to_windward <- function(.trj,proc.params){
  trj <- simmer::seize(
      .trj,
      "wait.ferry.to.windward"
    ) %>%
    simmer::seize(
      "transit-to-windward"
    ) %>%
    simmer::release("wait.ferry.to.windward") %>%
    simmer::timeout(2*small.epsilon()) %>%
    simmer::release("transit-to-windward") %>%
    simmer::timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    )
  return(trj)
}

ferry_to_leeward <- function(.trj,proc.params){
  trj <- simmer::seize(.trj,"wait.ferry.to.leeward")%>%                          # initiate leeward dock counter
    simmer::seize(
      "transit-to-leeward"
    ) %>%
    simmer::release("wait.ferry.to.leeward") %>%                       # counter for dock waiting time
    simmer::timeout(2*small.epsilon()) %>%
    simmer::release("transit-to-leeward") %>%
    simmer::timeout(
      function(){
        w <- which(proc.params$Parameter=="ferry.transit.time")
        return(proc.params$Value[w])
      }
    )
  return(trj)
}


timeout_to_boat_op <- function(.trj,proc.params){
  trj <- simmer::timeout(
    .trj,
    function(){
      time.now <- simmer::now(env)
      boat.op.hours <- proc.params$Value[which(proc.params$Parameter=="ferry.service.hours")]
      report.time <- (floor(time.now/24) + 1)*24 + (12-boat.op.hours/2)
      delay <- report.time-time.now
      return(delay)
    }
  )
  return(trj) 
}

security_risk_branch <- function(.trj,sec.repat){
  trj <- simmer::branch(
    .trj,
    option = function() {
      variable <- "security.risk"
      value <- simmer::get_attribute(env,variable)
      key.index <- 1                                     # Need this because security risks are handled differently.
      if(value == key.index){
        return(1)
      } else {
        return(0)
      }
    },
    continue = FALSE,                                  # security folks timeout and do not use the migrant repat/resettle path
    simmer::trajectory(name="security.risk") %>%
      simmer::seize("security.counter") %>%
      simmer::timeout(
        function(){
          time.now <- simmer::now(env)
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
      simmer::release("security.counter") %>%
      simmer::release("nsgb.counter")                           # security folks timeout and do not use the migrant repat/resettle path
  )
  return(trj)
}

health_branch <- function(.trj,health.probs,health.recovery){
  trj <- simmer::branch(
    .trj,
    option = function(){
      variable <- "health"
      key <- "Disease"
      df <- health.probs
      value <- simmer::get_attribute(env,variable)
      key.index <- which(df[,1]==key)
      if(value == key.index){
        return(1)
      } else {
        return(0)
      }
    },
    continue = TRUE,                                       #This guy jumps right back in line once he feels better
    simmer::trajectory(name="Sick")  %>%
      simmer::timeout(
        function(){
          time.now <- simmer::now(env)
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
  )
  return(trj)
}



set_all_attributes <- function(
  .trj,
  migrant.sources,
  source.rates,
  family.status.probs,
  health.probs,
  nationality.probs,
  protected.probs,
  security.risk.probs,
  src=NULL,
  nat=NULL,
  fs=NULL,
  hth = NULL,
  sr=NULL,
  prot=NULL,
  t=0
){
  if(is.null(src)){
    src <- src_generator(
      migrant.sources,
      source.rates,
      nationality.probs,
      nat,
      t
    )
  } else if(is.character(src)){
    w <- which(migrant.sources$Source==src)
    if(length(w) != 1){
      stop("Cannot set migrant source: bad input value.")
    }
    src <- w
  }
  src.str <- migrant.sources[src,1]
  if(is.null(nat)){
    nat <- prob_generator(nationality.probs,src.str)$value
  } else if(is.character(nat)) {
    w <- which(nationality.probs[,1]==nat)
    if(length(w) != 1){
      stop("Cannot set nationality: bad input value.")
    }
    nat <- w
  }
  if(is.null(fs)){
    fs <- prob_generator(family.status.probs,src.str)$value
  } else if(is.character(fs)) {
    w <- which(family.status.probs[,1]==fs)
    if(length(w) != 1){
      stop("Cannot set family status: bad input value.")
    }
    fs <- w
  }
  if(is.null(hth)){
    hth <- prob_generator(health.probs,src.str)$value
  } else if(is.character(hth)) {
    w <- which(health.probs[,1]==hth)
    if(length(w) != 1){
      stop("Cannot set health status: bad input value.")
    }
    hth <- w
  }
  if(is.character(prot)){
    w <- which(protected.probs[,1]==prot)
    if(length(w) != 1){
      stop("Cannot set protected status: bad input value.")
    }
    prot <- w
  }
  if(is.null(sr)){
    if(is.null(prot) || prot == 2){
      sr <- security_risk_prob_generator(fs,src.str,security.risk.probs)
    } else {
      sr <- 2
    }
  }
  if(is.null(prot) && sr ==2){
    prot <- prob_generator(protected.probs,src.str)$value
  } else if(is.null(prot)){
    prot <- 2
  }
  trj <- simmer::set_attribute(
    .trj,
    keys = c(
      "source",
      "family.status",
      "health",
      "nationality",
      "security.risk",
      "protected"
    ),
    values = c(
      src,
      fs,
      hth,
      nat,
      sr,
      prot
    )
  )
  return(trj)
}

repat_trajectory_list <- function(
  nationality.probs,
  family.status.probs
){
  repat.trajectory.list <- list()
  for(n in nationality.probs[,1]){
    for(protected in c("repat","resettle")){
      family.trajectory.list <- list()
      for(i in 1:length(family.status.probs[,1])){
        family.trajectory.list <- append(
          family.trajectory.list,
          family_status_counter_trajectory(
            family.status.probs,
            i,
            paste(protected,n,sep="-"),
            paste(protected,n,sep="-")
          )
        )
      }
      repat.trajectory.list <- append(
        repat.trajectory.list,
        simmer::trajectory() %>%
          simmer::branch(
            option = function(){
              fs <- simmer::get_attribute(env,"family.status")
              return(fs)
            },
            continue = TRUE,
            family.trajectory.list
          ) %>%
          simmer::timeout(
            function() return(stats::runif(1,0.25,23.99))
          )%>%
          simmer::release("nsgb.counter") %>%                          # release NSGB counter upon repat-resettle
          simmer::release(paste(protected,n,sep="-")) 
      )
    }
  }
  return(repat.trajectory.list)
}

repat <- function(
  .trj,
  nationality.probs,
  family.status.probs
){
  rtl <- repat_trajectory_list(
    nationality.probs,
    family.status.probs
  )
  trj <- simmer::branch(
    .trj,
    option = function() {
      protected <- simmer::get_attribute(env,"protected")
      n <- simmer::get_attribute(env,"nationality")
      return(2 * n + 1 - protected)
    },
    continue = TRUE,
    rtl
  )
  return(trj)
}

ICE_trajectory_list <- function(family.status.probs){
  ICE.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    ICE.trajectory.list <- append(
      ICE.trajectory.list,
      family_status_counter_trajectory(
        family.status.probs,
        i,
        "Inprocessing",
        "ICE.agent"
      )
    )
  }
  return(ICE.trajectory.list)
}

ICE_screening <- function(
  .trj,
  family.status.probs,
  inprocessing.schedule
){
  ICE.trajectory.list <- ICE_trajectory_list(family.status.probs)
  trj <- simmer::branch(
      .trj,
      option = function(){
        fs <- simmer::get_attribute(env,"family.status")
        return(fs)
      },
      continue = TRUE,
      ICE.trajectory.list
    ) %>%
    simmer::timeout(
      function(){
        time.now <- simmer::now(env)
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
    simmer::release("ICE.agent")
  return(trj)
}

cis_trajectory_list <- function(family.status.probs){
  cis.trajectory.list <- list()
  for(i in 1:length(family.status.probs[,1])){
    cis.trajectory.list <- append(
      cis.trajectory.list,
      family_status_counter_trajectory(
        family.status.probs,
        i,
        "CIS",
        "CIS.screener"
      )
    )
  }
  return(cis.trajectory.list)
}


CIS_screening <- function(
  .trj,
  family.status.probs,
  cis.schedule,
  cis.rescreening
){
  cis.traj.lst <- cis_trajectory_list(family.status.probs)
  trj <- simmer::branch(
    .trj,
    option = function(){
      fs <- simmer::get_attribute(env,"family.status")
      return(fs)
    },
    continue = TRUE,
    cis.traj.lst
  ) %>% 
  simmer::timeout(
    function(){
      time.now <- simmer::now(env)
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
  simmer::release("CIS.screener") %>%
  simmer::branch(
    option = function(){
      time.now <- simmer::now(env)
      w <- max(which(cis.rescreening$time <= time.now))
      cis.rescreen.rate <- cis.rescreening$rescreen.rate[w]
      if(stats::runif(1) < cis.rescreen.rate){
        return(1)
      } else {
        return(0)
      }
    },
    continue = TRUE,
    simmer::trajectory(name="re-screen")  %>%
      simmer::timeout(
        function(){
          time.now <- simmer::now(env)
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
  )
  return(trj)
}

pickup_area <- function(
  .trj,
  pickup.areas,
  ship.attributes,
  repat.at.sea.prob = 0.1
){
  trj.list <- list()
  for(area.index in 1:nrow(pickup.areas)){
    trj.list[[area.index]] <- simmer::trajectory() %>%
      simmer::set_attribute(
        keys = "area.journey.time",
        values=pickup.areas$transit.time[area.index]
      ) %>%
      simmer::renege_in(
        t = pickup.areas$migrant.timeout[area.index],                                      # add branches here? depart = count
        out = (function(pickup.areas, timeout.action){
          if(timeout.action=="depart"){
            trj <- simmer::trajectory() %>%
              simmer::seize("usa.counter",1) %>%                                                           # add USA counter
              simmer::release("usa.counter",1) %>%
              leave(1)
          } else {
            w <- which(pickup.areas[,1]==timeout.action)
            trj <- simmer::trajectory() %>%
              simmer::set_attribute(
                keys = "pickup.area",
                values = w
              ) %>%
              simmer::rollback(4)
          }
        })(pickup.areas,pickup.areas$timeout.action[area.index])
      ) %>%
      simmer::seize(paste("boat.area",area.index,sep="_")) %>%
      simmer::renege_abort() %>% 
      simmer::branch(
        option = function(){
          s <- sample(c(0,1),1,prob = c(1-repat.at.sea.prob,repat.at.sea.prob))
        },
        continue = FALSE,
        simmer::trajectory() %>%
          simmer::release(paste("boat.area",area.index,sep="_")) %>%
          simmer::seize("repat_afloat",1) %>%                                                     # repat afloat counter
          simmer::release("repat_afloat", 1)   ## 
      ) %>%
      simmer::seize("afloat.counter") %>%
      simmer::set_global(
        keys = paste("boat.count",area.index,sep="_"),
        values = 1,
        mod = "+"
      ) %>% 
      simmer::branch(
        option = function(){
          pickup.area <- simmer::get_attribute(env,"pickup.area")
          boat.count <- simmer::get_global(env,paste("boat.count",pickup.area,sep="_"))
          boat.type <- simmer::get_global(env,paste("boat.type",pickup.area,sep="_"))
          n <- ship.attributes$sat.capacity[boat.type]
          m <- ship.attributes$supersat.capacity[boat.type]
          if(length(boat.count==n)==0){
            cat(area.index,"\n")
            cat(boat.count,"\n")
            cat(boat.type,"\n")
            cat(n,"\n")
            cat(m,"\n")
          }
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
          "1" = simmer::set_global(
            simmer::trajectory(),
            keys = paste("boat.occupied",area.index,sep = "_"),
            values = function() return(simmer::now(env))
          ) %>%
            simmer::send(
              signals = paste("boat.depart",area.index,sep="_"),
              delay = function(){
                pickup.area <- simmer::get_attribute(env,"pickup.area")
                boat.type <- simmer::get_global(env,paste("boat.type",pickup.area,sep="_"))
                t.1 <- ship.attributes$occupied.time[boat.type]
                jt <- simmer::get_attribute(env,"area.journey.time")
                return(max(0,t.1 - jt))
              }
            ),
          "2" = simmer::set_global(
            simmer::trajectory(),
            keys = paste("boat.saturated",area.index,sep = "_"),
            values = function() return(simmer::now(env))
          ) %>%
            simmer::send(
              signals = paste("boat.depart",area.index,sep="_"),
              delay = function(){
                pickup.area <- simmer::get_attribute(env,"pickup.area")
                boat.type <- simmer::get_global(env,paste("boat.type",pickup.area,sep="_"))
                t.n <- ship.attributes$sat.time[boat.type]
                jt <- simmer::get_attribute(env,"area.journey.time")
                return(max(t.n - jt,0))
              }
            ),
          "3" = simmer::set_global(
            simmer::trajectory(),
            keys = paste("boat.supersaturated",area.index,sep = "_"),
            values = function() return(simmer::now(env))
          ) %>%
            simmer::send(
              signals = paste("boat.depart",area.index,sep="_"),
              delay = 0
            )
        )
      ) %>%
      simmer::trap(paste("boat.departed",area.index,sep="_")) %>%
      simmer::wait() %>%
      simmer::release(paste("boat.area",area.index,sep="_")) %>%
      simmer::timeout(pickup.areas$transit.time[area.index])
  }
  o <- simmer::branch(
    .trj,
    option = function(){
      pa <- simmer::get_attribute(
        env,
        "pickup.area"
      )
      return(pa)
    },
    continue = TRUE,
    trj.list
  ) %>%
    simmer::trap(
      signals = function(){
        return(
          sprintf("debark_%1.2f",simmer::now(env))
        )
      }
    )%>%
    simmer::wait() %>%
    simmer::release("afloat.counter")
  return(o)
}




boat_trajectory_list <- function(
  pickup.areas,
  ship.attributes,
  ship.allocation
){
  boat.trajectories <- list()
  count <- 0
  for(i in 1:nrow(pickup.areas)){
    area.index <- i
    for(j in 1:nrow(ship.attributes)){
      boat.type.index <- j
      boat.type <- ship.attributes[j,1]
      pickup.area <- pickup.areas[i,1]
      w <- which(ship.allocation[,1]==pickup.area)
      v <- ship.allocation[w,boat.type]
      journey.time <- pickup.areas$transit.time[i]
      if(v > 0){
        count <- count + 1
        trj <- simmer::trajectory(paste("Boat",area.index,boat.type.index,sep="_")) %>%
          simmer::seize(paste("area",area.index,sep="_")) %>%
          simmer::timeout(epsilon) %>%
          simmer::set_capacity(
            resource=paste("boat.area",area.index,sep="_"),
            value = ship.attributes$supersat.capacity[boat.type.index],
            mod = "+"
          ) %>%
          simmer::set_global(
            keys = paste("boat.type",area.index,sep="_"),
            values = boat.type.index
          ) %>%
          simmer::trap(
            paste("boat.depart",area.index,sep="_"),
            handler = simmer::trajectory() %>%
              simmer::rollback(
                amount = 2,
                check = function(){
                  count <- simmer::get_global(env,paste("boat.count",area.index,sep="_"))
                  occ <- simmer::get_global(env,paste("boat.occupied",area.index,sep="_"))
                  sat <- simmer::get_global(env,paste("boat.saturated",area.index,sep="_"))
                  full <- simmer::get_global(env,paste("boat.supersaturated",area.index,sep="_"))
                  time.now <- simmer::now(env)
                  if((count >=1) && (time.now - occ >= ship.attributes$occupied.time[boat.type.index])) return(FALSE)
                  else if((count >= ship.attributes$sat.capacity[boat.type.index]) && (time.now - sat) >= ship.attributes$sat.time[boat.type.index]) return(FALSE)
                  else if(count >= ship.attributes$supersat.capacity[boat.type.index]) return(FALSE)
                  else return(TRUE)
                }
              )
          ) %>%
          simmer::wait() %>%
          simmer::set_capacity(
            resource = paste("boat.area",area.index,sep="_"),
            value = -ship.attributes$supersat.capacity[boat.type.index],
            mod = "+"
          ) %>%
          simmer::timeout(epsilon) %>%
          simmer::send(paste("boat.departed",area.index,sep="_")) %>%
          simmer::timeout(small.epsilon()) %>%
          simmer::untrap(paste("boat.depart",area.index.sep="_")) %>% 
          simmer::set_global(
            paste("boat.count",area.index,sep="_"),
            0
          ) %>%
          simmer::timeout(small.epsilon()) %>% 
          simmer::release(paste("area",area.index,sep="_")) %>%
          simmer::timeout(journey.time) %>%
          simmer::set_attribute(
            keys = "debark.signal",
            values = function() return(simmer::now(env))
          ) %>%
          simmer::seize(
            "berth",
            amount = ship.attributes$berth.occupation[boat.type.index]
          ) %>%
          simmer::timeout(ship.attributes$offload.time[boat.type.index]) %>%
          simmer::send(
            signals = function(){
              return(sprintf("debark_%1.2f",simmer::get_attribute(env,"debark.signal")))
            }
          ) %>%
          release_all("berth") %>%
          simmer::timeout(ship.attributes$recovery.time[boat.type.index]) %>%
          simmer::timeout(journey.time) %>%
          simmer::rollback(22)
        boat.trajectories[[count]] <- list(
          trajectory = trj,
          boat.count = v,
          pickup.area = pickup.areas[i,1],
          boat.type = ship.attributes[j,1]
        )
      }
    }
  }
  return(boat.trajectories)
}


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
  ferry.trajectory <- simmer::trajectory() %>%
    simmer::set_attribute(keys = "start.time",values = function() return(simmer::now(env))) %>%
    simmer::set_capacity("transit-to-leeward",ferry.capacity,mod="+") %>%
    simmer::timeout(small.epsilon()) %>%
    simmer::set_capacity("transit-to-leeward",-ferry.capacity,mod="+") %>%
    simmer::timeout(ferry.transit.time) %>%
    simmer::set_capacity("transit-to-windward",ferry.capacity,mod="+") %>%
    simmer::timeout(small.epsilon()) %>%
    simmer::set_capacity("transit-to-windward",-ferry.capacity,mod="+") %>%
    simmer::timeout(ferry.transit.time) %>% 
    simmer::branch(
      option = function(){
        time.now <- simmer::now(env)
        start.time <- simmer::get_attribute(env,"start.time")
        if(time.now + ferry.transit.time + ferry.reset - start.time > ferry.service.hours){
          return(1)
        } else {
          return(2)
        }
      },
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::timeout(
          function(){
            start.time <- simmer::get_attribute(env,"start.time")
            time.now <- simmer::now(env)
            return(start.time + 24 - time.now)
          }
        ) %>%
        simmer::set_attribute(keys = "start.time",values = function() return(simmer::now(env))),
      simmer::trajectory() %>%
        simmer::timeout(ferry.reset)
    ) %>%
    simmer::rollback(9)
  return(ferry.trajectory)
}

check_area_trajectory_list <- function(atl,pickup.areas){
  if(!all(names(atl) %in% as.character(pickup.areas[,1]))){
    stop("Pickup Area Trajectory Name Mismatch!")
  }
  s <- match(
    as.character(pickup.areas[,1]),
    names(atl)
  )
  return(atl[s])
}

set_pickup_area <- function(
  .trj,
  migrant.sources,
  pickup.areas
){
  o <- simmer::set_attribute(
    .trj,
    keys = "pickup.area",
    values = function(){
      source <- simmer::get_attribute(env,"source")
      source.str <- migrant.sources$Source[source]
      p <- pickup.areas[,source.str]
      q <- prob_generator_int(p)
      return(q)
    }
  )
  return(o)
}

source_trajectory <- function(
  .trj,
  migrant.sources,
  source.rates,
  family.status.probs,
  health.probs,
  nationality.probs,
  protected.probs,
  security.risk.probs,
  pickup.areas,
  ...
){
  source.cols <- which(names(pickup.areas) %in% as.character(migrant.sources$Source))
  mig.sources <- names(pickup.areas)[source.cols]
  if(!all(as.character(migrant.sources$Source) %in% as.character(mig.sources))){
    warning("Pickup data does not contain columns for all sources.")
  }
  trj <- set_all_attributes(
    .trj,
    migrant.sources,
    source.rates,
    family.status.probs,
    health.probs,
    nationality.probs,
    protected.probs,
    security.risk.probs,
    ...
  ) %>%
    set_pickup_area(
      migrant.sources,
      pickup.areas
    )
  return(trj)
}

