library(simmer)

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

to_camp <- function(.trj,nationality.probs,family.status.probs){
  repat.trajectory.list <- repat_trajectories(
    nationality.probs,
    family.status.probs
  )
  b <- branch(
    .trj,
    option = function() {
      protected <- get_attribute(env,"protected")
      n <- get_attribute(env,"nationality")
      return(2 * n + 1 - protected)
    },
    continue = TRUE,
    repat.trajectory.list
  )
  return(b)

ferry_to_windward <- function(.trj,proc.params){
  trj <- trajectory() %>%
    .trj,
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
    )
  )
  return(trj)
}

ferry_to_leeward <- function(.trj,proc.params){
  trj <- .trj %>%
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
    )
  )
  return(trj)
}


timeout_to_boat_op <- function(.trj,proc.params){
  trj <- timeout(
    .trj,
    function(){
      time.now <- now(env)
      boat.op.hours <- proc.params$Value[which(proc.params$Parameter=="ferry.service.hours")]
      report.time <- (floor(time.now/24) + 1)*24 + (12-boat.op.hours/2)
      delay <- report.time-time.now
      return(delay)
    }
  )
  return(trj) 
}

security_risk_branch <- function(.trj,sec.repat){
  trj <- .trj %>%
    branch(
      option = function() {
        variable <- "security.risk"
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
    )
  return(trj)
}

ICE_screening <- function(.trj,family.status.probs){
  inprocessing.schedule <- inprocess_trajectories(family.status.probs)
  trj <- branch(
      .trj,
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
    release("ICE.agent")
  return(trj)
}

health_branch <- function(.trj,health.probs,health.recovery){
  trj <- branch(
    .trj,
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
  )
  return(trj)
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

security_risk_prob_generator <- function(family.status.value,mig.source,security.risk.probs){
  p <- security.risk.probs[family.status.value,mig.source]
  r <- runif(1)
  if(r <= p){
    return(1) # Security Risk
  } else {
    return(2)
  }
}

migrant_attributes <- function(
  mig.source,
  family.status.probs,
  health.probs,
  nationality.probs
  protected.probs,
  security.risk.probs
  ){
  family.status <- prob_generator(family.status.probs,mig.source)$value
  migrant.health <- prob_generator(health.probs,mig.source)$value
  migrant.nationality <- prob_generator(nationality.probs,mig.source)$value
  security.risk <- security_risk_prob_generator(family.status,mig.source,security.risk.probs)
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



set_all_attributes <- function(
  .trj,
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
    if(is.null(nat)){
      s <- source.rates[order(source.rates$time),]
      w.cuba <- max(which(s$source=="Cuba" & s$time <= t))
      w.haiti <- max(which(s$source=="Haiti" & s$time <= t))
      p.haiti <- s$rate[w.haiti]/(s$rate[w.haiti]+s$rate[w.cuba])
      r <- runif(1)
      if(p.haiti < r){
        src <- "Haiti"
      } else {
        src <- "Cuba"
      }
    } else {
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
      w.cuba <- max(which(s$source=="Cuba" & s$time <= t))
      w.haiti <- max(which(s$source=="Haiti" & s$time <= t))
      s.haiti <- s$rate[w.haiti]/(s$rate[w.haiti]+s$rate[w.cuba])
      p.haiti <- (nationality.probs$Haiti[w]*(s.haiti))/(nationality.probs$Haiti[w]*(s.haiti) + nationality.probs$Cuba[w]*(1-s.haiti))
      r <- runif(1)
      if(p.haiti < r){
        src <- "Haiti"
      } else {
        src <- "Cuba"
      }
    }
  }
  ma <- migrant_attributes(
    src,
    family.status.probs,
    health.probs,
    nationality.probs,
    protected.probs,
    security.risk.probs,
  )
  if(is.null(nat)){
    nat <- ma[3]
    
  }
  if(is.null(fs)){
    fs <- ma[1]
  }
  if(is.null(hth)){
    hth <- ma[2]
  }
  if(is.null(sr)){
    sr <- ma[4]
  }
  if(is.null(prot)){
    prot <- ma[5]
  }
  trj <- set_attribute(
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

repat_trajectories <- function(nationality.probs,family.status.probs){
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
  return(repat.trajectory.list)
}

inprocess_trajectories <- function(family.status.probs){
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
  return(inprocess.trajectory.list)
}

cis_trajectories <- function(family.status.probs){
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
  return(cis.trajectory.list)
}


to_cis <- function(.trj, family.status.probs)
  cis.traj.lst <- cis_trajectories(family.status.probs)
  trj <- branch(
    .trj,
    option = function(){
      fs <- get_attribute(env,"family.status")
      return(fs)
    },
    continue = TRUE,
    cis.traj.lst
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
  )
  return(trj)
}

area_trajectories <- function(
  pickup.areas,
  ship.attributes,
  follow.on = trajectory()
){
  area.trajectories <- list()
  for(area.index in 1:nrow(pickup.areas)){
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
    area.trajectories[[pickup.areas$pickup.area[area.index]]] <- trajectory() %>%
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
      release("afloat.counter") %>%
      follow.on
  }
  return(area.trajectories)
}

boat_trajectories <- function(
  pickup.areas,
  ship.attributes
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
        trj <- trajectory(paste("Boat",area.index,boat.type.index,sep="_")) %>%
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

