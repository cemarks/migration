#' ---
#' title: "USSOUTHCOM/ARSOUTH Migration Model"
#' author: "Christopher Marks"
#' date: "`r format(Sys.Date(),format='%b %d, %Y')`"
#' output:
#'   html_document:
#'     includes:
#'       in_header: F:/Desktop/Rscripts/HTML_META_TAG.txt
#' --- 

#' # Purpose
#' 
#' This document and accompanying R Script use the R [simmer](https://r-simmer.org/) package to
#' implement a discrete event simulation of migrant flows through multiple 
#' processing stations.  This simulation serves several purposes:
#' 
#' * Analyze migrant flows to inform decisions for USSOUTHCOM Exercise Integrated Advance.
#' * Analyze migrant flows to inform real world decisions concerning response to mass migrations.
#' * With minimial modification, analyze personnel flows to support non-combatant evacuation
#' or other personnel evacuation missions.
#' 
#' This simulation is jointly developed by the Operations Research teams at
#' USSOUTHCOM and ARSOUTH.

library(simmer)
library(readxl)
migrant.datafile <- "F:/Desktop/Rscripts/migration/migrant_inputs.xlsx"
areas.datafile <- "F:/Desktop/Rscripts/migration/ship_info.xlsx"
migrant.datafile <- "/home/cemarks/Projects/migration/migrant_inputs.xlsx"
areas.datafile <- "/home/cemarks/Projects/migration/ship_info.xlsx"
#' # Models and Inputs 
#' 
#' This simulation consists of three layers:
#' 
#' 1. The migrant source layer.
#' 1. The migrant consolidation layer.
#' 1. The migrant processing layer.
#' 
#' ## Migrant Sources
#' 
#' This layer deals with the source(s) of migrants.  The model can include 
#' multiple sources.  In this implementation, the sources are "Haiti" and "Cuba."


#+ echo=FALSE,fig.width=4
migrant.sources <- as.data.frame(
  read_excel(
    migrant.datafile,
    sheet="sources"
  )
)
knitr::kable(
  data.frame(
    migrant.sources
  ),
  align="c"
)


#' 
#' ### Inputs
#' 
#' Each source requires the following inputs.
#' 
#' * The rate of migrant generation.  This can be a single rate, or a schedule.
#' Unless otherwise specified, a Poisson arrival process will be assumed.
#' * The migrant demographic rates.  These must follow the prescribed format 
#' discussed below.  The rates are not time-dependent.
#' * The migrant consolidation layer routing probabilities.  These probabilities
#' determine which trajectories migrants from each source will follow.
#' 
#' #### Rate of migrant generation
#' 
#' The rates of migration for each source can be provided in a single table consisting of
#' times, sources, and rates.  Each entry in the table sets the migrant generation rate
#' for the specified source at the specified time.  The rate remains constant until the next
#' change time provided in another row in the table.  An example of this table could be 
#' formatted is provided below.
#' 

#+ echo=FALSE
check_table <- function(d,names.vector = migrant.sources$Source,error.d="",row1.vector = NULL){
  if(!all(names(d)[2:ncol(d)] %in% names.vector)){
    stop(sprintf("Invalid column in %s input data.",error.d))
  }
  if(!is.null(row1.vector)){
    if(!all(as.character(d[,1]) %in% as.character(row1.vector))){
      stop(sprintf("Invalid category in %s data inputs.",error.d))
    }
  }
  return(d)
}
source.rates <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "source.rates")
  ),
  error.d = "migrant source rate",
  names.vector = c(
    "Time",
    "Source",
    "Rate"
  )
)
knitr::kable(
  source.rates,
  align=c(
    "c",
    "c",
    "c"
  )
)

#' 
#' 
#' #### Migrant Demographics
#' 
#' Migrants in this model have attributes that affect how they are processed.
#' These attributes are specified in the table below.

#+ echo=FALSE
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



knitr::kable(
  data.frame(
    Category = names(migrant.attributes),
    Levels = sapply(
      migrant.attributes,
      function(x){
        return(
          paste(x,collapse="<br>")
        )
      }
    )
  ),
  row.names = FALSE
)

#' We assume that each generator has its own demographics inputs.  Given a source
#' each demographic is independent of all others, 
#' *except* `security.risk`.  The `security.risk` demographic depends on the
#' `family.status` demographic.  Furthermore, the `protected` and the `security.risk`
#' demographics are mutually exclusive.  The following tables depict the demographic
#' inputs that meet this criteria.
#' 
#' #### Family Status 
#' 
#' The table below shows the expected probability inputs for the `family.status` demographic.
#' Migrants generated from each source will be assigned to one of the `family.status` 
#' categories according to the input probabilities in each column.  Therefore, 
#' the columns of probabilities in this table should sum to 1.

#+ echo=FALSE
family.status.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "family.status")
  ),
  error.d = "family status",
  row1.vector = migrant.attributes[['family.status']]
)
knitr::kable(
  family.status.probs,
  row.names = FALSE,
  align = c("l","c","c")
)

#' #### Health 
#' 
#' The table below shows the expected probability inputs for the `health` demographic.
#' Migrants generated from each source will be independently assigned to the `Disease` category
#' according to the probability provided in the table.  Otherwise, the migrant will be assigned
#' to the `No disease` category.

#+ echo=FALSE
check_health_probs <- function(health.data){
  if(!all(as.character(health.data[,1]) %in% names(migrant.attributes$health))){
    stop("Invalid health status category in health status probability inputs.")
  }
  if(!all(names(health.data)[2:ncol(health.data)] %in% as.character(migrant.sources$Source))){
    stop("Invalid migrant source in health status probability inputs.")
  }
}
health.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "health")
  ),
  error.d = 'health',
  row1.vector = migrant.attributes[["health"]]
)
knitr::kable(
  health.probs,
  row.names = FALSE,
  align = c("l","c","c")
)

#' #### Nationality
#' 
#' The table below shows the expected probability inputs for the migrant `nationality` demographic.
#' Migrants generated from each source will be independently 
#' according to the probabilities provided in the column corresponding to the source.  Therefore, 
#' the columns of probabilities in this table should sum to 1.

#+ echo=FALSE
nationality.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "nationality")
  ),
  error.d = 'nationality',
  row1.vector = migrant.attributes[['nationality']]
)

knitr::kable(
  nationality.probs,
  row.names = FALSE,
  align = c("l","c","c")
)



#' #### Security Risk
#' 
#' The table below shows the expected probability inputs for the migrant `security.risk` demographic.
#' Migrants generated from each source will be assigned as a security risk depending on their
#' `family.status`.  Therefore, this input table is formatted differently from the others in this section.
#' The probability values in this table are the probability that a migrant generated from the
#' corresponding source, in the specified `family.status` category is labeled as a risk.  There is no
#' requirement for probabilities on either axis of this table to sum to 1; rather, each probability
#' will be applied as an input to a Bernoulli trial for each migrant according to the migrant's source
#' and `family.status` category.

#+ echo=FALSE
security.risk.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "security.risk")
  ),
  error.d = 'security risk',
  row1.vector = migrant.attributes[['family.status']]
)
security.risk.probs <- security.risk.probs[
  match(
    as.character(family.status.probs[,1]),
    as.character(security.risk.probs[,1])
  ),
]

knitr::kable(
  security.risk.probs,
  row.names = FALSE,
  align = c("l","c","c")
)

#' #### Protected Demographic Inputs
#' 
#' The table below shows the expected probability inputs for the `protected` status.
#' Migrants generated from each source *that are not labeled as security risks*
#' will be assigned to the `Protected` category
#' according to the probability provided in the table.  Otherwise, the migrant will be assigned
#' to the `Not protected` category.

#+ echo=FALSE
protected.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "protected")
  ),
  error.d = 'protected migrant rate',
  row1.vector = migrant.attributes$protected
)
knitr::kable(
  protected.probs,
  row.names = FALSE,
  align = c("l","c","c")
)


#'
#' #### Migrant Routing Probabilities
#' 
#' The migrant routing probabilities determine how migrants generated from each source will
#' be routed in the consolidation layer.  Inputting these probabilities requires knowledge of
#' the consolidation areas.  


#+ echo=FALSE
# For this migration model, the consolidation areas are
# consolidation.areas <- c(
#   "Offshore Haiti/WWP"=1,
#   "South Florida Straights"=2,
#   "Coastal Key West"=3,
#   "North Florida Straights" =4,
#   "Coastal Miami"=5
# )
# knitr::kable(
#   data.frame(
#     "ID"=as.numeric(consolidation.areas),
#     "Consolidation Area"=as.character(names(consolidation.areas))
#   ),
#   align=c("l","l")
# )

#' The migrant source-consolidation area routing probability matrix takes the form in the table
#' below.  The probabilities in each column should sum to 1, indicating that all of the
#' migrants generated at each source should be routed to one of the consolidation areas.  This
#' table also includes timeout information and transit times for the consolidation areas.

#+ echo=FALSE
check_pickup_areas <- function(d,names.vector,error.d="consolidation area"){
  d <- check_table(
    d,
    names.vector = names.vector,
    error.d = "consolidation area"
  )
  if(!all(as.character(migrant.sources$Source) %in% names(d))){
    stop("Not all migrant sources have columns in pickup area input")
  }
  timeout.action.possibilities <- c(
    "depart",
    as.character(
      d[,1]
    )
  )
  w <- which(!(as.character(d$timeout.action) %in% timeout.action.possibilities))
  if(length(w) > 0){
    warning("Invalid entries for timeout action in pickup area input table.  Invalid entries will be changed to 'depart'.")
    d$timeout.action[w] <- "depart"
  }
  return(d)
}
pickup.areas <- check_pickup_areas(
  as.data.frame(
    read_excel(
      areas.datafile,
      sheet = "pickup.areas"
    )
  ),
  names.vector = c(
    as.character(migrant.sources$Source),
    "migrant.timeout",
    "timeout.action",
    "transit.time"
  )
)


knitr::kable(
  pickup.areas,
  row.names = FALSE,
  align = c("l","c","c","c","c","c")
)


#' ### Trajectories
#' 
#' The migrant source process includes migrant generation and initial trajectories.  These 
#' trajectories simply branch the generated migrant stream to the consolidation layer trajectories
#' according to the routing probabilities.  
#' 

# Helper functions
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
  if(r <= 0){
    return(1) # Security Risk
  } else {
    return(2)
  }
}

get_output <- function(function.call,probs){
  p <- prob_generator(probs)
  n <- as.character(function.call)
  n <- sub("_",".",n,fixed=TRUE)
  output <- value_extractor(
    n,
    p
  )
  return(output)
}


migrant_attributes <- function(mig.source){
  family.status <- prob_generator(family.status.probs,mig.source)$value
  migrant.health <- prob_generator(health.probs,mig.source)$value
  migrant.nationality <- prob_generator(nationality.probs,mig.source)$value
  security.risk <- security_risk_prob_generator(family.status,mig.source)
  migrant.protected <- prob_generator(protected.probs,mig.source)$value
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

# Source trajectory generator
## Requires the trajectory routing table (dataframe) and the area trajectories as inputs.
## It will check to make sure the names in the trajectory routing dataframe match the 
## source names and consolidation area names.
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
  output <- list()
  for(i in 1:length(mig.sources)){
    p <- pickup.areas[,mig.source]
    output[[i]] <- trajectory() %>%
      set_attribute(
        keys = c(
          "family.status",
          "health",
          "nationality",
          "security.risk",
          "protected"
        ),
        values = as.numeric(
          migrant.attributes(
            mig.source
          )
        )
      )
      branch(
        option = function(){
          route = prob_generator_int(p)
          return(route)
        },
        continue = FALSE,
        atl
      )
  }
  return(output)
}

#'
#' ## Consolidation Layer
#' 
#' This layer consists of consolidation areas, where migrants accumlate and
#' are located and picked-up for transit to the processing layer.  Only a single
#' boat can occupy a consolidation area and pick up migrants at any time. 
#' 
#' The layer requires the following inputs
#' 
#' * Consolidation area matrix including source routing probabilities,
#' timeout routings (example provided at the end of the last section.)
#' * Boat types, capacities and capacity times, and berthing requirements.
#' * Transit times from each consolidation area to the processing station.
#' * Boat allocations to each of the consolidation areas, by boat type.
#' * Pier capacity at the processing station.
#' 
#' ### Inputs
#' 
#' #### Consoliation area information.
#' 
#' An example table was provided at the end of the last section.  This input includes the
#' probabilities of migrants generated at each source following each consolidation area 
#' trajectory.  It also includes timeout information, i.e., if a migrant is not picked up after
#' a certain amount of time in a consolidation area, then the migrant "times out" and moves to
#' another area as identified in the inputs.  Moving to area "0" indicates the migrant leaves 
#' the system.
#' 
#' #### Boat types
#' 
#' The boat type matrix input structure is provided in the table below.  For each boat type,
#' the saturation capacity, saturation capacity max time, oversaturation capacity, oversaturation
#' capacity max time, berth space, unload time, and reset time are required.
#' 

#+ echo=FALSE
knitr::kable(
  data.frame(
    boat.type=c(
      "<b>[Boat1 Name]</b>",
      "<b>[Boat2 Name]</b>",
      "<b>...</b>"
    ),
    sat.cap = c(
      "<b>[capacity1]</b>",
      "<b>[capacity2]</b>",
      "<b>...</b>"
    ),
    sat.time = c(
      "<b>[sat.time1]</b>",
      "<b>[sat.time2]</b>",
      "<b>...</b>"
    ),
    supersat.cap = c(
      "<b>[supercapacity1]</b>",
      "<b>[supercapacity2]</b>",
      "<b>...</b>"
    ),
    supersat.time = c(
      "<b>[supersat.time1]</b>",
      "<b>[supersat.time2]</b>",
      "<b>...</b>"
    ),
    berths.required = c(
      "<b>[berths1]</b>",
      "<b>[berths2]</b>",
      "<b>...</b>"
    ),
    offload.time = c(
      "<b>[offload.time1]</b>",
      "<b>[offload.time2]</b>",
      "<b>...</b>"
    ),
    reset.time = c(
      "<b>[reset.time1]</b>",
      "<b>[reset.time2]</b>",
      "<b>...</b>"
    )
  ),
  align = c(
    "l",
    "c",
    "c",
    "c",
    "c",
    "c"
  ),
  row.names = FALSE
)

#' #### Transit times.
#' 
#' For each area, the simulation requires the transit time from the consolidation area 
#' to the processing station.  These inputs take have the structure in the table below.
#' 

#+ echo=FALSE
knitr::kable(
  data.frame(
    Consolidation.Area=names(consolidation.areas),
    Transit.time = rep("<b>[transit.time]</b>",length(consolidation.areas))
  ),
  align = c(
    "l",
    "c"
  )
)

#' #### Boat allocations
#' 
#' The number of boats, by type, per consolidation area follows a simlar tabular structure.

#+ echo=FALSE
d <- data.frame(
  Consolidation.Area = consolidation.areas,
  boat.1 = rep("<b>[n]</b>",length(consolidation.areas)),
  boat.2 = rep("<b>[n]</b>",length(consolidation.areas)),
  boat.3 = rep("<b>[n]</b>",length(consolidation.areas))
)
knitr::kable(
  d,
  align=c(
    "l",
    "c",
    "c"
  ),
  row.names = FALSE
)

#' #### Pier capacity
#' 
#' This input is a single number of berthing spaces available at the processing station.  Boat
#' will occupy these spaces in order to offload migrants
#' 

pier.capacity <- 8 # Example pier capacity


#' ### Trajectories
#' 
#' With the above information, it is possible to write the consolidation area trajectory generator
#' functions.
#' 

ca.migrant.trajectories <- function(
  
)




#' ## Migrant batching
#' 
#' Initial migrant batching must be done using one or more generators, and joining batches 
#' into area trajectories.



#' ## Boat-Migrant Trajectories
#' 
#' The boats in this model queue at their respective pick up areas,
#' and change passenger pick-up
#' capacities upon arrival and departure.  Each boat can only be assigned to one
#' pick up area.  The boat departs the pick-up area when:
#' 
#' * It reaches max capacity, $m$.
#' * $t_n$ time units after reaching 'saturation' capacity, $n$.
#' * $t_1$ time units after picking up its first migrant.
#' 
#' These events are handled with variables in the global environment
#' and with signals.
#' 

boat.max.capacity <- 8
m <- 5 # boat full--immediate departure
n <- 3 # boat saturated
t.n <- 2 # time boat can remain saturated prior to departing
t.1 <- 10 # time boat can remain occupied prior to departing
boat.first.arrival <- 5 # time it takes for boat to initially reach pick-up location.
journey.time <- 9 # time it takes to reach processing stations once boat departs (+ admin delay)
boat.return.time <- 19 # time it takes boat to return to pick-up location after departing (+ admin delay)
pickup.epsilon <- 0.01 # admin delay from when boat receives departure signal to when it departs.

migrant.1 <- trajectory("Migrant 1") %>%
  set_attribute(
    keys = names(migrant.attributes),
    values = migrant_attributes
  ) %>%
  seize("boat.area.1") %>%
  set_global(
    keys = "boat1.count",
    values = 1,
    mod = "+"
  ) %>% 
  branch(
    option = function(){
      boat1count <- get_global(env,"boat1.count")
      if(boat1count == 1){
        return(1)
      } else if(boat1count==n){
        return(2)
      } else if(boat1count==m){
        return(3)
      } else {
        return(0)
      }
    },
    continue = TRUE,
    list(
      "1" = set_global(
        trajectory(),
        keys = "boat1.occupied",
        values = function() return(now(env))
      ) %>%
        log_(
          "Sending 1st depart signal (1st migrant boarded)"
        ) %>%
        send(
          signals = "boat1.depart",
          delay = t.1
        ),
      "2" = set_global(
        trajectory(),
        keys = "boat1.saturated",
        values = function() return(now(env))
      ) %>%
        log_(
          "Sending 2nd depart signal (boat saturated)"
        ) %>% 
        send(
          signals = "boat1.depart",
          delay = t.n
        ),
      "3" = set_global(
        trajectory(),
        keys = "boat1.full",
        values = function() return(now(env))
      ) %>%
        log_(
          "Sending final depart signal (boat full)"
        ) %>% 
        send(
          signals = "boat1.depart",
          delay = 0
        )
    )
  ) %>%
  trap("boat1.departed") %>%
  wait() %>%
  release("boat.area.1") %>%
  timeout(journey.time) %>%
  log_("Arrived!")

boat.1 <- trajectory("Boat 1") %>%
  timeout(boat.first.arrival) %>%
  seize("area1") %>%
  timeout(pickup.epsilon) %>%
  set_capacity(
    resource="boat.area.1",
    value = boat.max.capacity,
    mod = "+"
  ) %>%
  trap(
    "boat1.depart",
    handler = trajectory() %>%
      log_("Received boat1.depart signal") %>%
      rollback(
        amount = 2,
        check = function(){
          count <- get_global(env,"boat1.count")
          occ <- get_global(env,"boat1.occupied")
          sat <- get_global(env,"boat1.saturated")
          full <- get_global(env,"boat1.full")
          time.now <- now(env)
          if((count >=1) && (time.now - occ >= t.1)) return(FALSE)
          else if((count >= n) && (time.now - sat) >= t.n) return(FALSE)
          else if(count >= m) return(FALSE)
          else return(TRUE)
        }
      )
  ) %>%
  wait() %>%
  log_("Boat 1 Departed") %>%
  set_capacity(
    resource = "boat.area.1",
    value = -boat.max.capacity,
    mod = "+"
  ) %>%
  timeout(pickup.epsilon) %>%
  release("area1") %>%
  send("boat1.departed") %>%
  untrap("boat1.depart") %>% 
  set_global(
    "boat1.count",
    0
  ) %>%
  timeout(boat.return.time) %>%
  rollback(13)


#' ### Test of the boat-migrant model

env <- simmer(
  log_level = Inf
) %>%
  add_generator(
    "migrants",
    migrant.1,
    at(0:20)
    ) %>%
  add_generator(
    "boat 1",
    boat.1,
    at(5)
  ) %>%
  add_resource(
    "boat.area.1",
    capacity = 0,
    queue_size=Inf
  ) %>%
  add_resource(
    "area1",
    capacity = 1,
    queue_size = Inf
  ) %>%
  add_global(
    "boat1.count",
    0
  ) %>%
  add_global(
    "boat1.occupied",
    0
  ) %>%
  add_global(
    "boat1.saturated",
    0
  ) %>%
  add_global(
    "boat1.full",
    0
  )

env %>%
  run(until = 50)

knitr::kable(get_mon_resources(env))
knitr::kable(get_mon_attributes(env))
knitr::kable(get_mon_arrivals(env))


#' ## Pier arrival and camp processing trajectory
#' 
#' This section provides the processing and camp trajectories.  Sub-trajectories can be
#' joined to other sub-trajectories using the `join` method.
#' 
#' 


#' ### On-island transit trajectory
#' 
#' On-island transit capacity will be handled by a generator objects.
#' The following assumes there are `b` boats, and each requires a
#' `rt`-hour round-trip time.  Boats will load int `epsilon` time
#' units, and only operate for `b.hoursperday` hours each day.

b <- 2
rt <- 2
transit.time <- 1 #Half of round trip for now
epsilon <- 0.01
boat.capacity <- 100

interarrival.time <- rt/b
start.boat.time <- 0 #first boat
b.hoursperday <- 12

boat.arrivals <- seq(start.boat.time,start.boat.time+12,interarrival.time)

end.boat.time <- start.boat.time + b.hoursperday
transit.schedule <- schedule(
  c(0,
    rbind(
      boat.arrivals,
      boat.arrivals+epsilon
    )
  ),
  c(
    0,
    rep(c(100,0),length(boat.arrivals))
  ),
  period = 24
)

migrant.transit <- trajectory() %>%
  seize(
    "transit"
  ) %>%
  timeout(transit.time) %>%
  release("transit")


s<-simmer() %>%
  add_generator(
    "migrants",
    migrant.transit,
    at(0)
  ) %>%
  add_resource(
    "transit",
    capacity = transit.schedule
  ) %>% run(
    until = 200
  )



knitr::kable(get_mon_resources(s))
knitr::kable(get_mon_attributes(s))
knitr::kable(get_mon_arrivals(s))


#' ### Migrant Camp In-processing
#' 
#' This trajectory handles the processing on GTMO.
#' 
#' 

# Helper function for gamma distribution
gamma_params <- function(gamma.mean,gamma.sd){
  alpha <- gamma.mean^2/gamma.sd^2
  beta <- gamma.sd^2/gamma.mean
  return(
    c(
      alpha,
      beta
    )
  )
}

# Health timeout assume exponentially distributed.
# Average time
health.timeout.avg <- 24 # hours
health_timeout <- function(){
  return(rexp(1,1/health.timeout.avg))
}

# ICE agent interview time: assume gamma distribution.
ice.mean <- 5/60 # hours
ice.sd <- 2/60 # hours
ice.gamma.params <- gamma_params(ice.mean,ice.sd)
ice_timeout <- function(){
  return(rgamma(1,shape=ice.gamma.params[1],scale=ice.gamma.params[2]))
}

# Security risk service time

security.risk.min <- 90*24
security.risk.max <- 360*24
security_risk_service <- function(){
  return(runif(1,security.risk.min,security.risk.max))
}

# CIS screening time: assume gamma distribution
cis.mean <- 20/60
cis.sd <- 10/60
cis.gamma.params <- gamma_params(cis.mean,cis.sd)
cis_timeout <- function(){
  return(rgamma(1,shape=cis.gamma.params[1],scale=cis.gamma.params[2]))
}


# CIS rescreening
# What percent need follow-on interviews?
cis.rescreen.rate <- 0.2
# Assume gamma distribution here as well.
cis.rescreen.mean <- 48 #hours
cis.rescreen.sd <- 48 #hours
rescreen.gamma.params <- gamma_params(cis.rescreen.mean,cis.rescreen.sd)
cis_rescreen_timeout <- function(){
  return(rgamma(1,shape=rescreen.gamma.params[1],scale=rescreen.gamma.params[2]))
}

# Protected resettlement time: assume gamma distribution
resettle.mean <- 24*60
resettle.sd <- 24*30
resettle.gamma.params <- gamma_params(resettle.mean,resettle.sd)
resettle_timeout <- function(){
  return(
    rgamma(
      1,
      shape = resettle.gamma.params[1],
      scale = resettle.gamma.params[2]
    )
  )
}

# Non-Protected resettlement time: assume gamma distribution
repatriate.mean <- 24*20
repatriate.sd <- 24*7
repatriate.gamma.params <- gamma_params(repatriate.mean,repatriate.sd)
repatriate_timeout <- function(){
  return(
    rgamma(
      1,
      shape = repatriate.gamma.params[1],
      scale = repatriate.gamma.params[2]
    )
  )
}

welcome.to.GTMO <- trajectory(name = "GTMO") %>%
  # Initial health separation
  branch(
    option = function(){
      health <- get_attribute(env,"health")
      if(health == "Disease"){
        return(1)
      } else {
        return(0)
      }
    },
    continue = TRUE, #This guy jumps right back in line once he feels better
    trajectory(name="Sick") %>%
      timeout(
        health_timeout
      )
  ) %>%
  seize("ICE.agent") %>%
  timeout(
    ice_timeout
  ) %>%
  release("ICE.agent") %>%
  branch(
    option = function() {
      security.risk <- get_attribute(env,"security.risk")
      if(security.risk == "Security Risk"){
        return(1)
      } else {
        return(0)
      }
    },
    continue = FALSE,
    trajectory(name="security.risk") %>%
      timeout(security_risk_service)
  ) %>%
  seize("CIS.screener") %>%
  timeout(
    cis_timeout
  ) %>%
  release("CIS.screener") %>%
  branch(
    option = function(){
      if(runif(1) < cis.rescreen.rate){
        return(1)
      } else {
        return(0)
      }
    },
    continue = TRUE,
    trajectory(name="re-screen") %>%
      timeout(cis_rescreen_timeout)
  ) %>% 
  branch(
    option = function() {
      protected <- get_attribute(env,"protected")
      if(protected == "Protected"){
        return(1)
      } else {
        return(0)
      }
    },
    continue = FALSE,
    trajectory() %>%
      timeout(
        resettle_timeout
      )
  ) %>%
  timeout(
    repatriate_timeout
  )





