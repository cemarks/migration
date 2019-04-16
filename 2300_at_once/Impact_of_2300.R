#' ---
#' title: "Analysis of Migrant Surge"
#' author: "LTC Chris Marks"
#' output: html_document
#' ---
#' # Purpose
#' 
#' This analysis looks at the impact of putting dropping 2300 migrants onto NSGB in a very short period of time.
#' This script will only use the GTMO processing portion of the migration model.
#' 

#+ echo=FALSE

# load the libraries and model

library(simmer)
library(simmer.plot)
set.seed(123456)
model_dir = "/home/cemarks/Projects/migration/2300_at_once"
source(paste(model_dir,"migrant_model.R",sep="/"))

# Data input #
migrant.datafile <- paste(model_dir,"migrant_inputs.xlsx",sep="/")
processing.datafile <- paste(model_dir,"processing_inputs.xlsx",sep="/")
initial.conditions.datafile <- paste(model_dir,"all_arrive_excursion.xlsx",sep="/")
source(paste(model_dir,"migrant_data_input.R",sep="/"))


#' # Initial Conditions
#' 
#' The initial conditions for this analysis are the current numbers
#' of migrants in each of the camp.  We assume that there are no migrants
#' in any of the queues (ferries, camp inprocessing, CIS screening).  
#' 
#' ## Migrants in the camps
#' 
#' Initially, we assume the camps are populated with migrants awaiting repatriation
#' or resettlement according to the table below

#+ echo=FALSE
knitr::kable(
  initial.camp
)

#' ## Ferries
#' 
#' We assume there are 
#' **`r proc.params$Value[which(proc.params$Parameter == "ferry.count")]`
#' ferries**, each with a 
#' **`r proc.params$Value[which(proc.params$Parameter == "ferry.capacity")]`
#' personnel** carrying capacity, operating continuously for
#' `r proc.params$Value[which(proc.params$Parameter == "ferry.service.hours")]`
#' hours each day.  Each one-way trip takes
#' `r proc.params$Value[which(proc.params$Parameter == "ferry.transit.time")]`
#' hours (including loading/unloading times),
#' and after each round trip the ferry takes a 
#' `r proc.params$Value[which(proc.params$Parameter == "ferry.reset")]`
#' hour reset break.  
#' 
#' ## Service Times
#' 
#' The ICE/camp inprocessing station is manned with 
#' `r inprocessing.schedule$inprocessing.server.count[1]`
#' migrant inprocessing stations, which are active for
#' `r inprocessing.schedule$inprocessing.service.hours[1]`
#' hours each day.  Processing time for each migrant
#' follows a triangular distribution, with a minimum processing time 
#' of 
#' `r inprocessing.schedule$inprocessing.min[1]`
#' hours, a maximum processing time of 
#' `r inprocessing.schedule$inprocessing.max[1]`
#' hours, and a most likely processing time of 
#' `r inprocessing.schedule$inprocessing.mode[1]`
#' hours.
#' 
#' Similarly, CIS screening is manned with 
#' `r cis.schedule$cis.screening.server.count[1]`
#' CIS screening stations, which are active for
#' `r cis.schedule$cis.screening.service.hours[1]`
#' hours each day.  Screening time for each migrant
#' follows a triangular distribution, with a minimum time 
#' of 
#' `r cis.schedule$cis.screening.min[1]`
#' hours, a maximum time of 
#' `r cis.schedule$cis.screening.max[1]`
#' hours, and a most likely screening time of 
#' `r cis.schedule$cis.screening.mode[1]`
#' hours.
#' 
#' CIS rescreening occurs for 
#' `r 100*cis.rescreening$rescreen.rate[1]`%
#' of migrants.  These migrants must wait for final determination; rescreen
#' wait times follow a triangle distribution with a minimum waiting time
#' of
#' `r cis.rescreening$rescreen.wait.min[1]`
#' hours, a maximum waiting time of 
#' `r cis.rescreening$rescreen.wait.max[1]`
#' hours, and a most likely waiting time of 
#' `r cis.rescreening$rescreen.wait.mode[1]`
#' hours.
#' 
#' There is no repatriation or resettlement from the camps.
#' 
#' **All of these parameters remain fixed over time.**
#'  
#' ## Migrant arrival
#' 
#' We assume that all migrants that are afloat are routed immediately to NSGB
#' due to weather.  These migrants' demographics are summarized in the table below.

#+ echo=FALSE
knitr::kable(
  initial.boat
)

#' ### Initialization
#' 
#' In addition to specifying the time at which the migrants arrive,
#' we also set the number of days to run the simulation.

migrant.arrival.time <- 0
total.days <- 30

#+ echo=FALSE

# Build the model.
log_level <- 0
verbose <- FALSE
source(paste(model_dir,"migrant_model_build.R",sep="/"))
# Run the model
env <- env %>% run(until = total.days*24)


#' # Processing assumptions
#' 
#' We assume all of the migrants that are afloat arrive for processing simultaneously at Pier V.  They then
#' follow the following trajectory:
#' 
#' * Take the ferry to the leeward side of the island (sick migrants are delayed until healthy).
#' * Go through ICE screening and camp inprocessing.
#' * Take the ferry back to windward.
#' * Move into camp; report back to the ferry terminal the next calendar day at ferry opening time.
#'     * Security risk migrants do not go back to leeward for CIS screening.
#' * Take the ferry to leeward.
#' * Go through CIS screening.
#'     * Some migrants are delayed for CIS rescreening.
#' * Return to windward and await resettlement/repatriation.

#' # Results
#' 
#' ## Ferry transit queue
#' 
#' The numbers of migrants awaiting ferry transit over time is depicted in the figure below.
#' At the beginning of the simulation run, approximately 2400 migrants
#' arrive at Pier V.  Immediately, they all join the queue for the 
#' next leeward ferry.  When ferry service starts later that day, the
#' hourly trips begin moving migrants to the leeward processing facilities.
#' As migrants finish their inprocessing, they queue for the windward ferry
#' transit.  This queue never grows too long because the rate of arrival
#' on the leeward side is limited by the ferry rate.
#' 
#' By the fourth day of the simulation, all migrants (except security risk
#' migrants) have completed their second transit to the leeward side.  Some
#' migrants have prolonged stays on the leeward side due to extended
#' CIS screening; these migrants continue to make windward transits into
#' the 13th day of the simulation.
#' 

#+ echo=FALSE, fig.width=6, fig.height=5

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
w <- which(resources$resource %in% c("wait.ferry.to.leeward","wait.ferry.to.windward"))
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
    color = resource,
    group = resource
  )
) + 
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Queue Length",
    title = "Ferry Queue Lengths"
  ) +
  scale_color_discrete(
    breaks = c(
      "wait.ferry.to.leeward",
      "wait.ferry.to.windward"
    ),
    labels = c(
      "Leeward Trip",
      "Windward Trip"
    ),
    name = "Ferry Direction"
  )

plot(g)


#' ## Number of Migrants on the Leeward Side
#' 
#' The queue size does not show how many migrants are on the leeward side
#' of the island over time.  This metric is depicted in the plot below.
#' This plot shows that in the initial days, migrants are processed efficiently
#' enough to return the majority of them to the windward side of the island.
#' However, after the second day the processing on the Leeward side has 
#' transitioned to CIS screening interviews.  These interviews
#' are not efficient enough to process migrants as fast as the boats
#' are bringing them, and the number of migrants on the leeward
#' side overflows.
#' 

#+ echo=FALSE, fig.width=6, fig.height=5

w <- which(resources$resource == "leeward.counter")
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
    y = server
  )
) + 
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Queue Length",
    title = "Migrants on Leeward Side"
  ) 
plot(g)

#' ## Processing Queue Length and Waiting Times
#' 
#' Queue lengths for the ICE processing and CIS screening provide
#' additional insight into the migrant flow through the 
#' Leeward side of the island.  These queue lengths, over time,
#' are depicted below.  The wait-time histogram shows that
#' the waiting times for CIS screening are relatively evenly 
#' distributed, up to 120 hours.

#+ echo=FALSE, fig.width=6, fig.height=5
w <- which(resources$resource %in% c("ICE.agent","CIS.screener"))
df <- create_steps(
  resources[w,],
  "time",
  "queue",
  "resource"
)

g <- ggplot(
  data = df,
  mapping = aes(
    x = time,
    y = queue,
    color = resource,
    group = resource
  )
) + 
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Queue Length",
    title = "Processing Queue Lengths"
  ) +
  scale_color_discrete(
    breaks = c(
      "CIS.screener",
      "ICE.agent"
    ),
    labels = c(
      "CIS Screening",
      "ICE/Camp Inprocessing"
    ),
    name = "Process"
  )

plot(g)

a <- get_mon_arrivals(env,per_resource=TRUE)
w1 <- grep("CIS_",a$resource,fixed=TRUE)
w2 <- grep("Inprocessing_",a$resource,fixed=TRUE)
d <- a[c(w1,w2),]
w1 <- grep("CIS_",d$resource,fixed=TRUE)
w2 <- grep("Inprocessing_",d$resource,fixed=TRUE)
d$q <- "Inprocessing"
d$q[w1] <- "CIS"
d$waiting.time <- (d$end_time - d$start_time)

g <- ggplot(
  data = d,
  mapping = aes(
    x = waiting.time,
    fill = q,
    group = q
  )
) + 
  geom_histogram(bins=30) +
  labs(
    x = "Waiting Time (hours)",
    y = "Migrant Frequency",
    title = "Histogram of Inprocessing Waiting Times"
  ) +
  scale_fill_discrete(
    breaks = c(
      "CIS",
      "Inprocessing"
    ),
    labels = c(
      "CIS Screening",
      "ICE/Camp Inprocessing"
    ),
    name = "Process"
  )

plot(g)



#' ## Camp Population growth
#' 
#' The final plot shows the rate of camp growth, over time, as
#' the migrant surge returns from CIS screening.  This model
#' assumes no repatriation or resettlement during the time period
#' shown.  Essentially, after the third day migrants begin
#' returning from CIS screening in regular ferry loads.  It
#' takes about 10-12 days to process all of the new arrivals,
#' under the current assumptions.


#+ echo=FALSE, fig.width=6, fig.height=5

a <- get_mon_arrivals(env)
a <- a[order(a$end_time),]
a$count <- 1:nrow(a)
a$time <- a$end_time/24
df <- create_steps(
  a,
  "time",
  "count",
  "replication"
)
g <- ggplot(
  data = df,
  mapping = aes(
    x = time,
    y = count
  )
) +
  geom_line() +
  labs(
    y = "Camp Population",
    x = "Time (days)",
    title = "Repatriation/Resettlement Camp Population Growth"
  )

plot(g)


#' # Conclusion
#' 
#' Under the modeling assumptions and inputs in this simulation, 
#' he surge of migrants causes the queues on the leeward
#' side of the island to overflow.  There are several changes
#' that could help alleviate this problem:
#' 
#' * Move the CIS screening to the windward side of the island.
#' * Extend the processing time by only moving migrants to
#' the leeward side when capacity opens up as a result of 
#' completed screnings.
#' * Increase the number of CIS screeners to increase throughput.
#' 
#' Another important point is the inefficiency in the current 
#' approach.  In this model, all migrants completed inprocessing
#' and returned to the windward side, before returning to the 
#' leeward side for CIS screening.  As a result, CIS screeners
#' were idle for two days before the first migrants were
#' able to make it through the initial inprocessing and ferry
#' queues.  Naturally, it would be better to keep the CIS 
#' screeners continuously engaged.  However, even with this 
#' adjustment some other changes will be required, such as those
#' suggested above, to prevent capacity overflow on the leeward
#' side.

