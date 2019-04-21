## Model Build functions

## Migrant Arrivals

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

