library(readxl)





## Input helper functions & Parameters ##

### check table function ###

check_table <- function(d,names.vector = migrant.sources$Source,error.d="",row1.vector = NULL){
  if(!all(names(d)[2:ncol(d)] %in% c(names.vector,"remarks"))){
    stop(sprintf("Invalid column in %s input data.",error.d))
  }
  if(!is.null(row1.vector)){
    if(!all(as.character(d[,1]) %in% as.character(row1.vector))){
      stop(sprintf("Invalid category in %s data inputs.",error.d))
    }
  }
  for(j in 1:ncol(d)){
    d[which(is.na(d[,j])),j] <- 0
  }
  if(names(d)[1]=='time'){
    d$time <- d$time*24
  }
  return(d)
}

### check pickup areas ###

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


### triangle columns ### 

triangle.server.cols <- c(
  "mode",
  "min",
  "max",
  "server.count",
  "service.hours"
)


## migrant sources ##

migrant.sources <- as.data.frame(
  read_excel(
    migrant.datafile,
    sheet="sources"
  )
)



## migrant generator rate schedules ##
source.rates <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "source.rates")
  ),
  error.d = "migrant source rate",
  names.vector = c(
    "time",
    "source",
    "rate"
  )
)

## family status ##
family.status.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "family.status")
  ),
  error.d = "family status",
  row1.vector = migrant.attributes[['family.status']]
)

## health probabilities ##
health.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "health")
  ),
  error.d = 'health',
  row1.vector = migrant.attributes[["health"]]
)

## nationality probabilities ##
nationality.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "nationality")
  ),
  error.d = 'nationality',
  row1.vector = migrant.attributes[['nationality']]
)

## security risk probabilities ##
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

## protected status probabilities ##
protected.probs <- check_table(
  as.data.frame(
    read_excel(migrant.datafile,sheet = "protected")
  ),
  error.d = 'protected migrant rate',
  row1.vector = migrant.attributes$protected
)

## pickup areas ##

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

## Ship Attributes ##

ship.attributes <- check_table(
  as.data.frame(
    read_excel(
      areas.datafile,
      sheet = "ship.attributes"
    )
  ),
  names.vector <- c(
    "sat.capacity",
    "supersat.capacity",
    "occupied.time",
    "sat.time",
    "supersat.time",
    "offload.time",
    "berth.occupation",
    "recovery.time"
  ),
  error.d = "boat attributes"
)

## Ship Allocation ##

ship.allocation <- check_table(
  as.data.frame(
    read_excel(
      areas.datafile,
      sheet = "ship.allocation"
    )
  ),
  names.vector <- as.character(ship.attributes[,1]),
  error.d = "boat attributes",
  row1.vector = as.character(pickup.areas[,1])
)

## Processing Center Transit ##

proc.params <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "berth.ferry.params"
    )
  ),
  names.vector = c(
    "Value",
    "value"
  ),
  error.d = "processing location parameters",
  row1.vector = c(
    "ferry.count",
    "ferry.capacity",
    "ferry.transit.time",
    "ferry.reset",
    "ferry.service.hours",
    "big.boat.berth.capacity"
  )
)


## Inprocessing ##

inprocessing.schedule <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "inprocessing"
    )
  ),
  names.vector <- as.character(
    paste(
      "inprocessing",
      triangle.server.cols,
      sep="."
    )
  ),
  error.d = "camp inprocessing"
)

## CIS Screening

cis.schedule <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "cis.screening"
    )
  ),
  names.vector <- as.character(
    paste(
      "cis.screening",
      triangle.server.cols,
      sep="."
    )
  ),
  error.d = "CIS screening"
)


## CIS Rescreening ##

cis.rescreening <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "cis.rescreening"
    )
  ),
  names.vector <- c(
    paste(
      "rescreen.wait",
      c(
        "mode",
        "max",
        "min"
      ),
      sep="."
    ),
    "rescreen.rate"
  ),
  error.d = "CIS rescreening"
)


## Health Recovery ##

health.recovery <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "health.recovery"
    )
  ),
  names.vector <- paste(
    "recovery",
    c(
      "mode",
      "max",
      "min"
    ),
    sep="."
  ),
  error.d = "health recovery"
)

## Security Risk Repatriation ##

sec.repat <- check_table(
  as.data.frame(
    read_excel(
      processing.datafile,
      sheet = "repat.sec.risk"
    )
  ),
  names.vector <- paste(
    "sec.risk.repat",
    c(
      "mode",
      "max",
      "min"
    ),
    sep="."
  ),
  error.d = "security risk repatriation"
)
# This datasheet is currently in days.
# Convert to hours.
for(j in setdiff(names(sec.repat),c("time","remarks"))){
  sec.repat[,j] <- sec.repat[,j] * 24
}

## Resettle/Repatriate schedules ##

move.outs <- data.frame(matrix(ncol=6,nrow=0),stringsAsFactors = FALSE)
names(move.outs) <- c(
  "time",
  "mode",
  "min",
  "max",
  "nationality",
  "resettle.repatriate"
)

for(n in nationality.probs[,1]){
  for(r in c("repat","resettle")){
    temp.df <- check_table(
      as.data.frame(
        read_excel(
          processing.datafile,
          sheet = paste(r,n,sep=".")
        )
      ),
      names.vector <- paste(
        n,
        r,
        c(
          "mode",
          "min",
          "max"
        ),
        sep="."
      ),
      error.d = paste(n,r,sep=" ")
    )
    if(n == "Haitian" && r == "repat") df <- temp.df
    names(temp.df) <- c(
      "time",
      "mode",
      "min",
      "max",
      "nationality"
    )
    temp.df$nationality <- rep(n,nrow(temp.df))
    temp.df$resettle.repatriate <- rep(r,nrow(temp.df))
    move.outs <- rbind(move.outs,temp.df)
  }
}


