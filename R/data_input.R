## Input helper functions & Parameters ##

### check table function ###

check_table <- function(d, names.vector, error.d="", row1.vector = NULL){
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

check_pickup_areas <- function(d,migrant.sources,names.vector,error.d="consolidation area"){
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


#' Read Migrant Inputs
#' 
#' Read migrant inputs from Excel file
#' 
#' The tables must be formatted and labeled according to the
#' examples provided with this package.  
#' 
#' @param migrant.datafile character name of Excel file to read.
#' @return list of data.frame objects containing structure migrant
#' input data:
#' \itemize{
#'   \item migrant.sources
#'   \item source.rates
#'   \item family.status.probs
#'   \item health.probs
#'   \item nationality.probs
#'   \item security.risk.probs
#'   \item protected.probs
#' }
#' @seealso \code{\link{ship_area_inputs}}, 
#' \code{\link{processing_center_inputs}}
#' @export
#' @examples
#' migrant.input.file <- system.file(
#'   "extdata",
#'   "migrant_arrivals_inputs.xlsx",
#'   package="migration",
#'   mustWork=TRUE
#' )
#' mig.input.list <- migrant_inputs(migrant.input.file)
migrant_inputs <- function(migrant.datafile){
  migrant.sources <- as.data.frame(
    readxl::read_excel(
      migrant.datafile,
      sheet="sources"
    )
  )
  source.rates <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "source.rates")
    ),
    names.vector = c(
      "time",
      "source",
      "rate"
    ),
    error.d = "migrant source rate"
  )
  # Convert rate to per hour
  source.rates$rate <- source.rates$rate/24
  family.status.probs <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "family.status")
    ),
    names.vector = migrant.sources$Source,
    error.d = "family status",
    # row1.vector = migrant.attributes[['family.status']]
  )
  health.probs <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "health")
    ),
    names.vector = migrant.sources$Source,
    error.d = 'health',
    # row1.vector = migrant.attributes[["health"]]
  )
  nationality.probs <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "nationality")
    ),
    names.vector = migrant.sources$Source,
    error.d = 'nationality',
    # row1.vector = migrant.attributes[['nationality']]
  )
  security.risk.probs <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "security.risk")
    ),
    names.vector = migrant.sources$Source,
    error.d = 'security risk',
    # row1.vector = migrant.attributes[['family.status']]
  )
  security.risk.probs <- security.risk.probs[
    match(
      as.character(family.status.probs[,1]),
      as.character(security.risk.probs[,1])
    ),
  ]
  protected.probs <- check_table(
    as.data.frame(
      readxl::read_excel(migrant.datafile,sheet = "protected")
    ),
    names.vector = migrant.sources$Source,
    error.d = 'protected migrant rate',
    # row1.vector = migrant.attributes$protected
  )
  return(
    list(
      migrant.sources = migrant.sources,
      source.rates = source.rates,
      family.status.probs = family.status.probs,
      health.probs = health.probs,
      nationality.probs = nationality.probs,
      security.risk.probs = security.risk.probs,
      protected.probs = protected.probs
    )
  )
}

#' Read Ship/Area Inputs
#' 
#' Read ship and pickup area inputs from Excel file
#' 
#' The tables must be formatted and labeled according to the
#' examples provided with this package.  
#' 
#' @param areas.datafile character name of Excel file to read.
#' @param migrant.sources data.frame read in using 
#' \code{\link{migrant_inputs}}
#' @return list of data.frame objects containing structure migrant
#' input data:
#' \itemize{
#'   \item pickup.areas
#'   \item ship.attributes
#'   \item ship.allocation
#' }
#' @seealso \code{\link{migrant_inputs}}, 
#' \code{\link{processing_center_inputs}}
#' @export
#' @examples
#' migrant.input.file <- system.file(
#'   "extdata",
#'   "migrant_arrivals_inputs.xlsx",
#'   package="migration",
#'   mustWork=TRUE
#' )
#' ship.input.file <- system.file(
#'   "extdata",
#'   "ship_transit_inputs.xlsx",
#'   package="migration",
#'   mustWork=TRUE
#' )
#' mig.input.list <- migrant_inputs(migrant.input.file)
#' ship.input.list <- ship_area_inputs(
#'   ship.input.file,
#'   mig.input.list$migrant.sources
#' )
ship_area_inputs <- function(areas.datafile,migrant.sources){
  pickup.areas <- check_pickup_areas(
    as.data.frame(
      readxl::read_excel(
        areas.datafile,
        sheet = "pickup.areas"
      )
    ),
    migrant.sources = migrant.sources,
    names.vector = c(
      as.character(migrant.sources$Source),
      "migrant.timeout",
      "timeout.action",
      "transit.time"
    )
  )
  ship.attributes <- check_table(
    as.data.frame(
      readxl::read_excel(
        areas.datafile,
        sheet = "ship.attributes"
      )
    ),
    names.vector = c(
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
  ship.allocation <- check_table(
    as.data.frame(
      readxl::read_excel(
        areas.datafile,
        sheet = "ship.allocation"
      )
    ),
    names.vector = as.character(ship.attributes[,1]),
    error.d = "boat attributes",
    row1.vector = as.character(pickup.areas[,1])
  )
  return(
    list(
      pickup.areas = pickup.areas,
      ship.attributes = ship.attributes,
      ship.allocation = ship.allocation
    )
  )
}

#' Read Migrant Processing Inputs
#' 
#' Read migrant processing center and camp inputs from Excel file
#' 
#' The tables must be formatted and labeled according to the
#' examples provided with this package.  
#' 
#' @param processing.datafile character name of Excel file to read.
#' @param nationality.probs data.frame read in using 
#' \code{\link{migrant_inputs}}
#' @return list of data.frame objects containing structure migrant
#' input data:
#' \itemize{
#'   \item proc.params
#'   \item inprocessing.schedule
#'   \item cis.schedule
#'   \item cis.rescreening
#'   \item health.recovery
#'   \item sec.repat
#'   \item move.outs
#' }
#' @seealso \code{\link{migrant_inputs}}, 
#' \code{\link{ship_area_inputs}}
#' @export
#' @examples
#' migrant.input.file <- system.file(
#'   "extdata",
#'   "migrant_arrivals_inputs.xlsx",
#'   package="migration",
#'   mustWork=TRUE
#' )
#' proc.input.file <- system.file(
#'   "extdata",
#'   "camp_processing_inputs.xlsx",
#'   package="migration",
#'   mustWork=TRUE
#' )
#' mig.input.list <- migrant_inputs(migrant.input.file)
#' proc.input.list <- processing_center_inputs(
#'   proc.input.file,
#'   mig.input.list$nationality.probs
#' )
processing_center_inputs <- function(processing.datafile,nationality.probs){
  triangle.server.cols <- c(
    "mode",
    "min",
    "max",
    "server.count",
    "service.hours"
  )
  proc.params <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  inprocessing.schedule <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  cis.schedule <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  cis.rescreening <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  health.recovery <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  sec.repat <- check_table(
    as.data.frame(
      readxl::read_excel(
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
  for(j in setdiff(names(sec.repat),c("time","remarks"))){
    sec.repat[,j] <- sec.repat[,j] * 24
  }
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
          readxl::read_excel(
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
  return(
    list(
      proc.params = proc.params,
      inprocessing.schedule = inprocessing.schedule,
      cis.schedule = cis.schedule,
      cis.rescreening = cis.rescreening,
      health.recovery = health.recovery,
      sec.repat = sec.repat,
      move.outs = move.outs
    )
  )
}

