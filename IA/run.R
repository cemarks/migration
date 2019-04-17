library(simmer)
library(ggplot2)

set.seed(123456)

# Set Directory #

model_dir = "/home/cemarks/Projects/migration/IA"

# Import Model #

source(paste(model_dir,"migrant_model.R",sep="/"))

# Data input #

## Set Input files ##

migrant.datafile <- paste(model_dir,"migrant_inputs.xlsx",sep="/")
areas.datafile <- paste(model_dir,"ship_info.xlsx",sep="/")
processing.datafile <- paste(model_dir,"processing_inputs.xlsx",sep="/")
initial.conditions.datafile <- paste(model_dir,"initialization.xlsx",sep="/")

## Source Data File ##

source(paste(model_dir,"migrant_data_input.R",sep="/"))

# Build model with data (produces "env",total.days) #

## Set Build Parameters ##

total.days <- 30
repat.at.sea.prob <- 0.1

## Source Build Script ##

source(paste(model_dir,"migrant_model_build.R",sep="/"))


# Run and analyze #

env %>% run(
  until <- total.days*24
)

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
w <- which(resources$resource %in% c("nsgb.counter","afloat.counter"))
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
    color=resource,
    group=resource
  )
) + 
  geom_line() +
  labs(
    x = "Time (days)",
    y = "Migrants Afloat",
    title = "Migrant Locations Over Time"
  ) +
  scale_color_discrete(
    breaks = c(
      "afloat.counter",
      "nsgb.counter"
    ),
    labels = c(
      "Afloat",
      "GTMO"
    )
  ) +
  scale_x_continuous(
    labels = function(x) return(x+15)
  ) +
  theme(
    legend.text = element_text(size=15),
    legend.title = element_text(size=16),
    axis.text = element_text(size = 15),
    title = element_text(size = 22)
  )

plot(g)

# png("F:/Desktop/migrant_rates.png",width=800,height=600)
# plot(g)
# dev.off()



