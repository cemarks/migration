#' ---
#' title: "Migrant Simulation Analysis"
#' author: "USSOUTHCOM and ARSOUTH ORSAs"
#' date: "15 APR 2019"
#' --- 


#' # Purpose
#' 
#' This document summarizes the output generated from to implementation of the discrete
#' event simulation of migrant flows through multiple processing stations.  
#' This simulation serves several purposes:
#' 
#' * What are the daily number of migrants Afloat and on NSGB. (midnight snapshot)
#' * What are the daily totals associated with the key activities: total interdictions, 
#'   arrivals at NSGB, repatriations/resettlements.
#' * What waiting times are associated with key aspects of the operational processes: time afloat,
#'   maximum waits, average waits.
#' 
#' This simulation is jointly developed by the Operations Research teams at
#' USSOUTHCOM and ARSOUTH.

#+ echo=FALSE, warning=FALSE, message=FALSE, results=FALSE
#### LOAD PACKAGES
library(ggplot2)
library(simmer.plot)
library(reshape2)
library(simmer)

set.seed(123456)

# Set Directory #

model_dir = "/home/cemarks/Projects/migration/new_model"

# Import Model #

source(paste(model_dir,"migrant_model.R",sep="/"))

# Data input #

## Set Input files ##

migrant.datafile <- paste(model_dir,"migrant_inputs.xlsx",sep="/")
areas.datafile <- paste(model_dir,"ship_info.xlsx",sep="/")
processing.datafile <- paste(model_dir,"processing_inputs.xlsx",sep="/")

## Source Data File ##

source(paste(model_dir,"migrant_data_input.R",sep="/"))

# Build model with data (produces "env",total.days) #

## Set Build Parameters ##

total.days <- 365*2
repat.at.sea.prob <- 0.01

## Source Build Script ##

source(paste(model_dir,"migrant_model_build.R",sep="/"))


# Run and analyze #

env %>% run(
  until <- total.days*24
)

#+ echo=FALSE, warning=FALSE, message=FALSE
arrival.count <- nrow(get_mon_arrivals(env))

## READ IN and Prep data for Arrivals and Resources  
# resources <- get_mon_resources(env)

arrivals2 <- get_mon_arrivals(env,per_resource = T)   ## per resource data view


arrivals2$wait_time <- arrivals2$end_time - arrivals2$start_time                                  ## total resource times
arrivals2$start_day <- ceiling(arrivals2$start_time/24)                                           ## add start day of event
arrivals2$end_day <- ceiling(arrivals2$end_time/24)                                               ## add end day of event

# remove resources we dont want need:
unwanted.resources <- c("boat.area_1","boat.area_2","boat.area_3","boat.area_4","boat.area_5", "area_1", "area_2", "area_3", "area_4", "area_5")
arrivals2 <- arrivals2[!(arrivals2$resource %in% unwanted.resources),]

#### RESOURCE ANALYSIS  #####

#### BUILD MIDNIGHT COUNTS -- on NSGB and on AFLOAT
# cycle thru dataframe and detect in which resource phase every migrant was in at midnight

## First resource: afloat.counter
# construct a wide dataframe with the Arrivals2 info plus columns for every day. O or 1 is placed in the column
# reflecting whether that resource was in use at midnight (2400 hours) of that day.

# for(resource in c("afloat.counter","nsgb.counter")){
#   resource.df <- arrivals2[which(arrivals2$resource==resource),]
#   max.day <- ceiling(max(resource.df$end_time)/24)
#   for(day in 1:max.day){
#     midnight <- day*24
#     resource.df <- cbind(
#       resource.df,
#       as.numeric(
#         resource.df$end_time > midnight & resource.df$start_time <= midnight
#       )
#     )
#     names(resource.df)[ncol(resource.df)] <- paste("day",day,sep=".")
#   }
#   
#   day.cols <- grep("day.",names(resource.df),fixed=TRUE)
#   avg.days.usage <- mean(rowSums(resource.df[,day.cols]))
#   cat(avg.days.usage,"\n")
#   dev.new()
#   h <- hist(rowSums(resource.df[,day.cols]),main=resource)
#   midnights.usage <- colSums(resource.df[,day.cols])
#   plot(1:max.day,midnights.usage,main=resource)
#   cat(mean(midnights.usage))
# }


#' # End-of-Day Migrant counts: Afloat and at NSGB.
#' This plot shows the total migrants on maritime vessels and migrants that are at NSGB.
#' (Counts reflects a snapshot taken at midnight daily )

#+ echo=FALSE

## build a dataframe containing both the afloat and nsgb results
afloat.df <- arrivals2[which(arrivals2$resource=="afloat.counter"),]
GTMO.df <- arrivals2[which(arrivals2$resource=="nsgb.counter"),]
max.day <- max(arrivals2$end_day)

for(day in 1:max.day){
  midnight <- day*24
  afloat.df <- cbind(
    afloat.df,
    as.numeric(
      afloat.df$end_time > midnight & afloat.df$start_time <= midnight
    )
  )
  names(afloat.df)[ncol(afloat.df)] <- paste("day",day,sep=".")
  GTMO.df <- cbind(
    GTMO.df,
    as.numeric(
      GTMO.df$end_time > midnight & GTMO.df$start_time <= midnight
    )
  )
  names(GTMO.df)[ncol(GTMO.df)] <- paste("day",day,sep=".")
}

resource.df <- rbind(afloat.df,GTMO.df)

# reshape into a LONG format. Adds a day.1, day.2 columns and value is the 0 or 1. 
resource.df.melt <- melt(
  resource.df,
  measure.vars = grep("day.",names(resource.df),fixed = TRUE,value = TRUE)
)

df.agg2 <- aggregate(
  formula(value ~ variable + resource),
  data = resource.df.melt,
  FUN = sum
)
max.y <- max(df.agg2$value)
g1 <- ggplot(data=df.agg2,
             mapping=aes(x=as.numeric(gsub("day.","",variable,fixed=TRUE)), 
                         y=value, 
                         fill=resource) 
)+
  geom_line(aes(col=resource),size=2) +
  #geom_bar(stat="identity", position="dodge") +
  labs(title="Populations: Afloat and at NGSB", 
       subtitle= "as of 2400 hours each day",
       x="Day", y="number of migrants") +
  theme_bw()+
  ylim(0,max.y)+
  scale_color_manual(name="Location", labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 

print(g1)

rm(df.agg2,afloat.df,GTMO.df,resource.df,resource.df.melt,g1)

#' # Daily Repatriation Counts: .

#' This plot shows the total number of NSGB migrants repatriated in the 24-hour period. Repatriation 
#' populations are depicted by their nationality (Cuban, Haitian, Other) as well as the number of 
#' returned 'security risk' personnel who are segregated into a separate holding camp on NSGB.

#+ echo=FALSE, warning=FALSE, fig.width=7

resources <- c("repat-Haitian", "repat-Cuban", "repat-Other", "repat.security")

repat.all <- arrivals2[which(arrivals2$resource %in% resources),]

# Add start.day and end.day 0 or 1 checks
for(day in 1:max.day){
  midnight <- day*24
  repat.all <- cbind(
    repat.all,
    as.numeric(
      repat.all$end_time < midnight & repat.all$end_time >= (midnight-24)
    )
  )
  names(repat.all)[ncol(repat.all)] <- paste("end.day",day,sep=".")
  repat.all <- cbind(
    repat.all,
    as.numeric(
      repat.all$start_time < midnight & repat.all$start_time >= (midnight-24)
    )
  )
  names(repat.all)[ncol(repat.all)] <- paste("start.day",day,sep=".")
}

# reshape from wide to long format
repat.all.melt <- melt(
  repat.all,
  measure.vars = grep("day.",names(repat.all),fixed = TRUE,value = TRUE)
)
rm(repat.all)

df <- repat.all.melt[which(grepl("end.day",repat.all.melt$variable)),]
rm(repat.all.melt)

df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)
rm(df)
max.y <- max(df.agg$value)
g2 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Repatriations from NSGB", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,max.y)+
  scale_fill_discrete(name="Activity")
print(g2)

rm(df.agg,g2)
#' # Daily Resettlement Counts: .
#' This plot shows the total number of NSGB migrants resettled in the 24-hour period. Resettled 
#' populations are depicted by their nationality (Cuban, Haitian, Other).  

#+ echo=FALSE, warning=FALSE, fig.width=7
resource.list <- c("resettle-Haitian", "resettle-Cuban", "resettle-Other")

reset.all <- arrivals2[which(arrivals2$resource %in% resource.list),]

# Add start.day and end.day 0 or 1 checks
for(day in 1:max.day){
  midnight <- day*24
  reset.all <- cbind(
    reset.all,
    as.numeric(
      reset.all$end_time < midnight & reset.all$end_time >= (midnight-24)
    )
  )
  names(reset.all)[ncol(reset.all)] <- paste("end.day",day,sep=".")
  reset.all <- cbind(
    reset.all,
    as.numeric(
      reset.all$start_time < midnight & reset.all$start_time >= (midnight-24)
    )
  )
  names(reset.all)[ncol(reset.all)] <- paste("start.day",day,sep=".")
}

# reshape from wide to long format
reset.all.melt <- melt(
  reset.all,
  measure.vars = grep("day.",names(reset.all),fixed = TRUE,value = TRUE)
)
rm(reset.all)

df <- reset.all.melt[which(grepl("end.day",reset.all.melt$variable)),]
rm(reset.all.melt)
df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)
rm(df)
max.y <- max(df.agg$value)

g3 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Resettlements from NSGB", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,max.y)+
  scale_fill_discrete(name="Activity")
print(g3)
rm(df.agg,g3)

#' # Daily Interdiction Counts: .
#' This plot shows the total number of migrants interdicted in the 24-hour period. This count includes
#' migrants that were repatriated directly from afloat and migrants that were rescued and await 
#' delivery to NSGB.  

#+ echo=FALSE, warning=FALSE, fig.width=7

#### Interdictions ####
# counts repat_afloats which are 'automatically returned' and all interdictions that day.  Not an end of day count.

resource.list <- c("afloat.counter","repat_afloat")

interdictions.all <- arrivals2[which(arrivals2$resource %in% resource.list),]

# Add start.day and end.day 0 or 1 checks
for(day in 1:max.day){
  midnight <- day*24
  interdictions.all <- cbind(
    interdictions.all,
    as.numeric(
      interdictions.all$end_time < midnight & interdictions.all$end_time >= (midnight-24)
    )
  )
  names(interdictions.all)[ncol(interdictions.all)] <- paste("end.day",day,sep=".")
  interdictions.all <- cbind(
    interdictions.all,
    as.numeric(
      interdictions.all$start_time < midnight & interdictions.all$start_time >= (midnight-24)
    )
  )
  names(interdictions.all)[ncol(interdictions.all)] <- paste("start.day",day,sep=".")
}

# reshape from wide to long format
interdictions.all.melt <- melt(
  interdictions.all,
  measure.vars = grep("day.",names(interdictions.all),fixed = TRUE,value = TRUE)
)
rm(interdictions.all)

df <- interdictions.all.melt[which(grepl("start.day",interdictions.all.melt$variable)),]
rm(interdictions.all.melt)

df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)
max.y <- max(df.agg$value)
rm(df)

g4 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("start.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity")+
  labs(title="Interdictions", x="Day", y="number of migrants") +
  ylim(0,max.y) +
  scale_fill_discrete(name="Activity")
print(g4)
rm(df.agg,g4)

#' # Afloat-time Analysis: 
#' This plot conveys the waiting times for migrants afloat.  It shows the number of migrants
#' grouped into bins that represent their waiting-times. 

#+ echo=FALSE, warning=FALSE, fig.width=7

#### AFLOAT TIME Analysis.  Histogram and timeseries.
# First add a column to the dataframe identifying the migrant source.
arrivals2$source <- "Haiti"
arrivals2$source[grep("Cuba",arrivals2$name)] <- "Cuba"

#### build a histogram  ####
# use the single migrant dataframe which is arrivals2 #

g5 <- ggplot(
  data = arrivals2[which(arrivals2$resource=="afloat.counter"),],
  mapping=aes(x=wait_time/24)
) + 
  geom_histogram(aes(fill=source), binwidth=7) +
  labs(title="Afloat Times of Migrants delivered to NSGB in Days ", 
       x="Time Waiting (days)", 
       y="number of migrants") +
  scale_fill_discrete(name="Departure Source")
print(g5)
rm(g5)

#' ## Mean and MAX Waiting over Time
#' The following pair of plots depict both the Maximum and the Average time afloat for migrants
#' who are delivered to NSGB on the day shown.  

#+ echo=FALSE, warning=FALSE, message=FALSE, fig.width=7
require(gridExtra)

resource <- "afloat.counter"

a.max <- aggregate(
  formula = formula(wait_time ~ end_day),
  data = arrivals2[which(arrivals2$resource==resource),],
  FUN = max
)

g6 <- ggplot(
  data = a.max,
  mapping = aes(
    x=end_day,
    y=wait_time/24
  )
) +
  geom_point()+
  labs(title="Maximum Afloat Times for Migrants arriving that day to NSGB", 
       x="Arrival Day to NSGB", 
       y="Afloat Time (days)"
       )


a.mean <- aggregate(
  formula = formula(wait_time ~ end_day),
  data = arrivals2[which(arrivals2$resource==resource),],
  FUN = mean
)

g7 <- ggplot(
  data = a.mean,
  mapping = aes(
    x=end_day,
    y=wait_time/24
  )
) +
  geom_point() +
  labs(title="Average Afloat Times for Migrants arriving that day to NSGB ", 
       x="Arrival Day to NSGB", 
       y="Afloat Time (days)"
       )

grid.arrange(g6,g7)
rm(a.mean,g6,g7,a.max)

#' # NSGB Camp Population Details
#' This section depicts the end of day counts of migrants on NSGB broken out into the following
#' categories:  
#' 1.Haitians (not protected and awaiting repatriation); 
#' 2.Haitians (protected and awaiting resettlement)
#' 3.Cubans (not protected and awaiting repatriation); 
#' 4.Cubans (protected and awaiting resettlement);
#' 5.Others
#' 6.Security Risks (segregated from general population)
 
#' ## Haitian Populations 
#+ echo=FALSE, warning=FALSE, message=FALSE, fig.width=7

#### DAILY NSGB POPULATIONS BY CATEGORY: NATIONALITY AND SECURITY.  END OF DAY QUEUE COUNTS   ####
repat.haiti.df <- arrivals2[which(arrivals2$resource=="repat-Haitian"),]
resettle.haiti.df <- arrivals2[which(arrivals2$resource=="resettle-Haitian"),]
max.day<- max(arrivals2$end_day)
for(day in 1:max.day){
  midnight <- day*24
  repat.haiti.df <- cbind(
    repat.haiti.df,
    as.numeric(
      repat.haiti.df$end_time > midnight & repat.haiti.df$start_time <= midnight
    )
  )
  names(repat.haiti.df)[ncol(repat.haiti.df)] <- paste("day",day,sep=".")
  resettle.haiti.df <- cbind(
    resettle.haiti.df,
    as.numeric(
      resettle.haiti.df$end_time > midnight & resettle.haiti.df$start_time <= midnight
    )
  )
  names(resettle.haiti.df)[ncol(resettle.haiti.df)] <- paste("day",day,sep=".")
}

depart.haiti.df <- rbind(repat.haiti.df,resettle.haiti.df)

depart.haiti.df.melt <- melt(
  depart.haiti.df,
  measure.vars = grep("day.",names(depart.haiti.df),fixed = TRUE,value = TRUE)
)
rm(depart.haiti.df)
df.agg3 <- aggregate(
  formula(value ~ variable + resource),
  data = depart.haiti.df.melt,
  FUN = sum
)
rm(depart.haiti.df.melt)

# Plot Haitians
g8 <- ggplot(data=df.agg3,
             mapping=aes(x=as.numeric(gsub("day.","",variable,fixed=TRUE)), 
                         y=value, 
                         fill=resource) 
)+
  geom_bar(stat="identity") +
  labs(title="Haitian Populations on NSGB", 
       subtitle= "as of 2400 hours each day",
       x="Day", y="number of migrants") +
  theme_bw()+
  scale_color_manual(name="Location")  +
  facet_grid(resource~.)
print(g8)
rm(df.agg3,g8)

#' ## Cuban Populations 
#+ echo=FALSE, warning=FALSE, message=FALSE, fig.width=7

### CUBA CAMP POPULATION:
repat.cuba.df <- arrivals2[which(arrivals2$resource=="repat-Cuban"),]
resettle.cuba.df <- arrivals2[which(arrivals2$resource=="resettle-Cuban"),]

for(day in 1:max.day){
  midnight <- day*24
  repat.cuba.df <- cbind(
    repat.cuba.df,
    as.numeric(
      repat.cuba.df$end_time > midnight & repat.cuba.df$start_time <= midnight
    )
  )
  names(repat.cuba.df)[ncol(repat.cuba.df)] <- paste("day",day,sep=".")
  resettle.cuba.df <- cbind(
    resettle.cuba.df,
    as.numeric(
      resettle.cuba.df$end_time > midnight & resettle.cuba.df$start_time <= midnight
    )
  )
  names(resettle.cuba.df)[ncol(resettle.cuba.df)] <- paste("day",day,sep=".")
}

depart.cuba.df <- rbind(repat.cuba.df,resettle.cuba.df)

depart.cuba.df.melt <- melt(
  depart.cuba.df,
  measure.vars = grep("day.",names(depart.cuba.df),fixed = TRUE,value = TRUE)
)
rm(depart.cuba.df)
df.agg4 <- aggregate(
  formula(value ~ variable + resource),
  data = depart.cuba.df.melt,
  FUN = sum
)
rm(depart.cuba.df.melt)

## Plot Cubans
g9 <- ggplot(data=df.agg4,
             mapping=aes(x=as.numeric(gsub("day.","",variable,fixed=TRUE)), 
                         y=value, 
                         fill=resource) 
)+
  geom_bar(stat="identity") +
  labs(title="Cuban Populations on NSGB", 
       subtitle= "as of 2400 hours each day",
       x="Day", y="number of migrants") +
  theme_bw()+
  scale_color_manual(name="Location")+
  facet_grid(resource~.) 
print(g9)

rm(df.agg4,g9)

#' ## Other Populations 
#+ echo=FALSE, warning=FALSE, message=FALSE, fig.width=7

repat.other.df <- arrivals2[which(arrivals2$resource=="repat-Other"),]
resettle.other.df <- arrivals2[which(arrivals2$resource=="resettle-Other"),]

for(day in 1:max.day){
  midnight <- day*24
  repat.other.df <- cbind(
    repat.other.df,
    as.numeric(
      repat.other.df$end_time > midnight & repat.other.df$start_time <= midnight
    )
  )
  names(repat.other.df)[ncol(repat.other.df)] <- paste("day",day,sep=".")
  resettle.other.df <- cbind(
    resettle.other.df,
    as.numeric(
      resettle.other.df$end_time > midnight & resettle.other.df$start_time <= midnight
    )
  )
  names(resettle.other.df)[ncol(resettle.other.df)] <- paste("day",day,sep=".")
}

depart.other.df <- rbind(repat.other.df,resettle.other.df)

depart.other.df.melt <- melt(
  depart.other.df,
  measure.vars = grep("day.",names(depart.other.df),fixed = TRUE,value = TRUE)
)
rm(depart.other.df)
df.agg5 <- aggregate(
  formula(value ~ variable + resource),
  data = depart.other.df.melt,
  FUN = sum
)
rm(depart.other.df.melt)
# Plot Others
g10 <- ggplot(data=df.agg5,
              mapping=aes(x=as.numeric(gsub("day.","",variable,fixed=TRUE)), 
                          y=value, 
                          fill=resource) 
)+
  geom_bar(stat="identity") +
  labs(title="'Other' Populations on NSGB", 
       subtitle= "as of 2400 hours each day",
       x="Day", y="number of migrants") +
  theme_bw()+
  scale_color_manual(name="Location") + #, labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 
  facet_grid(resource~.)
print(g10)
rm(df.agg5,g10)

#' ## Security Risk Populations 
#+ echo=FALSE, warning=FALSE, message=FALSE, fig.width=7

sec.camp.df <- arrivals2[which(arrivals2$resource=="security.counter"),]


for(day in 1:max.day){
  midnight <- day*24
  sec.camp.df <- cbind(
    sec.camp.df,
    as.numeric(
      sec.camp.df$end_time > midnight & sec.camp.df$start_time <= midnight
    )
  )
  names(sec.camp.df)[ncol(sec.camp.df)] <- paste("day",day,sep=".")
}

#depart.other.df <- rbind(repat.other.df,resettle.other.df)

sec.camp.df.melt <- melt(
  sec.camp.df,
  measure.vars = grep("day.",names(sec.camp.df),fixed = TRUE,value = TRUE)
)
rm(sec.camp.df)
df.agg6 <- aggregate(
  formula(value ~ variable + resource),
  data = sec.camp.df.melt,
  FUN = sum
)
rm(sec.camp.df.melt)
# Plot Others
g11<- ggplot(data=df.agg6,
             mapping=aes(x=as.numeric(gsub("day.","",variable,fixed=TRUE)), 
                         y=value, 
                         fill=resource) 
)+
  geom_bar(stat="identity") +
  labs(title="'Security Camp' Populations on NSGB", 
       subtitle= "as of 2400 hours each day",
       x="Day", y="number of migrants") +
  theme_bw()+
  #ylim(0,15000)+
  scale_fill_discrete(name="Location")  #, labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 
print(g11)
rm(df.agg6,g11,arrivals2)


#' # NSGB Inprocessing Queue Lengths
#' This section depicts the inprocessing station queue length broken out by family category.
#' 
#+ echo=FALSE, warning=FALSE, fig.width=7

resources <- get_mon_resources(env)
r <- resources[
  which(resources$resource %in% paste("Inprocessing",family.status.probs[,1],sep="_")),
  ]
rm(resources)
r$resource <- factor(
  r$resource,
  levels = rev(
    paste(
      "Inprocessing",
      c(
        "Ch/Accompanied",
        "F/Accompanied",
        "M/Accompanied",
        "Ch/Unaccompanied",
        "F/Unaccompanied",
        "M/Unaccompanied"
      ),
      sep = "_"
    )
  )
)

g12 <- ggplot(
  data = r,
  mapping = aes(
    x = time/24,
    y = server+1,
    color = resource,
    group = resource
  )
) +
  geom_line(size=1) +
  labs(
    x = "Time (days)",
    y = "Quantity",
    title = "Inprocessing Queue Length"
  ) +
  scale_y_continuous(
    trans = "log10"
  ) + 
  scale_color_manual(
    values = c(
      "Inprocessing_Ch/Accompanied" = "black",
      "Inprocessing_F/Accompanied" = "green",
      "Inprocessing_M/Accompanied" = "brown",
      "Inprocessing_Ch/Unaccompanied" = "blue",
      "Inprocessing_F/Unaccompanied" = "orange",
      "Inprocessing_M/Unaccompanied" = "red"
    ),
    breaks = rev(
      c(
        "Inprocessing_Ch/Accompanied",
        "Inprocessing_F/Accompanied",
        "Inprocessing_M/Accompanied",
        "Inprocessing_Ch/Unaccompanied",
        "Inprocessing_F/Unaccompanied",
        "Inprocessing_M/Unaccompanied"
      )
    ),
    labels = rev(
      c(
        "Ch/Accompanied",
        "F/Accompanied",
        "M/Accompanied",
        "Ch/Unaccompanied",
        "F/Unaccompanied",
        "M/Unaccompanied"
      )
    ),
    name = "Family Status"
  )

print(g12)
rm(env, r,g12)