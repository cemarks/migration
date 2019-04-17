
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


#### LOAD PACKAGES
library(ggplot2)
library(simmer.plot)
library(reshape2)

arrival.count <- nrow(get_mon_arrivals(env))

## READ IN and Prep data for Arrivals and Resources  
# resources <- get_mon_resources(env)

arrivals2 <- get_mon_arrivals(env,per_resource = T,ongoing=T)   ## per resource data view

rm(env)

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



## build a dataframe containing both the afloat and nsgb results
afloat.df <- arrivals2[which(arrivals2$resource=="afloat.counter"),]
GTMO.df <- arrivals2[which(arrivals2$resource=="nsgb.counter"),]
max.day <- max(arrivals2$end_day)

for(day in 1:max.day){
  midnight <- day*24
  # afloat.df <- cbind(
  #   afloat.df,
  #   as.numeric(
  #     afloat.df$end_time > midnight & afloat.df$start_time <= midnight
  #   )
  # )
  # names(afloat.df)[ncol(afloat.df)] <- paste("day",day,sep=".")
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
  #theme(panel.background = element_blank())+
  ylim(0,15000)+
  scale_color_manual(name="Location", labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 
  
print(g1)
rm(df.agg,df.agg2,afloat.df,GTMO.df,resource.df,resource.df.melt,g1)
      
#+ echo = FALSE, fig.width=7, fig.height=5
# compute dataframe that shows the end of day totals for Cuba or Haiti both Afloat and GTMO
# max.day <- ceiling(max(resource.df$end_time)/24)
# sums.df <- data.frame(
#   day = rep(1:max.day,4),
#   source = c(rep("Haiti",max.day),rep("Haiti",max.day),rep("Cuba",max.day),rep("Cuba",max.day)),
#   resource = c(rep("Afloat",max.day),rep("GTMO",max.day),rep("Afloat",max.day),rep("GTMO",max.day)),
#   quantity = c(
#     colSums(afloat.df[grep("Haiti",afloat.df$name),grep("day.",names(afloat.df),fixed=TRUE)]),
#     colSums(GTMO.df[grep("Haiti",GTMO.df$name),grep("day.",names(GTMO.df),fixed=TRUE)]),
#     colSums(afloat.df[grep("Cuba",afloat.df$name),grep("day.",names(afloat.df),fixed=TRUE)]),
#     colSums(GTMO.df[grep("Cuba",GTMO.df$name),grep("day.",names(GTMO.df),fixed=TRUE)])
#   )
# )
# 
# g2 <- ggplot(
#   data=sums.df,
#   mapping = aes(
#     x = day,
#     y = quantity,
#     color = source,
#     lty = resource
#   )
# ) + geom_line()
# 
# g2



#' # Daily Repatriation Counts: .

#' This plot shows the total number of NSGB migrants repatriated in the 24-hour period. Repatriation 
#' populations are depicted by their nationality (Cuban, Haitian, Other) as well as the number of 
#' returned 'security risk' personnel who are segregated into a separate holding camp on NSGB.

# tag for deletion:
# arrivals2 <- get_mon_arrivals(env,per_resource = T)   ## per resource data view
# arrivals2$wait_time <- arrivals2$end_time - arrivals2$start_time                                  ## total resource times
# arrivals2$start_day <- ceiling(arrivals2$start_time/24)                                           ## add start day of event
# arrivals2$end_day <- ceiling(arrivals2$end_time/24)                                               ## add end day of event


# add columns for start.day1, end.day1. etc and fill with 0 or 1 for whether the event is active 
# this code takes too much memory for large migrant populations. therefore chunck it over the sources.
# for(day in 1:max.day){
#   midnight <- day*24
#   arrivals2 <- cbind(
#     arrivals2,
#     as.numeric(
#       arrivals2$end_time < midnight & arrivals2$end_time >= (midnight-24)
#     )
#   )
#   names(arrivals2)[ncol(arrivals2)] <- paste("end.day",day,sep=".")
#   arrivals2 <- cbind(
#     arrivals2,
#     as.numeric(
#       arrivals2$start_time < midnight & arrivals2$start_time >= (midnight-24)
#     )
#   )
#   names(arrivals2)[ncol(arrivals2)] <- paste("start.day",day,sep=".")
# }
# # keep first nine columns of arrrival2 and then add the end.day.1 and start.day.1 with value in the rows
# arrivals.melt <- melt(
#   arrivals2,
#   measure.vars = grep("day.",names(arrivals2),fixed = TRUE,value = TRUE)
# )

#### REPAT ANALYSIS ####
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

g2 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
   #geom_point(aes(col=resource)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Repatriations from NSGB", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,100)+
  scale_fill_discrete(name="Activity")
print(g2)

rm(df.agg,g2)

#' # Daily Resettlement Counts: .
#' This plot shows the total number of NSGB migrants resettled in the 24-hour period. Resettled 
#' populations are depicted by their nationality (Cuban, Haitian, Other).  

resources <- c("resettle-Haitian", "resettle-Cuban", "resettle-Other")

reset.all <- arrivals2[which(arrivals2$resource %in% resources),]

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


df <- reset.all.melt[which(grepl("end.day",reset.all.melt$variable)),]
rm(reset.all.melt)
df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)
rm(df)

g3 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Resettlements from NSGB", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,100)+
  scale_fill_discrete(name="Activity")
print(g3)

rm(df.agg,g3)

#' # Daily Interdiction Counts: .
#' This plot shows the total number of migrants interdicted in the 24-hour period. This count includes
#' migrants that were repatriated directly from afloat and migrants that were rescued and await 
#' delivery to NSGB.  

#### Interdictions ####
# counts repat_afloats which are 'automatically returned' and all interdictions that day.  Not an end of day count.

resources <- c("afloat.counter","repat_afloat")

interdictions.all <- arrivals2[which(arrivals2$resource %in% resources),]

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
rm(df)

g4 <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("start.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity")+
  labs(title="Interdictions", x="Day", y="number of migrants") +
  ylim(0,100) +
  scale_fill_discrete(name="Activity")
print(g4)
rm(df.agg,g4)

#' # Afloat-time Analysis: 
#' This plot conveys the waiting times for migrants afloat.  It shows the number of migrants
#' grouped into bins that represent their waiting-times. 

#### AFLOAT TIME Analysis.  Histogram and timeseries.
# First add a column to the dataframe identifying the migrant source.
arrivals2$source <- "Haiti"
arrivals2$source[grep("Cuba",arrivals2$name)] <- "Cuba"


#### build a histogram  ####
# use the single migrant dataframe which is arrivals2 #

# hist(arrivals2$wait_time[which(arrivals2$resource=="afloat.counter")])
# 
# df <- arrivals.melt[which(arrivals.melt$resource=="afloat.counter"),]

g5 <- ggplot(
  data = arrivals2[which(arrivals2$resource=="afloat.counter"),],
  mapping=aes(x=wait_time)
) + 
  geom_histogram(aes(fill=source)) +
  labs(title="Afloat Waiting Times ", x="Time Waiting", y="number of migrants") +
  scale_fill_discrete(name="Departure Source")
print(g5)

rm(g5)

#' ## Mean and MAX Waiting over Time
#' The following pair of plots depict both the Maximum and the Average time afloat for migrants
#' who are delivered to NSGB on the day shown.  

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
     y=wait_time
    )
  ) +
  geom_point()+
  labs(title="Afloat Waiting Times ", x="Arrival Day to NSGB", y="Migrant Max Waiting Time")


a.mean <- aggregate(
  formula = formula(wait_time ~ end_day),
  data = arrivals2[which(arrivals2$resource==resource),],
  FUN = mean
)

g7 <- ggplot(
  data = a.mean,
  mapping = aes(
    x=end_day,
    y=wait_time
  )
) +
  geom_point() +
  labs(title="Afloat Waiting Times ", x="Arrival Day to NSGB", y="Migrant Average Waiting Time")


require(gridExtra)
grid.arrange(g6,g7)

rm(a.mean,g6,g7,a.max)

#### DAILY NSGB POPULATIONS BY CATEGORY: NATIONALITY AND SECURITY.  END OF DAY QUEUE COUNTS   ####
repat.haiti.df <- arrivals2[which(arrivals2$resource=="repat-Haitian"),]
resettle.haiti.df <- arrivals2[which(arrivals2$resource=="resettle-Haitian"),]

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
rm(repat.cuba.df,resettle.cuba.df)
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

### OTHER CAMP POPULATION:
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
rm(repat.other.df,resettle.other.df)

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
  #ylim(0,15000)+
  scale_color_manual(name="Location") + #, labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 
  facet_grid(resource~.)
print(g10)
rm(df.agg5,g10)


### Security Camp

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
  scale_color_manual(name="Location") + #, labels = c("Afloat", "at NSGB"), values = c("blue", "green")) 
  facet_grid(resource~.)
print(g11)

rm(df.agg6,g11)





# AVERAGE AND MAX REPAT/RESETTLE TIMES PER DAY FOR THOSE DEPARTING.


# 
# 
# 
# arr <- get_mon_arrivals(env)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# check_day<- function(df.row, midnight){
#   resource.start <- df.row[2]
#   resource.end <- df.row[3]
# if (resource.start <= midnight && resource.end >= midnight){
#   return(1)
# }else{
#   return(0)
# }
# }
# 
# # df with migrant afloat counter
# # afloat.counter includes time onboard in the boat.area plus transport/offload at NSGB.
# 
# d<- data.frame(matrix(nrow=100,ncol=2),stringsAsFactors = FALSE)
# names(d) <- c("day","number_afloat")
# df <- arrivals2[which(arrivals2$resource == "afloat.counter"),]
#   for (i in 1:100){ 
#     d[i,1] <- i
#     d[i,2] <- sum(as.numeric(df$start_time <= i*24 & df$end_time >i*24))
# }
# # df with migrant on nsgb counter
# d.add<- data.frame(matrix(nrow=100,ncol=1),stringsAsFactors = FALSE)
# names(d.add) <- c("number_nsgb")
# 
# df <- arrivals2[which(arrivals2$resource == "nsgb.counter"),]
#   for (i in 1:100){ 
#     d.add[i,1] <- sum(as.numeric(df$start_time <= i*24 & df$end_time > i*24))
#   }  
#   
# count.midnight <- cbind(d,d.add)                                            ## WIDE format dataframe
# count.midnight.melt <- reshape2::melt(count.midnight,
#                                       id.vars=c("day"),
#                                       variable.name = "resource",
#                                       value.name="count"
#                                       )
# 
# 
# #### BUILD DAILY SUMMARY COUNTS --> repats/resettles; interdicted, dropped NSGB, Conus Landings
# 
# unique(arrivals2$resource)
# j <- data.frame(table(arrivals2$end_day, arrivals2$resource))  # builds a LONG format dataframe
# names(j)<- c("day","resource","count")
# j$day<-as.numeric(j$day)
# 
# ## Note: data does not exist for every day 
# plot.data <- rbind(j,count.midnight.melt)
# tail(plot.data)
# 
# 
# g1 <- ggplot(data=plot.data[which(plot.data$resource=="afloat.counter"),], aes(x=day, y= count))+
#   geom_bar(stat="identity")+
#   labs(title="End of Day Afloat Count", 
#        subtitle="As of Midnight",
#        caption = "daily snapshot taken at midnight") +
#   theme(axis.text.x = element_text(angle=65, vjust=.6, size=7)) 
# 
# g2 <- ggplot(data=plot.data[which(plot.data$resource=="nsgb.counter"),], aes(x=day, y= count))+
#   geom_bar(stat="identity")+
#   labs(title="End of Day NSGB Count", 
#        subtitle="As of Midnight",
#        caption = "daily snapshot taken at midnight") +
#   theme(axis.text.x = element_text(angle=65, vjust=.6, size=7)) 
# 
# 
# #### PLOTS of Waiting Times for each Resource
# resource.list<- unique(arrivals2$resource)
# 
# # build vector of the counters we dont care about
# ignore.waiting <- c(
#   "repat.security",
#   "wait.ferry.to.windward",
#   "wait.ferry.to.leeward",
#   "area_1",
#   "area_2",
#   "area_3",
#   "area_4",
#   "area_5",
#   "inprocessed.counter",
#   "berth",
#   "repat_afloat"
# )
# 
# w <- !(resource.list %in% ignore.waiting)
# plot.vector <- resource.list[w]
# for (item in plot.vector){
#     dev.new()
#     print(ggplot(data=arrivals2[which(arrivals2$resource==item & arrivals2$end_day<100),],aes(x=end_day,y=wait_time))+
#     geom_point()+
#     geom_smooth(method=lm, se=F) + 
#     ggtitle(item)+
#     xlim(0,100)
#   )
# }
# 
# 
# 
# 
# 
# ##### DATA Checks  #####
# 
# 
# #track single  migrant.  Overall time and then by resource.
# arrivals[which(arrivals$name=="Haiti-migrant138"),]
# ab[which(ab$name=="Haiti-migrant138"),]
# table(arrivals)
# 
# 
# e <- arrivals2[which(arrivals2$resource=='transit-to-leeward'),]
# head(e,n=160)
# 
# 
# attributes <- get_mon_attributes(env)
# a <-attributes[which(attributes$key=="ICE.counter"),]
# max(a$value)                                                                            # ID the max number representing folks completing ICE screening. 
# 
# arrivals2[which(arrivals2$name=="Haiti-migrant138"),]
# arrivals2[which(arrivals2$name=="Cuba-migrant624"),]
# arrivals2[which(arrivals2$name=="Cuba-migrant637"),]
# 
# ab <- arrivals2[which(arrivals2$resource=="nsgb.counter"),]
# ab
# head(ab, n=25)
# 
# 
# ## Compute average stats on the resources.  Uses the resources df.
# ## do this for resources of interest that generate a queue.
# 
# queue_state <- head(resources$queue,-1)    #remove last entry to enable differencing
# server_state <- head(resources$server,-1)
# 
# time_state_lasted <- diff(resources$time)  
# time_at_end <- max(resources$time)
# 
# mean_server_activity <- sum(server_state * time_state_lasted)/time_at_end
# mean_waiting_migrants <- sum(queue_state * time_state_lasted)/time_at_end
# 
# 
# 

