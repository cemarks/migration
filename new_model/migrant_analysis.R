#### LOAD PACKAGES
library(ggplot2)
library(simmer.plot)
library(reshape2)

#### BUILD FUNCTIONS TO PRODUCE PLOTS

#1. Resource Analysis
# a.  by type of resource: area, boat.area, repat, resettle, ferry transits, CIS.screener, ICE.agent
# b.  produce a 'usage plot' and a 'utilization' plot
# c. queue sizes
# d. wait times

#2. Migrant analysis (arrivals):
# a. flow_time
# b. activity_time
# c. waiting_time
# d. average wait time


## READ IN and Prep data for Arrivals and Resources  
# resources <- get_mon_resources(env)

arrivals2 <- get_mon_arrivals(env,per_resource = T)   ## per resource data view

# w <- which(arrivals2$name=="Haiti-migrant0")
# arrivals2[w,]

arrivals2$wait_time <- arrivals2$end_time - arrivals2$start_time                                  ## total resource times
arrivals2$start_day <- ceiling(arrivals2$start_time/24)                                           ## add start day of event
arrivals2$end_day <- ceiling(arrivals2$end_time/24)                                               ## add end day of event


#### RESOURCE ANALYSIS  #####
unique(arrivals2$resource)


### Not coded:  remove items we do not need in generating products
 
# resources.unwanted <- c(
#   "area_1",
#   "area_2",
#   "area_3",
#   "area_4",
#   "area_5"
# )

#### BUILD MIDNIGHT COUNTS -- on NSGB and on AFLOAT
# cycle thru dataframe and detect in which resource phase every migrant was in at midnight

## First resource: afloat.counter
# construct a wide dataframe with the Arrivals2 info plus columns for every day. O or 1 is placed in the column
# reflecting whether that resource was in use at midnight (2400 hours) of that day.

for(resource in c("afloat.counter","nsgb.counter")){
  resource.df <- arrivals2[which(arrivals2$resource==resource),]
  max.day <- ceiling(max(resource.df$end_time)/24)
  for(day in 1:max.day){
    midnight <- day*24
    resource.df <- cbind(
      resource.df,
      as.numeric(
        resource.df$end_time > midnight & resource.df$start_time <= midnight
      )
    )
    names(resource.df)[ncol(resource.df)] <- paste("day",day,sep=".")
  }
  
  day.cols <- grep("day.",names(resource.df),fixed=TRUE)
  avg.days.usage <- mean(rowSums(resource.df[,day.cols]))
  cat(avg.days.usage,"\n")
  dev.new()
  h <- hist(rowSums(resource.df[,day.cols]),main=resource)
  midnights.usage <- colSums(resource.df[,day.cols])
  plot(1:max.day,midnights.usage,main=resource)
  cat(mean(midnights.usage))
}

# OPTION 2.  BUild separate NSGB and AFLOAT dataframes and then bind them

## build a dataframe containing both the afloat and nsgb results
afloat.df <- arrivals2[which(arrivals2$resource=="afloat.counter"),]
GTMO.df <- arrivals2[which(arrivals2$resource=="nsgb.counter"),]

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


# compute dataframe that shows the end of day totals for Cuba or Haiti both Afloat and GTMO
max.day <- ceiling(max(resource.df$end_time)/24)
sums.df <- data.frame(
  day = rep(1:max.day,4),
  source = c(rep("Haiti",max.day),rep("Haiti",max.day),rep("Cuba",max.day),rep("Cuba",max.day)),
  resource = c(rep("Afloat",max.day),rep("GTMO",max.day),rep("Afloat",max.day),rep("GTMO",max.day)),
  quantity = c(
    colSums(afloat.df[grep("Haiti",afloat.df$name),grep("day.",names(afloat.df),fixed=TRUE)]),
    colSums(GTMO.df[grep("Haiti",GTMO.df$name),grep("day.",names(GTMO.df),fixed=TRUE)]),
    colSums(afloat.df[grep("Cuba",afloat.df$name),grep("day.",names(afloat.df),fixed=TRUE)]),
    colSums(GTMO.df[grep("Cuba",GTMO.df$name),grep("day.",names(GTMO.df),fixed=TRUE)])
  )
)

g <- ggplot(
  data=sums.df,
  mapping = aes(
    x = day,
    y = quantity,
    color = source,
    lty = resource
  )
) + geom_line()

g


#### OPTION B:  Make big melted resource df.

arrivals2 <- get_mon_arrivals(env,per_resource = T)   ## per resource data view

# w <- which(arrivals2$name=="Haiti-migrant0")
# arrivals2[w,]

arrivals2$wait_time <- arrivals2$end_time - arrivals2$start_time                                  ## total resource times
arrivals2$start_day <- ceiling(arrivals2$start_time/24)                                           ## add start day of event
arrivals2$end_day <- ceiling(arrivals2$end_time/24)                                               ## add end day of event

# add columns for start.day1, end.day1. etc and fill with 0 or 1 for whether the event is active 
for(day in 1:max.day){
  midnight <- day*24
  arrivals2 <- cbind(
    arrivals2,
    as.numeric(
      arrivals2$end_time < midnight & arrivals2$end_time >= (midnight-24)
    )
  )
  names(arrivals2)[ncol(arrivals2)] <- paste("end.day",day,sep=".")
  arrivals2 <- cbind(
    arrivals2,
    as.numeric(
      arrivals2$start_time < midnight & arrivals2$start_time >= (midnight-24)
    )
  )
  names(arrivals2)[ncol(arrivals2)] <- paste("start.day",day,sep=".")
}
# keep first nine columns of arrrival2 and then add the end.day.1 and start.day.1 with value in the rows
arrivals.melt <- melt(
  arrivals2,
  measure.vars = grep("day.",names(arrivals2),fixed = TRUE,value = TRUE)
)

#### REPAT ANALYSIS ####

resources <- c("repat-Haitian", "repat-Cuban", "repat-Other", "repat.security")
df <- arrivals.melt[which(arrivals.melt$resource %in% resources & grepl("end.day",arrivals.melt$variable)),]
df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)

g <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
   #geom_point(aes(col=resource)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="repatriations", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,100)+
  scale_fill_discrete(name="Activity")
g


#### RESETTLE ANALYSIS #####

resources <- c("resettle-Haitian", "resettle-Cuban", "resettle-Other")
df <- arrivals.melt[which(arrivals.melt$resource %in% resources & grepl("end.day",arrivals.melt$variable)),]
df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)

g <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("end.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="resettlements", x="Day", y="number of migrants", legend="Activity") +
  ylim(0,100)+
  scale_fill_discrete(name="Activity")
g




#### Interdictions ####
# counts repat_afloats which are 'automatically returned' and all interdictions that day.  Not an end of day count.

resources <- c("afloat.counter","repat_afloat")
df <- arrivals.melt[which(arrivals.melt$resource %in% resources & grepl("start.day",arrivals.melt$variable)),]
df.agg <- aggregate(
  formula(value ~ variable + resource),
  data = df,
  FUN = sum
)

g <- ggplot(
  data = df.agg,
  mapping=aes(x=as.numeric(gsub("start.day.","",variable,fixed=TRUE)), y=value, fill=resource)
) + 
  geom_bar(stat="identity")+
  labs(title="interdictions", x="Day", y="number of migrants") +
  ylim(0,100) +
  scale_fill_discrete(name="Activity")
g




#### AFLOAT TIME Analysis.  Histogram and timeseries.
# First add a column to the dataframe identifying the migrant source.
arrivals2$source <- "Haiti"
arrivals2$source[grep("Cuba",arrivals2$name)] <- "Cuba"

## alternative method
id_source <- function (migrant.name){
  if(grepl("Cuba", migrant.name)){
    return("Cuba")
  }else{
    return("Haiti")
  }
}

arrivals.melt$source <- sapply(arrivals.melt$name, id_source)
  

#### build a histogram  ####
# use the single migrant dataframe which is arrivals2 #

hist(arrivals2$wait_time[which(arrivals2$resource=="afloat.counter")])

df <- arrivals.melt[which(arrivals.melt$resource=="afloat.counter"),]

g <- ggplot(
  data = arrivals2[which(arrivals2$resource=="afloat.counter"),],
  mapping=aes(x=wait_time)
) + 
  geom_histogram(aes(fill=source)) +
  labs(title="Afloat Waiting Times ", x="Time Waiting", y="number of migrants") +
  #ylim(0,500) +
  scale_fill_discrete(name="Departure Source")
g


####  Mean and MAX Waiting time  ######
resource <- "afloat.counter"

a <- aggregate(
  formula = formula(wait_time ~ end_day),
  data = arrivals2[which(arrivals2$resource==resource),],
  FUN = max
)

plot(a$end_day,a$wait_time)

g <- ggplot(
  data = a,
  mapping = aes(
    x=end_day,
    y=wait_time
  )
) +
    geom_point()
  
plot(g)


str(a)


## PRODUCE FACET CHART BY SOURCE WITH BOTH LINES ON IT.

## PRODUCE A GTMO GRAPHIC AT END OF DAY THAT SHOWS NATIONLITY - SIZE OF REPAT QUEUES.





















check_day<- function(df.row, midnight){
  resource.start <- df.row[2]
  resource.end <- df.row[3]
if (resource.start <= midnight && resource.end >= midnight){
  return(1)
}else{
  return(0)
}
}

# df with migrant afloat counter
# afloat.counter includes time onboard in the boat.area plus transport/offload at NSGB.

d<- data.frame(matrix(nrow=100,ncol=2),stringsAsFactors = FALSE)
names(d) <- c("day","number_afloat")
df <- arrivals2[which(arrivals2$resource == "afloat.counter"),]
  for (i in 1:100){ 
    d[i,1] <- i
    d[i,2] <- sum(as.numeric(df$start_time <= i*24 & df$end_time >i*24))
}
# df with migrant on nsgb counter
d.add<- data.frame(matrix(nrow=100,ncol=1),stringsAsFactors = FALSE)
names(d.add) <- c("number_nsgb")

df <- arrivals2[which(arrivals2$resource == "nsgb.counter"),]
  for (i in 1:100){ 
    d.add[i,1] <- sum(as.numeric(df$start_time <= i*24 & df$end_time > i*24))
  }  
  
count.midnight <- cbind(d,d.add)                                            ## WIDE format dataframe
count.midnight.melt <- reshape2::melt(count.midnight,
                                      id.vars=c("day"),
                                      variable.name = "resource",
                                      value.name="count"
                                      )


#### BUILD DAILY SUMMARY COUNTS --> repats/resettles; interdicted, dropped NSGB, Conus Landings

unique(arrivals2$resource)
j <- data.frame(table(arrivals2$end_day, arrivals2$resource))  # builds a LONG format dataframe
names(j)<- c("day","resource","count")
j$day<-as.numeric(j$day)

## Note: data does not exist for every day 
plot.data <- rbind(j,count.midnight.melt)
tail(plot.data)


g1 <- ggplot(data=plot.data[which(plot.data$resource=="afloat.counter"),], aes(x=day, y= count))+
  geom_bar(stat="identity")+
  labs(title="End of Day Afloat Count", 
       subtitle="As of Midnight",
       caption = "daily snapshot taken at midnight") +
  theme(axis.text.x = element_text(angle=65, vjust=.6, size=7)) 

g2 <- ggplot(data=plot.data[which(plot.data$resource=="nsgb.counter"),], aes(x=day, y= count))+
  geom_bar(stat="identity")+
  labs(title="End of Day NSGB Count", 
       subtitle="As of Midnight",
       caption = "daily snapshot taken at midnight") +
  theme(axis.text.x = element_text(angle=65, vjust=.6, size=7)) 


#### PLOTS of Waiting Times for each Resource
resource.list<- unique(arrivals2$resource)

# build vector of the counters we dont care about
ignore.waiting <- c(
  "repat.security",
  "wait.ferry.to.windward",
  "wait.ferry.to.leeward",
  "area_1",
  "area_2",
  "area_3",
  "area_4",
  "area_5",
  "inprocessed.counter",
  "berth",
  "repat_afloat"
)

w <- !(resource.list %in% ignore.waiting)
plot.vector <- resource.list[w]
for (item in plot.vector){
    dev.new()
    print(ggplot(data=arrivals2[which(arrivals2$resource==item & arrivals2$end_day<100),],aes(x=end_day,y=wait_time))+
    geom_point()+
    geom_smooth(method=lm, se=F) + 
    ggtitle(item)+
    xlim(0,100)
  )
}





##### DATA Checks  #####


#track single  migrant.  Overall time and then by resource.
arrivals[which(arrivals$name=="Haiti-migrant138"),]
ab[which(ab$name=="Haiti-migrant138"),]
table(arrivals)


e <- arrivals2[which(arrivals2$resource=='transit-to-leeward'),]
head(e,n=160)


attributes <- get_mon_attributes(env)
a <-attributes[which(attributes$key=="ICE.counter"),]
max(a$value)                                                                            # ID the max number representing folks completing ICE screening. 

arrivals2[which(arrivals2$name=="Haiti-migrant138"),]
arrivals2[which(arrivals2$name=="Cuba-migrant624"),]
arrivals2[which(arrivals2$name=="Cuba-migrant637"),]

ab <- arrivals2[which(arrivals2$resource=="nsgb.counter"),]
ab
head(ab, n=25)


## Compute average stats on the resources.  Uses the resources df.
## do this for resources of interest that generate a queue.

queue_state <- head(resources$queue,-1)    #remove last entry to enable differencing
server_state <- head(resources$server,-1)

time_state_lasted <- diff(resources$time)  
time_at_end <- max(resources$time)

mean_server_activity <- sum(server_state * time_state_lasted)/time_at_end
mean_waiting_migrants <- sum(queue_state * time_state_lasted)/time_at_end



