## Understanding who is atteding DS DC meetups
# I'm trying to understand who is attending the DataScience DC meetups with
# intent to vizualize it for accurate understanding about the popularity
# of meetups. I have a few initial questions:
# 
# 1. How quickly does each meetup reach it's RSVP limit and how does that 
# compare to the topics and the venue? e.g., do Google-hosted events fill up 
# faster than the HelloWallet-hosted events? << we may not want to know!
# 
# 2. What about the rates of replies independent of the venue; perhaps by the 
# day of the week the meetup occurs?


## Here are the tools we'll use to manipulate and visualize the data
library(stringr)
library(ggplot2)
library(plyr)
library(Hmisc)

## ...and here's the data
# Import the registraiton/response data given to us by DataScience DC
d <- read.csv("~/Documents/School/CSI773_Carr/dsdcProject/RegistrationData.csv", 
              as.is=T, 
              colClasses=c(rsvp_id="character", 
                           member_id="character", 
                           event_id="character"))

# Get more data on the events to know th econtext; import the event data
e <- read.csv("~/Documents/School/CSI773_Carr/dsdcProject/DSDCevents.csv")
ebig <- e
eplus <- subset(ebig, select=-description)

# We need to know which events are in our registration/response data; make
# certain that we have all of them and exclude events for which there is no
# response data. 
events <- d$event_id
ec <- e$id

setdiff(events, ec) # This seems to test one direction and not the other setdiff(ec, events) # so check each subset; good to know!
dif <- setdiff(ec, events) 
keepers <- apply(eplus, 1, function(x) !any(x %in% dif))
tossers <- apply(eplus, 1, function(x)  any(x %in% dif))
e <- eplus[keepers,]
eminus <- eplus[tossers,]
# We have 17 in common. Missing events are 3 evnts listed in 'dif':
# [1] Seminar on Bayesian Inference (full day event!)
# [2] PAW-Gov Data Drinks                            
# [3] Workshop: Number Crunching in Python with NumPy
# Fair enough. The Bayes seminar was all day (had a cost, was not in DC, ...); the 
# PAW-Gov Data Drinks was just a social event after a conference that most of us
# didn't attend; and, thw NumPy was acually hosted by the DC Python group at GWU 
# and wasn't announced on the DSDC list until after it was full (humph!)
# Let's look more closely at this event data:
sapply(e, mode)
sapply(e, class)

# But, some of those mode types should be characters to make for better labeling
E <- transform(e, venue_id = as.character(venue_id), 
               venue_zip = as.character(venue_zip),
               venue_name = as.character(venue_name),      
               venue_state     =   as.character(venue_state),
               venue_address_1 =   as.character(venue_address_1),
               venue_city      =   as.character(venue_city),
               venue_country   =   as.character(venue_country),
               id              =   as.character(id),
               waitlist_count  =   as.character(waitlist_count),
               event_url       =   as.character(event_url),
               how_to_find_us  =   as.character(how_to_find_us),
               name            =   as.character(name),
               group_id        =   as.character(group_id),
               group_name      =   as.character(group_name),
               group_join_mode =   as.character(group_join_mode),
               group_urlname   =   as.character(group_urlname),
               group_who       =   as.character(group_who))
# Did it work?
sapply(E, mode)
sapply(E, class)

# It turns out that 'SRA Touchstone Consulting Group' may be a nice name for 
# a company but, it takes up too much space in the label; so I'm renaming it 
# 'SRA' to save space and hope they will understand.
E$venue_name[8] <- "SRA"
E$venue_name[9] <- "SRA"
# Looks better. Now let's do some analysis


# Rate of fill (how optimistic are people that they will show up?)
E$fill <- E$rsvp_limit/E$yes_rsvp_count # pretty optimistic
E$fill# How much did they actually show
E$fill_heads <- E$headcount/E$yes_rsvp_count # pretty reasonable
E$fill_heads
plot(E$venue_id, E$fill) # Nope. That's wrong, ugly, and useless; needs factors
plot(as.factor(E$venue_name), E$fill) # Much better. Box plots are interesting
# Typical rough work; now it's time to apply what I've learned

# This is typically what we get at Meetup intro briefings.
# ...but does not give the context of rsvp limits.
d$idfac <- as.factor(d$event_id)
qplot(idfac, data=d, geom="bar",
      main="Growth of Meetup Popularity",
      ylab = "Number of 'yes' RSVPs", 
      xlab = "Events") + 
  theme(axis.title.y=element_text(angle=0)) +
  coord_flip() 
# Build the story with this plot


b <- qplot(as.factor(E$venue_name), 
            E$fill, main="Distribution of Data Scientists' Attendence Plans: Percentage of Venue Capacity", 
            xlab = "DataScience DC Venue Hosts", ylab = "%") 
b + theme(axis.title.y=element_text(angle=0))
b + geom_boxplot() + theme(axis.title.y=element_text(angle=0))
# Figure 1: Final

# Ooh! More likely to rsvp at some venues
a <- qplot(as.factor(E$venue_name), 
            E$fill_heads, main="Distribution of Data Scientists' Actual Attendance: Percentage of RSVPs",
            xlab = "DataScience DC Venue Hosts", ylab = "%") # Much better. Box plots are interesting
a + theme(axis.title.y=element_text(angle=0))
a + geom_boxplot() + theme(axis.title.y=element_text(angle=0))
# I'm tempted to assume from the data that the double-occupancy meetup at 
# HelloWallet is bad data but I was there and it was packed to overflowing; 
# several people left early for lack of space to even stand. Who knew regression
# was such a draw?!

# Just for fun, I'm keeping these but the plots work better (especially because of
# the titles) when they are horizontal. Notice the difference in how extreme the 
# one "twice the attendance as rsvp'd" event at HelloWallet seems in each flip
b + coord_flip() + geom_boxplot() #Really doesn't add anything, IMO
a + coord_flip()

# How does early notice (create time - event time) impact rsvp and attendance
mtime <- E$time
# That's pretty difficult to read because the times are more or less meaningless.
# Now starts some drudgery for converting JSON's 'milliseconds since epoch' time
# class to something useful. The trick seems to be this: take the json time and 
# divide it by 1000 (R uses seconds since epoch, not milliseconds), run it through 
# the as.POSIXct funtion using a 01 JAN 1970 origin and the computer's own timezone
# setting; then, alter the structure to use the standard time class (I do not
# understand this step but it seems to be necessary to get the timezone offset 
# to show up correctly). Finally, I reformat the time to give me a weekday so that
# I can look at that as a consideration of optimism and attendance.
as.POSIXct(mtime, origin = "1970-01-01")
E$mtime = structure(mtime/1000,class=c('POSIXt','POSIXct'))
E$mtime
# Checks out. This is a good conversion algorythm

ctime <- E$created
# Repeat the complicated time-cruching...
as.POSIXct(ctime, origin = "1970-01-01")
E$ctime = structure(ctime/1000,class=c('POSIXt','POSIXct'))
E$ctime

E$notime <- E$mtime - E$ctime

compTime <- data.frame(E$mtime, E$ctime, E$notime)
compTime

# Is there any correlation between the amount of notice people get and the 
# number of RSVPs (compared to RSVP limit)
qplot(data=E, mtime, ctime, geom="point",
      main="Regularity of Announcements and Meetups",
      xlab = "Meeting Dates", 
      ylab = "Announcement \nDates") + 
  geom_smooth(method="lm") + 
  theme(axis.title.y=element_text(angle=0))
# by RSVPs
qplot(data=E, 
  as.factor(notime), fill ) + coord_flip() + geom_smooth(aes(group=1))
# by attendance
qplot(data=E, as.factor(notime), fill_heads) + 
  coord_flip() + 
  geom_smooth(aes(group=1))
# Perhaps, but I'll develop it later...


# Now let's take a closer look at the data that the DataSciences DC leaderhsip
# gave us to work with. ...starting with those troublesome dates!
d$created #looks like a character string, not the time-date data above.

checktime<-as.Date(d$created, format="%m/%d/%Y %H:%M")
checktime # better?
# Nope!
checktime <- as.POSIXct(d$created, format="%m/%d/%Y")
checktime # better?
# Nope!
checktime <- strptime(d$time, format ="%m/%d/%Y %H:%M")
checktime
# Eureka!

# Now that we know what works, let's create some columns in 'd' to calculate
# the amount of time it takes people to respond to meetup announcements.

# someday when I know what I'm doing I'll do this with 'apply' but until then...
# I'll make r_time' the response time in 'd' and rsvp_time the moment they responded
# saying that they'd attend (wouldn't it be nice to compare this with how quickly
# people responded saying that they would NOT attend? Alas, I don't have that data.)
d$rsvp_time <- as.POSIXct(strptime(d$created, format ="%m/%d/%Y %H:%M"))
d$rsvp_time

d$rsvp_time[1] - E$ctime[1]
d$r_time <- NULL
for (i in 1:17) {
  for (j in 1:2434) {
    if (d$event_id[j]==E$id[i]) {
      d$r_time[j] <- difftime(d$rsvp_time[j], E$ctime[i], units="days")
      d$mday[j] <- format(E$ctime[i],'%A')
    }
  }
}

structure(d$r_time/1000,class=c('POSIXt','POSIXct'))
d$r_time/1000

## This is a pretty good plot (print at 660x600)
rr <- qplot(r_time, event_id, data=d, geom = "jitter",
      alpha = I(1/5), 
      main="Jittered Response Times in Days for Each Event (From Event Creation to RSVP)",
      xlab = "Days", 
      ylab = "Events")
rr + theme(axis.title.y=element_text(angle=0))
# Final

# This is a good statistical comparrison for the jitter plot; I like jitter better
ggplot(data=ed, aes(x=event_id, 
                    lower=q1,
                    upper=q3, 
                    middle=median, 
                    ymin=min, 
                    ymax=max)) +
  geom_boxplot(stat="bin") +
  ylab("Days") + 
  xlab("Events") + 
  ggtitle("Quantiles for Response Times in Days for Each Event") +
  theme(axis.title.y=element_text(angle=0)) +
  coord_flip()

# Finally, we have the data to the point that we can do some simple comparrisons
# between information about Meetup announcements/events and the opportunity to 
# attend vs. willingness to acually show up.
# Intent is to combine these calculations of the registration data (d) with the
# event data(E) to make 'event registration data' (ed).
evresps <- data.frame(d$event_id, d$r_time)
head(evresps)
rsvpstats <- ddply(evresps, c('d.event_id', 'd.r_time'), 
                   function (x) c(count=nrow(x),
                                  mean=mean(x$d.r_time),
                                  median=median(x$d.r_time),
                                  sd=sd(x$d.r_time)))

esums <- ddply(d, ~event_id, summarise, 
               mean=mean(r_time), 
               median=median(r_time),
               min=min(r_time),
               max=max(r_time),         
               sd=sd(r_time),
               q1=quantile(r_time,1/4),
               q3=quantile(r_time,3/4))
esums

ed <- merge(esums, E, by.x = "event_id", by.y = "id", all = TRUE)
head(ed)


# What if I want to plot the rate of replies to compare the slopes? I need to track
# each rsvp by count. 
d$ith_rsvp[1] <- 1
j = 2
for (i in 2:2434) {
  if (d$event_id[i] == d$event_id[i-1]) {
    d$ith_rsvp[i] <- j
  } else {
    j = 1
    d$ith_rsvp[i] <- j
  }
  j=j+1
}

d$meetday = as.numeric(format(as.POSIXlt(d$time, format="%m/%d/%Y %H:%M"),"%u"))

# Das es Fantastiche! I can see that some days are more popular than others, that
# some events have a steep slope and that some events start off with a rapid rsvp
# rate while others do not. 
# I wish I could order the facets by the date tho...
# d <- within(d, var <- factor(var, levels=f))

## This was going to be an attempt at making more complicated facet labels; to show
# on two lines the event name followed by event id and date. ...WAY too much work
# for too little payoff.

as.POSIXct(ctime, origin = "1970-01-01")
E$ctime = structure(ctime/1000,class=c('POSIXt','POSIXct'))

llply(ed$name, function(x) paste(strwrap(x, width=40), collapse=" \n "))

paste(tw, collapse=" \n ")

wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=44),collapse=" \n ")
  return(wtext)
}

ed$wname <- llply(ed$name, wrapit)
ed$wname <- unlist(ed$wname)


day_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="meetday") {
    value[value==0] <- "Sunday"
    value[value==1] <- "Monday"
    value[value==2] <- "Tueaday"
    value[value==3] <- "Wednesday"
    value[value==4] <- "Thursday"
    value[value==5] <- "Friday"
    value[value==6] <- "Saturday"
  } else {
    xx <- which(ed$event_id==value)
    da <- as.character(structure(ed$time[xx]/1000,class=c('POSIXt','POSIXct')), 
                  format="%m/%d/%Y")
    na <- ed$wname[xx]
    value <-str_c(na, " \n Met on ", da)
    }
  return(value)
}

## Begin the varieties of this plot. 
#  My favorite is 'ip'. This is the money plot. 
#  'ap' is necessary because it is the reference for the log10-scaled charts
#  'lxp' scales to log10 on the x axis
#  'lxyp' scales to log10 on both the x and y axes
#  'lyp' scales to log10 on the y axis
#  I will show all of them but 'lxp' is the best example of something with meaning
##
# best plot size seems to be 600w x 720h
ip <- qplot(offset, ith_rsvp/yes_rsvp_count, data=d,
            geom="point", alpha = I(1/5), 
            main="Rates of Responses by Event \n and by Day of Meetups",
            xlab = "Days from Event Creation until Meetup", 
            ylab = "% of \n RSVP \n Limit") +
  geom_smooth(method = "lm", se = F) + 
  theme(axis.title.y=element_text(angle=0)) +
  theme(strip.text.y=element_text(angle=0, hjust=0, vjust=0.9)) +
  facet_grid(event_id ~ meetday, labeller=day_labeller)  
ip

ap <- qplot(abs(offset), ith_rsvp/yes_rsvp_count, data=d,
             geom="point", alpha = I(1/5), 
             main="Rates of Responses by Event and \n by Day of Meetups: Absolute X Values",
             xlab = "Days from Event Creation until Meetup", 
             ylab = "% of \n RSVP \n Limit") +
  geom_smooth(method = "lm", se = F) + 
  theme(axis.title.y=element_text(angle=0)) +
  theme(strip.text.y=element_text(angle=0, hjust=0, vjust=0.9)) +
  facet_grid(event_id ~ meetday, labeller=day_labeller)  
ap


lxp <- qplot(abs(offset), ith_rsvp/yes_rsvp_count, data=d,
            geom="point", alpha = I(1/5), 
            main="Rates of Responses by Event and \n by Day of Meetups: X Axis as Log Scale",
            xlab = "Days from Event Creation until Meetup", 
            ylab = "% of \n RSVP \n Limit") +
  geom_smooth(method = "lm", se = F) + 
  scale_x_log10() +
  theme(axis.title.y=element_text(angle=0)) +
  theme(strip.text.y=element_text(angle=0, hjust=0, vjust=0.9)) +
  facet_grid(event_id ~ meetday, labeller=day_labeller)  
lxp

lxyp <- qplot(abs(offset), ith_rsvp/yes_rsvp_count, data=d,
             geom="point", alpha = I(1/5), 
             main="Rates of Responses by Event and \n by Day of Meetups at Log-Log Scale",
             xlab = "Days from Event Creation until Meetup", 
             ylab = "% of \n RSVP \n Limit") +
  geom_smooth(method = "lm", se = F) + 
  scale_y_log10() +
  scale_x_log10() +
  theme(axis.title.y=element_text(angle=0)) +
  theme(strip.text.y=element_text(angle=0, hjust=0, vjust=0.9)) +
  facet_grid(event_id ~ meetday, labeller=day_labeller)  
lxyp

lyp <- qplot(abs(offset), ith_rsvp/yes_rsvp_count, data=d,
              geom="point", alpha = I(1/5), 
              main="Rates of Responses by Event and \n by Day of Meetups: Y Axis as Log Scale",
              xlab = "Days from Event Creation until Meetup", 
              ylab = "% of \n RSVP \n Limit") +
  geom_smooth(method = "lm", se = F) + 
  scale_y_log10() +
  theme(axis.title.y=element_text(angle=0)) +
  theme(strip.text.y=element_text(angle=0, hjust=0, vjust=0.9)) +
  facet_grid(event_id ~ meetday, labeller=day_labeller)  
lyp


led <- subset(ed[2:16, ]) #Subset of 'ed' omiting high headcount and future event
str(led) #works!
ledr <- data.frame(eid=ed$event_id[2:16], count=ed$yes_rsvp_count[2:16])
ledr$type <- "RSVP"
str(ledr)
ledh <- data.frame(eid=ed$event_id[2:16], count=ed$headcount[2:16])
str(ledh)
ledh$type <- "Headcount"
ledc <- rbind(ledr, ledh)
str(ledc)

# Thanks to Chase on SO for pointing the way here but I have yet to figure it out!
# (http://stackoverflow.com/questions/5226807/multiple-graphs-in-one-canvas-using-ggplot2)
# cplots <- ggplot(NULL, aes(eid, count)) + 
#   geom_point(data = ledr, color="red") +
#   geom_point(data = ledh, color="blue") +
#   coord_flip()
# cplots
## Not so fast! That's not quite what I want. 


ehcp <- ggplot(ledc, aes(x=eid, y=count, colour=type, group=type)) + 
  geom_point() + 
  geom_smooth(method="loess") + 
  labs(list(title = "Growth in Popularity: RSVPs and Headcounts", 
            x = "Each \n Event \n by ID", 
            y = "Counts",
            colour = "Count Type")) +
  coord_flip() +
  theme(axis.title.y=element_text(angle=0)) +
  annotate("text", label = "Note: Omits first \n and last Meetups.", 
           x = 6, y = 240, size = 4, colour = "black")
ehcp


# Could there be a power law distribution in RSVP times? Let's take a closer look!
library("igraph", lib.loc="/usr/lib64/R/library")
s17 <- subset(d, d$event_id=="87687152")
head(s17)
power.law.fit(abs(s17$offset),2)
# The important thing here is that there is no power law here; too many
# RSVPs in the top. 
qplot(abs(offset), ith_rsvp, data=s17) + geom_smooth(method="loess") + 
  labs(list(title = "Close-up of Event 87687152", 
            x = "Days from Announcement until Meetup (log10 scale)", 
            y = "RSVPs")) + 
  scale_x_log10() +
  theme(axis.title.y=element_text(angle=0))


s16 <- subset(d, d$event_id=="75041762")
power.law.fit(abs(s16$offset),20)
qplot(offset, ith_rsvp, data=s16) + geom_smooth(method="loess") + 
  labs(list(title = "Close-up of Event 75041762", 
            x = "Days from Announcement until Meetup", 
            y = "RSVPs"))+
  theme(axis.title.y=element_text(angle=0))


## Well, that's about all I came up with.
