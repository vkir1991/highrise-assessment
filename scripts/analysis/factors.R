
# building model factors

# eventually need wide format for regression, but store in long format for convenience until then:
# device_id, event_type, factor, value

# dummy data table to create SQL table, store all this in SQL for convenience and backup
tmp <- data.table(device_id = character(0), event_type = character(0), factor = character(0), value = numeric(0))
createSQL(tmp, 'factors', vec.primary_key=c('device_id', 'event_type', 'factor'), bln.insert=FALSE)

dt.device_id <- readSQL("SELECT DISTINCT(device_id) device_id FROM first_session")

####################################################################################################
# simple factors: we just care about the event happening in some capacity, strictly before a player's first purchase
# we don't care at all about the event's specifics; any values, etc.
## event_binary (did it happen at all)
## event_binary_first_session (did it happen in the first session)
## event_count_first_session (number of times it happened in first session)
## event_count_session_avg (across all time (up to first purchase), average number of times done per session)
## event_count_first_3_days (number of times it happened in the first 3 days)
## event_count_first_7_days (number of times it happened in the first 7 days)
## event_distinct_days_first_3_days (number of unique days among the first 3 that it happened)
## event_distinct_days_first_7_days (number of unique days among the first 7 that it happened)

vec.simple_factors <- c('Closet_SavedCloset', 'Event_ClaimedCrewBonusReward', 'LuckyWheel_SpunLuckyWheel', 'Marketplace_CompletedPurchase',
                        'Marketplace_ListedItem', 'OOTD_SavedPhoto', 'OOTD_Submitted', 'Quest_Finished', 'Share_OpenedDeepLink',
                        'Shop_BoughtItem', 'Shop_BoughtItemOffer', 'Shop_WatchedAd', 'Tipping_ClaimedTipJar', 'Tipping_SentTip')

for (cur.event in vec.simple_factors){
  cat('starting',cur.event,'...')
  
  dt.event_count_total <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=9999, bln.before_purchase=TRUE)
  dt.event_count_first_session <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type=cur.event, bln.before_purchase=TRUE)
  
  dt.event_binary <- copy(dt.event_count_total)
  dt.event_binary[,n:=as.numeric(as.logical(n))]
  
  dt.event_binary_first_session <- copy(dt.event_count_first_session)
  dt.event_binary_first_session[,n:=as.numeric(as.logical(n))]
  
  dt.session_count <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='session_start', num.days=9999, bln.before_purchase=TRUE)
  dt.event_count_session_avg <- merge(dt.event_count_total, dt.session_count, by=c('device_id'))
  dt.event_count_session_avg <- dt.event_count_session_avg[,list(device_id, n=n.x/n.y)]
  
  dt.event_count_first_3_days <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=3, bln.before_purchase=TRUE)
  dt.event_count_first_7_days <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=7, bln.before_purchase=TRUE)
  
  dt.event_distinct_days_first_3_days <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=3, bln.before_purchase=TRUE)
  dt.event_distinct_days_first_7_days <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=7, bln.before_purchase=TRUE)
  
  # insert false/0 values, since these all only return the positives
  dt.event_binary <- merge(dt.event_binary, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_binary[is.na(n), n:=0]
  
  dt.event_binary_first_session <- merge(dt.event_binary_first_session, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_binary_first_session[is.na(n), n:=0]
  
  dt.event_count_first_session <- merge(dt.event_count_first_session, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_count_first_session[is.na(n), n:=0]
  
  dt.event_count_session_avg <- merge(dt.event_count_session_avg, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_count_session_avg[is.na(n), n:=0]
  
  dt.event_count_first_3_days <- merge(dt.event_count_first_3_days, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_count_first_3_days[is.na(n), n:=0]
  
  dt.event_count_first_7_days <- merge(dt.event_count_first_7_days, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_count_first_7_days[is.na(n), n:=0]
  
  dt.event_distinct_days_first_3_days <- merge(dt.event_distinct_days_first_3_days, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_distinct_days_first_3_days[is.na(n), n:=0]
  
  dt.event_distinct_days_first_7_days <- merge(dt.event_distinct_days_first_7_days, dt.device_id, by=c('device_id'), all.y=TRUE)
  dt.event_distinct_days_first_7_days[is.na(n), n:=0]
  
  # combine everything into a single table for this event
  dt.factors <- rbind(dt.event_binary[,list(device_id, event_type=cur.event, factor='event_binary', value=n)],
                      dt.event_binary_first_session[,list(device_id, event_type=cur.event, factor='event_binary_first_session', value=n)],
                      dt.event_count_first_session[,list(device_id, event_type=cur.event, factor='event_count_first_session', value=n)],
                      dt.event_count_session_avg[,list(device_id, event_type=cur.event, factor='event_count_session_avg', value=n)],
                      dt.event_count_first_3_days[,list(device_id, event_type=cur.event, factor='event_count_first_3_days', value=n)],
                      dt.event_count_first_7_days[,list(device_id, event_type=cur.event, factor='event_count_first_7_days', value=n)],
                      dt.event_distinct_days_first_3_days[,list(device_id, event_type=cur.event, factor='event_distinct_days_first_3_days', value=n)],
                      dt.event_distinct_days_first_7_days[,list(device_id, event_type=cur.event, factor='event_distinct_days_first_7_days', value=n)])
  
  writeSQL(dt.factors, 'factors')
  cat('done!\n\n')
}


####################################################################################################
# semi-simple: the same set of calculations as above, but the events require some finessing to be easily usable, and we don't necessarily care about all of the calcs

# Crew:
## combine Crew_CreatedCrew and Crew_JoinedCrew
## only care about event_binary, event_binary_first_session
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=c('Crew_CreatedCrew', 'Crew_JoinedCrew'), num.days=9999, bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Crew_CreateOrJoin', factor='event_binary', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type=c('Crew_CreatedCrew', 'Crew_JoinedCrew'), bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Crew_CreateOrJoin', factor='event_binary_first_session', value=n)]
writeSQL(tmp, 'factors')


# Crew_LeftCrew
## binary, binary_first_session only
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='Crew_LeftCrew', num.days=9999, bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Crew_LeftCrew', factor='event_binary', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type='Crew_LeftCrew', bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Crew_LeftCrew', factor='event_binary_first_session', value=n)]
writeSQL(tmp, 'factors')

# session_start
## pseudo retention metric, works as is but only care about count, distinct_days
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='session_start', num.days=3, bln.before_purchase=TRUE)
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='session_start', factor='event_count_first_3_days', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='session_start', num.days=7, bln.before_purchase=TRUE)
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='session_start', factor='event_count_first_7_days', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type='session_start', num.days=3, bln.before_purchase=TRUE)
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='session_start', factor='event_distinct_days_first_3_days', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type='session_start', num.days=7, bln.before_purchase=TRUE)
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='session_start', factor='event_distinct_days_first_7_days', value=n)]
writeSQL(tmp, 'factors')


# Registration_ChangedDefaultOutfit
## binary only
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='Registration_ChangedDefaultOutfit', num.days=9999, bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Registration_ChangedDefaultOutfit', factor='event_binary', value=n)]
writeSQL(tmp, 'factors')

# Registration_FinishedRegistration
## actually just use this as a first-pass filter; we don't want to dilute data with anybody who didn't even do this, that's an onboarding issue not a monetization one

# Shared
## combine Share_SharedOOTD, Share_SharedProfile, Share_SharedRoom, Share_SharedScreenshot
## really just care about you trying to share something, it's unlikely what you share makes a big difference
cur.event <- c('Share_SharedOOTD', 'Share_SharedProfile', 'Share_SharedRoom', 'Share_SharedScreenshot')
dt.event_count_total <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=9999, bln.before_purchase=TRUE)
dt.event_count_first_session <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type=cur.event, bln.before_purchase=TRUE)

dt.event_binary <- copy(dt.event_count_total)
dt.event_binary[,n:=as.numeric(as.logical(n))]

dt.event_binary_first_session <- copy(dt.event_count_first_session)
dt.event_binary_first_session[,n:=as.numeric(as.logical(n))]

dt.session_count <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='session_start', num.days=9999, bln.before_purchase=TRUE)
dt.event_count_session_avg <- merge(dt.event_count_total, dt.session_count, by=c('device_id'))
dt.event_count_session_avg <- dt.event_count_session_avg[,list(device_id, n=n.x/n.y)]

dt.event_count_first_3_days <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=3, bln.before_purchase=TRUE)
dt.event_count_first_7_days <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=7, bln.before_purchase=TRUE)

dt.event_distinct_days_first_3_days <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=3, bln.before_purchase=TRUE)
dt.event_distinct_days_first_7_days <- eventCountDistinctDays(vec.device_id=NULL, vec.event_type=cur.event, num.days=7, bln.before_purchase=TRUE)

# insert false/0 values, since these all only return the positives
dt.event_binary <- merge(dt.event_binary, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_binary[is.na(n), n:=0]

dt.event_binary_first_session <- merge(dt.event_binary_first_session, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_binary_first_session[is.na(n), n:=0]

dt.event_count_first_session <- merge(dt.event_count_first_session, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_count_first_session[is.na(n), n:=0]

dt.event_count_session_avg <- merge(dt.event_count_session_avg, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_count_session_avg[is.na(n), n:=0]

dt.event_count_first_3_days <- merge(dt.event_count_first_3_days, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_count_first_3_days[is.na(n), n:=0]

dt.event_count_first_7_days <- merge(dt.event_count_first_7_days, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_count_first_7_days[is.na(n), n:=0]

dt.event_distinct_days_first_3_days <- merge(dt.event_distinct_days_first_3_days, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_distinct_days_first_3_days[is.na(n), n:=0]

dt.event_distinct_days_first_7_days <- merge(dt.event_distinct_days_first_7_days, dt.device_id, by=c('device_id'), all.y=TRUE)
dt.event_distinct_days_first_7_days[is.na(n), n:=0]

# combine everything into a single table for this event
dt.factors <- rbind(dt.event_binary[,list(device_id, event_type='Share_SharedAnything', factor='event_binary', value=n)],
                    dt.event_binary_first_session[,list(device_id, event_type='Share_SharedAnything', factor='event_binary_first_session', value=n)],
                    dt.event_count_first_session[,list(device_id, event_type='Share_SharedAnything', factor='event_count_first_session', value=n)],
                    dt.event_count_session_avg[,list(device_id, event_type='Share_SharedAnything', factor='event_count_session_avg', value=n)],
                    dt.event_count_first_3_days[,list(device_id, event_type='Share_SharedAnything', factor='event_count_first_3_days', value=n)],
                    dt.event_count_first_7_days[,list(device_id, event_type='Share_SharedAnything', factor='event_count_first_7_days', value=n)],
                    dt.event_distinct_days_first_3_days[,list(device_id, event_type='Share_SharedAnything', factor='event_distinct_days_first_3_days', value=n)],
                    dt.event_distinct_days_first_7_days[,list(device_id, event_type='Share_SharedAnything', factor='event_distinct_days_first_7_days', value=n)])

writeSQL(dt.factors, 'factors')


# SocialLink_Linked
## binary and binary_first_session only
## not too worried about which platform you link, that's more reflective of what socials you're on than your behavior pattern
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='SocialLink_Linked', num.days=9999, bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='SocialLink_Linked', factor='event_binary', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type='SocialLink_Linked', bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='SocialLink_Linked', factor='event_binary_first_session', value=n)]
writeSQL(tmp, 'factors')


####################################################################################################
# more complex factors:
# derived from event data

# first session length
tmp <- calcFirstSessionLength()
tmp[first_session_length<5, first_session_length:=NA]
tmp <- tmp[,list(device_id, event_type='first_session_length', factor='raw', value=first_session_length)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[,value:=as.character(value)]
writeSQL(tmp, 'factors')

# retention - this is effectively encompassed in session_start as a simple event, so can skip here, just noting for posterity

# platform (iOS, Android, web, reseller)
## just take this from the first session_start by device_id, since it's not actually an event
## one-hot this since categorical
tmp <- readSQL("SELECT device_id, platform FROM events WHERE event_type='session_start' GROUP BY device_id HAVING MIN(ROWID) ORDER BY ROWID")
tmp <- oneHotEncode(tmp, 'platform')
tmp <- melt.data.table(tmp, id.vars='device_id', variable.name='event_type')
tmp <- tmp[,list(device_id, event_type, factor='raw', value)]
writeSQL(tmp, 'factors')

# country
## similar to platform, first session_start by device_id, and one-hot but split into broad groups:
# US: United States + Puerto Rico
# Commonwealth: United Kingdom + Canada + Australia + New Zealand
# Developed Asia: China + Japan + South Korea + Hong Kong + Singapore
# EU: Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden
# Other: everybody else
# skipping for now; the can of worms which is country groupings is in itself a whole project

# age: from Registration_PickedBirthday -> properties -> age
tmp <- readSQL("SELECT device_id, min(value) age
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               WHERE event_type='Registration_PickedBirthday'
               AND property='age'
               GROUP BY device_id")
tmp[,age:=as.numeric(sapply(age, fromJSON))]
tmp[age<13, age:=NA]
tmp <- tmp[,list(device_id, event_type='Registration_PickedBirthday', factor='age', value=age)]
tmp[,value:=as.character(value)] # for some reason sql handles character <NA> fine but not numeric NA
writeSQL(tmp, 'factors')

# gender: from Registration_ChoseGender -> properties -> gender
tmp <- readSQL("SELECT device_id, min(value) gender
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               WHERE event_type='Registration_ChoseGender'
               AND property='gender'
               GROUP BY device_id")
tmp[,gender:=sapply(gender, fromJSON)]
tmp[,gender:=tolower(gender)]
tmp <- oneHotEncode(tmp, 'gender')
tmp <- melt.data.table(tmp, id.vars='device_id', variable.name='event_type')
tmp <- tmp[,list(device_id, event_type, factor='raw', value)]
writeSQL(tmp, 'factors')

# Shop_SpunGacha
## event_binary, event_binary_first_session
## total spins (sum of properties -> spin_count): first session, 3, 7 days
## have a close-to-completed collection (boolean, any( properties -> completion_pct >= 0.8) at any point)
tmp <- eventCountInFirstXDays(vec.device_id=NULL, vec.event_type='Shop_SpunGacha', num.days=9999, bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='event_binary', value=n)]
writeSQL(tmp, 'factors')

tmp <- eventCountInFirstSession(vec.device_id=NULL, vec.event_type='Shop_SpunGacha', bln.before_purchase=TRUE)
tmp[,n:=as.numeric(as.logical(n))]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='event_binary_first_session', value=n)]
writeSQL(tmp, 'factors')

# total spins first session
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND a.event_time >= c.start AND a.event_time <= c.end
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='Shop_SpunGacha'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'spin_count'")
tmp[,value:=sapply(value, fromJSON)]
tmp <- tmp[,list(n=sum(value)), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='event_count_first_session', value=n)]
writeSQL(tmp, 'factors')

# total spins first 3 days
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+2 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='Shop_SpunGacha'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'spin_count'")
tmp[,value:=sapply(value, fromJSON)]
tmp <- tmp[,list(n=sum(value)), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='event_count_first_3_days', value=n)]
writeSQL(tmp, 'factors')

# total spins first 7 days
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+6 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='Shop_SpunGacha'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'spin_count'")
tmp[,value:=sapply(value, fromJSON)]
tmp <- tmp[,list(n=sum(value)), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='event_count_first_7_days', value=n)]
writeSQL(tmp, 'factors')

# have a close-to-completed collection, ever
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+9998 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='Shop_SpunGacha'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'completion_pct'")
tmp[,value:=sapply(value, fromJSON)]
tmp <- tmp[,list(n=as.numeric(any(value>=0.8 & value<1))), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='Shop_SpunGacha', factor='close_to_collection_completion', value=n)]
writeSQL(tmp, 'factors')


# VW_FinishedVisit
## number of unique rooms visited
## types of rooms visited - only types are room and world, not sure if there's a difference, doesn't seem worth messing with until "game" becomes a room type
## visited the same room >=3, >=10 times (i.e. you have a favorite)
## time_spent_minutes - really want time_spent_rooms_minutes or time_spent_worlds_minutes, since you can be in a room but have it minimized while you're looking at profile (time_spent_profile_minutes), etc.
### this seems more useful to analyze the room, not the player; maybe just check if there are any rooms that they spent at least 5 minutes in? if not that's a sign. binary and binary_first_session
## social taps: instagram, snapchat, tiktok, x; not 100% sure what this is, maybe looking at other players' profiles? but could be useful

# number of unique rooms visited, first session
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND a.event_time >= c.start AND a.event_time <= c.end
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='VW_FinishedVisit'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'vw_id'")
tmp <- tmp[,list(n=length(unique(value))), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='VW_FinishedVisit', factor='unique_rooms_first_session', value=n)]
writeSQL(tmp, 'factors')

# number of unique rooms visited, first 3 days
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+2 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='VW_FinishedVisit'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'vw_id'")
tmp <- tmp[,list(n=length(unique(value))), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='VW_FinishedVisit', factor='unique_rooms_first_3_days', value=n)]
writeSQL(tmp, 'factors')

# number of unique rooms visited, first 7 days
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+6 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='VW_FinishedVisit'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'vw_id'")
tmp <- tmp[,list(n=length(unique(value))), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='VW_FinishedVisit', factor='unique_rooms_first_7_days', value=n)]
writeSQL(tmp, 'factors')

# visited same room >=3, >=10 times ever (binary)
tmp <- readSQL("SELECT a.device_id, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND DATE(a.event_time) >= DATE(c.start) AND DATE(a.event_time) <= DATE(c.start, '+9998 day')
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='VW_FinishedVisit'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property = 'vw_id'")
tmp <- tmp[,list(n=.N), by=list(device_id, value)]
tmp <- tmp[,list(n=max(n)), by=list(device_id)]

tmp1 <- copy(tmp)
tmp1[,n:=as.numeric(n>=3)]
tmp1 <- merge(tmp1, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp1[is.na(n), n:=0]
tmp1 <- tmp1[,list(device_id, event_type='VW_FinishedVisit', factor='same_room_3_times', value=n)]
writeSQL(tmp1, 'factors')

tmp1 <- copy(tmp)
tmp1[,n:=as.numeric(n>=10)]
tmp1 <- merge(tmp1, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp1[is.na(n), n:=0]
tmp1 <- tmp1[,list(device_id, event_type='VW_FinishedVisit', factor='same_room_10_times', value=n)]
writeSQL(tmp1, 'factors')


# have any room you spent at least 5 minutes in, in your first session

tmp <- readSQL("SELECT a.device_id, a.event_id, property, value
               FROM events a
               INNER JOIN event_properties b
               ON a.event_id = b.event_id
               INNER JOIN first_session c
               ON a.device_id = c.device_id AND a.event_time >= c.start AND a.event_time <= c.end
               LEFT JOIN first_purchase d
               ON a.device_id = d.device_id
               WHERE event_type='VW_FinishedVisit'
               AND (d.timestamp IS NULL or event_time < d.timestamp)
               AND property IN('time_spent_rooms_minutes', 'time_spent_worlds_minutes')")
tmp[,value:=sapply(value, fromJSON)]
tmp <- tmp[,list(value=sum(value)), by=list(device_id, event_id)]
tmp <- tmp[,list(n=as.numeric(any(value>=5))), by=list(device_id)]
tmp <- merge(tmp, dt.device_id, by=c('device_id'), all.y=TRUE)
tmp[is.na(n), n:=0]
tmp <- tmp[,list(device_id, event_type='VW_FinishedVisit', factor='spent_5_mins_in_room_first_session', value=n)]
writeSQL(tmp, 'factors')
