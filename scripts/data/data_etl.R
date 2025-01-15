# load all data into something reasonable

source('scripts/data/workspace_refresh.R')

vec.files <- list.files('data/highrise_events/', full.names=TRUE)

# for each file, have to read line by line, as the files aren't actually a proper json in total, each individual line is
# store event properties in a separate table by reference (create a unique id for each event)
# because these properties can themselves be list, json-ify each individual piece before saving, to be safe
# i = 2 for good example with multiple properties, i = 100 for no event properties

for (i in 1:length(vec.files)){
  cur.file <- vec.files[i]
  cur.lines <- readLines(gzfile(cur.file))
  
  events <- lapply(cur.lines, function(x){
    # create a uuid so we can tie events and their properties and not worry about collision
    cur.uuid <- UUIDgenerate()
    
    cur.json <- fromJSON(x)
    cur.metadata <- cur.json[names(cur.json)!="event_properties"]
    cur.properties <- cur.json$event_properties
    cur.properties <- lapply(cur.properties, function(x){toString(toJSON(x))}) # json-string-ify to avoid list unpacking and save all info
    
    cur.metadata <- c('event_id'=cur.uuid, cur.metadata)
    cur.properties <- c('event_id'=cur.uuid, cur.properties)
    
    return(list(metadata=cur.metadata, properties=cur.properties))
  })
  
  cur.event_metadata <- rbindlistnull(lapply(events, "[[", 1))
  cur.event_properties <- rbindlistnull(lapply(events, "[[", 2))
  
  cur.event_metadata[,event_time:=format(as.POSIXct(event_time, format="%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d %H:%M:%S")]
  cur.event_properties <- melt.data.table(cur.event_properties, id.vars="event_id", variable.name="property")
  
  if (!dbExistsTable(sql.con, "events")){
    createSQL(cur.event_metadata, "events", vec.primary_key=c('event_id'))
    Sys.sleep(5)
    dbExecute(sql.con, "CREATE INDEX device_id_idx on events (device_id)")
    dbExecute(sql.con, "CREATE INDEX event_time_idx on events (event_time)")
    dbExecute(sql.con, "CREATE INDEX event_type_idx on events (event_type)")
    Sys.sleep(2)
  }
  writeSQL(cur.event_metadata, "events")
  
  if (!dbExistsTable(sql.con, "event_properties")){
    createSQL(cur.event_properties, "event_properties", vec.primary_key=c('event_id', 'property'))
    Sys.sleep(5)
  }
  writeSQL(cur.event_properties, "event_properties")
  
  progBar(i, length(vec.files), TRUE)
}


# create a new table; for each device_id (our identifier for a person), get their first purchase timestamp
# purchase events:
## UA_PurchasedFirstIAP
## UA_PurchasedIAP
## SubscriptionStarted
## SubscriptionRenewed
## ShopBoughtIAP

dbExecute(sql.con, "CREATE TABLE first_purchase AS
          SELECT device_id, min(event_time) timestamp
          FROM events
          WHERE event_type IN ('UA_PurchasedFirstIAP', 'UA_PurchasedIAP', 'SubscriptionStarted', 'SubscriptionRenewed', 'ShopBoughtIAP')
          GROUP BY device_id")
dbExecute(sql.con, "CREATE INDEX device_id_idx_fp ON first_purchase (device_id)")

dbExecute(sql.con, "CREATE TABLE first_session AS
          SELECT a.device_id, a.start, b.end FROM
          (SELECT device_id, min(event_time) start FROM events WHERE event_type = 'session_start' AND event_time IS NOT NULL GROUP BY device_id) a
          INNER JOIN (SELECT device_id, min(event_time) end FROM events WHERE event_type = 'session_end' AND event_time IS NOT NULL GROUP BY device_id) b
          ON a.device_id = b.device_id")
dbExecute(sql.con, "CREATE INDEX device_id_idx_fs ON first_session (device_id)")

