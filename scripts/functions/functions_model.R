# functions related to models, factors, etc.

# given device_id, find timestamp of their first purchase, or NA if never purchased anything
getFirstPurchaseTimestamp <- function(chr.device_id){
  return(as.timestamp(readSQL("SELECT min(timestamp) t FROM first_purchase WHERE device_id = '",chr.device_id,"'")$t))
}


# given a device_id, calculate their first session length
calcFirstSessionLength <- function(vec.device_id=NULL){
  dt.session_length <- readSQL("SELECT device_id, (JULIANDAY(end) - JULIANDAY(start)) * 24 * 60 * 60 AS first_session_length
                               FROM first_session",
                               ifelse(!is.null(vec.device_id), paste0(" WHERE device_id IN ('",paste(vec.device_id, collapse="', '"),"')"), ""))
  
  return(dt.session_length)
}

# given device_id and event_type, number of times done in first 
# can just take bool to get binary, whether you did it in first session or not
eventCountInFirstSession <- function(vec.device_id=NULL, vec.event_type, bln.before_purchase=FALSE){
  dt.event_count <- readSQL("SELECT a.device_id, count(*) n
                            FROM events a
                            INNER JOIN first_session b
                            ON a.device_id = b.device_id AND a.event_time >= b.start AND a.event_time <= b.end
                            LEFT JOIN first_purchase c
                            ON a.device_id = c.device_id
                            WHERE event_type IN ('",paste(vec.event_type, collapse="', '"),"')",
                            ifelse(!is.null(vec.device_id), paste0(" WHERE device_id IN ('",paste(vec.device_id, collapse="', '"),"')"), ""),
                            ifelse(bln.before_purchase," AND (c.timestamp IS NULL OR event_time < c.timestamp)", ""),
                            " GROUP BY a.device_id")
  
  return(dt.event_count)
}

# given device_id and event_type, number of times done in first X days (by calendar date)
# can just take bool to get binary, whether you did it in x days or not
# if you want to get total count, just set X = some big number
eventCountInFirstXDays <- function(vec.device_id=NULL, vec.event_type, num.days, bln.before_purchase=FALSE){
  dt.event_count <- readSQL("SELECT a.device_id, count(*) n
                            FROM events a
                            INNER JOIN first_session b
                            ON a.device_id = b.device_id AND DATE(a.event_time) >= DATE(b.start) AND DATE(a.event_time) <= DATE(b.start, '+",num.days-1," day')
                            LEFT JOIN first_purchase c
                            ON a.device_id = c.device_id
                            WHERE event_type IN ('",paste(vec.event_type, collapse="', '"),"')",
                            ifelse(!is.null(vec.device_id), paste0(" WHERE device_id IN ('",paste(vec.device_id, collapse="', '"),"')"), ""),
                            ifelse(bln.before_purchase," AND (c.timestamp IS NULL OR event_time < c.timestamp)", ""),
                            " GROUP BY a.device_id")
  
  return(dt.event_count)
}

# given device_id and event_type, number of unique days event was done in first X days (by calendar date)
# useful if e.g. doing something once every day is more valuable than doing it 7 times on day 1 and then never again
eventCountDistinctDays <- function(vec.device_id=NULL, vec.event_type, num.days, lst.first_session=NULL, bln.before_purchase=FALSE){
  dt.event_count <- readSQL("SELECT a.device_id, count(DISTINCT(DATE(event_time))) n
                            FROM events a
                            INNER JOIN first_session b
                            ON a.device_id = b.device_id AND DATE(a.event_time) >= DATE(b.start) AND DATE(a.event_time) <= DATE(b.start, '+",num.days-1," day')
                            LEFT JOIN first_purchase c
                            ON a.device_id = c.device_id
                            WHERE event_type IN ('",paste(vec.event_type, collapse="', '"),"')",
                            ifelse(!is.null(vec.device_id), paste0(" WHERE device_id IN ('",paste(vec.device_id, collapse="', '"),"')"), ""),
                            ifelse(bln.before_purchase," AND (c.timestamp IS NULL OR event_time < c.timestamp)", ""),
                            " GROUP BY a.device_id")
  
  return(dt.event_count)
}


oneHotEncode <- function(dt.data, varname){
  for (cur.val in unique(dt.data[,varname, with=FALSE])[[1]]){
    dt.data[,c(paste0('is_',cur.val)):=as.numeric((get(varname)==cur.val))]
  }
  dt.data[,c(varname):=NULL]
  return(dt.data[])
}