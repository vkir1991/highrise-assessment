# feature exploration, to see if some should be made categorical, etc.

# using as an example, would take too long to actually go through them all here

featureSummary <- function(chr.event_type, chr.factor){
  dt.data <- readSQL("SELECT * FROM factors
                     WHERE event_type = ",sQuote(chr.event_type),
                     " AND factor = ",sQuote(chr.factor))
  # need to now add boolean on who actually purchased something
  dt.purchase <- readSQL("SELECT * FROM first_purchase")
  dt.data[,purchase:=as.numeric(device_id %in% dt.purchase$device_id)]
  
  # simple stats:
  # if feature is boolean (range == 0,1), then we want to show the 2x2 grid and MCC
  # if not boolean, if <= 50 distinct values (i.e. likely counts), split on unique feature value and get count and pct that purchase within that
  # otherwise, it's probably a calculated value, so do a distribution
  if (identical(range(dt.data$value), c(0,1))){
    vec.actual <- as.logical(dt.data$purchase)
    vec.predicted <- as.logical(dt.data$value)
    cm <- table(Actual = vec.actual, Predicted = vec.predicted)
    rownames(cm) <- c("Actual Negative", "Actual Positive")
    colnames(cm) <- c("Predicted Negative", "Predicted Positive")
    mcc <- mcc(vec.predicted, vec.actual)
    cat('feature appears to be boolean\n')
    cat('confusion matrix:\n')
    print(cm)
    cat('MCC: ',mcc,'\n')
  } else if (length(unique(dt.data$value))<=50){
    tmp.summary <- dt.data[,list(count=as.numeric(.N), pct=sum(purchase)/.N), by=list(feature_value=value)]
    tmp.summary <- tmp.summary[feature_value<min(tmp.summary[count<10]$feature_value)][order(feature_value)]
    tmp.plot <- melt.data.table(tmp.summary, id.vars='feature_value')
    cat('feature appears to be discrete\n')
    cat('distinct feature values with >= 10 observations:\n')
    print(tmp.summary)
    ggplot(tmp.plot)+
      geom_bar(aes(x=feature_value, y=value), stat='identity')+
      facet_grid(variable~., scales='free')+
      xlab('feature_value')+
      ylab('')+
      ggtitle(paste0(chr.event_type,' > ',chr.factor))+
      kTheme(TRUE)
  } else {
    tmp.summary <- dt.data[!is.na(value)]
    tmp.summary[,bucket:=cut_number(value, min(20,floor(length(unique(value))/10)))]
    tmp.plot <- tmp.summary[,list(pct=sum(purchase)/.N), by=list(bucket)]
    cat('feature appears to be continuous\n')
    p1 <- ggplot(tmp.plot)+
      geom_bar(aes(x=bucket, y=pct), stat='identity')+
      xlab('feature_value')+
      ylab('pct_purchase')+
      ggtitle(paste0(chr.event_type,' > ',chr.factor))+
      kTheme(TRUE)+
      theme(axis.text.x=element_text(angle = 30, hjust=1))
    p2 <- ggplot(tmp.summary)+
      geom_density(aes(x=value, color=factor(purchase)), adjust=0.5)+
      scale_x_log10()+
      xlab('feature_value')+
      ylab('')+
      guides(color=guide_legend('purchase'))+
      ggtitle(paste0(chr.event_type,' > ',chr.factor))+
      kTheme(TRUE)
    multiplot(p1, p2, cols=2)
  }
}


# look into:
## LuckyWheel_SpunLuckyWheel (all)
## Marketplace_CompletedPurchase (all)
## Registration_ChangedDefaultOutfit (event_binary)
## Share_SharedAnything (event_binary)

featureSummary(chr.event_type = 'LuckyWheel_SpunLuckyWheel', chr.factor = 'event_binary')
# MCC = 0.148, not too strong
featureSummary(chr.event_type = 'LuckyWheel_SpunLuckyWheel', chr.factor = 'event_binary_first_session')
# MCC = 0.069, no cor
featureSummary(chr.event_type = 'LuckyWheel_SpunLuckyWheel', chr.factor = 'event_count_session_avg')
# surprisingly useless given coef in model, suggest replacing

featureSummary(chr.event_type = 'Marketplace_CompletedPurchase', chr.factor = 'event_binary')
# mcc = 0.198, not crazy but 20% isn't nothing
featureSummary(chr.event_type = 'Marketplace_CompletedPurchase', chr.factor = 'event_binary_first_session')
# mcc = 0.09
featureSummary(chr.event_type = 'Marketplace_CompletedPurchase', chr.factor = 'event_count_first_7_days')
# some pattern but very low counts
featureSummary(chr.event_type = 'Marketplace_ListedItem', chr.factor = 'event_binary')
# MCC = 0.165, not very strong
featureSummary(chr.event_type = 'Marketplace_ListedItem', chr.factor = 'event_count_first_7_days')
# caps out at 2? seems not different from binary, so just don't use

featureSummary(chr.event_type = 'Registration_ChangedDefaultOutfit', chr.factor = 'event_binary')
# MCC = -0.208, surprising direction
featureSummary(chr.event_type = 'Share_SharedAnything', chr.factor = 'event_binary')
# MCC = 0.0029, no cor

featureSummary(chr.event_type = 'Tipping_SentTip', chr.factor = 'event_binary')
# MCC = 0.136, not too high
featureSummary(chr.event_type = 'Tipping_SentTip', chr.factor = 'event_distinct_days_first_7_days')
# big jump at 0>1, and then 4>5, so doing it at all is good and the habit-building is very strong


