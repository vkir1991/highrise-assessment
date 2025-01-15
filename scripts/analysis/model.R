
# now that we have all factors defined, we need to do some sanity checks on them:
# winsorize the non-binary ones (5%)

####################################################################################################
# setup
dt.factors <- readSQL("SELECT * FROM factors")

dt.factors[,feature:=paste0(event_type,'..', factor)]
dt.factors <- dt.factors[,list(device_id, feature, value)]

# winsorize all non-binary factors; for most it makes no difference, and this is somewhat heavyhanded, properly should go through each individually
f <- function(x){
  if(!identical(range(x),c(0,1))){
    x <- winsorize(x, 0.05)
  }
  return(x)
}
dt.factors[,value:=f(value), by=list(feature)]

# turn into wide format, so we can actually use it for models!
dt.factors_wide <- reshape(dt.factors[,list(device_id, feature, value)], idvar='device_id', timevar='feature', direction='wide')

# need to now add boolean on who actually purchased something
dt.purchase <- readSQL("SELECT * FROM first_purchase")
dt.factors_wide[,purchase:=as.numeric(device_id %in% dt.purchase$device_id)]

# some cleanup - if we can't even calculate a first session length (or it's <5 seconds), your data is probably messed up
dt.factors_wide <- dt.factors_wide[!is.na(value.first_session_length..raw)]

# the factors we care about:
vec.fac <- grep('^value\\.', colnames(dt.factors_wide), value=TRUE)

# replace NAs with 0s, for better model outputs, UNLESS at least 10 are NA
dt.factors_wide[,num_na:=rowSums(is.na(as.matrix(dt.factors_wide)))]
# loop through factors:
for (cur.fac in vec.fac){
  dt.factors_wide[num_na<10 & is.na(get(cur.fac)), (cur.fac):=0]
}

dt.factors_wide <- na.omit(dt.factors_wide)

# split the data into a training and prediction set - make sure it's balanced; using 20/80 split
dt.factors_wide[,idx:=1:.N, by=list(purchase)]
dt.factors_wide[,dataset:=ifelse(idx %in% sample(idx, size=ceiling(.N*0.2), replace=FALSE), 'test', 'train'), by=list(purchase)]
dt.factors_wide[,idx:=NULL]

# weights, to balance the data
dt.factors_wide[,wt:=ifelse(purchase, sum(!purchase)/sum(purchase), 1)]


####################################################################################################
# structure to hold all model outputs for comparison:
dt.pred_base <- dt.factors_wide[dataset=='test',list(wt, purchase)]
dt.pred <- NULL

####################################################################################################
# base case: simple logistic regression

mdl.lrm <- glm(as.formula(paste0('purchase ~ ',paste(vec.fac, collapse=' + '))), data=dt.factors_wide[dataset=='train'], weights=dt.factors_wide[dataset=='train']$wt, family='binomial')

dt.pred <- rbind(dt.pred, data.table(model='base logistic regression', dt.pred_base, pred=predict(mdl.lrm, dt.factors_wide[dataset=='test'], type='response')))


####################################################################################################
# glmnet
# glmnet implements elastic net, which is a combination of ridge and lasso regularization
# alpha = weight of ridge (0) vs lasso (1)

x <- as.matrix(dt.factors_wide[dataset=='train', vec.fac, with=FALSE])
y <- dt.factors_wide[dataset=='train']$purchase

for (cur.alpha in seq(0,1,0.1)){
  cat('running alpha =',cur.alpha,'...')
  cv.out <- cv.glmnet(x, y, alpha=cur.alpha, family='binomial', type.measure='auc')
  
  dt.pred <- rbind(dt.pred, data.table(model=paste0('glmnet: alpha=',cur.alpha), dt.pred_base, 
                                       pred=as.vector(predict(cv.out, as.matrix(dt.factors_wide[dataset=='test',vec.fac,with=FALSE]), s=cv.out$lambda.1se, type='response'))))
  cat('done!\n')
}

# draw ROC curves for every single model; for each, thresholds are just the unique values of pred

genROC <- function(pred, actual){
  tmp <- data.table(threshold = unique(c(0,pred,1)))
  tmp[,fpr:=sapply(threshold, function(x){sum(pred>x & actual==0)/sum(actual==0)})]
  tmp[,tpr:=sapply(threshold, function(x){sum(pred>x & actual==1)/sum(actual==1)})]
  return(tmp)
}

dt.plot <- dt.pred[,genROC(pred, purchase), by=list(model)]
dt.plot <- dt.plot[order(model, fpr)]


ggplot(dt.plot)+
  geom_line(aes(x=fpr, y=tpr, color=model))+
  coord_fixed()+
  geom_abline(slope=1, intercept=0, linetype='dashed', color='darkgrey')+
  kTheme(TRUE)

dt.pred[,list(auc=roc(purchase, pred)$auc), by=list(model)]


####################################################################################################
# now let's pick our favorite:
# alpha = 0.5 seems to be the best; base logistic is weak, which implies too many factors which is not surprising
# rerun a simple logistic regression against just the factors that are left; not ideal but gives a sense of their p-values

mdl.glmnet <- cv.glmnet(x, y, alpha=0.2, family='binomial', type.measure='auc')
mdl.glmnet_lasso <- cv.glmnet(x, y, alpha=1, family='binomial', type.measure='auc')

dt.coef <- data.table(feature=rownames(coef(mdl.glmnet, s=mdl.glmnet$lambda.1se)), coef=as.vector(coef(mdl.glmnet, s=mdl.glmnet$lambda.1se)))

dt.coef[coef!=0]

# also not quite right, but in a very simple way, we can calculate the impact of a given factor on your odds, all else equal
# log(p/(1-p)) = b0 + b1x for intercept b0 and coefficients b1 applied to features x
# baseline probability = exp(b0)/(1+exp(b0)); new probability = exp(b0+b1x)/(1+exp(b0+b1x))
mdlProb <- function(chr.feature){
  coef.intercept <- dt.coef[feature=='(Intercept)']$coef
  coef.feature <- dt.coef[feature==chr.feature]$coef
  p.base <- exp(coef.intercept)/(1+exp(coef.intercept))
  p.new <- exp(coef.intercept+coef.feature)/(1+exp(coef.intercept+coef.feature))
  return(list('prob_base'=p.base, 'prob_new'=p.new))
}
mdlProb('value.is_iOS..raw')
mdlProb('value.is_Android..raw')
mdlProb('value.Marketplace_CompletedPurchase..event_binary_first_session')
mdlProb('value.Tipping_ClaimedTipJar..event_count_session_avg')


mdl.lrm_final <- glm(as.formula(paste0('purchase ~ ',paste(dt.coef[coef!=0]$feature[-1], collapse=' + '))), data=dt.factors_wide, weights=dt.factors_wide$wt, family='binomial')
summary(mdl.lrm_final)


dt.pred_final <- data.table(dt.pred_base, pred=predict(mdl.lrm_final, dt.factors_wide[dataset=='test'], type='response'))
roc(dt.pred_final$purchase, dt.pred_final$pred)


# some fun tests to show off we know something:
# whatever the best model is, apply it to both the train and test sets, to show that it performs better on the train = overfitting
# also do this to the base lrm
# then we can hint at k-fold cross-validation




