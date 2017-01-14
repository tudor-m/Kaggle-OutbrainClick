library(data.table)
source("futil.R")

get_probs <- function (dt,var,target,w1,w2){
  p=dt[,sum(get(target))/.N]
  dt[ ,.( prob=(sum(get(target))+w1*p )/(.N+w2) ),by=eval(var)]
}

DT_fill_NA <- function(DT,replacement=0) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,replacement)
}

super_fread <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

events <- fread("../input/events.csv",
                select=c("display_id","platform"),
                colClasses=c(rep("numeric",4),"character","numeric"))

#nn_ = "ad_id_pl"
nn_ = "ad_id"

events[platform < "1",platform:="2"]
setkeyv(events,"display_id")
clicks_train1  <- super_fread( "../input/clicks_train.csv", key_var = "display_id" )
clicks_test1   <- super_fread( "../input/clicks_test.csv" , key_var = "display_id" )
vTrgt = clicks_test1[clicked==1,2,with=F]
for (wprob1 in seq(19,19,1)) for (wprob2 in seq(22,22,1)){ #19 22 is the best so far: 0.6239419
  print(c(wprob1,wprob2))
clicks_train <- merge( clicks_train1, events, all.x = F )
clicks_train[,ad_id_pl:= paste0(ad_id,'_',platform)]
setkeyv(clicks_train,eval(nn_))

click_prob = clicks_train[,.(sum(clicked)/.N)]
#wprob = 16
ad_id_pl_probs   <- get_probs(clicks_train,eval(nn_),"clicked",wprob1,wprob2)
#rm(clicks_train)
#gc()

clicks_test <- merge( clicks_test1, events, all.x = T )

clicks_test[,ad_id_pl:= paste0(ad_id,'_',platform)]
setkeyv(clicks_test,eval(nn_))

clicks_test <- merge( clicks_test, ad_id_pl_probs, all.x = T )

DT_fill_NA( clicks_test, click_prob )
clicks_test[,display_id:=as.integer(display_id)]
setkey(clicks_test,"prob") 
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)

estErr = errMeasure1(submission[,2,with=F],vTrgt)
print(estErr)
}