library(data.table)
source("futil.R")

get_probs <- function (dt,var,target,w){
  p=dt[,sum(get(target))/.N]
  dt[ ,.( prob=(sum(get(target))+w*p )/(.N+w) ),by=eval(var)]
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

events <- fread("../cv/events.csv",
                select=c("display_id","platform"),
                colClasses=c(rep("numeric",4),"character","numeric"))

events[platform < "1",platform:="2"]
setkeyv(events,"display_id")
for (wprob in seq(16:24)) {
clicks_train  <- super_fread( "../cv/clicks_train.csv", key_var = "display_id" )
clicks_train <- merge( clicks_train, events, all.x = F )
clicks_train[,ad_id_pl:= paste0(ad_id,'_',platform)]
setkeyv(clicks_train,"ad_id_pl")

click_prob = clicks_train[,.(sum(clicked)/.N)]
#wprob = 16
ad_id_pl_probs   <- get_probs(clicks_train,"ad_id_pl","clicked",wprob)
rm(clicks_train)
gc()

clicks_test   <- super_fread( "../cv/clicks_test.csv" , key_var = "display_id" )
clicks_test <- merge( clicks_test, events, all.x = T )
vTrgt = clicks_test[clicked==1,2,with=F]

clicks_test[,ad_id_pl:= paste0(ad_id,'_',platform)]
setkeyv(clicks_test,"ad_id_pl")

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