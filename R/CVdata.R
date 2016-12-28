# Build CV data
# Store it in ../cv
library(data.table)
super_fread <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

clicks_train  <- super_fread( "../input/clicks_train.csv", key_var = "display_id" )
clicks_test  <- super_fread( "../input/clicks_test.csv", key_var = "display_id" )
events  <- super_fread( "../input/events.csv", key_var = "display_id" )

nrow(clicks_train)
nrow(clicks_test)

cvmult = 2
cv_train_size = 1000000*cvmult
cv_test_size = 380000*cvmult
set.seed(121)

s12 = sample(unique(clicks_train$display_id),size=cv_train_size+cv_test_size)
s12 = sort(s12,decreasing = FALSE)
s1 = s12[1:cv_train_size]
s2 = s12[(1+cv_train_size):(cv_train_size+cv_test_size)]

c1 = clicks_train[display_id %in% s1]
c2 = clicks_train[display_id %in% s2]
ev = events[display_id %in% s12]

write.csv(c1,file = "../cv/clicks_train.csv",quote = FALSE,row.names = FALSE)
write.csv(c2,file = "../cv/clicks_test.csv",quote = FALSE,row.names = FALSE)
write.csv(ev,file = "../cv/events.csv",quote = FALSE,row.names = FALSE)

