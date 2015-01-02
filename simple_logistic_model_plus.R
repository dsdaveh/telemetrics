# Starting with the posted code from Stephane Soulier  add a few selected features to see if it boosts 
speedDistribution <- function(trip)
{
  vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  return(quantile(vitesse, seq(0.05,1, by = 0.05)))
}

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers" #DAH
chunk.dir <- "chunks"   #relative to current dir

drivers = list.files(data.dir)  #DAH-mod
randomDrivers = sample(drivers, size = 5)

chunk.files <- dir(chunk.dir)
drivers.df <- data.frame()
for (chunk in chunk.files) {
    load( file= paste0(chunk.dir, "/", chunk ))  # creates chunk.df
    drivers.df <- rbind(drivers.df, chunk.df)
}   
rm( chunk.df, chunk, chunk.dir, chunk.files )

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
  dirPath = paste0(data.dir, '/', driver, '/')
  
  #calculate imputed values
  driver.df <- drivers.df[drivers.df$driver == driver, ]
  attach(driver.df, warn.conflicts=FALSE)
  wavg.feature  <- numeric(5)
  wavg.feature[1]   <- sum(ss.vmid.sd   * ss.n,  na.rm=TRUE) / sum(ss.n)
  wavg.feature[2]   <- sum(acc.amid.avg * acc.n, na.rm=TRUE) / sum(acc.n)
  wavg.feature[3]   <- sum(acc.amid.sd  * acc.n, na.rm=TRUE) / sum(acc.n)
  wavg.feature[4]   <- sum(dec.amid.avg * dec.n, na.rm=TRUE) / sum(dec.n)
  wavg.feature[5]   <- sum(dec.amid.sd  * dec.n, na.rm=TRUE) / sum(dec.n)
  detach(driver.df)

  for(i in 1:200) {
      trip = read.csv(paste0(dirPath, i, ".csv"))
      
      #speed profile (original features)
      f.vitesse = speedDistribution(trip)
      
      #segment derived features
      f.seg <- as.numeric(driver.df[driver.df$trip==i ,c(13,25,26,38,39)])
      for (f in 1:5) f.seg[f] <- ifelse( is.na(f.seg[f]), wavg.feature[f], f.seg[f])   #replace NA with imputed
      names(f.seg) <- colnames(driver.df[, c(13,25,26,38,39)])
      
      features <- c( f.vitesse, f.seg, target) 
      refData = rbind(refData, features)
  }
}

target = 1
names(target) = "target"
submission = NULL
#for(driver in drivers)
for(driver in drivers[1:3])  #replace with the above line
{
  cat("calculating probabilities for driver", driver, "...\n")  #DAH mod_1
  dirPath = paste0(data.dir, '/', driver, '/')  #DAH-mod
  currentData = NULL
  
  #calculate imputed values
  driver.df <- drivers.df[drivers.df$driver == driver, ]
  attach(driver.df, warn.conflicts=FALSE)
   wavg.feature  <- numeric(5)
   wavg.feature[1]   <- sum(ss.vmid.sd   * ss.n,  na.rm=TRUE) / sum(ss.n)
   wavg.feature[2]   <- sum(acc.amid.avg * acc.n, na.rm=TRUE) / sum(acc.n)
   wavg.feature[3]   <- sum(acc.amid.sd  * acc.n, na.rm=TRUE) / sum(acc.n)
   wavg.feature[4]   <- sum(dec.amid.avg * dec.n, na.rm=TRUE) / sum(dec.n)
   wavg.feature[5]   <- sum(dec.amid.sd  * dec.n, na.rm=TRUE) / sum(dec.n)
  detach(driver.df)
  
  for(i in 1:200) {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    
    #speed profile (original features)
    f.vitesse = speedDistribution(trip)
    
    #segment derived features
    f.seg <- as.numeric(driver.df[driver.df$trip==i ,c(13,25,26,38,39)])
    for (f in 1:5) f.seg[f] <- ifelse( is.na(f.seg[f]), wavg.feature[f], f.seg[f])   #replace NA with imputed
    names(f.seg) <- colnames(driver.df[, c(13,25,26,38,39)])
    
    features <- c( f.vitesse, f.seg, target)
    currentData = rbind(currentData, features)
  }
  train = rbind(currentData, refData)
  rownames(train) <- 1:nrow(train)
  train <- as.data.frame(train)
  g = glm(target ~ ., data=train, family = binomial("logit"))
  currentData = as.data.frame(currentData)
  p =predict(g, currentData, type = "response")
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "submission.csv", row.names=F, quote=F)
