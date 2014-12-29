# Kaggle project: 
# use trip time as single feature to classify the trips made and did not make
# by certain drivers.

# get a clean start
rm(list=ls())

findDriverList <- function(directory){
  cmd = paste("ls", directory)
  result_drivers = system(cmd, intern=T)
  return(as.numeric(result_drivers))
}

findOneTripTime <- function(folder, trip_idx){
  # find the trip time recorded in one csv file
  filename = file.path(folder, paste(trip_idx,'.csv', sep=""))
  cmd = paste("cat ", filename, " | wc -l")
  total_lines = system(cmd, intern=T)
  total_time  = as.numeric(total_lines) - 2 # header and first line
  return(total_time)
}

findOneDriverTripTime <- function(folder, driver_idx, trip_range){
  # wrapper of function findOneTripTime()
  # find the trip time of all trips of a certain drivers
  # return: data frame in format: driver_id, trip_id, trip_time
  oneDriver.df = data.frame(driver_id = rep(driver_idx, length(trip_range)),
                            trip_id  = trip_range,
                            trip_time = 0)
  folder_driver = file.path(folder, driver_idx)
  # is folder exist
  if(file.exists(folder_driver)==F){
    stop(c('findOneDriverTripTime(): \n No such folder: ', folder_driver))    
  }
  # loop over all trips
  for(i in 1:length(trip_range)){
    oneDriver.df[i,3] = findOneTripTime(folder_driver, trip_range[i])
  }
  return(oneDriver.df)
}

classifyOneDriver_kMeans <- function(folder, driver_idx, trip_list){
  # use kNN to classify the trips belong and not belong to the driver with kmeans
  # clustering, cluster with the largest number of trips in it is made by driver
  
  oneDriver.result = findOneDriverTripTime(folder, driver_idx, trip_list)
  km.result = rep(0, length(trip_list))
  # classification using k-means
  km.cluster = kmeans(oneDriver.result[,3], 2)$cluster
  cluster_1_num = sum(km.cluster==1)
  cluster_2_num = sum(km.cluster==2)
  if(cluster_1_num>cluster_2_num){
    km.result[km.cluster==1]=1
  }else{
    km.result[km.cluster==2]=1
  }
  # store data
  oneDriver.result$prob = km.result
  return(oneDriver.result)
}

classifyAllDrivers <- function(folder, driver_idx_list, trip_list){
# classify all drivers
  drivers.result = NULL
  for(i_driver in driver_idx_list){
    driver_result_now = classifyOneDriver_kMeans(folder, i_driver, trip_list)
    drivers.result = rbind(drivers.result, driver_result_now)
    cat('Driver ', i_driver, ' records classified!\n')
  }
  return(drivers.result)
}

dumpResults <- function(result.df, dumpfilename){
  dump.df = data.frame(driver_trip = paste(result.df[,1], result.df[,2], sep="_"),
                       prob = result.df[,4])
  write.csv(dump.df, file=dumpfilename, row.names=F)
}
                         
# main program
driver_list= findDriverList("drivers") #1:findTotalDriversNum("drivers")
trip_list = 1:200 # according to competition website

# start to classify
result.df = classifyAllDrivers("drivers",driver_list, trip_list)
dumpResults(result.df, "predict.csv")
