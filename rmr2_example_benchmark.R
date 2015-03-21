# R (r-project.org) example 
# for running R with rmr2 package and RStudio server on AWS EMR
# please be aware of the coresponding bootstraping script
#
# schmidbe@amazon.de
# 31. July 2014
##############################

# set environments
Sys.setenv(HADOOP_CMD="/home/hadoop/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/home/hadoop/contrib/streaming/hadoop-streaming.jar")
Sys.setenv(JAVA_HOME="/usr/java/latest/jre")

# load library
library(rmr2)

# run locally - good for debuging
# rmr.options(backend="local")

benchmark.time <- numeric()

# now run it on the AWS EMR Hadoop Cluster
#rmr.options(backend="hadoop")
for (mode in c("local", "hadoop")) {
    rmr.options(backend=mode)
    rmr.options(backend="local")
    
    # write some data to hdfs
    small.ints <- to.dfs(keyval(1, 1:10000))

    
    p1 <- proc.time()
    # a simple mapReduce job (no reduce function)
    out <- mapreduce(
        input = small.ints, 
        map = function(k, v) cbind(v, v^2))
    res <- from.dfs(out)
    p2 <- proc.time()
    benchmark.time <- c(benchmark.time, (p2-p1)[3])
    head(res$key)
    head(res$val)
    
    p1 <- proc.time()
    # no map and no reduce function
    out <- mapreduce(
        input = small.ints)
    p2 <- proc.time()
    benchmark.time <- c(benchmark.time, (p2-p1)[3])
    res <- from.dfs(out)
    # please be aware, dfs objects will be a list with two fields: key and val
    head(res$key)
    head(res$val)
    
    p1 <- proc.time()
    # mapreduce job with map and reduce function
    out <- mapreduce(
        input = small.ints, 
        map = function(k, v){
            keyval(ifelse(v > 10, 0, 1), v)
        },
        reduce = function(k,v){
            keyval(k, length(v))
        }
    )
    p2 <- proc.time()
    benchmark.time <- c(benchmark.time, (p2-p1)[3])
    res <- from.dfs(out)
    head(res$key)
    head(res$val)
    
    names(benchmark.time) <- c("no reduce", "no map or reduce", "map and reduce")
    benchmark.time <- cbind(benchmark.time, mode=mode)
}

boxplot( benchmark.time ~ benchmark.time$mode , ylab="Time (sec)")