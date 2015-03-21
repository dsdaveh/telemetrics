# aws emr create-cluster --ami-version 3.2.1 \
# --instance-groups InstanceGroupType=MASTER,InstanceCount=1,InstanceType=m3.xlarge InstanceGroupType=CORE,InstanceCount=5,InstanceType=m3.xlarge \
# --bootstrap-actions Path=s3://<BUCKET_NAME>/setup/emR_bootstrap.sh,Name=CustomAction,Args=[--rstudio,--rexamples,--plyrmr,--rhdfs] \
# --steps Name=HDFS_tmp_permission,Jar=s3://elasticmapreduce/libs/script-runner/script-runner.jar,Args=s3://<BUCKET_NAME>/setup/hdfs_permission.sh \
# --region us-west-1 --ec2-attributes KeyName=<KEYPAIR>,AvailabilityZone=us-west-1a --no-auto-terminate --name emR-example


#modes <- "local"  #dev
modes <- c("local", "hadoop")

# to run:
# benchmark( modes )
###################

Sys.setenv(HADOOP_CMD="/home/hadoop/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/home/hadoop/contrib/streaming/hadoop-streaming.jar")
Sys.setenv(JAVA_HOME="/usr/java/latest/jre")

library(rmr2);  

N.id <- 25
N.xy <- 100000

set.seed(2015)
data <- data.frame( x=rnorm(N.id * N.xy), y=rnorm(N.id * N.xy), id=rep(1:N.id, each=N.xy))

map.fun <- function(k, v) {   
    for (id in v) {
        xy <- data[ id==id, ]
        xy$len <- with(xy, sqrt( x^2 + y^2))
        xy$bearing <- with(xy, atan(y / x))
        xy.names <- colnames(xy)
        calcs <- paste0("calc", 1:8)
        for (calc in calcs) xy <- cbind(xy, atan2(xy[ ,length(xy)-1], xy[ ,length(xy)]))
        colnames(xy) <- c(xy.names, calcs)
    }
    keyval("head(xy)", head(xy))  #head to insure from.dfs won't choke
}

benchmark <- function( modes ) {
    if (length(modes) <= 0 ) return("modes is empty")
    
    benchmark.times <- numeric()
    
    # ## Test1 - rmr in local mode
    for (mode in modes) {
        rmr.options(backend=mode)
        
        p1 <- proc.time()
        out <- mapreduce(
            input = to.dfs( 1:N.id ),
            map = map.fun,
            combine = TRUE
        )
        p2 <- proc.time()
        benchmark.times <- c( benchmark.times, (p2-p1)[3] )
        print(values(from.dfs(out)))
        print( sprintf("%s mode finished in %8.1f seconds", mode, (p2-p1)[3]))
    }
    
    names(benchmark.times) <- modes
    barplot(benchmark.times, ylab="Time (sec)")
}
