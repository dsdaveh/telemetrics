source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

accels.dir <- "profile_accels"

#queue <- driver.list[9:15]   # <-- SET THIS MANUALLY
#gen_asegs(queue)   # to launch

gen_asegs <- function (queue, plot.trip=TRUE) {
    
    #segs.fn <- "gsegs_all.RData"
    #load( file=segs.fn )
    ptime1 <- proc.time()
    time.log <- rbind(ptime1, ptime1)
    time.log <- time.log[-(1:2), ]   #kludge
    for (driver.id in queue) {
        accels <- getAccels( driver.id , plot.trip=plot.trip)
        
        prof.fn <- sprintf("%s/driver_%s_accels.RData", accels.dir, driver.id);
        cat(prof.fn, "...\n")
        save(accels, file=prof.fn)
        
        ptime2 <- proc.time();  delta <- ptime2-ptime1
        time.log <- rbind(time.log, delta)
        ptime1 <- ptime2;
        print(delta)
    }
    print(time.log)
    #save( segs.all, file=segs.fn)
}
