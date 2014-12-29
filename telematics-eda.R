data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))

N.DRIVERS <- 2
driver.df <- data.frame(id=integer()
                        , speed.avg=numeric(), speed.max=numeric()
                        , break.max=numeric(), accel.max=numeric())
for (i in 1:N.DRIVERS) {
    driver.dir <- paste(c(data.dir, drivers.dir[random.driver[i]]), collapse="/")
    driver.files <- dir(driver.dir)
    for (j in 1:length(driver.files)) {
        trip <- read.csv( paste(c(driver.dir, driver.files[j] ), collapse="/"))
        trip.last <- trip[-nrow(trip), ]
        trip <- trip[-1, ]
        trip$x.d <- trip$x - trip.last$x
        trip$y.d <- trip$y - trip.last$y
        trip$v <- sqrt( trip$x.d^2 + trip$y.d^2 )  # distance travelled per second
        trip.last <- trip[-nrow(trip), 3:5 ]
        trip <- trip[-1, ]
        trip$x.d2 <- trip$x.d - trip.last$x.d
        trip$y.d2 <- trip$y.d - trip.last$y.d
        trip$a <- trip$v - trip.last$v       
    }
    driver.df[i,"id"] = drivers.dir[random.driver[1]]
    driver.df[i, "speed.avg"] =  mean(trip$v)
    driver.df[i, "speed.max"] =  max(trip$v)
    driver.df[i, "break.max"] =  min(trip$a)
    driver.df[i, "accel.max"] =  max(trip$a)
    
    
#     plot(trip$v, type="l", main="Velocity")
#     plot(trip$a, type="l", main="Acceleration")
#     plot(trip[, 1:2], type="l", main= "Plot of Route")

plot(trip[, 1:2], type="n", main="plot of Route")

v.up <- v.dn <- 0
v.inc <- 10
markSpeed <- function(t) {
    
    points( t$x, t$y )
    v.up <<- (floor(t$v / v.inc) + 1) * v.inc
    v.dn <<- (floor(t$v / v.inc)    ) * v.inc
    text( t$x, t$y, round(t$v), pos=3)
}
markSpeed(trip[1,])

for (i in 2:nrow(trip)) {
    i.v <- trip[i,"v"]
    i.a <- trip[i,"a"]
    thk <- round( i.v / 2)
    color <- ifelse( abs(i.a) < 1, "orange", 
             ifelse(    (i.a) > 0, "green", "red") ) 
    lines( trip[c(i,i-1), 1], trip[c(i,i-1), 2], type="l", lwd=thk, col=color)
    if( i.v > v.up | i.v < v.dn ) markSpeed( trip[i, ])
}
 
driver.df
