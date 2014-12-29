import sys
import os
import numpy as np
from scipy.stats import norm

"""
Simple ranking of distances with the normal law

Running time: about 1min
Score: 0.55454
"""

drivers = sorted([int(folderName) for folderName in os.listdir("drivers/")])
# Check that we unzipped the archive properly and have all the drivers
try:
    if len(drivers) != 2736:
        raise
except:
    print "Error: {} drivers found instead of 2736".format(len(drivers))
    sys.exit(0)

trips = range(1, 201)

dists = np.zeros(200) # trips are numbered 1:200, dists is indexed by 0:199

#drivers = drivers[0:10]
driverIndex = -1
# Main loop
for driver in drivers:
    driverIndex += 1
    
    # Load trips data, a list of 2D numpy arrays
    tripsData = np.load("drivers/{}/trips.npy".format(driver))
    
    # Compute the 200 distances
    for trip in trips:
        
        #Xcoord = np.loadtxt("drivers/{}/{}.csv".format(driver, trip), delimiter=',', skiprows=1) # Slow loading
        Xcoord = tripsData[trip-1] # (x, y)
        Xspeed = Xcoord[1:,]-Xcoord[:-1,] # (vx, vy)
        dists[trip-1] = np.sum(np.sqrt(Xspeed[...,0]**2 + Xspeed[...,1]**2)) # total distance of the trip
    
    # Compute the 200 probabilities
    distsCenteredReduced = (dists - np.mean(dists)) / np.std(dists)
    probs = 2 * norm.cdf(-1 * np.fabs(distsCenteredReduced)) # cdf = cumulative distribution function for the normal law
    
    # Write the 200 probabilities in the score file
    if driver == 1:
        f = open("v1.csv",'w')
        f.write("driver_trip,prob\n")
    else:
        f = open("v1.csv",'a')
    for trip in trips:
        f.write("{}_{},{}\n".format(driver, trip, probs[trip-1]))
    f.close()
    
    if driverIndex%100 == 0:
        print "{} driver{} done".format(driverIndex, driver)
    
print "Work done"
