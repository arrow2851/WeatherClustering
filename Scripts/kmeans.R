#2008, 2009, 2010 September K = 6
#Temp, DewP, STP, and WDSP compute daily average and using that the monthly average

"
1. Pre-process the data to separate data set for your team using the parameters in the spread sheet [20 pts] and do the following for submission:
I. Do clustering using for 1st year (Y1 data) given random k points. Change the seed for random number generator and repeat for another set of starting points for the same k
II. Do the above for two distance metrics: Euclidean and Pearson correlation coefficient, using the same initial points for each
III. Compare the two clustering's using SSE for the Euclidean metric and provide your analysis - for each cluster and total (10 pts)
IV. Compare the two clustering's using SSE for the Pearson correlation coefficient metric and provide your analysis - for each cluster and total (10 pts)
V. Compare the clusters from different metric using Jaccard coefficient [10 pts]
VI. Repeat the above for 2nd and 3rd year using the same seeds as in Y1 and Y2
VII. Compute the change in clusters from year to year using the Jaccard coefficient for each distance metric and provide analysis of (Y1, Y2), (Y2, Y3), and (Y1, Y2, Y3) [ 30 pts, 10 each]
VIII. 3 Problems encountered and solved [3 pts]
IX. Visualization of stations belonging to the same cluster (if possible on the TX map) for one or two of them [10 pts]
X. Answering questions during the demo [7 pts]

"
library(dplyr)
library(ggmap)
library(lubridate)
library(amap)
library(fpc)
library(clusteval)
library(rworldmap)
k = 6
stations = stations = read.csv("stations.csv", sep = ',')
stations = stations[!(stations$StationNumber == 999999),]
stations = stations[c("StationNumber","Lat","Lon")]

#arguments 
#stn.column = station column from weather data
#stn.vect = vector of unique station numbers
#cluster.vec = vector of the cluster obtained
#K = k (number of clusters present)
get.jaccard.vectors <- function(stn.column, stn.vect,cluster.vec, K)
{
  # create k zero vectors of length(station vector)
  
  # go through the cluster vector
  #     x = current cluster number = clust.vec[i]
  #     stn = current station number from the stn.column[i]
  #     stnarray[x,stn] = 1
  
  dframe <- data.frame(matrix(ncol = length(stn.vect), nrow = 0))
  colnames(dframe) <- stn.vect
  for (i in 1:K) {
    dframe[i,] = integer(length(stn.vect))
  }
  
  for(j in 1:length(cluster.vec))
  {
    row = cluster.vec[j]
    stn.num = stn.column[j]
    col = which(colnames(dframe)==stn.num)
    dframe[row, col] = 1
    
  }
  dframe
}

get.year.vector <- function(filename,seed.x, metric = "euclidean", data, plot, sse.calc )
{
set.seed(seed.x)
datafile = read.delim(filename,header = TRUE, sep = "")
weather.data = datafile[c("TEMP", "DEWP","STP","WDSP","YEARMODA", "STN...")]
weather.data = weather.data[!(weather.data$TEMP==9999.9 | weather.data$DEWP==9999.9 | weather.data$STP==9999.9 | weather.data$WDSP==999.9),]
weather.data$YEARMODA = ymd_h(weather.data$YEARMODA)
weather.data = filter(weather.data, month(weather.data$YEARMODA)==9)
weather.data2 = weather.data[c("STN...", "TEMP", "DEWP","STP","WDSP")]
weather.data = weather.data[c("TEMP", "DEWP","STP","WDSP")]
#get k initial centers
indices = sample(1:nrow(weather.data), k)
init_points = (weather.data[indices,])


clusters = Kmeans(weather.data, init_points, 10000, method = metric)

if(data)
{
print(filename)
cat("Method = ")
print(metric)
cat("Seed  =  ")
print(seed.x)
# print("cluster vector = ")
# print(cluster)
print("cluster centers = ")
print(clusters$centers)
print("cluster (scaled) wss =")
print(clusters$withinss)
print("cluster sizes = ")
print(clusters$size)
}

if(plot)# show seperate plot on the map since this makes no sense by itself
{
  plotcluster(weather.data,clusters$cluster)
}
#--------------------------------------------
# I, II complete




# III and IV are analysis. need to calculate euclidean and pearson sse. Do it below here

if(sse.calc)
{
sse.vector = c(0,0,0,0,0,0)

for (i in 1:nrow(weather.data)) {
  cluster.num = clusters$cluster[i]
  sqe = (weather.data[i,] - clusters$centers[cluster.num])
  sqe = sqe^2
  sse.vector[cluster.num] = sse.vector[cluster.num] + rowSums(sqe)

}
print(sse.vector)
print(sum(sse.vector))

}
#---------------------------------------------------------------
# III, IV complete



# V Use Jaccard on the station numbers of pearson and Jaccard clusters.
stn.col = weather.data2$STN...
stn.vec = unique(stn.col)
stn.vec = stn.vec[stn.vec != 999999]

jaccard.vector = get.jaccard.vectors(stn.column = stn.col, stn.vect = stn.vec, cluster.vec = clusters$cluster, k)

}


get.jaccard.coeffs <- function(vector1, vector2, K)
{
  result = integer(K)
  for (i in 1:K) {
    c1 = as.integer(vector1[i,])
    for (j in 1:K) {
      c2 = as.integer(vector2[j,])
      sim.val = cluster_similarity(c1,c2, similarity ="jaccard", method = "independence")
      if(sim.val>result[i])
        result[i] = sim.val
    }
  }
  result
}

plot.station <- function(year.cluster.vector,title, K =6)
{
  newmap <- getMap(resolution = "low")
  plot(newmap, main = title, xlim = c(-105, -95), ylim = c(26, 36.5), asp = 1)
  year.stations = colnames(year.cluster.vector)
  station.nums = as.numeric(stations$StationNumber)
  station.nums = intersect(station.nums, year.stations)
  colors.vec = c("red","blue","orange","black","green","pink")
  for (i in 1:K) {
    subvector = year.cluster.vector[i,!year.cluster.vector[i,]==0]
    station.subdata = stations[stations$StationNumber %in% colnames(subvector), ]
    points(station.subdata$Lon, station.subdata$Lat, col = colors.vec[i])  
  }
}




# VI Repeat above I - V steps for Y2, Y3 below here
print("seed = 40")

Y1.40.e = get.year.vector(filename= "hourly_2008.txt",seed.x = 40, metric = "euclidean", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y1.40.e,title = "Year 1 euclidean seed 40", 6)
Y1.40.p = get.year.vector(filename= "hourly_2008.txt",seed.x = 40, metric = "pearson", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y1.40.p, title = "Year 1 Pearson seed 40", 6)

Y1.40.e.p = get.jaccard.coeffs(Y1.40.e, Y1.40.p, k)
Y1.40.p.e = get.jaccard.coeffs(Y1.40.p, Y1.40.e, k)

Y2.40.e = get.year.vector(filename= "hourly_2009.txt",seed.x = 40, metric = "euclidean", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y2.40.e,title = "Year 2 euclidean seed 40", 6)
Y2.40.p = get.year.vector(filename= "hourly_2009.txt",seed.x = 40, metric = "pearson", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y2.40.p, title = "Year 2 Pearson seed 40", 6)


Y2.40.e.p = get.jaccard.coeffs(Y2.40.e, Y2.40.p, k)
Y2.40.p.e = get.jaccard.coeffs(Y2.40.p, Y2.40.e, k)


Y3.40.e = get.year.vector(filename= "hourly_2010.txt",seed.x = 40, metric = "euclidean", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y3.40.e,title = "Year 3 euclidean seed 40", 6)
Y3.40.p = get.year.vector(filename= "hourly_2010.txt",seed.x = 40, metric = "pearson", data = TRUE, plot = FALSE, sse.calc = FALSE)
plot.station(Y3.40.p, title = "Year 3 Pearson seed 40", 6)


Y3.40.e.p = get.jaccard.coeffs(Y3.40.e, Y3.40.p, k)
Y3.40.p.e = get.jaccard.coeffs(Y3.40.p, Y3.40.e, k)


print("Y1 E X P")
print(Y1.40.e.p)
print("Y1 P X E")
print(Y1.40.p.e)
#
print("Y2 E X P")
print(Y2.40.e.p)
print("Y2 P X E")
print(Y2.40.p.e)
#
print("Y3 E X P")
print(Y3.40.e.p)
print("Y3 P X E")
print(Y3.40.p.e)


# need to repeat for seed = 10!

print("seed = 10")
Y1.10.e = get.year.vector(filename= "hourly_2008.txt",seed.x = 10, metric = "euclidean", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y1.10.e,title = "Year 1 euclidean seed 10", 6)
Y1.10.p = get.year.vector(filename= "hourly_2008.txt",seed.x = 10, metric = "pearson", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y1.10.p, title = "Year 1 Pearson seed 10", 6)

Y1.10.e.p = get.jaccard.coeffs(Y1.10.e, Y1.10.p, k)
Y1.10.p.e = get.jaccard.coeffs(Y1.10.p, Y1.10.e, k)

Y2.10.e = get.year.vector(filename= "hourly_2009.txt",seed.x = 10, metric = "euclidean", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y2.10.e,title = "Year 2 euclidean seed 10", 6)
Y2.10.p = get.year.vector(filename= "hourly_2009.txt",seed.x = 10, metric = "pearson", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y2.10.p, title = "Year 2 Pearson seed 10", 6)

Y2.10.e.p = get.jaccard.coeffs(Y2.10.e, Y2.10.p, k)
Y2.10.p.e = get.jaccard.coeffs(Y2.10.p, Y2.10.e, k)


Y3.10.e = get.year.vector(filename= "hourly_2010.txt",seed.x = 10, metric = "euclidean", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y3.10.e,title = "Year 3 euclidean seed 10", 6)
Y3.10.p = get.year.vector(filename= "hourly_2010.txt",seed.x = 10, metric = "pearson", data = FALSE, plot = FALSE, sse.calc = FALSE)
plot.station(Y3.10.p, title = "Year 3 Pearson seed 10", 6)


Y3.10.e.p = get.jaccard.coeffs(Y3.10.e, Y3.10.p, k)
Y3.10.p.e = get.jaccard.coeffs(Y3.10.p, Y3.10.e, k)

print("Y1 E X P")
print(Y1.10.e.p)
print("Y1 P X E")
print(Y1.10.p.e)
#
print("Y2 E X P")
print(Y2.10.e.p)
print("Y2 P X E")
print(Y2.10.p.e)
#
print("Y3 E X P")
print(Y3.10.e.p)
print("Y3 P X E")
print(Y3.10.p.e)

# VII Compute and analyze change in clusters for multiple years.
# Do it for (Y1, Y2), (Y2, Y3), and (Y1, Y2, Y3) for both Euclidean and Pearson

# For (Y1, Y2) 

Y1Y2cols = intersect(colnames(Y1.40.e), colnames(Y2.40.e))

Y1Y2.Y1.40.e = Y1.40.e[Y1Y2cols]
Y1Y2.Y1.40.p = Y1.40.p[Y1Y2cols]
Y1Y2.Y1.10.e = Y1.10.e[Y1Y2cols]
Y1Y2.Y1.10.p = Y1.10.p[Y1Y2cols]

Y1Y2.Y2.40.e = Y2.40.e[Y1Y2cols]
Y1Y2.Y2.40.p = Y2.40.p[Y1Y2cols]
Y1Y2.Y2.10.e = Y2.10.e[Y1Y2cols]
Y1Y2.Y2.10.p = Y2.10.p[Y1Y2cols]

#for seed = 40
Y1Y2.40.e = get.jaccard.coeffs(Y1Y2.Y1.40.e, Y1Y2.Y2.40.e, k)
Y1Y2.40.p = get.jaccard.coeffs(Y1Y2.Y1.40.p, Y1Y2.Y2.40.p, k)
print("seed = 40")
print("Y1 TO Y2 EUCLIDEAN")
print(Y1Y2.40.e)
print("Y1 TO Y2 PEARSON")
print(Y1Y2.40.p)

#for seed = 10
Y1Y2.10.e = get.jaccard.coeffs(Y1Y2.Y1.10.e, Y1Y2.Y2.10.e, k)
Y1Y2.10.p = get.jaccard.coeffs(Y1Y2.Y1.10.p, Y1Y2.Y2.10.p, k)
print("seed = 10")
print("Y1 to Y2 euclidean")
print(Y1Y2.10.e)
print("Y1 to Y2 pearson")
print(Y1Y2.10.p)

#-------------------------------------------------------------------------------
# For (Y2, Y3)

Y2Y3cols = intersect(colnames(Y2.40.e), colnames(Y3.40.e))

Y2Y3.Y3.40.e = Y3.40.e[Y2Y3cols]
Y2Y3.Y3.40.p = Y3.40.p[Y2Y3cols]
Y2Y3.Y3.10.e = Y3.10.e[Y2Y3cols]
Y2Y3.Y3.10.p = Y3.10.p[Y2Y3cols]

Y2Y3.Y2.40.e = Y2.40.e[Y2Y3cols]
Y2Y3.Y2.10.e = Y2.10.e[Y2Y3cols]
Y2Y3.Y2.40.p = Y2.40.p[Y2Y3cols]
Y2Y3.Y2.10.p = Y2.10.p[Y2Y3cols]

#for seed = 40
Y2Y3.40.e = get.jaccard.coeffs(Y2Y3.Y2.40.e, Y2Y3.Y3.40.e, k)
Y2Y3.40.p = get.jaccard.coeffs(Y2Y3.Y2.40.p, Y2Y3.Y3.40.p, k)
print("seed = 40")
print("Y2 TO Y3 EUCLIDEAN")
print(Y2Y3.40.e)
print("Y2 TO Y3 PEARSON")
print(Y2Y3.40.p)

#for seed = 10

Y2Y3.10.e = get.jaccard.coeffs(Y2Y3.Y2.10.e, Y2Y3.Y3.10.e, k)
Y2Y3.10.p = get.jaccard.coeffs(Y2Y3.Y2.10.p, Y2Y3.Y3.10.p, k)

print("seed = 10")
print("Y2 TO Y3 EUCLIDEAN")
print(Y2Y3.10.e)
print("Y2 TO Y3 PEARSON")
print(Y2Y3.10.p)



