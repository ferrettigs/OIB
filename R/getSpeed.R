#' Get Speed from Satellite Track
#'
#' 
#' @param dat a data.frame with a timeseries of location points from PAT tags or AIS track. It has to have coordinates (lat and lon) and timestamps (datetime) in GMT
#' @param track whether it is a PAT track or AIS
#' @export 
getSpeed = function(dat, track = "PAT"){
	dat = dat[order(dat$datetime),]
	if (track=="PAT") dat = dat[dat$lc %in% c("3","G"),] 
	dat$distance = (geosphere::distm(x = dat[,c("lon","lat")])/1000)[1,] # distance in kilometers
	dat$reldist = abs(c(0,diff(dat$distance))) # distance cannot be negative. Sometimes is negative because the next point may be closer to the first because the drifter is changing course and getting closer to the initial point.
	#dat$reltime = c(0, diff(dat$datetime)/60) # time diffe in hours, decimal
	dat$reltime = c(0, as.numeric(diff(dat[,grep("timestamp|datetime", names(dat))]), units = "hours"))
	dat$speed = with(dat, reldist/reltime)
	dat
}


#' Get Closest Positions between Fish and Ships
#'
#' 
#' @param out output object from \code{searchVessel} function
#' @export 
getClosestPosition = function(out){

out$drfdat = unique(out$drfdat)
out$locs = unique(out$locs)

dm = geosphere::distm(out$drfdat[,c("lon","lat")], out$locs[,c("lon","lat")])/1000
# matrix of distances in km

tm = outer(out$drfdat[,"datetime"], out$locs[,"timestamp"], "-")
# matrix of differences in time in days
# now I can try to find the euclidean distance
edist = dist(cbind(as.vector(dm), as.vector(tm))) # but it sucks all the memory

#library(pdist)
dists <- pdist(out$drfdat[,c("lon","lat","datetime")], out$locs[,c("lon","lat", "timestamp")])
edist = as.matrix(dists) # matrix of euclidean distances
tag = out$drfdat[which(edist == min(edist), arr.ind = TRUE)[,1],][,c("lat","lon", "datetime")]
boat = out$locs[which(edist == min(edist), arr.ind = TRUE)[,2],][,c("lat","lon","timestamp")]
names(boat)<-c("lat","lon","datetime")

segment = rbind(tag,boat)
segment$distance = (geosphere::distm(segment[,c("lon","lat")])/1000)[1,]
segment$timelag = c(0,diff(segment[,c("datetime")]))
segment

}