#' Get Drifting Positions of PAT tags
#'
#' This function connects to the TOPP database and extract drifting positions of floating PATs based on their ptt. 
#' @param con a connection to the OIB - TOPP database.
#' @param ptt is the argos ID. If the tag was reused that it is possible to have multiple tags for the sam ptt. In that case use latest date.
#' @export 
getDrift = function(con, ptt = 173165){ 
	dat2 = fetch(dbSendQuery(con, statement = paste("select pttnumber,datetime,lat1,lon1,lc from popups_diag where pttnumber = ",ptt," order by 2",sep = "")), n = -1)

# This has all the drifters - not all of the tags that are associated with the ARGOS account. This only drifting positions, not popup data. It has the location quality. this is importnat for the reliability of the speed. But I can come out with confidence intervals. Mike is going to create the table with the wildlife computer associated tags. 

	require(raster) 
	dat2$lat1 = trim(dat2$lat1) # removes leading and trailing spaces
	dat2$emi = substr(as.character(dat2$lat1),regexpr("N",dat2$lat1),regexpr("N",dat2$lat1)) # extract emisphere
	dat2 = dat2[(dat2$emi!=""),] # removes lines with  non coords
	dat2$lat = as.numeric(sub("N|S","",dat2$lat1))
	dat2$lat = with(dat2, ifelse(emi=="N",lat*1,lat*-1))

	dat2$lon1 = trim(dat2$lon1) # removes leading and trailing spaces
	dat2$side = with(dat2,substr(as.character(lon1),regexpr("W|E",lon1),regexpr("W|E",lon1))) # extract whether is east or west
	dat2$lon = as.numeric(sub("W|E","",dat2$lon1))
	dat2$lon = with(dat2, ifelse(side=="E",lon*1,lon*-1))
	dat2$datetime = strptime(dat2$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") # make sure I have timestamps in the right format
	dat2[order(dat2$datetime),] # make sure it goes from popup to most recent location\
dat2
}