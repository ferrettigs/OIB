#' Search for Vessels Close to Popups
#'
#' This function searches for vessels nearby popup events. It uses the Global Fishing Watch (GFW) AIS database and to Tag of Pelagic Predator (TOPP, `http://gtopp.org/`) database and searches within a time window specified by the user, centered on the popup date, with a spatial box specified by the user in latitude and longitude degrees.
#' @param con a connection to the OIB - TOPP database.
#' @param project Google Cloud Platform project name. This is needed to query the GFW database in bigQuery.
#' @param ptt is the argos ID. If the tag was reused that it is possible to have multiple tags for the sam ptt. In that case use latest date.
#' @param spatWin spatial window in degrees
#' @param tempWin temporal window in hours
#' @param gears fishing gears to search
#' @export 
searchVessel = function(con, project, ptt = "153499", spatWin = 1, tempWin = 24, gears = c("drifting_longlines","trawlers","purse_seines","unknown_fishing","squid_jigger", "pole_and_line","drifting_gillnets","no_idea_what_it_is", "research other_fishing", "set_longlines", "troller", "set_gillnets", "pots_and_traps")){


#-------------------------------------------------------------------------------------
# 	dat = fetch(dbSendQuery(con, statement = paste("select poplat, poplon, popdate, popup_lc from oibtable where ptt = ",ptt,sep = "")), n = -1)
# 
# 	names(dat) = c("lat","lon","date","Location.class")
# 
# 
# 	dat$date = strptime(dat$date, format = "%m/%d/%Y %H:%M", tz = "GMT") # timestamps from drifters are indeed in GMT format - same is for AIS data
#----------------------------------------------------------------------
# need to use a connection from TOP or move the popups_diag in OIB


dat = getDrift(con,ptt)

#-------------------------------------------------------------------------

#locErr = read.csv("../data/locationErr.csv") # location error ranges
locErr = locationErr # location error ranges - this is an internal dataset

dat = merge(dat, locErr, by.x = "lc", by.y = "Location.class", all.x =TRUE)
popdat = dat[dat$datetime==max(dat$datetime),] # popup row - so date
# I am going to change this line to allow the search for the last pinging location
gfwlag = 7 # in days
if (abs(difftime(max(dat$datetime),as.POSIXlt(Sys.Date()), units = "days"))<gfwlag) {
					dat = dat[dat$datetime< max(dat$datetime)-as.difftime(gfwlag, units = "days"), ]
					popdat = dat[dat$datetime==max(dat$datetime),]
					}

mindate = popdat$date-as.difftime(paste(tempWin/2,":00:00",sep=""))
maxdate = popdat$date+as.difftime(paste(tempWin/2,":00:00",sep=""))	
# create a spatial window of 1 degree centered on the ping location in both lat and lon
minlat = popdat$lat-spatWin/2 # these are too large
maxlat = popdat$lat+spatWin/2

minlon = popdat$lon-spatWin/2
maxlon = popdat$lon+spatWin/2

# now connect with bigquery

require(AIS) # for cleanSql

sql = paste("SELECT
    mmsi, 
    timestamp, 
    lat,
    lon, 
    shiptype
  FROM TABLE_DATE_RANGE([",project,":pipeline_p_p550_daily.classify_],TIMESTAMP('",mindate,"'),TIMESTAMP('",maxdate,"'))
 WHERE 	lat >",minlat,"
    AND lon <",maxlon,"
    AND lat <",maxlat,"
    AND lon >",minlon, sep = "")
sql = cleanSql(sql)


res <- bigrquery::query_exec(sql, project)

res$distance = geosphere::distm(x = res[,c("lon","lat")], y = popdat[,c("lon","lat")])/1000 # distance in kilometers
res$timediff = difftime(res$timestamp,popdat$datetime[1], units = "mins")

# now I need to check how many of these have been identified as fishing ships
mmsis = unique(res$mmsi)

sql2 = paste("SELECT 
	shiptype_text,
	shipname,
	inferred_label,
	known_geartype,
	mmsi,
	on_fishing_list_best 
	FROM [gfw_research_archived.vessel_info_20180726] 
	WHERE mmsi IN (",paste(mmsis, collapse = ","),")", sep = "")
sql2 = cleanSql(sql2)	

ships <- bigrquery::query_exec(sql2, project)
fs = subset(ships, shiptype_text=="Fishing" | inferred_label %in% gears)
if (nrow(fs)==0) print("No fishing boat identified")

# now extract locations in res
locs = subset(res, mmsi %in% unique(fs$mmsi))
locs <- with(locs, locs[order(mmsi, timestamp),]) 

out = list(locs = locs, ships = ships, popdat = popdat, drfdat = dat)
out
}


  
  
#' Get Fishing Vessel Tracks in in Bounding Box
#'
#' This function searches for vessels nearby popup events. It uses the Global Fishing Watch (GFW) AIS database and to Tag of Pelagic Predator (TOPP, `http://gtopp.org/`) database and searches within a time window specified by the user, centered on the popup date, with a spatial box specified by the user in latitude and longitude degrees.
#' @param con a connection to the OIB - TOPP database.
#' @param project Google Cloud Platform project name. This is needed to query the GFW database in bigQuery.
#' @param time1 initial date of observation period: "YYY-mm-dd"
#' @param time2 initial date of observation period: "YYY-mm-dd"
#' @param gears fishing gears to search
#' @export 
getVesselTracksBox = function(con, project, bbox = c(18,40,-145,-120), date1 = "2018-04-25", date2 = "2018-05-06", gears = c("drifting_longlines","trawlers","purse_seines","unknown_fishing","squid_jigger", "pole_and_line","drifting_gillnets","no_idea_what_it_is", "research other_fishing", "set_longlines", "troller", "set_gillnets", "pots_and_traps","tuna_purse_seines")){

	time1 = paste(date1," 00:00", sep = "")
	time2 = paste(date2," 23:59", sep = "")

	minlat = bbox[1]
	maxlat = bbox[2]
	minlon = bbox[3]
	maxlon = bbox[4]



# now connect with bigquery

require(AIS) # for cleanSql


sql = paste("SELECT
  	* 
	FROM (
  		SELECT
    	mmsi,
    	timestamp,
    	lat,
    	lon,
    	shipname,
    	measure_new_score,
    	seg_id 
  		FROM 	TABLE_DATE_RANGE([",project,":pipeline_p_p550_daily.classify_],TIMESTAMP('",time1,"'),TIMESTAMP('",time2,"'))
 		WHERE 	lat >",minlat,"
    	AND lon <",maxlon,"
    	AND lat <",maxlat,"
    	AND lon >",minlon,") 
		WHERE mmsi in (SELECT mmsi 
				 	   FROM [gfw_research.vessel_info_20180418] 
					   WHERE best_label IS NOT NULL  
				 	   AND offsetting IS FALSE)  
    	AND measure_new_score > .5 
    	AND seg_id IN (
    	SELECT seg_id 
		FROM [world-fishing-827:gfw_research.pipeline_p_p550_daily_segs] 
		WHERE good_seg)", sep = "") 
  		# the segment id is useful to avoid plotting bad signals		
sql = cleanSql(sql)


# ------------------------------------

res <- bigrquery::query_exec(sql, project, max_pages = Inf)
mmsis = unique(res$mmsi)

# now I need to check how many of these have been identified as fishing ships


sql2 = paste("SELECT 
	shiptype_text,
	inferred_label,
	mmsi,
	shipname_norm,
	country_name,
	on_fishing_list 
	FROM [gfw_research.vessel_info_20180418] 
	WHERE mmsi IN (",paste(mmsis, collapse = ","),")", sep = "")
sql2 = cleanSql(sql2)	

ships <- bigrquery::query_exec(sql2, project)

fs = subset(ships, shiptype_text=="Fishing" | inferred_label %in% gears)
if (nrow(fs)==0) print("No fishing boat identified")
fs = unique(fs[,c("inferred_label","mmsi","on_fishing_list","shipname_norm","country_name")])
fs$country_name = with(fs, ifelse(country_name=="United States","USA",country_name))
res2 = res[res$mmsi %in% fs$mmsi,]
res2 = merge(res2, fs, by = "mmsi", all.x = TRUE)
res2$vessel = with(res2, paste(shipname_norm," ", gsub("_","",abbreviate(res2$inferred_label))," (",country_name,")", sep = "")) 


res3 = with(res2, aggregate(list(timestamp = timestamp), list(vessel = vessel), max))
res4 = unique(merge(res3, res2[,c("lat","lon","timestamp","vessel","shipname_norm")], by = c("vessel","timestamp"), all.x = TRUE))

out = list(ships = ships, fs = fs)
out
}

#-----------------------------------------------------------------------------------------
# 							including tags
#----------------------------------------------------------------------------------------
#' Get White Shark Tag data from Cafe'
#'
#' @param date1 initial date
#' @param date2 final date
#' @export

getCafeTagData = function(date1, date2){

	con = connectTOPP() # this work with admin connectTOPP
	metadat = fetch(dbSendQuery(con, statement = "select sgdepkey,tagcode,ptt,tagdate,progpopdate,popdate,poplon,poplat,rdate from tblshark_deploy_rec_all where sgdepkey::text like '1917%' and tagtype ilike 'sat%' and seriesname not ilike '%skomal%' and progpopdate is not null order by 1", n = -1)) 

# white sharkpopup tags - tags that have popped during the white shark cafe trips
	system(" scp mola.stanford.edu:/TOPP/code/bin/castleton/files/cafe_drift_positions.csv ../data/")
	dat = read.csv("~/RA/SeaQL/tagSearch/data/cafe_drift_positions.csv")
	dat$date = strptime(dat$date, format = "%m/%d/%Y %H:%M", tz = "GMT") 

	dat2 = dat[dat$date > strptime(time1, format = "%Y-%m-%d %H:%M", tz = "GMT"), ]
	dat2 = dat2[dat2$date < strptime(time2, format = "%Y-%m-%d %H:%M", tz = "GMT"), ]

	dat3 = with(dat2, aggregate(list(date = date), list(ptt = ptt), max))
	dat4 = merge(dat3, dat2, by = c("date","ptt"), all.x = TRUE)
 	dat4
 }
 	
