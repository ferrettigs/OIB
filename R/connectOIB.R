#' Connect to The OIB database
#'
#' @export
connectOIB = function(){
  require(RPostgreSQL)
  require(RH2) # this is important to avoid errors in the connection
 	dbname <- "oib"
  	dbuser <- "oibuser"
  	dbpass <- "oibpass"
  	dbhost <- "mola.stanford.edu"
  	drv <- dbDriver("PostgreSQL") 
  	con <- dbConnect(drv, host=dbhost, dbname=dbname,  user=dbuser, password=dbpass
  	) 
}