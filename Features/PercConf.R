function(outcome,exposure){
	f <- function(name,options){
		if("PercConf" %in% names(features) && !is.na(features$PercConf)){
			features[[name]] <<- features$PercConf
		}else{
			features[[name]] <<- features$Conf/features$ATE
		}
	}
	return(f)
}
