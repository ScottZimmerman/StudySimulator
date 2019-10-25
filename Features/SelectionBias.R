function(outcome,exposure){

  measureATE_givenS_NoLoop <- function(selector){
    print("Measuring ATE without loop")
    Y_Table <- pXgivenPa[[outcome]]
    W_string <- paste0(confounders,collapse=", ")

    A1_confs <- paste0(paste0("a1.",confounders, " as ",confounders),collapse=", ")
    joinString <- paste0(paste0("a1.",confounders, "=a0.",confounders),collapse=" and ")

    A1_query <- paste0("select ",W_string,", P from Y_Table where ",selector," = 1 and ",exposure," = 1 and ",outcome," = 1 order by ",W_string)
    A0_query <- paste0("select ",W_string,", P from Y_Table where ",selector," = 1 and ",exposure," = 0 and ",outcome," = 1 order by ",W_string)
    RD_query <- paste0(paste0("select ",A1_confs,", a1.P - a0.P as RD_W from (",A1_query,") as a1 join (",A0_query,") as a0 on ",joinString),collapse="")
    W_query <- paste0("select ",W_string,", Sum(P) as P from fullJD group by ",W_string," order by ",W_string)
    joinString2 <- paste0(paste0("j.",confounders, "=w.",confounders),collapse=" and ")

    query <- paste0(c("select sum(j.RD_W*w.P) as ATE from (",RD_query,") as j join (",W_query,") as w on ",joinString2),collapse="")
    print(query)
    ATE <- sqldf(query)[1,"ATE"]
    return(ATE)
  }
  
	f <- function(name,options){
		if("SelectionBias" %in% names(features) && !is.na(features$SelectionBias)){
			features[[name]] <<- features$SelectionBias
		}else{
			features[[name]] <<- measureATE_givenS_NoLoop(options$selector) - features$ATE
		}
	}
	return(f)
}
