function(outcome,exposure){
	f <- function(name,options){
		outList <- list()
		givenList <- list()
		outList[[outcome]] <- 1

		var1Name <- options$variables[[1]]$name
		var2Name <- options$variables[[2]]$name

		givenList[[var1Name]] <- options$variables[[1]]$values$lb
		givenList[[var2Name]] <- options$variables[[2]]$values$lb
		R_00 <- CP(outList,givenList)
		
		givenList[[var1Name]] <- options$variables[[1]]$values$ub
		givenList[[var2Name]] <- options$variables[[2]]$values$lb
		R_10 <- CP(outList,givenList)
		
		givenList[[var1Name]] <- options$variables[[1]]$values$lb
		givenList[[var2Name]] <- options$variables[[2]]$values$ub
		R_01 <- CP(outList,givenList)
		
		givenList[[var1Name]] <- options$variables[[1]]$values$ub
		givenList[[var2Name]] <- options$variables[[2]]$values$ub
		R_11 <- CP(outList,givenList)
		features[[name]] <<- (R_11-R_00)-((R_10-R_00)+(R_01-R_00))
	}
	return(f)
}
