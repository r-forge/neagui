checkObject <- function (x,del=TRUE) {
			if (del==F) {
				exists(x, envir=.GlobalEnv)
			} 
			else {
				if(exists(x, envir=.GlobalEnv) ) remove(list=x, envir=.GlobalEnv)
			}
			
		}