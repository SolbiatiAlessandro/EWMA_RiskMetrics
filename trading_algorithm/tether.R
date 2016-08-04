tether <- function(HL,t=50){
	l=length(HL[,1])
	tether <- c()
	for(i in 51:l){
		high <- max(HL[(i-t):i,1])
		low <- min(HL[(i-t):i,2])
		tether[i] <- (high+low)/2
		#print(c(high,low,tether[i],serie[i],i,(i-t)))
		
	}
	tether
}
