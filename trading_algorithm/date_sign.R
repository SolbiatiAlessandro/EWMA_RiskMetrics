


date_sign <- function(serie,sign,mode){
	date_sign_first <- time(serie)[sign[1,mode]]
	date_sign <- c(date_sign_first)
	for(i in 2:length(sign[,mode])){
		date_sign[i]=time(serie)[sign[i,mode]]
		#print(date_sign[i])
	}
	date_sign
}

