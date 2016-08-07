effettiv_VaR <- function(nome){
	
	serie <- get.hist.quote(nome)
	serie_h <- HTA_signal(nome,400)
	serie_p <- proceed(serie,serie_h)
	serie_date <- clean_date(date_sign(serie,serie_h,1),date_sign(serie,serie_h,3))
	
	i_alti <- c()
	count_alti <- 1
	for(i in 1:length(serie_p)){
		if(as.numeric(serie_p[i]) > 0.1 || as.numeric(serie_p[i]) < -0.1){
			print(serie_p[i])
			i_alti[count_alti] <- i
			count_alti <- count_alti+1		
			}	
	}
	
	serie_alti <- serie_p[i_alti]
	serie_date_alti <- serie_date[i_alti]
	
	vars <- c()
	vars_mean <- c()
	count_vars <- 1
	
	for(j in 1:length(serie_date_alti)){
		
		print(serie_date_alti)
		print(head(time(serie)))
		day_numeric <- count_date(serie[,4], serie_date_alti[j])
		print(day_numeric)
		sst_study <- study(as.timeSeries(serie[,4]),"1900-01-01", end=time(serie)[day_numeric-130] , N_day=day_numeric)
		vars[count_vars] <- sst_study[length(sst_study[,1]),1]	
		
		
		mean_with_na <- sst_study[(length(sst_study[,1])-100):length(sst_study[,1]),1]
		vars_mean[count_vars] <- mean(mean_with_na[!is.na(mean_with_na)])
		
		count_vars <- count_vars+1

		
	}
	
	print("--------------------")
	cbind(serie_alti, as.Date(serie_date_alti), vars, vars_mean)
}