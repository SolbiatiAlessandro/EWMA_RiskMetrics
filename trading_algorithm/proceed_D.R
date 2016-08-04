

proceed <- function(serie,HTA_sign){
	
	sign_up <- c(date_sign(serie,HTA_sign,1),as.Date("3000-01-01"))
	#print(sign_up)
	count_up <- 1
	sign_sell <- c(date_sign(serie,HTA_sign,2),as.Date("3000-01-01"))
	sign_down <- c(date_sign(serie,HTA_sign,3),as.Date("3000-01-01"))
	#print(sign_down)
	count_down <- 1
	sign_buy <- c(date_sign(serie,HTA_sign,4),as.Date("3000-01-01"))

	output <- c(1)
	count_out <- 1
	
	jump <- 0
	
	for(i in 1:(4*length(sign_up))){

			if(sign_up[count_up]<sign_down[count_down]){	
				if(count_up==1 || HTA_sign[count_up,1]>HTA_sign[(count_up-1),1]){
				
				ss <- as.numeric(serie[HTA_sign[count_up,1]+1,1])
				print("OPEN POSITION - buy")
				print(sign_up[count_up]) #trigger's date
				print(ss) #open of the next day
				
				ee <- as.numeric(serie[HTA_sign[count_up,2]+1,1])
				print("CLOSE POSITION - sell")
				print(sign_sell[count_up])
				print(ee)
				
				
				
				if(jump==0){
				output[count_out] <- (ee-ss)/ss
				print(output[count_out])
				if(output[count_out] > 0.075){
					jump <- 1
					}
				}
				
				else {
					print("JUMPPPPP**************")
					jump <- 0
				}
				
		
		
				count_out <- count_out+1
				count_up <- count_up+1
				
				print("---------------")
				
				}
				else{ count_up = length(sign_up) }
			}
			
			if(sign_down[count_down]<sign_up[count_up]){
				if(count_down==1 || HTA_sign[count_down,3]>HTA_sign[(count_down-1),3]){
					
				ss <- as.numeric(serie[HTA_sign[count_down,3]+1,1])	
				print("OPEN POSITION - sell")
				print(sign_down[count_down])
				print(ss)
				
				ee <- as.numeric(serie[HTA_sign[count_down,4]+1,1])
				print("CLOSE POSITION - buy")
				print(sign_buy[count_down])
				print(ee)			
				
				
				
				
				if(jump==0){
				output[count_out] <- (ee-ss)/ss
				print(output[count_out])
				if(output[count_out]>0.075){
					jump <- 1
					}
				}
				
				else {
					print("JUMPPPPP**************")
					jump <- 0
				}
				
				count_out <- count_out+1
				count_down <- count_down+1
				
				
				print("---------------")

				
			}
				else{ count_down = length(sign_down) }
			}
	
	}
	
	
	output

}