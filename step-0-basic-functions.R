

##
### 1. basic function to ungroup grouped data with smooth.spline
##

to_ungroup <- function(to_ungroup,nr_grouped_years){
	
	seq_ungrouped_years <- seq(0,length(to_ungroup)*nr_grouped_years)	
	cumsum_to_ungroup <- cumsum(c(sum(to_ungroup),to_ungroup))
	grouped_time_points <- c(0,(1:length(to_ungroup))*nr_grouped_years)
	
	applied_smooth_spline <- smooth.spline(x=grouped_time_points,y=cumsum_to_ungroup)
	predict_cumsum_ungroup <- predict(applied_smooth_spline,x=seq_ungrouped_years)$y
	ungrouped <- diff(predict_cumsum_ungroup)
	return(ungrouped)
}

##
### 2. basic function ungroup remaining life years
##

get_ungrouped_ex_2015_2020 <- function(country_name, lt_1950_2020){
	current_period_data <- lt_1950_2020[which(lt_1950_2020[,8]=="2015-2020"),]
	current_period_data <- current_period_data[which(current_period_data[,3]==country_name),]  
	current_ex_data <- as.numeric(current_period_data[,19])
	smooth_current_ex_data <- smooth.spline(x=c(0,1,seq(5,100,5)),y=current_ex_data)
	new_x <- c(seq(0,0.99,0.01),seq(1,4.99,0.01),seq(5,100,0.01))
	predict_smooth_current_ex_data <- predict(smooth_current_ex_data,new_x,len=new_x)
	return(predict_smooth_current_ex_data)
}

##
### 3. basic function to map ungrouped ifr based on thanatological age and ungrouped ex 
##

map_fr_betw_ref_and_coi_thanatAge <- function(deaths,lt_1950_2020,ungrouped_cfr_by_single_age_china_sp){

	## step 1: select ten countries wrt most deaths from COVID-19 on latest day

	country_labels <- c(0)
	country_row_number <- c(NA)
	for(pop in 1:10){
		current_pop <- deaths[order(deaths[,ncol(deaths)],decreasing=TRUE),][pop,1:2]
		country_row_number[pop] <- rownames(current_pop)
		if(!current_pop["Province.State"]==''){
			country_labels[pop] <- current_pop["Province.State"]
		}
		if(current_pop["Province.State"]==''){			
			country_labels[pop] <- current_pop["Country.Region"]
		}
	} 
	country_labels <- unlist(country_labels)
	countries_selected_JHU <- unlist(country_labels)

	## step 2: map cfr based on thanatological age

	cfr_coi_mapped_rc_china_based_on_thanat_x <- matrix(NA,nr=10,nc=length(ungrouped_cfr_by_single_age_china_sp))
	rownames(cfr_coi_mapped_rc_china_based_on_thanat_x) <- unlist(country_labels)

	for(pop in 1:10){

		current_pop <- unlist(country_labels)[pop]

		current_pop_insert <- current_pop
		if(current_pop=="US"){
			current_pop_insert <- "United States of America" 	
		}
		if(current_pop=="Hubei"){
			current_pop_insert <- "China"
		}
		if(current_pop=="Iran"){
			current_pop_insert <- "Iran (Islamic Republic of)"
	 	}

		for(chronAge in 1:90){
			current_ref_y <- get_ungrouped_ex_2015_2020(country_name="China", lt_1950_2020)$y
			current_ref_x <- get_ungrouped_ex_2015_2020(country_name="China", lt_1950_2020)$x

			current_coi_y <- get_ungrouped_ex_2015_2020(country_name=current_pop_insert, lt_1950_2020)$y
			current_coi_x <- get_ungrouped_ex_2015_2020(country_name=current_pop_insert, lt_1950_2020)$x
		 	
			current_y_ref_of_chronAge <- current_ref_y[which(current_ref_x==(chronAge-1))]
			equal_y <- which(round(current_coi_y,3)==round(current_y_ref_of_chronAge,3))[1]
			
			if(is.na(equal_y)){
				n <- 0.001
				while(is.na(equal_y)){
					equal_y <- which(round(current_coi_y,3)==(round(current_y_ref_of_chronAge,3)-n))[1]
					n <- n+0.001 
				} ## while	
			} ## if

			equivalent_x_coi <- current_coi_x[equal_y]
		
			if((round(equivalent_x_coi,0)+1)>length(ungrouped_cfr_by_single_age_china_sp)){
				equivalent_x_coi <- 89
			}

			cfr_coi_mapped_rc_china_based_on_thanat_x[pop,equivalent_x_coi] <- ungrouped_cfr_by_single_age_china_sp[chronAge]

		} ## for chronAge
	} ## for pop

	## step 3: adjust for na values that cannot be mapped

	return(cfr_coi_mapped_rc_china_based_on_thanat_x)

} ## function

##
### 4. basic function to calculate lambda_x, mapping ifr based on thanatological age, and matching age-specific deaths 
### based on global age distribution of deaths
##

get_lambda_thanatAge_globalPattern_ageSpecificDeaths <- function(deaths,global_pattern,days_observed,wom_select_10y,men_select_10y,cfr_coi_mapped_rc_china_based_on_thanat_x){

	##
	### step 1: SELECT COUNTRIES OF INTEREST WRT DEATHS:
	##

	country_labels <- c(0)
	country_row_number <- c(NA)
	for(pop in 1:10){
		current_pop <- deaths[order(deaths[,ncol(deaths)],decreasing=TRUE),][pop,1:2]
		country_row_number[pop] <- rownames(current_pop)
		if(!current_pop["Province.State"]==''){
			country_labels[pop] <- current_pop["Province.State"]
		}
		if(current_pop["Province.State"]==''){			
			country_labels[pop] <- current_pop["Country.Region"]
		}
	} 

	countries_selected_JHU <- unlist(country_labels)
	countries_selected_JHU_row <- country_row_number

	##
	### step 2: calc lambda:
	##

	lambda_thanat_x_percent_infected <- array(NA,dim = c(9,length(5:ncol(deaths)),length(countries_selected_JHU)),dimnames = list(seq(0,80,10),days_observed,countries_selected_JHU))	
	## lambda_thanat_x_percent_infected[,,1] 
	
	for(pop in 1:length(countries_selected_JHU)){
	
		current_JHU_country <- countries_selected_JHU[pop]
		current_JHU_country_row <- countries_selected_JHU_row[pop]
		
		current_pop_insert <- current_JHU_country
		if(current_pop_insert=="US"){
			current_pop_insert <- "United States of America" 	
		}
		if(current_pop_insert=="Hubei"){
			current_pop_insert <- "China"
		}
		if(current_pop_insert=="Iran"){
			current_pop_insert <- "Iran (Islamic Republic of)"
		}

		for(day in 1:(ncol(deaths)-4)){
			current_cfr <- cfr_coi_mapped_rc_china_based_on_thanat_x[current_JHU_country,] 
			current_cfr_sum <- c(0)
			for(group in 1:9){
				pos <- (1+10*(group-1)):(10+10*(group-1))
				current_cfr_sum[group] <- sum(current_cfr[pos])
			}

			current_sum <- (current_cfr_sum * ((wom_select_10y[current_pop_insert,]+men_select_10y[current_pop_insert,]) * 1000)) 

			if(current_JHU_country=="Hubei"){
				current_sum <- (current_cfr_sum * ((wom_select_10y["China",]+men_select_10y["China",]) * 0.0425 * 1000)) 
			}

			current_deaths <- deaths[current_JHU_country_row,(day+4)]*global_pattern
			lambda <- current_deaths / current_sum

			lambda_thanat_x_percent_infected[,day,pop] <- lambda 

		} ## for day	
	} ## for pop
	
	return(lambda_thanat_x_percent_infected)
} ## function

##
### 5. basic function to get output: sum(lambda_x), I_x, sum(I_x)
### based on lambda_x, mapping ifr based on thanatological age, and matching age-specific deaths 
### based on global age distribution of deaths
##

get_output_thanatAge_globalPattern_ageSpecificDeaths <- function(lambda_thanat_x_percent_infected,days_observed,wom_select_10y,men_select_10y){

	##
	### step 1: SELECT COUNTRIES OF INTEREST WRT DEATHS:
	##

	country_labels <- c(0)
	country_row_number <- c(NA)
	for(pop in 1:10){
		current_pop <- deaths[order(deaths[,ncol(deaths)],decreasing=TRUE),][pop,1:2]
		country_row_number[pop] <- rownames(current_pop)
		if(!current_pop["Province.State"]==''){
			country_labels[pop] <- current_pop["Province.State"]
		}
		if(current_pop["Province.State"]==''){			
			country_labels[pop] <- current_pop["Country.Region"]
		}
	} 

	countries_selected_JHU <- unlist(country_labels)
	countries_selected_JHU_row <- country_row_number

	##
	### step 2: calc output:
	##

	output <- list()

	I_x <- lambda_thanat_x_percent_infected

	total_I <- matrix(NA,nr=length(countries_selected_JHU),nc=length(5:ncol(deaths)))
	rownames(total_I) <- countries_selected_JHU	

	total_lambda <- matrix(NA,nr=length(countries_selected_JHU),nc=length(5:ncol(deaths)))
	rownames(total_lambda) <- countries_selected_JHU	

	for(pop in 1:length(countries_selected_JHU)){
	
		current_JHU_country <- countries_selected_JHU[pop]
		current_JHU_country_row <- countries_selected_JHU_row[pop]
		
		current_pop_insert <- current_JHU_country
		if(current_pop_insert=="US"){
			current_pop_insert <- "United States of America" 	
		}
		if(current_pop_insert=="Hubei"){
			current_pop_insert <- "China"
		}
		if(current_pop_insert=="Iran"){
			current_pop_insert <- "Iran (Islamic Republic of)"
		}

		for(day in 1:(ncol(deaths)-4)){
			current_pop <- ((wom_select_10y[current_pop_insert,]+men_select_10y[current_pop_insert,]) * 1000)
			
			if(current_JHU_country=="Hubei"){
				current_pop <- ((wom_select_10y["China",]+men_select_10y["China",]) * 0.0425 * 1000)
			}

			current_I_x <- lambda_thanat_x_percent_infected[,day,pop] * current_pop

			I_x[,day,pop] <- current_I_x

			total_I[pop,day] <- sum(current_I_x)

			total_lambda[pop,day] <- sum(current_I_x) / sum(current_pop)

		} ## for day	

	} ## for pop
	
	
	output$I_x <- I_x
	output$total_I <- total_I
	output$lambda_x <- lambda_thanat_x_percent_infected
	output$total_lambda <- total_lambda

	return(output)

} ## function

##
### 6. basic function to calculate lambda_x, mapping ifr based on chronological age, and matching age-specific deaths 
### based on global age distribution of deaths
##

get_lambda_chronAge_globalPattern_ageSpecificDeaths <- function(deaths,global_pattern,days_observed,wom_select_10y,men_select_10y,ifr_ref){

	##
	### step 1: SELECT COUNTRIES OF INTEREST WRT DEATHS:
	##

	country_labels <- c(0)
	country_row_number <- c(NA)
	for(pop in 1:10){
		current_pop <- deaths[order(deaths[,ncol(deaths)],decreasing=TRUE),][pop,1:2]
		country_row_number[pop] <- rownames(current_pop)
		if(!current_pop["Province.State"]==''){
			country_labels[pop] <- current_pop["Province.State"]
		}
		if(current_pop["Province.State"]==''){			
			country_labels[pop] <- current_pop["Country.Region"]
		}
	} 

	countries_selected_JHU <- unlist(country_labels)
	countries_selected_JHU_row <- country_row_number

	##
	### step 2: calc lambda:
	##

	lambda_chron_x_percent_infected <- array(NA,dim = c(9,length(5:ncol(deaths)),length(countries_selected_JHU)),dimnames = list(seq(0,80,10),days_observed,countries_selected_JHU))	
	
	for(pop in 1:length(countries_selected_JHU)){
	
		current_JHU_country <- countries_selected_JHU[pop]
		current_JHU_country_row <- countries_selected_JHU_row[pop]
		
		current_pop_insert <- current_JHU_country
		if(current_pop_insert=="US"){
			current_pop_insert <- "United States of America" 	
		}
		if(current_pop_insert=="Hubei"){
			current_pop_insert <- "China"
		}
		if(current_pop_insert=="Iran"){
			current_pop_insert <- "Iran (Islamic Republic of)"
		}

		for(day in 1:(ncol(deaths)-4)){
			current_cfr_sum <- ifr_ref

			current_sum <- (current_cfr_sum * ((wom_select_10y[current_pop_insert,]+men_select_10y[current_pop_insert,]) * 1000)) 

			if(current_JHU_country=="Hubei"){
				current_sum <- (current_cfr_sum * ((wom_select_10y["China",]+men_select_10y["China",]) * 0.0425 * 1000)) 
			}

			current_deaths <- deaths[current_JHU_country_row,(day+4)]*global_pattern
			lambda <- current_deaths / current_sum

			lambda_thanat_x_percent_infected[,day,pop] <- lambda 

		} ## for day	
	} ## for pop
	
	return(lambda_thanat_x_percent_infected)
} ## function

##
### 7. basic function to get output: sum(lambda_x), I_x, sum(I_x)
### based on lambda_x, mapping ifr based on chronological age, and matching age-specific deaths 
### based on global age distribution of deaths
##

get_output_chronAge_globalPattern_ageSpecificDeaths <- function(lambda_chron_x_percent_infected,days_observed,wom_select_10y,men_select_10y){

	##
	### step 1: SELECT COUNTRIES OF INTEREST WRT DEATHS:
	##

	country_labels <- c(0)
	country_row_number <- c(NA)
	for(pop in 1:10){
		current_pop <- deaths[order(deaths[,ncol(deaths)],decreasing=TRUE),][pop,1:2]
		country_row_number[pop] <- rownames(current_pop)
		if(!current_pop["Province.State"]==''){
			country_labels[pop] <- current_pop["Province.State"]
		}
		if(current_pop["Province.State"]==''){			
			country_labels[pop] <- current_pop["Country.Region"]
		}
	} 

	countries_selected_JHU <- unlist(country_labels)
	countries_selected_JHU_row <- country_row_number

	##
	### step 2: calc output:
	##

	output <- list()

	I_x <- lambda_chron_x_percent_infected

	total_I <- matrix(NA,nr=length(countries_selected_JHU),nc=length(5:ncol(deaths)))
	rownames(total_I) <- countries_selected_JHU	

	total_lambda <- matrix(NA,nr=length(countries_selected_JHU),nc=length(5:ncol(deaths)))
	rownames(total_lambda) <- countries_selected_JHU	

	for(pop in 1:length(countries_selected_JHU)){
	
		current_JHU_country <- countries_selected_JHU[pop]
		current_JHU_country_row <- countries_selected_JHU_row[pop]
		
		current_pop_insert <- current_JHU_country
		if(current_pop_insert=="US"){
			current_pop_insert <- "United States of America" 	
		}
		if(current_pop_insert=="Hubei"){
			current_pop_insert <- "China"
		}
		if(current_pop_insert=="Iran"){
			current_pop_insert <- "Iran (Islamic Republic of)"
		}

		for(day in 1:(ncol(deaths)-4)){
			current_pop <- ((wom_select_10y[current_pop_insert,]+men_select_10y[current_pop_insert,]) * 1000)
			
			if(current_JHU_country=="Hubei"){
				current_pop <- ((wom_select_10y["China",]+men_select_10y["China",]) * 0.0425 * 1000)
			}

			current_I_x <- lambda_chron_x_percent_infected[,day,pop] * current_pop

			I_x[,day,pop] <- current_I_x

			total_I[pop,day] <- sum(current_I_x)

			total_lambda[pop,day] <- sum(current_I_x) / sum(current_pop)

		} ## for day	

	} ## for pop
	
	
	output$I_x <- I_x
	output$total_I <- total_I
	output$lambda_x <- lambda_thanat_x_percent_infected
	output$total_lambda <- total_lambda

	return(output)

} ## function

