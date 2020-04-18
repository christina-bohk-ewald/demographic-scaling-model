
####
##
## Estimate COVID-19 infections based on mapping Chinese infection fatality rates via thanatological age
## age-specific deaths derived with global deaths distribution by age
## ----take ifr of Verity et al. (2020)
## ----calculate global deaths distribution by age based on data of Dudel et al. (2020) 
##
####

##
### 1. Ungroup reference infection fatality rate with smooth.spline
##

ungrouped_mode_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=ifr_by_age_china_verity[,2],
								nr_grouped_years=10)

ungrouped_low95_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=ifr_by_age_china_verity[,3],
								nr_grouped_years=10)

ungrouped_up95_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=ifr_by_age_china_verity[,4],
								nr_grouped_years=10)

##
### 2. Ungroup remaining life years (ex) & map reference countrie's infection fatality rate via thanatological age
## (this could take a minute...)
##

mapped_mode_ifr_thanatAge <- map_fr_betw_ref_and_coi_thanatAge(deaths=deaths,
						lt_1950_2020=lt_1950_2020,
						ungrouped_cfr_by_single_age_china_sp=ungrouped_mode_ifr_by_single_age_china_sp)

mapped_low95_ifr_thanatAge <- map_fr_betw_ref_and_coi_thanatAge(deaths=deaths,
						lt_1950_2020=lt_1950_2020,
						ungrouped_cfr_by_single_age_china_sp=ungrouped_low95_ifr_by_single_age_china_sp)

mapped_up95_ifr_thanatAge <- map_fr_betw_ref_and_coi_thanatAge(deaths=deaths,
						lt_1950_2020=lt_1950_2020,
						ungrouped_cfr_by_single_age_china_sp=ungrouped_up95_ifr_by_single_age_china_sp)

#
## Adjust for NAs (that may be there in rare cases when values cannot be mapped) 
#

mapped_mode_ifr_thanatAge 

	for(pop in 1:10){
		pos_na <- which(is.na(mapped_mode_ifr_thanatAge[pop,]))
		if(length(pos_na)>0){
		for(pos in 1:length(pos_na)){
			if(pos_na[pos] < 6){
				mapped_mode_ifr_thanatAge[pop,pos_na[pos]] <- min(mapped_mode_ifr_thanatAge[pop,],na.rm=TRUE)
			}
			if(pos_na[pos] >= 6){
				mapped_mode_ifr_thanatAge[pop,pos_na[pos]] <- mapped_mode_ifr_thanatAge[pop,pos_na[pos]-1]
			}
		} ## for pos
		} ## if
	} ## for pop

mapped_mode_ifr_thanatAge 

##

mapped_low95_ifr_thanatAge 

	for(pop in 1:10){
		pos_na <- which(is.na(mapped_low95_ifr_thanatAge[pop,]))
		if(length(pos_na)>0){
		for(pos in 1:length(pos_na)){
			if(pos_na[pos] < 6){
				mapped_low95_ifr_thanatAge[pop,pos_na[pos]] <- min(mapped_low95_ifr_thanatAge[pop,],na.rm=TRUE)
			}
			if(pos_na[pos] >= 6){
				mapped_low95_ifr_thanatAge[pop,pos_na[pos]] <- mapped_low95_ifr_thanatAge[pop,pos_na[pos]-1]
			}
		} ## for pos
		} ## if
	} ## for pop

mapped_low95_ifr_thanatAge 

##

mapped_up95_ifr_thanatAge 

	for(pop in 1:10){
		pos_na <- which(is.na(mapped_up95_ifr_thanatAge[pop,]))
		if(length(pos_na)>0){
		for(pos in 1:length(pos_na)){
			if(pos_na[pos] < 6){
				mapped_up95_ifr_thanatAge[pop,pos_na[pos]] <- min(mapped_up95_ifr_thanatAge[pop,],na.rm=TRUE)
			}
			if(pos_na[pos] >= 6){
				mapped_up95_ifr_thanatAge[pop,pos_na[pos]] <- mapped_up95_ifr_thanatAge[pop,pos_na[pos]-1]
			}
		} ## for pos
		} ## if
	} ## for pop

mapped_up95_ifr_thanatAge 

##
### 3. Calculate lambda_x (age-specific population fraction of people being infected with COVID-19)
### mapping Chinese IFR via thanatological age and age-specific deaths
##

## Get global age distribution of deaths from previous step 1-b

setwd(the.data.path)
source("global_age_dist_deaths.R")

##

lambda_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_lambda_thanatAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										cfr_coi_mapped_rc_china_based_on_thanat_x=mapped_mode_ifr_thanatAge)

## lambda_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths

lambda_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_lambda_thanatAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										cfr_coi_mapped_rc_china_based_on_thanat_x=mapped_low95_ifr_thanatAge)

## lambda_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths

lambda_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_lambda_thanatAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										cfr_coi_mapped_rc_china_based_on_thanat_x=mapped_up95_ifr_thanatAge)

## lambda_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths

##
### 4. Calculate different output: sum(lambda_x), I_x, sum(I_x) 
##

output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_output_thanatAge_globalPattern_ageSpecificDeaths(lambda_thanat_x_percent_infected=lambda_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths)
## output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda

output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_output_thanatAge_globalPattern_ageSpecificDeaths(lambda_thanat_x_percent_infected=lambda_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths)
## output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda

output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths <- get_output_thanatAge_globalPattern_ageSpecificDeaths(lambda_thanat_x_percent_infected=lambda_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths)
## output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda

##
### 5. Visualize total lambda (based on data downloaded on April 18, 2020)
##

##
### modal ifr
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-mode-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(cfr))),ylim=c(0,0.02),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's modal IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(cfr)),y=output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(cfr)),y=output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(cfr)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.02,0.005),labels=TRUE,lwd=3,pos=0)

	text(1,0.005,"Estimation based on \n1. Mapping modal IFR via thanatological age \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.02,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-low95-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(cfr))),ylim=c(0,0.06),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's lower 95% IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(cfr)),y=output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(cfr)),y=output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(cfr)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.06,0.005),labels=TRUE,lwd=3,pos=0)

	text(1,0.015,"Estimation based on \n1. Mapping lower 95% IFR via thanatological age \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.06,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-up95-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(cfr))),ylim=c(0,0.009),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's upper 95% IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(cfr)),y=output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(cfr)),y=output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(cfr)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(cfr)),7),length(5:ncol(cfr)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.009,0.001),labels=TRUE,lwd=3,pos=0)

	text(1,0.0025,"Estimation based on \n1. Mapping upper 95% IFR via thanatological age \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.009,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### 6. Visualize total infections versus confirmed cases (based on data downloaded on April 18, 2020)
##

##
### all three (mode, high, low) together
##

setwd(the.plot.path)

dev.off()

pdf(file="top-10-confirmed-infected-allThree-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,2.4,1.2,0.4))

 	plot(x=-100,y=-100,xlim=c(0-1050,6500),ylim=c(0,10.5),xlab="",ylab="",cex.main=0.9,
		main="Confirmed cases vs probably infected, in thousand\nChina's IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

	text(c(3800,5650),c(10.15,10.15),c("Confirmed","Infected"),pos=3,cex=0.9,col="black",font=2)
	text(-1600,10.1,"Quantiles:",pos=4,cex=0.9,col="black",font=2)

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

			## 95% lower IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.8))
			text(5350,9.25-1*(pop-1)+0.05,paste(round(current_infected[length(current_infected)],1),"k)",sep=""),pos=4,col=grey(0.8),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.975",pos=3,col=grey(0.8),font=2,cex=0.9)
			}

			## modal IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.6))
			text(4800,9.25-1*(pop-1)+0.25+0.2,paste(round(current_infected[length(current_infected)],1),"k",sep=""),pos=4,col=grey(0.6),font=2)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.5",pos=3,col=grey(0.6),font=2,cex=0.9)
			}
		
			## 95% upper IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.4))
			text(4250,9.25-1*(pop-1)+0.05,paste("(",round(current_infected[length(current_infected)],1),"k, ",sep=""),pos=4,col=grey(0.4),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.025",pos=3,col=grey(0.4),font=2,cex=0.9)
			}

			lines(x=c(0,3000),y=rep((9.25-1*(pop-1)+0.25),2),col=pal[pop],lty=2,lwd=1)
			rect(xleft=0,xright=confirmed[current_JHU_country_row,ncol(confirmed)]/1000,ybottom=9.25-1*(pop-1),ytop=9.25-1*(pop-1)+0.5,col=pal[pop])
			text(3000,9.25-1*(pop-1)+0.25,paste(round(current_confirmed[length(current_infected)],1),"k",sep=""),pos=4,col=pal[pop],font=2)

	}

	axis(side=1,at=seq(0,3000,500),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=seq(0,3000,1000),labels=TRUE,lwd=3,pos=0)
	axis(side=2,at=seq(0.5,9.5,1),labels=paste(rev(seq(1,10,1)),". ",rev(country_labels),sep=""),lwd=3,pos=0)

dev.off()


##
### 7. How many more infections than confirmed cases (based on data downloaded on April 18, 2020)
##

##
### modal IFR
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-times-as-many-infections-modal-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,7),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's modal IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

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

			current_deaths <- deaths[current_JHU_country_row,5:ncol(deaths)]
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]
			current_infected <- output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_mode_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]
			}

			points(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)
			lines(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)

	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,7,0.5),labels=FALSE,lwd=1,pos=0)
	axis(side=2,at=seq(0,7,1),labels=TRUE,lwd=3,pos=0)

	legend(0,7.25,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### low95 IFR
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-times-as-many-infections-low95-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,18),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's lower 95% IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

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

			current_deaths <- deaths[current_JHU_country_row,5:ncol(deaths)]
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]
			current_infected <- output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_low95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]
			}

			points(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)
			lines(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)
	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,18,1),labels=FALSE,lwd=1,pos=0)
	axis(side=2,at=seq(0,18,2),labels=TRUE,lwd=3,pos=0)

	legend(0,18.5,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### up95 IFR
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-times-as-many-infections-up95-IFR-thanatAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,3),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's upper 95% IFR mapped via thanatological age\n January 22 - April 17, 2020",axes=FALSE)

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

			current_deaths <- deaths[current_JHU_country_row,5:ncol(deaths)]
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]
			current_infected <- output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_up95_ifr_china_map_thanatAge_globalPattern_ageSpecificDeaths$total_I[pop,]
			}

			points(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)
			lines(x=1:length(current_infected),y=current_infected/current_confirmed,col=pal[pop],lwd=2)
	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,3,0.5),labels=FALSE,lwd=1,pos=0)
	axis(side=2,at=seq(0,3,1),labels=TRUE,lwd=3,pos=0)

	legend(0,3.2,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()





