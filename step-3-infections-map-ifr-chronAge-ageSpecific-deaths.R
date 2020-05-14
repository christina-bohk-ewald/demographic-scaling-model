
####
##
## Estimate COVID-19 infections based on taking Chinese infection fatality rates 
## age-specific deaths derived with global deaths distribution by age
## ----take ifr of Verity et al. (2020)
## ----calculate global deaths distribution by age based on data of Dudel et al. (2020) 
##
####


##
### 1. Calculate lambda_x (age-specific population fraction infected with COVID-19)
##

## Get global age distribution of deaths from previous step 5

setwd(the.data.path)
source("global_age_dist_deaths.R")

##

lambda_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_lambda_chronAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										ifr_ref=ifr_by_age_china_verity[,2])

## lambda_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths

lambda_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_lambda_chronAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										ifr_ref=ifr_by_age_china_verity[,3])

## lambda_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths

lambda_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_lambda_chronAge_globalPattern_ageSpecificDeaths(deaths=deaths,
										global_pattern=global_age_dist_deaths,
										days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
										wom_select_10y=wom_select_10y,
										men_select_10y=men_select_10y,
										ifr_ref=ifr_by_age_china_verity[,4])

## lambda_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths

##
### 2. Calculate sum(lambda_x) (population fraction infected with COVID-19)
##

output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_output_chronAge_globalPattern_ageSpecificDeaths(lambda_chron_x_percent_infected=lambda_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths)
## output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda

output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_output_chronAge_globalPattern_ageSpecificDeaths(lambda_chron_x_percent_infected=lambda_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths)
## output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda

output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths <- get_output_chronAge_globalPattern_ageSpecificDeaths(lambda_chron_x_percent_infected=lambda_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths,
									days_observed=str_obs_ahead_2020_selected[1:length(5:ncol(deaths))],
									wom_select_10y=wom_select_10y,
									men_select_10y=men_select_10y) 

## names(output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths)
## output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda

##
### 3. Visualize total lambda (based on data of April 18, 2020)
##

##
### modal ifr
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-modal-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,0.014),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's modal IFR \n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(deaths)),y=output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(deaths)),y=output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.014,0.002),labels=TRUE,lwd=3,pos=0)

	text(1,0.0035,"Estimation based on \n1. China's modal IFR \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.014,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### low95% ifr
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-low95-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,0.032),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's lower 95% IFR \n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(deaths)),y=output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(deaths)),y=output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.032,0.004),labels=TRUE,lwd=3,pos=0)

	text(1,0.008,"Estimation based on \n1. China's lower 95% IFR \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.032,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### up95% ifr
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-lambda-up95-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,1.0,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,0.007),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19\n China's upper 95% IFR \n January 22 - April 17, 2020",axes=FALSE)

	for(pop in 1:length(country_labels)){
		points(x=1:length(5:ncol(deaths)),y=output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
		lines(x=1:length(5:ncol(deaths)),y=output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}

	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.007,0.001),labels=TRUE,lwd=3,pos=0)

	text(1,0.002,"Estimation based on \n1. China's upper 95% IFR \n2. Population size and age structure, 2019\n3. Empirical deaths by age",col=grey(0.6),font=2,cex=0.8,pos=4)

	legend(0,0.007,unlist(country_labels),col=pal,bty="n",lwd=2,lty=1)

dev.off()

##
### 4. Visualize total infections versus confirmed cases (based on data of April 18, 2020)
##

##
### all three (mode, high, low) together
##

setwd(the.plot.path)

dev.off()

pdf(file="top-10-confirmed-infected-allThree-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,2.4,1.2,0.4))

 	plot(x=-100,y=-100,xlim=c(0-1050,6500),ylim=c(0,10.5),xlab="",ylab="",cex.main=0.9,
		main="Confirmed cases vs probably infected, in thousand\nChina's IFR \n January 22 - April 17, 2020",axes=FALSE)

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
			current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.8))
			text(5350,9.25-1*(pop-1)+0.05,paste(round(current_infected[length(current_infected)],1),"k)",sep=""),pos=4,col=grey(0.8),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.975",pos=3,col=grey(0.8),font=2,cex=0.8)
			}

			## modal IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.6))
			text(4800,9.25-1*(pop-1)+0.25+0.2,paste(round(current_infected[length(current_infected)],1),"k",sep=""),pos=4,col=grey(0.6),font=2)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.5",pos=3,col=grey(0.6),font=2,cex=0.8)
			}
		
			## 95% upper IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=0,xright=current_infected,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=grey(0.4))
			text(4250,9.25-1*(pop-1)+0.05,paste("(",round(current_infected[length(current_infected)],1),"k, ",sep=""),pos=4,col=grey(0.4),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.025",pos=3,col=grey(0.4),font=2,cex=0.8)
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
### 5. How many more infections than confirmed cases (based on data of April 18, 2020)
##

##
### modal IFR
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top-10-times-as-many-infections-modal-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,7),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's modal IFR \n January 22 - April 17, 2020",axes=FALSE)

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
			current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]
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

pdf(file="top-10-times-as-many-infections-low95-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,18),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's lower 95% IFR \n January 22 - April 17, 2020",axes=FALSE)

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
			current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]
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

pdf(file="top-10-times-as-many-infections-up95-IFR-chronAge-ageSpecificDeaths-20200418.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,3),xlab="",ylab="",cex.main=0.9,
		main="Times as many COVID-19 infections than confirmed cases\n China's upper 95% IFR \n January 22 - April 17, 2020",axes=FALSE)

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
			current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]

			if(current_JHU_country=="Hubei"){
				current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]
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

##
### 8. Visualize confirmed cases vs estimated infections (based on data downloaded on May 14, 2020)
### all three (mode, high, low) together
### wide format
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="Figure-S3.pdf", width=18, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.6,0.0,1.2,0.2))

 	plot(x=-100,y=-100,xlim=c(0-1400,8500),ylim=c(0,10.5),xlab="",ylab="",cex.main=0.9,
		main="Confirmed cases vs estimated infections, in thousand, as of May 13, 2020",axes=FALSE)

	text(c(7250,8150),c(10.35,10.35),c("Confirmed","Estimated"),pos=3,cex=0.9,col="black",font=2)
	text(c(7250,8150),c(10.0,10.0),c("cases","infections"),pos=3,cex=0.9,col="black",font=2)
	text(-500,10.1,"Quantiles:",pos=4,cex=0.9,col="black",font=2)

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
			current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			current_infected_low <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=current_infected_low[length(current_infected_low)],xright=current_infected[length(current_infected)],ybottom=9.25-1*(pop-1),ytop=9.25-1*(pop-1)+0.5,col=grey(0.8))
			text(8050,9.25-1*(pop-1)+0.05,paste(round(current_infected[length(current_infected)],0),"k)",sep=""),pos=4,col=grey(0.8),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.975",pos=3,col=grey(0.8),font=2,cex=0.9)
			}

			## modal IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			current_infected_low <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=current_infected_low[length(current_infected_low)],xright=current_infected[length(current_infected)],ybottom=9.25-1*(pop-1),ytop=9.25-1*(pop-1)+0.5,col=grey(0.6))
			text(7750,9.25-1*(pop-1)+0.25+0.2,paste(round(current_infected[length(current_infected)],0),"k",sep=""),pos=4,col=grey(0.6),font=2)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.5",pos=3,col=grey(0.6),font=2,cex=0.9)
			}
		
			## 95% upper IFR:
			current_confirmed <- confirmed[current_JHU_country_row,5:ncol(confirmed)]/1000
			current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000

			if(current_JHU_country=="Hubei"){
				current_infected <- output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_I[pop,]/1000
			}

			rect(xleft=current_infected[length(current_infected)],xright=current_infected[length(current_infected)],ybottom=9.25-1*(pop-1),ytop=9.25-1*(pop-1)+0.5,col=grey(0.4))
			text(7500,9.25-1*(pop-1)+0.05,paste("(",round(current_infected[length(current_infected)],0),"k, ",sep=""),pos=4,col=grey(0.4),font=2,cex=0.8)

			if(pop==1){
				text(current_infected[length(current_infected)],9.25-1*(pop-1)+0.45,"0.025",pos=3,col=grey(0.4),font=2,cex=0.9)
			}


			lines(x=c(0,6900),y=rep((9.25-1*(pop-1)+0.25),2),col=pal[pop],lty=2,lwd=1)
			rect(xleft=0,xright=confirmed[current_JHU_country_row,ncol(confirmed)]/1000,ybottom=9.15-1*(pop-1),ytop=9.15-1*(pop-1)+0.7,col=pal[pop],border=NA)
			text(6900,9.25-1*(pop-1)+0.25,paste(round(current_confirmed[length(current_infected)],0),"k",sep=""),pos=4,col=pal[pop],font=2)

	}

	axis(side=1,at=seq(0,7000,500),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=seq(0,7000,1000),labels=TRUE,lwd=3,pos=0)
	axis(side=2,at=seq(0.5,9.5,1),labels=paste(rev(seq(1,10,1)),". ",rev(country_labels),sep=""),lwd=3,pos=0)

dev.off()




##
### 9. Visualize population fraction infected (based on data downloaded on May 14, 2020)
### all three (mode, high, low) together
### wide format
##


setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="Figure-S4.pdf", width=20, height=10, family="Times", pointsize=24, onefile=TRUE)

	par(fig = c(0,1,0,1), las=1, mai=c(0.6,3.2,1.0,0.4))
	plot(x=-100,y=-100,xlim=c(0,length(5:ncol(deaths))),ylim=c(0,0.08),xlab="",ylab="",cex.main=0.9,
		main="Fraction of people probably infected with COVID-19, January 22 - May 13, 2020",axes=FALSE)

	par(fig = c(0,0.35,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))
	segments(x0=rep(0,3),x1=rep(length(5:ncol(deaths)),3),y0=seq(0.01,0.08,0.01),y1=seq(0.01,0.08,0.01),lty=2,col=grey(0.8))
 	for(pop in c(8,5,3,1)){
		polygon(x=c(c(1:length(5:ncol(deaths))),rev(1:length(5:ncol(deaths)))),y=c(output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],rev(output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,])),border=NA,col=adjustcolor(pal[pop],alpha.f=0.2))
		lines(x=1:length(5:ncol(deaths)),y=output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}
	legend(0,0.08,unlist(country_labels)[c(1,3,5,8)],col=pal[c(1,3,5,8)],bty="n",lwd=2,lty=1)
	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)
	axis(side=2,at=seq(0,0.08,0.01),labels=TRUE,lwd=3,pos=0)

	par(fig = c(0.32,0.67,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))
	segments(x0=rep(0,3),x1=rep(length(5:ncol(deaths)),3),y0=seq(0.01,0.08,0.01),y1=seq(0.01,0.08,0.01),lty=2,col=grey(0.8))
 	for(pop in c(2,4,9)){
		polygon(x=c(c(1:length(5:ncol(deaths))),rev(1:length(5:ncol(deaths)))),y=c(output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],rev(output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,])),border=NA,col=adjustcolor(pal[pop],alpha.f=0.2))
		lines(x=1:length(5:ncol(deaths)),y=output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}
	legend(0,0.08,unlist(country_labels)[c(2,4,9)],col=pal[c(2,4,9)],bty="n",lwd=2,lty=1)
	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)

	par(fig = c(0.64,0.99,0,1), las=1, mai=c(0.6,0.0,1.4,0.4))
	segments(x0=rep(0,3),x1=rep(length(5:ncol(deaths)),3),y0=seq(0.01,0.08,0.01),y1=seq(0.01,0.08,0.01),lty=2,col=grey(0.8))
 	for(pop in c(6,7,10)){
		polygon(x=c(c(1:length(5:ncol(deaths))),rev(1:length(5:ncol(deaths)))),y=c(output_up95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],rev(output_low95_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,])),border=NA,col=adjustcolor(pal[pop],alpha.f=0.2))
		lines(x=1:length(5:ncol(deaths)),y=output_mode_ifr_china_map_chronAge_globalPattern_ageSpecificDeaths$total_lambda[pop,],col=pal[pop],lwd=3)
	}
	legend(0,0.08,unlist(country_labels)[c(6,7,10)],col=pal[c(6,7,10)],bty="n",lwd=2,lty=1)
	axis(side=1,at=seq(1,length(5:ncol(deaths)),7),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths))),labels=str_obs_ahead_2020_selected[c(seq(1,length(5:ncol(deaths)),7),length(5:ncol(deaths)))],lwd=3,pos=0)

dev.off()









