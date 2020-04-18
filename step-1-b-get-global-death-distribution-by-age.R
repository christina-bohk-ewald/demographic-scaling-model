
##
### 1. run "00_functions.R" and "01_input_data.R" 
### as provided in Supplementary Materials of Dudel et al. (2020) at https://osf.io/vdgwt/ 
##


dat$Age    <- unlist(dat$Age)
dat$AgeInt <- unlist(dat$AgeInt)


##
### 2. Select deaths for all countries, dates, and sex ("b"=both)
##

## unique(dat$Sex)
## unique(dat$Country)
## unique(dat$Code)

collect_deaths <- matrix(NA,nr=10,nc=length(unique(dat$Code)))
colnames(collect_deaths) <- unique(dat$Code)
rownames(collect_deaths) <- seq(0,90,10)

for(code in 1:length(unique(dat$Code))){
	current_pop_extract <- dat %>% 
  					filter(Code == paste(unique(dat$Code)[code]),Sex == "b")
 	collect_deaths[1:10,code] <- unlist(current_pop_extract[1:10,"Deaths"]) 
}

collect_deaths_0_80 <- rbind( collect_deaths[1:8,], (colSums(collect_deaths[9:10,], na.rm=TRUE)) ) 
rownames(collect_deaths_0_80) <- seq(0,80,10)

##
### 3. Remove NA entries
##

remove_entry <- NA 
for(code in 1:length(unique(dat$Code))){
	current_entry <- collect_deaths_0_80[,code]
	if(length(which(is.na(current_entry)))>=1){
		remove_entry <- c(remove_entry,code)
	}
}
remove_entry <- remove_entry[-1] 

collect_deaths_0_80 <- collect_deaths_0_80[,-(remove_entry)]

##
### 4. Save collected deaths by age
##

setwd(the.data.path)
dump(list="collect_deaths_0_80",file="collect_deaths_0_80.R")

##
### 5. Load population counts for Spain, Italy, South Korea, United States of America, Germany  
##

coi <- c("Spain", "Italy", "Republic of Korea", "United States of America", "Germany", "China")  

collect_pop <- matrix(NA,nr=9,nc=length(coi))
colnames(collect_pop) <- coi

for(pop in 1:length(coi)){
	collect_pop[,pop] <- ((wom_select_10y[coi[pop],]+men_select_10y[coi[pop],]) * 1000)	
}
colSums(collect_pop)

##
### 6. Save collected pop by age
##

setwd(the.data.path)
dump(list="collect_pop",file="collect_pop.R")

##
### 7. Population age structure of deaths, standardized by age, and normalized to sum to 1
##		

standardized_death_profile <- collect_deaths_0_80

for(pop in 1:ncol(collect_deaths_0_80)){
	current_deaths <- collect_deaths_0_80[,pop]
	current_name <- colnames(collect_deaths_0_80)[pop]

	if(substr(current_name, start = 1, stop = 2)=="ES"){
		current_pop <- collect_pop[,"Spain"]
	}
	if(substr(current_name, start = 1, stop = 2)=="IT"){
		current_pop <- collect_pop[,"Italy"]
	}
	if(substr(current_name, start = 1, stop = 2)=="SK"){
		current_pop <- collect_pop[,"Republic of Korea"]
	}
	if(substr(current_name, start = 1, stop = 2)=="DE"){
		current_pop <- collect_pop[,"Germany"]
	}
	if(substr(current_name, start = 1, stop = 2)=="WA"){
		current_pop <- collect_pop[,"United States of America"]
	}
	if(substr(current_name, start = 1, stop = 2)=="US"){
		current_pop <- collect_pop[,"United States of America"]
	}
	if(substr(current_name, start = 1, stop = 2)=="CN"){
		current_pop <- collect_pop[,"China"]
	}
		
	standardized_death_profile[,pop] <- current_deaths / current_pop / sum(current_deaths / current_pop) 
}

colSums(standardized_death_profile)

## remove unreasonable data before calculating global age distribution of deaths

sel <- standardized_death_profile[,-which(standardized_death_profile["80",]==0)]
global_age_dist_deaths <- rowMeans(sel)

## save global age distribution of deaths

setwd(the.data.path)
dump(list="global_age_dist_deaths",file="global_age_dist_deaths.R")

##
### 8. Plot age-standardized and normalized age profiles of collected deaths 
##

setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="global-age-distribution-deaths-20200417.pdf", width=10, height=10, family="Times", pointsize=24, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.8,0.8,1.4,0.4))

 	plot(x=-100,y=-100,xlim=c(0,80),ylim=c(0,0.9),xlab="Age group",ylab="",cex.main=0.9,
		main="Global age distribution of deaths\n Age-standardized and normalized",axes=FALSE)
	
	for(pop in 1:ncol(sel)){
		lines(x=seq(0,80,10),y=sel[,pop],col=gray(0.7))
		points(x=seq(0,80,10),y=sel[,pop],col=gray(0.5),lwd=3)
	}
	lines(x=seq(0,80,10),y=rowMeans(sel),col="black",lwd=4)
	points(x=seq(0,80,10),y=rowMeans(sel),col="black",lwd=6)

	axis(side=1,at=seq(0,80,5),labels=FALSE,lwd=1,pos=0)
	axis(side=1,at=seq(0,80,10),labels=TRUE,lwd=3,pos=0)
	axis(side=2,at=seq(0,0.9,0.1),labels=TRUE,lwd=3,pos=0)

	legend(0,0.9,c("Global","Countries"),col=c("black",grey(0.7)),bty="n",lwd=2,lty=1)

dev.off()


