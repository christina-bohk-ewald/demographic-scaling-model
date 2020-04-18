
########################################################################################
########################################################################################
########################################################################################
########################################################################################
#### R packages:

## install.packages("openxlsx")
require(openxlsx)

########################################################################################
########################################################################################
########################################################################################
########################################################################################

## Download and save UNWPP 2019 data in folder on your PC

####
##
## 1. Load and prepare input data
##
####

##
### 1.1 UNWPP2019: Population counts in 2019
##

wom <- read.xlsx(file.path(the.data.path,paste("WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",sep="")),sheet = 1,startRow = 17)
wom_select <- wom[which(wom[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] 
wom_select_2010 <- wom[which(wom[,"Reference.date.(as.of.1.July)"]=="2010"),c(3,8:109)] 

men <- read.xlsx(file.path(the.data.path,paste("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",sep="")),sheet = 1,startRow = 17)
men_select <- men[which(men[,"Reference.date.(as.of.1.July)"]=="2019"),c(3,8:109)] ## men[,c(3,8:109)]
men_select_2010 <- men[which(men[,"Reference.date.(as.of.1.July)"]=="2010"),c(3,8:109)] ## men[,c(3,8:109)]

##
### 1.2 JHU CCSE: Confirmed cases, deaths, and recovered
##
	confirmed <- read.csv("time_series_covid19_confirmed_global.csv",header=TRUE, stringsAsFactors = FALSE)
	deaths <- read.csv("time_series_covid19_deaths_global.csv",header=TRUE, stringsAsFactors = FALSE)

##
### 1.3 Verity 2020: Adjusted case fatality rates by age in China (w 95% credible interval) 
## 

ifr_by_age_china_verity <- read.table("infection-fatality-rates-by-age-china-Verity.txt",header=FALSE, stringsAsFactors = FALSE)

##
### 1.4 Population counts by 10-year age groups
##

wom_select_10y <- matrix(NA,nr=dim(wom_select)[1],ncol=length(seq(0,80,10)))
men_select_10y <- matrix(NA,nr=dim(men_select)[1],ncol=length(seq(0,80,10)))

rownames(wom_select_10y) <- wom_select[,"Region,.subregion,.country.or.area.*"]
rownames(men_select_10y) <- men_select[,"Region,.subregion,.country.or.area.*"]
colnames(wom_select_10y) <- seq(0,80,10) 
colnames(men_select_10y) <- seq(0,80,10) 

for(country in 1:dim(wom_select)[1]){

	current_wom_select <- wom_select[country,]
	current_men_select <- men_select[country,]

	for(age in 1:nrow(cfr_by_age_china)){
		current_age <- seq(0,80,10)[age] 

		wom_select_10y[country,age] <- sum( as.numeric( current_wom_select[as.character((current_age):(current_age+9))] ) ) 
		men_select_10y[country,age] <- sum( as.numeric( current_men_select[as.character((current_age):(current_age+9))] ) )
		
		if(current_age==80){
			wom_select_10y[country,age] <- sum( as.numeric( current_wom_select[as.character((current_age):(current_age+20))] ) ) 
			men_select_10y[country,age] <- sum( as.numeric( current_men_select[as.character((current_age):(current_age+20))] ) )
		}
	}
}

##
### 1.5 Abridged life tables
##

### Load abridged life tables, 1950-55 -- 2015-20 

lt_1950_2020 <- read.xlsx("WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",sheet = 1,startRow = 17)
lt_wom_1950_2020 <- read.xlsx("WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx",sheet = 1,startRow = 17)
lt_men_1950_2020 <- read.xlsx("WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx",sheet = 1,startRow = 17)

##
### 1.9 Day format
##

str_vect <- colnames(deaths)[5:ncol(deaths)]
str_vect_without_X <- str_vect

for(day in 1 : length(str_vect)){

	if(nchar(str_vect[day])==8+0){
		str_vect_without_X[day] <- gsub(".(.......)$", "\\1", str_vect[day])
	}

	if(nchar(str_vect[day])==7+0){
		str_vect_without_X[day] <- gsub(".(......)$", "\\1", str_vect[day])
	}
}

str_vect_without_X[seq(1,length(str_vect_without_X),5)]

days_per_month <- matrix(1:12,nr=12,nc=1)
days_per_month[,1] <- c(31,29,31, 30,31,30, 31,31,30, 31,30,31)

str_obs_ahead_2020 <- c(0)

	i <- 0
	for(month in 1:12){	
		current_month <- month
		for(day in 1:days_per_month[current_month,]){
			i <- i+1
			current_day <- day
			str_obs_ahead_2020[i] <- c(paste(current_month,".",current_day,".20",sep=""))
		} ## for month
	} ## for day


str_obs_ahead_2020_selected <- str_obs_ahead_2020[which(str_obs_ahead_2020==str_vect_without_X[1]):length(str_obs_ahead_2020)]

##
### 2: SELECT COUNTRIES OF INTEREST WRT DEATHS:
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

