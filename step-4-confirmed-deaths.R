
setwd(the.plot.path)

require(wesanderson)
pal <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))

dev.off()

pdf(file="top10-confirmed-deaths-20200418.pdf", width=10, height=10, family="Times", pointsize=20, onefile=TRUE)

par(fig = c(0,1,0,1), las=1, mai=c(0.8,0.8,0.8,0.4))

 	plot(x=-100,y=-100,xlim=c(0,700000),ylim=c(0,40000),xlab="Confirmed cases",ylab="Deaths",
		main="Confirmed cases versus deaths from COVID-19 \n January 22 - April 17, 2020",axes=FALSE)

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


	axis(side=1,at=seq(0,700000,50000),labels=TRUE,lwd=3,pos=0)
	axis(side=2,at=seq(0,40000,2500),labels=FALSE,lwd=1,pos=0)
	axis(side=2,at=seq(0,40000,5000),labels=TRUE,lwd=3,pos=0)

	for(cfr in seq(0.01,0.15,0.02)){
		lines(x=seq(0,700000,1000),y=seq(0,700000,1000)*cfr,col=grey(0.8))
		text(x=c(250000,225000,195000,170000,150000,135000,118000,100000)[which(seq(0.01,0.15,0.02)==cfr)]*1.3*1.27*1.15,y=(c(250000,225000,195000,170000,150000,135000,121000,110000)*cfr)[which(seq(0.01,0.15,0.02)==cfr)]*1.3*1.27*1.15,paste(cfr*100,"%",sep=""),col=grey(0.5),font=2)
	}

	for(country in 1:501){
		points(x=confirmed[country,5:ncol(confirmed)],y=deaths[country,5:ncol(confirmed)],lwd=1,col=grey(0.7))
	}

	for(pop in 1:10){
		points(x=confirmed[country_row_number[pop],5:ncol(confirmed)],y=deaths[country_row_number[pop],5:ncol(confirmed)],lwd=2,col=pal[pop])
		lines(x=confirmed[country_row_number[pop],5:ncol(confirmed)],y=deaths[country_row_number[pop],5:ncol(confirmed)],lwd=2,col=pal[pop])
	}

	country_labels_legend <- country_labels
	country_labels_legend[which(country_labels_legend=="United Kingdom")] <- "UK"
	legend(0,40000,unlist(country_labels_legend),col=pal,bty="n",lwd=2,lty=1,cex=0.9)

dev.off()

