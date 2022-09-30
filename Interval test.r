setwd("M:/TBOG/TBOG_Data")

library(readr)
if (!require(R.utils))	install.package("R.utils")
library(R.utils)

TBOG	<-	read_csv("TBOG.csv")

test	<-	TBOG[TBOG$PULSE >= 83, ]

temp	<-	seqToIntervals(test$"Time in Video")

num2time	<-	function(IN)	format(structure(IN, class = c("POSIXct", "POSIXt"), tzone = "UTC"), "%T")

apply(temp, 1, function(IN)	{
	paste(num2time(IN), collapse = " - ")
})

