if (!require(R.utils))	install.package("R.utils")
library(R.utils)
num2time	<-	function(IN)	format(structure(IN, class = c("POSIXct", "POSIXt"), tzone = "UTC"), "%T")

PART	<-	eventReactive(list(input$dataSelLOAD, input$plotsSel),	
	DATA$HRclean[DATA$HRclean$Part == DATA$levs[as.numeric(input$plotsSel)], ]
	)

output$aboveTABL	<-	renderTable({
	hold	<-	PART()$"Time in Video"[PART()$PULSE >= input$aboveTHRS]
	out	<-	seqToIntervals(hold)
	
	data.frame("Intervals above Threshold" = apply(out, 1, function(IN)	paste(num2time(IN), collapse = " - ")), check.names = FALSE)
},	striped = TRUE)
