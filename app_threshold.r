if (!require(R.utils))	install.package("R.utils")
library(R.utils)

output$aboveTABL	<-	renderTable({
	hold	<-	PART()$"Time in Video"[PART()$PULSE >= input$aboveTHRS]
	out	<-	seqToIntervals(hold)
	
	data.frame("Intervals above Threshold" = apply(out, 1, function(IN)	paste(num2time(IN), collapse = " - ")), check.names = FALSE)
},	striped = TRUE)
