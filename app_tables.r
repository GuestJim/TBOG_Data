partTIME	<-	reactive({	timeSum(STATS()[filtROW(), "Time.in.Video"])	})

filtCOL	<-	reactive(	names(STATS())	%in%	c("Part", input$tabCOLS)	)
filtROW	<-	reactive(	STATS()$Part	%in%	c(input$tabROWS)	)

output$timeTable	=	renderTable({
	cbind(
		c(paste0("Total time in ", DATA$game),	"Time in selected parts"),
		c(timeSum(DATA$HRtime$Time),			partTIME()	)
	)
},	striped = FALSE,	colnames = FALSE)


output$summaryTable	=	renderTable({
	out	<-	STATS()[filtROW(), filtCOL()]
	levels(out$Part)	=	sapply(levels(out$Part), prettyNUM)
	out
},	digits = 2, striped = TRUE)

output$downloadTable	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Table.csv")},
	content	=	function(file)	{write_csv(roundFRAME(STATS()[filtROW(), c("Part", "Time", input$tabCOLS)]), file)}
)
