observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD), {
	req(DATA$HRclean)
	
	STATS		=	sepCOL(aggregate(list(Pulse = DATA$HRclean$PULSE), list(Part = DATA$HRclean$Part), stats))
	STATS		=	merge(STATS, DATA$HRtime, by="Part", sort = FALSE)
	STATS$Time	=	sapply(as.numeric(STATS$Time), timeSum)
	
	DATA$STATS	=	STATS
	output$timeTotal	=	renderText({
		paste0("Total time in ", DATA$game, ": ", timeSum(DATA$HRtime$Time))
	})
})

observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD, input$tabCOLS, input$tabROWS), {
	req(DATA$STATS)
	output$summaryTable	=	renderTable({
		out	<-	DATA$STATS[filtROW(), filtCOL()]
		levels(out$Part)	=	sapply(levels(out$Part), prettyNUM)
		out
	},	digits = 2, striped = TRUE)
	
	output$timePartsSel	=	renderText({	paste0("Time in selected parts: ", partTIME())	})
	output$timeTable	=	renderTable({
		cbind(
			c(paste0("Total time in ", DATA$game),	"Time in selected parts"),
			c(timeSum(DATA$HRtime$Time),			partTIME())
		)
	},	striped = FALSE,	colnames = FALSE)
})

output$downloadTable	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Table.csv")},
	content	=	function(file)	{write_csv(roundFRAME(DATA$STATS[filtROW(), c("Part", "Time", input$tabCOLS)]), file)}
)
