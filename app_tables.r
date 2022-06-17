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

	formatTABLE	=	function(IN, ROW, COL)	{
		out	<-	IN[ROW, COL]
		levels(out$Part)	=	sapply(levels(out$Part), prettyNUM)
		return(out)
	}

	observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD, input$tabCOLS, input$tabROWS), {
		req(DATA$STATS)
		output$summaryTable	=	renderTable({
			formatTABLE(DATA$STATS, filtROW(), filtCOL())
		},	digits = 2, striped = TRUE)
		
		output$timePartsSel	=	renderText({	paste0("Time in selected parts: ", partTIME())	})
	})
	
	output$downloadTable	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - Table.csv")},
		content	=	function(file)	{write_csv(roundFRAME(DATA$STATS[filtROW(), c("Part", "Time", input$tabCOLS)]), file)}
	)
