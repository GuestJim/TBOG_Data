sumLines	=	function(PART, wid = 1)	{
	list(
		# geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Mean), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Median), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Lower), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Upper), size = wid)
	)
}

partHIST	<-	reactive({
	graphHIST(PART()) +
	sumLines(partSELlev()) +
	ggtitle(prettyNUM(partSELlev()), subtitle = paste0("Total Time: ", timeSum(DATA$HRtime[partSEL(), ]$Time)))
})	%>%	bindEvent(list(input$dataSelLOAD, partSELlev()))

output$graphPART	<-	renderCachedPlot(	
	partHIST(),	cacheKeyExpr = list(GAME(), partSELlev())
)	%>%	bindEvent(list(input$dataSelLOAD, partSELlev()))

observe(	saveGRAPHServer('PART',		partHIST(),		partSELlev())	)	%>%	bindEvent(input$dataSelLOAD, input$plotsSel)
#	the observe is necessary so that it updates the PLOT and NAME arguments correctly

output$statsPART	=	renderTable({	STATS()[partSEL(), filtCOL()]	})


partCOURSE	<-	reactive(	graphCOURSE(PART(), partSELlev())	)	%>%	bindEvent(input$dataSelLOAD, partSELlev())
	
					output$graphCOURSE	=	renderCachedPlot(	partCOURSE(),	cacheKeyExpr = list(GAME(), partSELlev())	)
if	(VIEW$THRESH)	output$aboveCOURSE	=	renderCachedPlot(	partCOURSE(),	cacheKeyExpr = list(GAME(), partSELlev())	)

output$brushCOURSEzoom	=	renderPlot({
	partCOURSE() + coord_cartesian(xlim = c(input$COURSEbrush$xmin, input$COURSEbrush$xmax),	expand = FALSE)
})

output$brushCOURSEtable	=	renderTable({
	TAB	<-	data.frame(matrix(nrow = 4, ncol = 2, dimnames = list(
			c("Min", "Max", "Mean", "Median"),
			c("Time in Video", "Pulse")
		)),	check.names = FALSE)
	
	brushPART	<-	reactive(
		PART()$PULSE[
		as.numeric(PART()$"Time in Video")	>=	input$COURSEbrush$xmin &
		as.numeric(PART()$"Time in Video")	<=	input$COURSEbrush$xmax]
	)
	
	TAB$"Time in Video"	<-	c(num2time(input$COURSEbrush$xmin), num2time(input$COURSEbrush$xmax), NA, NA)
	
	TAB$Pulse	<-	c(
		min(brushPART(),	na.rm = TRUE),
		max(brushPART(),	na.rm = TRUE),
		mean(brushPART(),	na.rm = TRUE),
		median(brushPART(),	na.rm = TRUE)
	)
	TAB
},	digits = 2,	rownames = TRUE,	striped = TRUE)
