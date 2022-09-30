sumLines	=	function(PART, wid = 1)	{
	list(
		# geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Mean), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Median), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Lower), size = wid),
		geom_vline(data = STATS()[STATS()$Part == PART, ], aes(xintercept = Upper), size = wid)
	)
}

partHIST	<-	reactive({
	facetHIST(DATA$HRclean[DATA$HRclean$Part == partSELlev(), ]) +
	sumLines(partSELlev()) +
	ggtitle(prettyNUM(partSELlev()), subtitle = paste0("Total Time: ", timeSum(DATA$HRtime[partSEL(), ]$Time)))
})	%>%	bindCache(input$dataSel, input$plotsSel)

output$graphPART	<-	renderCachedPlot({	
	partHIST()
},	cacheKeyExpr = {list(input$dataSel, input$plotsSel)}	)

output$statsPART	=	renderTable({	STATS()[partSEL(), filtCOL()]	})

output$downloadGraphPart	=	downloadHandler(
	filename	=	function()	{paste(partSELlev(), "Hist.png", sep = " - ")},
	content	=	function(file)	{ggsave(file, plot = partHIST(),	device = "png",
		width = input$partWIDTH,	height = input$partHEIGHT)}
)


partCOURSE	<-	reactive(	graphCOURSE(DATA$HRclean, partSELlev())	)	%>%	bindCache(input$dataSel, input$plotsSel)
	
output$graphCOURSE	=	renderPlot({	partCOURSE()	})
if	(VIEW$THRESH)	output$aboveCOURSE	=	renderPlot({	partCOURSE()	})

output$brushCOURSEzoom	=	renderPlot({
	graphCOURSE(PART(), partSELlev()) +
	coord_cartesian(xlim = c(input$COURSEbrush$xmin, input$COURSEbrush$xmax),	expand = FALSE)
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
