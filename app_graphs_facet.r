partFACET	<-	reactive(
	graphHIST(DATA$HRclean[DATA$HRclean$Part %in% input$tabROWS, ]) +
	facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	sapply(gsub(paste0(DATA$game, " - "), "", IN), prettyNUM))) +
	ggtitle(DATA$game, subtitle = paste0("Total Time: ", partTIME() ))
)	%>%	bindCache(input$dataSel, input$tabROWS)

output$graphFACET	=	renderCachedPlot({
	partFACET()
	},	cacheKeyExpr = list(input$dataSel, input$tabROWS)
)

output$downloadGraph	=	downloadHandler(
	filename	=	function()	{paste(DATA$game, "Hist.png", sep = " - ")},
	content	=	function(file)	{ggsave(file,	plot = partFACET(),	device = "png",
		width = input$facetWIDTH,	height = input$facetHEIGHT)}
)
