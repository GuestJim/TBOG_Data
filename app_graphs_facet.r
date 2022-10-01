partFACET	<-	reactive({	req(DATA$levs)
	graphHIST(DATA$HRclean[DATA$HRclean$Part %in% input$tabROWS, ]) +
	facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	sapply(gsub(paste0(DATA$game, " - "), "", IN), prettyNUM))) +
	ggtitle(DATA$game, subtitle = paste0("Total Time: ", partTIME() ))
})	%>%	bindEvent(input$dataSelLOAD, input$tabROWS)

output$graphFACET	=	renderCachedPlot(	
	partFACET(), cacheKeyExpr = list(GAME(), input$tabROWS)
	)	%>%	bindEvent(input$dataSelLOAD, input$tabROWS)

observe(	saveGRAPHServer('FACET',	partFACET(),	DATA$game)		)	%>%	bindEvent(input$dataSelLOAD, input$tabROWS)
#	the observe is necessary so that it updates the PLOT and NAME arguments correctly