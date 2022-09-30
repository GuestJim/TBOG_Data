observeEvent(input$tabROWS,	{	req(DATA$HRclean)
	GRAPH$FACET	=	facetHIST(DATA$HRclean[DATA$HRclean$Part %in% input$tabROWS, ]) +
						facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	sapply(gsub(paste0(DATA$game, " - "), "", IN), prettyNUM))) +
						ggtitle(DATA$game, subtitle = paste0("Total Time: ", partTIME() ))

	output$graphFACET	=	renderPlot({	GRAPH$FACET	})
},	priority	=	-1)

output$downloadGraph	=	downloadHandler(
	filename	=	function()	{paste(DATA$game, "Hist.png", sep = " - ")},
	content	=	function(file)	{ggsave(file, plot = GRAPH$FACET, device = "png",
		width = input$facetWIDTH,	height = input$facetHEIGHT)}
)

graphCOURSE	=	function(DATA, PART)	{
	ggplot(DATA[DATA$Part == PART, ], aes(x = get("Time in Video"), y = PULSE, color=PULSE)) +
	ggtitle(prettyNUM(PART), subtitle = "Heart Rate over Time in Video") +
	scale_color_gradient("Pulse", low = "#6d59ff", high = "#ab4b41", labels = NULL) + 
	geom_step() + 
	scale_x_time(name = "Time in Video", expand = c(0.02, 0)) +
	scale_y_continuous(name = "Heart Rate (bpm)", expand = c(0.02, 0)) +
	theme(legend.position = "none", plot.title.position = "plot")
}