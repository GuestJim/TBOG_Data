	sumLines	=	function(PART, wid = 1)	{
		list(
			# geom_vline(data = DATA$STATS[DATA$STATS$Part == PART, ], aes(xintercept = Mean), size = wid),
			geom_vline(data = DATA$STATS[DATA$STATS$Part == PART, ], aes(xintercept = Median), size = wid),
			geom_vline(data = DATA$STATS[DATA$STATS$Part == PART, ], aes(xintercept = Lower), size = wid),
			geom_vline(data = DATA$STATS[DATA$STATS$Part == PART, ], aes(xintercept = Upper), size = wid)
		)
	}

	facetHIST	=	function(DATA = DATA$HRclean)	{
		ggplot(DATA, aes(PULSE, fill=after_stat(ncount), group = Part)) +
		# ggtitle(game, subtitle = paste0("Total Time: ", HRtimeSum())) +
		scale_fill_gradient("Count", low = "#6d59ff", high = "#ab4b41", labels = NULL) +
		stat_bin(binwidth = 1, col = "black") +
		scale_x_continuous(name = "Heart Rate (bpm)", minor_breaks = NULL) +
		scale_y_continuous(name = "Count", expand = c(0.02, 0)) +
		theme(legend.position = "none", plot.title.position = "plot")
	}

	observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD),	{
		req(DATA$HRclean)
		GRAPH$PLOTs	=	lapply(1:length(DATA$levs), function(PART)	{
			facetHIST(DATA$HRclean[DATA$HRclean$Part == DATA$levs[PART], ]) +
			sumLines(DATA$levs[PART]) +
			ggtitle(prettyNUM(DATA$levs[PART]), subtitle = paste0("Total Time: ", timeSum(DATA$HRtime[PART, ]$Time)))
		})
		output$statsPART	=	renderTable({	DATA$STATS[input$plotsSel, filtCOL()]	})
		output$graphPART	=	renderPlot({	GRAPH$PLOTs[as.numeric(input$plotsSel)]	})
		#	as.numeric is necessary because the input$plotsSel is not a number
	},	priority	=	-1)

	brushZOOM	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf))
	observeEvent(list(input$COURSEbrush),	{
		brush <- input$COURSEbrush
			if (!is.null(brush)) {
				brushZOOM$x <- c(brush$xmin, brush$xmax)
			} else {
				brushZOOM$x <- NULL
			}
			
		output$brushCOURSEtable	=	renderTable({
			req(input$COURSEbrush)
			brushTABLE(DATA$HRclean, DATA$levs[as.numeric(input$plotsSel)], brushZOOM)
		},	digits	=	2,	rownames	=	TRUE,	striped	=	TRUE)
	})
	
	output$brushCOURSEtext	=	renderText("")	
	observeEvent(list(input$dataInput, input$dataSelLOAD),	{
		output$brushCOURSEtext	=	renderText("Click and Drag to Zoom Below")	
	},	ignoreInit	=	TRUE,	once	=	TRUE)
	
	brushTABLE	=	function(IN = DATA$HRclean, PART = DATA$levs[as.numeric(input$plotsSel)], BRUSH = brushZOOM)	{
		if (is.null(BRUSH$x))	return(NULL)
		IN$Secs	=	as.numeric(IN$"Time in Video")
		hold		=	IN[
			IN$Part == PART			&
			IN$Secs >= BRUSH$x[1]	&
			IN$Secs <= BRUSH$x[2]
			, ]
		
		out	=	setNames(rbind(	"Min"	=	hold[which.min(hold$PULSE), c("Time in Video", "PULSE")],
								"Max"	=	hold[which.max(hold$PULSE), c("Time in Video", "PULSE")]	),
						c("Time in Video", "Pulse")	)
		out$"Time in Video"	=	sapply(as.numeric(out$"Time in Video"), timeSum)
		out	=	as.data.frame(rbind(out,	Mean	=	c("",	round(mean(hold$PULSE),		2)	),
							Median	=	c("",	round(median(hold$PULSE),	2)	)	))
		rownames(out)	=	c("Min", "Max", "Mean", "Median")
		return(out)
	}
	
	
	graphCOURSE	=	function(DATA, PART)	{
		ggplot(DATA[DATA$Part == PART, ], aes(x = get("Time in Video"), y = PULSE, color=PULSE)) +
		ggtitle(prettyNUM(PART), subtitle = "Heart Rate over Time in Video") +
		scale_color_gradient("Pulse", low = "#6d59ff", high = "#ab4b41", labels = NULL) + 
		geom_step() + 
		scale_x_time(name = "Time in Video", expand = c(0.02, 0)) +
		scale_y_continuous(name = "Heart Rate (bpm)", expand = c(0.02, 0)) +
		theme(legend.position = "none", plot.title.position = "plot")
	}
	
	observeEvent(list(input$dataInput, input$dataSelLOAD),	{
		req(DATA$HRclean)
		output$graphCOURSE	=	renderPlot({
			graphCOURSE(DATA$HRclean, DATA$levs[as.numeric(input$plotsSel)])
		})
		output$brushCOURSEzoom	=	renderPlot({
			graphCOURSE(DATA$HRclean, DATA$levs[as.numeric(input$plotsSel)]) +
			coord_cartesian(xlim = brushZOOM$x,	expand = FALSE)
		})
	})

	output$downloadGraphPart	=	downloadHandler(
		filename	=	function()	{paste(DATA$levs[as.numeric(input$plotsSel)], "Hist.png", sep=" - ")},
		content	=	function(file)	{ggsave(file, plot = GRAPH$PLOTs[[as.numeric(input$plotsSel)]], device = "png", width = input$gWIDTH, height = input$gHEIGH)}
	)

	observeEvent(input$tabROWS,	{
		req(DATA$HRclean)
		GRAPH$FACET	=	facetHIST(DATA$HRclean[DATA$HRclean$Part %in% input$tabROWS, ]) +
							facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	sapply(gsub(paste0(DATA$game, " - "), "", IN), prettyNUM))) +
							ggtitle(DATA$game, subtitle = paste0("Total Time: ", partTIME() ))

		output$graphFACET	=	renderPlot({	GRAPH$FACET	})
	},	priority	=	-1)

	output$downloadGraph	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - Hist.png")},
		content	=	function(file)	{ggsave(file, plot = GRAPH$FACET, device = "png", width = input$gWIDTH, height = input$gHEIGH)}
	)