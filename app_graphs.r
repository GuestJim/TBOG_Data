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
		GRAPH$PLOTs	=	lapply(1:length(DATA$levs), function(PART)	{
			facetHIST(DATA$HRclean[DATA$HRclean$Part == DATA$levs[PART], ]) +
			sumLines(DATA$levs[PART]) +
			ggtitle(DATA$levs[PART], subtitle = paste0("Total Time: ", timeSum(DATA$HRtime[PART, ]$Time)))
		})
		output$statsPART	=	renderTable({	DATA$STATS[input$plotsSel, filtCOL()]	})
		output$graphPART	=	renderPlot({	GRAPH$PLOTs[as.numeric(input$plotsSel)]	})
		#	as.numeric is necessary because the input$plotsSel is not a number
	},	priority	=	-1)
	

	output$downloadGraphPart	=	downloadHandler(
		filename	=	function()	{paste(DATA$levs[as.numeric(input$plotsSel)], "Hist.png", sep=" - ")},
		content	=	function(file)	{ggsave(file, plot = GRAPH$PLOTs[[as.numeric(input$plotsSel)]], device = "png", width = input$gWIDTH, height = input$gHEIGH)}
	)

	observeEvent(input$tabROWS,	{
		req(DATA$HRclean)
		GRAPH$FACET	=	facetHIST(DATA$HRclean[DATA$HRclean$Part %in% input$tabROWS, ]) +
							facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	gsub(paste0(DATA$game, " - "), "", IN))) +
							ggtitle(DATA$game, subtitle = paste0("Total Time: ", partTIME() ))

		output$graphFACET	=	renderPlot({	GRAPH$FACET	})
	},	priority	=	-1)
	
	output$downloadGraph	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - Hist.png")},
		content	=	function(file)	{ggsave(file, plot = GRAPH$FACET, device = "png", width = input$gWIDTH, height = input$gHEIGH)}
	)