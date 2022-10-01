graphHIST	=	function(IN = DATA$HRclean)	{
	ggplot(IN, aes(PULSE, fill=after_stat(ncount), group = Part)) +
	scale_fill_gradient("Count", low = "#6d59ff", high = "#ab4b41", labels = NULL) +
	stat_bin(binwidth = 1, col = "black") +
	scale_x_continuous(name = "Heart Rate (bpm)", minor_breaks = NULL) +
	scale_y_continuous(name = "Count", expand = c(0.02, 0)) +
	theme(legend.position = "none", plot.title.position = "plot")
}

graphCOURSE	=	function(IN, PART)	{
	ggplot(IN, aes(x = get("Time in Video"), y = PULSE, color=PULSE)) +
	ggtitle(prettyNUM(PART), subtitle = "Heart Rate over Time in Video") +
	scale_color_gradient("Pulse", low = "#6d59ff", high = "#ab4b41", labels = NULL) + 
	geom_step() + 
	scale_x_time(name = "Time in Video", expand = c(0.02, 0)) +
	scale_y_continuous(name = "Heart Rate (bpm)", expand = c(0.02, 0)) +
	theme(legend.position = "none", plot.title.position = "plot")
}


					source("app_graphs_single.r",	local = TRUE)
					source("app_graphs_facet.r",	local = TRUE)
if	(VIEW$THRESH)	source("app_threshold.r",			local = TRUE)