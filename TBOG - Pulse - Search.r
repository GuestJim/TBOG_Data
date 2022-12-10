library(readr)
library(ggplot2)

# setwd("")
#	only necessary if running by GUI, so leaving it to be done manually

ggdevice	=	"png"	;	theme_set(theme_bw(base_size = 16))
gWIDTH	=	16	;	gHEIGH	=	9	;	DPI			=	120

simpsplit	=	function(...)	unlist(strsplit(...))
game	=	rev(simpsplit(getwd(), "/"))[1]

timepad	=	function(timesec) sprintf("%02d", c(timesec %/% 3600, timesec %%3600 %/% 60, round(timesec %% 60))) |> paste(collapse = ":")

prettyNUM	=	function(IN){
	out	=	simpsplit(IN, " ")
	if (length(out) == 1)	return(out)
	NUM	=	try(as.numeric(out[length(out)]), silent = TRUE)
	if (is.na(NUM))	NUM	=	out[length(out)]
	paste0(c(
		out[1:(length(out)-1)],
		NUM),
		collapse = " "
	)
}

sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	LABS	=	aggOUT[, !matCOL]
	if (length(which(matCOL)) == 1)	LABS	=	aggOUT[!matCOL]
	#	necessary for when there is only one column of GROUP labels
	out	=	cbind(LABS, as.data.frame(aggOUT[, matCOL]))
	return(out)
}

stats	=	function(x)	{c(
	Mean	=	mean(x),
	Lower	=	quantile(x, 0.25, names	=	FALSE),
	Median	=	median(x),
	Upper	=	quantile(x, 0.75, names	=	FALSE),
	Min		=	min(x),
	Max		=	max(x)
	)
}

sumLines	=	function(IN,	wid = 2)list(
	# geom_vline(data = IN, aes(xintercept = Mean), linewidth = wid),
	geom_vline(data = IN, aes(xintercept = Median), linewidth = wid),
	geom_vline(data = IN, aes(xintercept = Lower), linewidth = wid),
	geom_vline(data = IN, aes(xintercept = Upper), linewidth = wid)
)

baseHIST	<-	function(IN)	{
	ggplot(IN, aes(PULSE, fill=after_stat(count))) + 
	scale_fill_gradient("Count", low = "#6d59ff", high = "#ab4b41") + 
	stat_bin(binwidth = 1, col = "black") + 
	scale_y_continuous(name = "Count", expand = c(0.02, 0)) + 
	theme(legend.position = c(1, 1), legend.justification = c(1, 1))
}

customSave	=	function(name="", plot = last_plot(), fold = "", device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if (fold != "")	name	=	paste0(fold, "/", name)
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(name, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(name, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

csvFIND	=	function(DIRECT = getwd(), PAT = "Edited.csv")	{
	LIST		=	list.files(DIRECT, recursive = TRUE, pattern = PAT)
	LIST		=	LIST[!startsWith(LIST, "@")]
	# LIST.full	=	paste0(DIRECT, "/", LIST)

	return(LIST)
}

CSVs	=	csvFIND()

HRdata	=	data.frame(matrix(nrow = 0, ncol = 3))
HRdataALL	=	paste0(game, ".csv.bz2")	;	HRdataOld	<-	NULL
if (file.exists(HRdataALL))	HRdataOld	=	read_csv(HRdataALL, guess_max = 10, lazy = FALSE, show_col_types = FALSE)

for (IND in csvFIND())	{
	PART	=	simpsplit(IND, "/")[1]	;	print(PART)
	CSVtimed	<-	paste0(PART, "/", PART, " - Timed.csv")
	
	if (PART %in% unique(HRdataOld$Part))	next
	
	if (file.exists(CSVtimed))	{
		hold		<-	read_csv(CSVtimed, guess_max = 10, lazy = FALSE, show_col_types = FALSE)
		hold$Part	<-	PART
		# hold		=	hold[hold$PULSE != 0, }
		HRdata		=	rbind(HRdata, hold)
		next
	}
	hold		=	read_csv(IND, guess_max = 10, lazy = FALSE, show_col_types = FALSE)
	hold$SPO2	=	NULL
	hold$Part	<-	gsub("_", ":", PART)	|>	gsub("[0]([1-9])", "\\1", x = _)
	#	replacing _ with : and removing zero padding

#	CSV with Time in Video
	times	=	format(seq(ISOdate(1,1,1, 0),	by = "sec",	length.out = nrow(hold)), "%H:%M:%S")
	hold$"Time in Video"	=	times
	write_csv(hold[, c("Time in Video", "PULSE")], CSVtimed)
	
	HRdata		<<-	rbind(HRdata, hold)
	hold		<-	hold[hold$PULSE != 0, ]
	
#	Table
	holdTABL	=	as.data.frame(table(hold$PULSE))	|>	setNames(c("Rate", "Count"))
	holdTABL$Rate	=	as.numeric(as.character(holdTABL$Rate))
	write.table(holdTABL, file=paste0(PART, "/", PART, " Frequency.txt"), sep = ",", row.names = FALSE, quote = FALSE)
	
#	Graph
	hold6570	=	sum(holdTABL[holdTABL$Rate >= 65 & holdTABL$Rate < 70, "Count"])
	hold6065	=	sum(holdTABL[holdTABL$Rate >= 60 & holdTABL$Rate < 65, "Count"])
	
	PULSEseq	=	70:max(hold$PULSE, 100)
	if	(hold6570 > 10)	PULSEseq	=	65:max(hold$PULSE, 100)
	if	(hold6065 > 10)	PULSEseq	=	60:max(hold$PULSE, 100)
	
	holdSUM	=	sepCOL(aggregate(hold$PULSE, list(Part = hold$Part), stats))

	graphHIST	<-	baseHIST(hold) + 
		ggtitle(prettyNUM(PART), subtitle=paste0("Length - ", hold[nrow(hold), "Time in Video"])) + 
		sumLines(holdSUM) + 
		stat_bin(binwidth = 1, col = "black") + 
		scale_x_continuous(
			breaks			=	PULSEseq,
			name			=	"Heart Rate (bpm)",
			limits			=	c(min(PULSEseq) - 1, max(PULSEseq, 101)),
			minor_breaks	=	NULL,
			expand			=	c(0, 0),
			sec.axis		=	dup_axis(
				name	=	NULL,
				breaks	=	holdSUM[, c("Lower", "Median", "Upper")],
				labels	=	c("25%", "Median", "75%"))
			)
	customSave(name = paste0(PART, " - Hist"), plot = graphHIST, fold = PART)
}

if (!is.null(HRdataOld))	{HRdata	<-	rbind(HRdataOld, HRdata)	;	rm(HRdataOld)}
write_csv(HRdata, HRdataALL)

HRtime	=	sum(aggregate(list(Time = HRdata[, "Time in Video"]), list(Part = HRdata$Part), max)$Time)
HRtime	=	timepad(as.numeric(HRtime))

HRclean		=	HRdata[HRdata$PULSE != 0, ]
HRsummary	=	sepCOL(aggregate(HRclean$PULSE, list(Part = HRclean$Part), stats))

facetHIST	<-	baseHIST(HRclean) %+% aes(fill = after_stat(ncount)) + 
	ggtitle(game, subtitle = paste0("Total Time: ", HRtime)) + 
	guides(fill = "none") + 
	# sumLines(HRsummary) + 
	stat_bin(binwidth = 1, col = "black") + 
	scale_x_continuous(name = "Heart Rate (bpm)", minor_breaks = NULL) + 
	facet_wrap(vars(Part), scales = "free_y", labeller = labeller(Part = function(IN)	gsub(paste0(game, " - "), "", IN)))

customSave(name = paste0(game, " - Hist"), plot = facetHIST, width = 16)

##	see about making it so this one Search script will find all of the CSVs and make the separate Timed, Hist, and table outputs for each. It should be doable, and having some check on if Timed.csv exists, to skip making the graph and such when it is already there. This single-script design might be superior to the current multi-script, at least for simplicity. Also will be a fun exercise to get working.

# for (ind in 1:length(CSVs))	{
	# VAR	=	paste0("CSV", ind)
	# assign(VAR, read_csv(CSVs[ind]))
# }
#	this will assign a separate CSV to separate, generated variables
#	not really needed though as having a single data frame with all of the data, and an identifying column, is all that is really needed
#		remembering that may help to get this working as I want it to, especially as I already achieved that with OCAT - Search - PA.r
