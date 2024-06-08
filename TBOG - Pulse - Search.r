library(readr)	;	library(ggplot2)	;	library(dplyr)

setwd("M:/TBOG/TBOG Done/Resident Evil 3")
#	only necessary if running by GUI, so leaving it to be done manually

game	<-	((getwd()	|>	strsplit("/"))[[1]]	|>	rev())[1]
# game	<-	(getwd()	|>	strsplit("/")	|>	unlist()	|>	rev())[1]
#	same result, but different approach


ggdevice	=	"png"	;	theme_set(theme_bw(base_size = 16))
gWIDTH	=	16	;	gHEIGH	=	9	;	DPI	=	120

timepad	<-	function(timesec) sprintf("%02d", c(timesec %/% 3600, timesec %%3600 %/% 60, round(timesec %% 60))) |> paste(collapse = ":")

csvFIND	<-	function(DIRECT = getwd(), PAT = "Edited.csv")	{
	LIST	=	list.files(DIRECT, recursive = TRUE, pattern = PAT)
	LIST	=	LIST[!startsWith(LIST, "@")]
	return(LIST)
}

CSVs	<-	csvFIND()
CSVnames	<-	strsplit(CSVs, "/")	|>	sapply("[[", 1)
NAMEclean	<-	function(IN)	IN	|>	gsub("Part 0", "Part ", x = _)

#	apply must be used to specify rows only
HRdata	<-	apply(	cbind(CSVs, CSVnames),	1,
	function(IN)	{
		hold	<-	read_csv(IN[1], lazy= FALSE, show_col_types = FALSE)
		hold$"Time in Video"	<-	sapply(0:(nrow(hold) - 1), timepad)	;	hold$Part	<-	factor(IN[2])
		return(hold[, c("Time in Video", "PULSE", "Part")])
		}
	)	|>	setNames(CSVnames)

HRdataCOMB	<-	Reduce(rbind, HRdata)

HRdataALL	=	paste0(game, ".csv.bz2")	;	HRpartOld	<-	NULL
if (file.exists(HRdataALL))	{
	HRpartOld	<-	read_csv(HRdataALL, col_types = "cdf", lazy = FALSE, show_col_types = FALSE, col_select=Part)[[1]]	|>	unique()
#	if older combined file exists, load in just the Part column as this is all that needs to be checked for duplication

	if (!all(HRpartOld %in% CSVnames)	|	!all(CSVnames %in% HRpartOld))	{
		HRdataOld	<-	read_csv(HRdataALL, col_types = "cdf", lazy = FALSE, show_col_types = FALSE)

		HRdataNew	<-	Reduce(rbind, HRdata[setdiff(CSVnames, HRpartOld)])
		#	setdiff is directional
		#		in this order, it returns those in CSVnames and not HRpartOld
		HRdataCOMB	<-	rbind(HRdataOld, HRdataNew)
	}
}
write_csv(HRdataCOMB, HRdataALL)

HRclean	<-	HRdataCOMB	|>	filter(PULSE != 0)

customSave	=	function(name="", plot = last_plot(), fold = "", device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if (fold != "")	name	=	paste0(fold, "/", name)
	if	(device	==	"png" | device == "both")	ggsave(filename=paste0(name, ".png"), device="png", dpi=dpi, plot = plot, width=width, height=height)
	if	(device	==	"pdf" | device == "both")	ggsave(filename=paste0(name, ".pdf"), device="pdf", plot = plot, width=width, height=height)
}

baseHIST	<-	function(IN)	{
	ggplot(IN, aes(PULSE, fill=after_stat(count))) +
	scale_fill_gradient("Count", low = "#6d59ff", high = "#ab4b41") +
	stat_bin(binwidth = 1, col = "black") +
	scale_y_continuous(name = "Count", expand = c(0.02, 0)) +
	theme(legend.position = c(1, 1), legend.justification = c(1, 1))
}

SUMM	<-	function(IN)	IN	|>	group_by(Part)	|>	filter(PULSE != 0)	|>	summarize(summary(PULSE)	|>
						as.list()	|>	data.frame(check.names=FALSE))	|>
						rename(Lower = "1st Qu.", Upper = "3rd Qu.", Min = "Min.", Max = "Max.")

sumLines	=	function(IN,	wid = 1)	list(
		# geom_vline(data = IN, aes(xintercept = Mean),		linewidth = wid),
		geom_vline(data = IN, aes(xintercept = Median),	linewidth = wid),
		geom_vline(data = IN, aes(xintercept = Lower),	linewidth = wid,	linetype = "dashed"),
		geom_vline(data = IN, aes(xintercept = Upper), 	linewidth = wid,	linetype = "dashed")
	)

for (IND in CSVnames)	{
	if (IND	%in%	HRpartOld)	next
	PART	<-	HRdata[[IND]]	;	PARTclean	<-	PART	|>	filter(PULSE !=0 )
	CSVtimed	<-	paste0(IND, "/", IND, " - Timed.csv")
	if	(!file.exists(CSVtimed))	write_csv(PART[, c("Time in Video", "PULSE")], CSVtimed)

	PARTtable	<-	PART$PULSE	|>	table()	|>	as.data.frame()	|>	setNames(c("Rate", "Count"))
	write.table(PARTtable, file=paste0(IND, "/", IND, " Frequency.txt"), sep = ",", row.names = FALSE, quote = FALSE)

	PULSEupp	<-	max(PART$PULSE, 100)
	PULSElow	<-	70	;	ECDF	<-	ecdf(PARTclean$PULSE)
	if (ECDF(65) >= 0.05)	PULSElow	<-	65
	if (ECDF(60) >= 0.05)	PULSElow	<-	60

	PULSEseq	=	PULSElow:PULSEupp

	PARTsumm	<-	PARTclean	|>	SUMM()
	LABform	<-	function(IN)	{
		if (any(diff(IN |> unlist()) < 2))	return(c("25%", "Median\n", "75%"))
		c("25%", "Median", "75%")
	}
	graphHIST	<-	baseHIST(PARTclean) +
		ggtitle(NAMEclean(IND), subtitle=paste0("Length - ", PARTclean[nrow(PARTclean), "Time in Video"])) +
		sumLines(PARTsumm) +
		stat_bin(binwidth = 1, col = "black") +
		scale_x_continuous(
			breaks		=	PULSEseq,	minor_breaks	=	NULL,
			name		=	"Heart Rate (bpm)",
			limits		=	c(PULSElow - 1, PULSEupp + 1),	expand	=	c(0, 0),
			sec.axis	=	dup_axis(
				name	=	NULL,
				breaks	=	PARTsumm[, c("Lower", "Median", "Upper")] |> as.numeric(),
				labels	=	LABform)
			)
	customSave(name = paste0(IND, " - Hist"), plot = graphHIST, fold = IND)
}

facetHIST	<-	baseHIST(HRclean) %+% aes(fill = after_stat(ncount)) +
	ggtitle(game, subtitle = paste0("Total Time: ", timepad(nrow(HRclean)))) + guides(fill = "none") +
	# sumLines(SUMM(HRclean)) +
	stat_bin(binwidth = 1, col = "black") +
	scale_x_continuous(name = "Heart Rate (bpm)", minor_breaks = NULL) +
	facet_wrap(vars(Part), scales = "free_y",
		labeller = labeller(Part = function(IN)	gsub(paste0(game, " - "), "", IN)	|>	NAMEclean()	)
		)

customSave(name = paste0(game, " - Hist"), plot = facetHIST, width = 16)