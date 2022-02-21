library(readr)
library(readxl)

# setwd("M:/TBOG/@Data/")
#	only necessary if running by GUI, so leaving it to be done manually

simpsplit	=	function(...)	unlist(strsplit(...))
game	=	rev(simpsplit(getwd(), "/"))[1]

prettyNUM	=	function(IN){
	out	=	simpsplit(IN, " ")
	paste0(c(
		out[1:(length(out)-1)],
		as.numeric(out[length(out)])),
		collapse = " "
	)
}

timepad	=	function(timesec) {
	paste(sprintf("%02d", c(timesec %/% 3600, timesec %%3600 %/% 60, round(timesec %% 60))), collapse = ":")
}

xlsFIND	=	function(DIRECT = getwd(), PAT = ".xlsx")	{
	LIST		=	list.files(DIRECT, recursive = TRUE, pattern = PAT)
	LIST		=	LIST[!startsWith(LIST, "@")]
	# LIST.full	=	paste0(DIRECT, "/", LIST)

	return(LIST)
}

XLSs	=	xlsFIND()

csvFIND	=	function(DIRECT = getwd(), PAT = "Timed.csv")	{
	LIST		=	list.files(DIRECT, recursive = TRUE, pattern = PAT)
	LIST		=	LIST[!startsWith(LIST, "@")]
	# LIST.full	=	paste0(DIRECT, "/", LIST)

	return(LIST)
}

CSVs	=	csvFIND()

HRdata	=	data.frame(matrix(nrow = 0, ncol = 3))

if (length(CSVs) > length(XLSs))	{
	for (IND in 1:length(CSVs))	{
		PART	=	simpsplit(CSVs[IND], "/")[1]
		PART	=	simpsplit(CSVs[IND], "_")[1]
		print(PART)

		temp		=	read_csv(CSVs[IND])
		temp$SPO2	=	NULL
		temp$Part	=	PART

		HRdata		<<-	rbind(HRdata, temp)
	}
}	else	{
	for (IND in 1:length(XLSs))	{
		PART	=	simpsplit(XLSs[IND], " Data")[1]
		PART	=	simpsplit(XLSs[IND], ".xlsx")[1]
		temp	=	read_excel(XLSs[IND])
		
		temp[, 1]	=	sapply(0:(nrow(temp) - 1), timepad)
		
		write_csv(temp, paste0(PART, " - Timed.csv"))
		temp$SPO2	=	NULL
		temp$Part	=	PART
		
		HRdata		<<-	rbind(HRdata, temp)
	}
}

write_csv(HRdata[, 3:1], paste0(game, ".csv.bz2"))