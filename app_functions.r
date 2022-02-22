stats	=	function(x)	{c(
	Mean	=	mean(x),
	Lower	=	quantile(x, 0.25, names	=	FALSE),
	Median	=	median(x),
	Upper	=	quantile(x, 0.75, names	=	FALSE),
	Min		=	min(x),
	Max		=	max(x)
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
timepad	=	function(timesec) sprintf("%02d", c(timesec %/% 3600, timesec %%3600 %/% 60, round(timesec %% 60)))
timeSum	=	function(TIME)	paste(timepad(as.numeric(sum(TIME))), collapse = ":")

roundFRAME	=	function(IN, r = 2)	{
	numCOL	=	sapply(IN, is.numeric)
	IN[numCOL]	=	round(IN[, numCOL], r)
	return(IN)
}