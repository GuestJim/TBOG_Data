game	=	gsub(".*/(.*)$", "\\1", getwd())

OLD	=	FALSE
if (file.exists(paste0(game, " - Archive Sizes.csv")))	OLD	=	TRUE

ARC.full	=	list.files(pattern = "*Archive.mkv", recursive = TRUE)
ARC.final	=	list.files(pattern = "*Archive Final.mkv", recursive = TRUE)

size.name	=	function(IN, NAME = "Size")	{
	hold	=	file.info(IN)
	hold$Name	=	gsub("(.*)/.*", "\\1", rownames(hold))
	out	<-	hold[, c("Name", "size")]
	rownames(out)	<-	NULL	;	colnames(out)	<-	c("Part", NAME)
	return(out)
}
#	by using file.info instead of just file.size, I can ensure the sizes are paired with names

SIZE.full	=	size.name(ARC.full, "Original")	;	SIZE.final	=	size.name(ARC.final, "Final")
SIZES	=	merge(SIZE.full, SIZE.final, all = TRUE, by = "Part")

if (OLD)	{
	SIZES.old	=	read.csv(paste0(game, " - Archive Sizes.csv"))
	SIZES.all	=	merge(SIZES.old, SIZES, all = TRUE, by = "Part", suffixes = c(".Old", ".New"))
	SIZES.all$Original	<-	ifelse(is.na(SIZES.all$Original.Old),	SIZES.all$Original.New,	SIZES.all$Original.Old)
	SIZES.all$Final		<-	ifelse(is.na(SIZES.all$Final.Old),		SIZES.all$Final.New,	SIZES.all$Final.Old)
	#	this is to remove instances when the old or new list of sizes is missing values
	SIZES	<-	SIZES.all[, c("Part", "Original", "Final")]
}

write.csv(SIZES, paste0(game, " - Archive Sizes.csv"), row.names = FALSE, quote = FALSE)

SUM	=	data.frame(
	Original	=	sum(SIZES$Original, na.rm = TRUE)/1024^3,
	Final		=	sum(SIZES$Final, na.rm = TRUE)/1024^3
	)

write.table(t(SUM), file = paste0(game, " - Archive Sizes.txt"), col.names = FALSE, quote = FALSE)