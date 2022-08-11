game	=	gsub(".*/(.*)$", "\\1", getwd())

OLD	=	FALSE
if (file.exists(paste0(game, " - Archive Sizes.csv")))	OLD	=	TRUE

ARC.full	=	list.files(pattern = "*Archive.mkv", recursive = TRUE)
ARC.final	=	list.files(pattern = "*Archive Final.mkv", recursive = TRUE)

SIZE.full	=	file.size(ARC.full)	;	SIZE.final	=	file.size(ARC.final)

if (length(SIZE.final) < length(SIZE.full))	SIZE.final = c(SIZE.final, rep(NA, length(SIZE.full) - length(SIZE.final)))
#	in case not all of the Archive Final.mkv files are present

SIZES	=	data.frame(Part = gsub("(.*)/.*", "\\1", ARC.full),	Original = SIZE.full,	Final = SIZE.final)

if (OLD)	{
	SIZES.old	=	read.csv(paste0(game, " - Archive Sizes.csv"))
	SIZES		=	merge(SIZES.old, SIZES, all = TRUE)
}

write.csv(SIZES, paste0(game, " - Archive Sizes.csv"), row.names = FALSE, quote = FALSE)

SUM	=	data.frame(
	Original	=	sum(SIZES$Original, na.rm = TRUE)/1024^3,
	Final		=	sum(SIZES$Final, na.rm = TRUE)/1024^3
	)

write.table(t(SUM), file = paste0(game, " - Archive Sizes.txt"), col.names = FALSE, quote = FALSE)