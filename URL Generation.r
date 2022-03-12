setwd("M:/TBOG/TBOG_Data")

URL	=	NULL
URL	=	"https://guestjim.shinyapps.io/The_Body_on_Games-List/"
FILES	=	list.files(path = "Data", pattern = "*.csv*")

hold	=	cbind(Name = FILES, URL = paste0(URL, "?_inputs_&dataSel=%22", URLencode(FILES), "%22"))
#	the URL encoding of characters is not necessary it seems, so dropping it for ease of reading
hold	=	cbind(Name = FILES, URL = paste0(URL, '?_inputs_&dataSel="', FILES, '"'))

write.csv(hold, "URLs.csv", row.names=FALSE, quote=FALSE)
