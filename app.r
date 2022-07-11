library(shiny)
# setwd("C:/Users/Jim/Documents/TBOG_Data")
# setwd("M:/TBOG/TBOG_Data")
library(readr)
library(ggplot2)

DATA	=	new.env()
FILES	=	list.files(path = "Data", pattern = "*.csv*")
DATA$LOAD		=	FALSE	#used for tracking if data has been loaded automatically
# DATA$Default	=	FILES[which.max(file.mtime(paste0("Data/", FILES)))]
#	with file.mtime, the modified timestamp is found and which.max will find the newest file in the list. In theory then, just updating the Data folder is enough, even though that's not an option with ShinyApps.io
#		above not useful on ShinyApps.io because the modified times are when they all were uploaded
# DATA$Default	=	"Amnesia_ Rebirth.csv.bz2"
GRAPH	=	new.env()
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with
VIEW	=	new.env()

VIEW$YTlink	=	"_p9Ifq4ln-c"
VIEW$YTlist	=	"https://www.youtube.com/channel/UCtzfp-ZWZWLhGTjhP5NeYqQ/playlists"

dataLOAD	=	function(name, datapath	=	NULL)	{
	if (is.null(datapath))	datapath	=	name
	HRdata	=	read_csv(datapath, guess_max = 10, lazy = TRUE, show_col_types = FALSE)
	# DATA$game	=	unlist(strsplit(HRdata[1, ]$Part, " - "))[1]
	# DATA$game	=	gsub(" - Part [0-9]*", "", HRdata[1, ]$Part)
	DATA$game	=	gsub("_", ":", gsub(".csv.bz2", "", name))

	HRdata$Part		=	ordered(HRdata$Part, unique(HRdata$Part))
	DATA$levs		<-	levels(HRdata$Part)
	DATA$HRclean	<-	HRdata[HRdata$PULSE != 0, ]
	DATA$HRtime	<-	aggregate(list(Time = DATA$HRclean[, "Time in Video"]), list(Part = DATA$HRclean$Part), max)
}

source("app_functions.r",	local = TRUE)

source("app_UI_grab.r", local	=	TRUE)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
	setBookmarkExclude(c("dataSelLOAD", "dataInput", "graphs", "tabCOLS", "tabROWS", "gWIDTH", "gHEIGH", "plotsSel"))
	output$Title	=	renderUI({	titlePanel("Heart Rate Statistics and Graphs")	})
	if (exists("FILE", envir	=	DATA))	{
		dataLOAD(DATA$FILE)
		DATA$LOAD	=	TRUE
	}
	observeEvent(input$dataInput,	{
		FILE	=	input$dataInput
		dataLOAD(FILE$name, FILE$datapath)
		DATA$LOAD	=	TRUE
	},	priority	=	10)
	
	observeEvent(input$dataSelLOAD,	{
		dataLOAD(input$dataSel, paste0("Data/", input$dataSel))
		DATA$LOAD	=	TRUE
	},	priority	=	10)

	observeEvent(input$tutorial,	{
		showModal(	modalDialog(
			HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', VIEW$YTlink,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe><br><a href="https://www.youtube.com/watch?v=', VIEW$YTlink, '" target="_blank">YouTube Link</a> (Some features may appear different)')),
			title = "YouTube Tutorial Video",
			easyClose = TRUE,	footer = modalButton("Close")	)	)
	})

	observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD), {
		req(DATA$levs)

		output$Title	=	renderUI({	titlePanel(paste0(DATA$game, " - Heart Rate Statistics and Graphs"))	})
		updateCheckboxGroupInput(inputId	=	"tabROWS",
			choiceValues	=	DATA$levs,	choiceNames	=	lapply(DATA$levs, prettyNUM),	selected	=	DATA$levs
		)

		updateSelectInput(inputId	=	"plotsSel",
			choices	=	setNames(1:length(DATA$levs), sapply(DATA$levs, prettyNUM))	#will assign the second argument to the first as names
		)
	})

	filtCOL	=	reactiveVal(c("Part", "Mean", "Median"))
	filtROW	=	reactiveVal(TRUE)
	observeEvent(input$tabCOLS,	{
		req(DATA$STATS)
		filtCOL(names(DATA$STATS)	%in%	c("Part", input$tabCOLS))
	})
	observeEvent(input$tabROWS,	{
		req(DATA$STATS)
		filtROW(DATA$STATS$Part		%in%	c(input$tabROWS))
	})

	partTIME	=	reactiveVal(timeSum(DATA$HRtime$Time))
	observeEvent(filtROW(),	{
		partTIME(timeSum(DATA$STATS[filtROW(), "Time.in.Video"]))
	})

	source("app_tables.r",	local = TRUE)
	source("app_graphs.r",	local = TRUE)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")