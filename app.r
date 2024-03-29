library(shiny)
library(readr)
library(ggplot2)
library(magrittr)

DATA	=	new.env()
FILES	=	list.files(path = "Data", pattern = "*.csv*")
# DATA$Default	=	FILES[which.max(file.mtime(paste0("Data/", FILES)))]
#	this will set the default data to be whatever file is newest

GRAPH	=	new.env()
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with
VIEW	=	new.env()

VIEW$UPLOAD	<-	FALSE
# VIEW$YTlink	=	"_p9Ifq4ln-c"	#	old video
VIEW$YTlink	=	"5MaUXtQh4FU"
VIEW$YTlist	=	"https://www.youtube.com/channel/UCtzfp-ZWZWLhGTjhP5NeYqQ/playlists"
VIEW$THRESH	<-	TRUE

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

source("app_UI.r", local	=	TRUE)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
	setBookmarkExclude(c("dataSelLOAD", "dataInput", "graphs", "tabCOLS", "tabROWS", "gWIDTH", "gHEIGH", "plotsSel"))
	output$Title	=	renderUI({	titlePanel("Heart Rate Statistics and Graphs")	})
	
	if (exists("FILE", envir	=	DATA))	dataLOAD(DATA$FILE)
	
	observeEvent(input$dataInput,	{
		FILE	=	input$dataInput
		dataLOAD(FILE$name, FILE$datapath)
	},	priority	=	10)

	observeEvent(input$dataSelLOAD,	{
		dataLOAD(input$dataSel, paste0("Data/", input$dataSel))
	},	priority	=	10)

	observeEvent(input$tutorial,	{
		showModal(	modalDialog(
			HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', VIEW$YTlink,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe><br><a href="https://www.youtube.com/watch?v=', VIEW$YTlink, '" target="_blank">YouTube Link</a> (Some features may appear different)')),
			title = "YouTube Tutorial Video",
			easyClose = TRUE,	footer = modalButton("Close")	)	)
	})

	observeEvent(list(input$dataInput, input$dataSelLOAD), {
		req(DATA$levs)

		output$Title	=	renderUI({	titlePanel(paste0(DATA$game, " - Heart Rate Statistics and Graphs"))	})
		updateCheckboxGroupInput(inputId = "tabROWS",
			choiceValues = DATA$levs,	choiceNames = lapply(DATA$levs, prettyNUM),	selected = DATA$levs
		)

		updateSelectInput(inputId = "plotsSel",	choices = setNames(1:length(DATA$levs), sapply(DATA$levs, prettyNUM))	)
	},	priority = 10)

	GAME		<-	reactive(	DATA$game	)	%>%	bindEvent(input$dataSelLOAD)
	partSEL		<-	reactive(	as.numeric(input$plotsSel)	)
	partSELlev	<-	reactive(	DATA$levs[as.numeric(input$plotsSel)]	)
	
	PART	<-	reactive({
		out	<-	DATA$HRclean[DATA$HRclean$Part == partSELlev(), ]
		updateNumericInput(inputId = "aboveTHRS",	value = quantile(out$PULSE,	0.75,	names = FALSE,	na.rm = TRUE))
		return(out)
	})	%>%	bindEvent(list(input$dataSelLOAD, input$plotsSel))

	STATS	<-	reactive({
		out			=	sepCOL(aggregate(list(Pulse = DATA$HRclean$PULSE), list(Part = DATA$HRclean$Part), stats))
		out			=	merge(out, DATA$HRtime, by="Part", sort = FALSE)
		out$Time	=	sapply(as.numeric(out$Time), timeSum)
		return(out)
	})	%>%	bindCache(GAME())	%>%	bindEvent(input$dataSelLOAD)
	
	source("app_tables.r",	local = TRUE)
	source("app_graphs.r",	local = TRUE)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")