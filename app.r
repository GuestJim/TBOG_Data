library(shiny)
# setwd("C:/Users/Jim/Documents/TBOG_Data")
library(readr)
library(ggplot2)

DATA	=	new.env()
DATA$LOAD	=	FALSE	#used for tracking if data has been loaded automatically
DATA$Default	=	"Dead Rising 4.csv.bz2"
GRAPH	=	new.env()
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with

# DATA$FILE	=	"Dead Rising 4.csv.bz2"
#	by giving this a file, we can avoid needing to upload a file

dataLOAD	=	function(name, datapath	=	NULL)	{
	if (is.null(datapath))	datapath	=	name
	HRdata	=	read_csv(datapath, guess_max = 10, lazy = TRUE, show_col_types = FALSE)
	DATA$game	=	unlist(strsplit(HRdata[1, ]$Part, " - "))[1]

	HRdata$Part		=	ordered(HRdata$Part, unique(HRdata$Part))
	DATA$levs		<-	levels(HRdata$Part)
	DATA$HRclean	<-	HRdata[HRdata$PULSE != 0, ]
	DATA$HRtime	<-	aggregate(list(Time = DATA$HRclean[, "Time in Video"]), list(Part = DATA$HRclean$Part), max)
}

source("app_functions.r",	local = TRUE)

source("app_UI_grab.r", local	=	TRUE)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
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

	observeEvent(list(input$dataInput, input$dataSelLOAD, DATA$LOAD), {
		req(DATA$levs)

		output$Title	=	renderUI({	titlePanel(paste0(DATA$game, " - Heart Rate Statistics and Graphs"))	})
		updateCheckboxGroupInput(inputId	=	"tabROWS",
			choices		=	DATA$levs,	selected	=	DATA$levs
		)

		updateSelectInput(inputId	=	"plotsSel",
			choices	=	setNames(1:length(DATA$levs), DATA$levs)	#will assign the second argument to the first as names
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
shinyApp(ui = ui, server = server)