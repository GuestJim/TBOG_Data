saveGraphUI	<-	function(name, TYPE)	{
	ns	<-	NS(name)
	
	tagList(
		fixedRow(
			column(3,	downloadButton(outputId	=	ns("downloadGRAPH"),	label	=	TYPE)	),
			column(3,	numericInput(inputId	=	ns("WIDTH"),
					label	=	"Download Width (In)",
					value	=	16,	width	=	"90%"	)
			),
			column(4,	numericInput(inputId	=	ns("HEIGHT"),
					label	=	"Download Height (In)",
					value	=	9,	width	=	"90%"	)
			)
		)
	)
}

ui <- function(request)	{fluidPage(
	uiOutput('Title'),
	sidebarLayout(
		sidebarPanel(
			selectInput(inputId	=	"dataSel",	label	=	"Data to Load",	selectize	=	FALSE,
				choices	=	setNames(FILES,	gsub("_", ":", gsub(".csv.bz2", "", FILES))),	selected	=	DATA$Default
			),
			actionButton(inputId	=	"dataSelLOAD",	label	=	"Load Selected Data"),
			if (VIEW$UPLOAD)	fileInput(inputId	=	"dataInput",
				label	=	"CSV Data to Import",
				accept	=	c(".csv", ".csv.bz2", ".csv.gz", ".csv.xz"),
				placeholder	=	"No File Selected"
			),
			# bookmarkButton(),
			checkboxGroupInput(inputId	=	"tabCOLS",	label	=	"Statistics to show:",
				choices		=	names(stats(Inf)),
				selected	=	c("Mean", "Median"),
				),
			checkboxGroupInput(inputId	=	"tabROWS",	label	=	"Parts to show:",
				choices		=	NULL,	selected	=	NULL,
				),
			width	=	3
			),
		mainPanel(
			fluidRow(
				column(8, verticalLayout(
					tableOutput("timeTable"),
				)	),
				column(4, verticalLayout(
					if (is.character(VIEW$YTlink))	actionButton('tutorial', "Show YouTube Tutorial"),
					if (is.character(VIEW$YTlist))	a("YouTube Playlists", href = VIEW$YTlist, target = "_blank"),
				)	),
			),
			tabsetPanel(
				tabPanel("Table",
					downloadButton(outputId	=	"downloadTable",	label	=	"Download Table (CSV)"),
					tableOutput("summaryTable"),
				),
				tabPanel("Single Part",
					tabsetPanel(
						tabPanel("Graph",
							plotOutput("graphPART",		height	=	480),
							saveGraphUI('PART',	"Histogram (PNG)"),
							plotOutput("graphCOURSE",	height	=	480,
								brush	=	brushOpts(id	=	"COURSEbrush", resetOnNew	=	TRUE, direction	=	"x")),
							strong("Click and Drag to Zoom Below"),
							plotOutput("brushCOURSEzoom",	height	=	480),
							tableOutput('brushCOURSEtable'),
							),
						tabPanel("Threshold",
							plotOutput("aboveCOURSE",	height	=	480),
							fluidRow(
								column(3,	numericInput('aboveTHRS',	"Heart Rate Threshold",	value = 100,	min = 0,	max = 150,	step = 1)),
								column(6,	tableOutput('aboveTABL')),
								),
							),
						type	=	"pills",
						header	=	tagList(
							fluidRow(
								column(5,	selectInput(inputId = "plotsSel",	label = "Selected Part:",	choices = NULL	)	),
								column(7,	tableOutput("statsPART")	),
								),
							helpText("If graphs are blank after loading data, select a different part and return to force an update"),
							),
						),
					),
				tabPanel("Multiple Parts",
					tabsetPanel(
						tabPanel("Faceted Graph",
							saveGraphUI('FACET',	"Faceted Graph (PNG)"),
							plotOutput("graphFACET", height	=	480),
						),
						id	=	"graphs",
						type	=	"pills"
					)
					)
			),
			width	=	9
		),
	)
)
}