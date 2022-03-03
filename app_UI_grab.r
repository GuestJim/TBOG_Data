ui <- fluidPage(
	uiOutput('Title'),
	sidebarLayout(
		sidebarPanel(
			selectInput(inputId	=	"dataSel",	label	=	"Data to Load",	selectize	=	FALSE,
				choices	=	setNames(FILES,	gsub(".csv.bz2", "", FILES)),	selected	=	DATA$Default
			),
			actionButton(inputId	=	"dataSelLOAD",	label	=	"Load Selected Data"),
			fileInput(inputId	=	"dataInput",
				label	=	"CSV Data to Import",
				accept	=	c(".csv", ".csv.bz2", ".csv.gz", ".csv.xz"),
				placeholder	=	"No File Selected"
			),
			checkboxGroupInput(inputId	=	"tabCOLS",	label	=	"Statistics to show:",
				choices		=	names(stats(Inf)),
				selected	=	c("Mean", "Median"),	#	the default selected values
				),
			checkboxGroupInput(inputId	=	"tabROWS",	label	=	"Parts to show:",
				choices		=	NULL,	selected	=	NULL,	#	the default selected values
				),
			width	=	3
			),
		mainPanel(
			textOutput("timeTotal"),
			textOutput("timePartsSel"),
			tabsetPanel(
				tabPanel("Table",
					# verticalLayout(
						downloadButton(outputId	=	"downloadTable",	label	=	"Download Table (CSV)"),
					# ),
					tableOutput("summaryTable"),
				),
				tabPanel("Single Graph",
					fixedRow(
						column(3,
								downloadButton(outputId	=	"downloadGraphPart",	label	=	"Histogram (PNG)")
							),
						column(3,
							numericInput(inputId	=	"gWIDTH",
								label	=	"Download Width (In)",
								value	=	16,
								width	=	"90%"
								)
							),
						column(3,
							numericInput(inputId	=	"gHEIGH",
								label	=	"Download Height (In)",
								value	=	9,
								width	=	"90%"
								)
							),
						),
						fixedRow(
							column(5,
								selectInput(inputId	=	"plotsSel",
									label	=	"Specific Part Graph:",	choices	=	NULL
								),
							),
							column(7,
								tableOutput("statsPART"),
							),
						),
						plotOutput("graphPART",	height	=	480),
						plotOutput("graphCOURSE",	height	=	480,
							brush	=	brushOpts(id	=	"COURSEbrush", resetOnNew	=	TRUE, direction	=	"x")),
						textOutput("brushCOURSEtext"),
						plotOutput("brushCOURSEzoom",	height	=	480),
						tableOutput("brushCOURSEtable"),
					),
				tabPanel("Faceted Graph", 
					fixedRow(
						column(3,
							downloadButton(outputId	=	"downloadGraph",	label	=	"Faceted Graph (PNG)")
						),
						column(3,
							numericInput(inputId	=	"gWIDTH",
								label	=	"Download Width (In)",
								value	=	16,
								width	=	"90%"
							)
						),
						column(3,
							numericInput(inputId	=	"gHEIGH",
								label	=	"Download Height (In)",
								value	=	9,
								width	=	"90%"
							)
						),
					),
					plotOutput("graphFACET", height	=	480),
				),
				id	=	"graphs",
				type	=	"pills"
			),
			width	=	9
		),
	)
)