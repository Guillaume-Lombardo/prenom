navbarPage("Prénoms",
					 tabPanel("listes prénoms",
					 				 column(2,
					 				 			 wellPanel(style = "overflow-y:scroll; max-height: 600px",
					 				 			 	radioButtons('finesse', 'Finesse du tableau', 
					 				 			 							 choices = setNames(c('fin','annee', 'depart', 'rien'), 
					 				 			 							 									 c('detail maximaux', 'par année', 
					 				 			 							 									 	'par départements', 'tout en même temps')),
					 				 			 							 selected = 'rien'),
					 				 			 	sliderInput("annee", "Année",
					 				 			 							min = 1900, max = 2017,
					 				 			 							value = c(1900,2017),
					 				 			 							step = 1),
					 				 			 	textInput('departement', 'Departement : ', 
					 				 			 						placeholder = '75, 77, 78, ...',
					 				 			 						value = NULL)
					 				 			 )
					 				 ),
					 				 column(10,
					 				 			 dataTableOutput("table_prenom")
					 				 )
					 ),
					 tabPanel("Info sur un prénom",
					 				 column(2,
					 				 			 wellPanel(style = "height: 600px",
					 				 			 	textInput('prenom2', 'Prénom(s) :', value = 'GINETTE'),
					 				 			 	sliderInput("annee2", "Année",
					 				 			 							min = 1900, max = 2017,
					 				 			 							value = c(1900,2017),
					 				 			 							step = 1),
					 				 			 	textInput('departement2', 'Departement : ', 
					 				 			 						placeholder = '75, 77, 78, ...'),
					 				 			 	actionButton('dpt_button2', 'Actualiser la carte')
					 				 			 	# ,
					 				 			 	# checkboxInput('limitefille', 'Se limiter aux filles ?', value = TRUE)
					 				 			 )
					 				 ),
					 				 column(10,
					 				 			 fluidRow(
					 				 			 	column(6,
					 				 			 				 plotlyOutput('graph_annee_abs'), height = "200px"),
					 				 			 	column(6,
					 				 			 				 plotlyOutput('graph_annee_rel'), height = "200px")
					 				 			 ),
					 				 			 fluidRow(
					 				 			 	column(6,
					 				 			 				 plotlyOutput('graph_dpart_abs'), height = "600px"),
					 				 			 	column(6,
					 				 			 				 plotlyOutput('graph_dpart_rel'), height = "600px")
					 				 			 )
					 				 )		 
					 )
)
