shinyUI(fluidPage(theme = "kappa.css",
	
	titlePanel("KappaGUI — Cohen's and Fleiss' kappa calculator"),
	tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0098B6}")), # pour régler la couleur de la barre du slider
	# Pour le bouton d'aide :
	tags$style(HTML(" 
		.btn-help.btn {
			display: inline-block;
			padding: 7px 12px;
			font-size: 1em;
			margin: 0 0 0 0;
			vertical-align: middle;
			color: gray;
			font-weight: bold;
			background-color: white;
			border-color: gray;
		}
		.btn-help.btn:hover {
			color: white;
			background-color: #0098B6;
		}
		.btn-help.active {
			color: white;
			background-color: #0098B6;
			border-color: #0098B6;
		}
		"
	)),
	

	sidebarLayout(
		# 1. Le menu de gauche, présentant les options de l'analyse :
		sidebarPanel(
			# a) Menu d'importation du fichier de données :
			fileInput("file", label=h3("1. Import dataset"), accept=c(".csv", ".txt")), # le widget de sélection d'un fichier sur le PC. On n'accepte que les types TXT et CSV
			fluidRow(
				column(6,
					selectInput("fieldSep", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" "))
				),
				column(6,
					textInput("charNA", label="Indicator for missing values", value="")
				)
			),
			checkboxInput("checkboxRownames", label="The file contains the row names as its first column", value=TRUE),
			actionButton("load_dataset", "Load dataset"), # un bouton pour lancer le chargement effectif du fichier avec read.csv, une fois tous les parametres definis
			actionButton("helpKappa", label="Help", class="btn-help"),
			uiOutput("helpBox"), # ... qui s'affiche ici, en fonction de la valeur retournée par la valeur "help_me" du bouton d'aide (cf. server.R)
			br(),
			br(), # sauts de ligne
	
			# b) Choix entre Kappa de Cohen ou de Fleiss :
			h3("2. Settings"),
			radioButtons("choiceKappa", label=h4("Number of raters"), choices=c("Two raters", "Three raters or more")),	
			conditionalPanel( # panneau ne s'affichant que si l'utilisateur choisit le kappa de Cohen
				condition = "input.choiceKappa == 'Two raters'", # condition javascript. Cf. aide en ligne pour la syntaxe. 
				radioButtons("weightingScheme", label=h4("Cohen's kappa weighting scheme"), choices=list("Unweighted kappa"="unweighted", "Linear weighting"="equal", "Quadratic weighting"="squared"))
			),
			conditionalPanel( # panneau ne s'affichant que si l'utilisateur choisit le kappa de Fleiss
				condition = "input.choiceKappa == 'Three raters or more'",
				sliderInput("nb_raters", label=h4("Number of raters for Fleiss' kappa calculation"), min=3, max=10, value=3)
			),
			actionButton("go_kappa", "Execute") # bouton pour lancer le calcul
		),

		# 2. Le panneau principal, pour l'affichage des résultats :
		mainPanel(
			h4(textOutput("texte_resume")), # cet élément est calculé dans server.R seulement après l'importation du fichier : il ne s'affiche donc qu'à ce moment
			h5(textOutput("texte_conseil")), # idem
			tableOutput("resume"), # on affiche le resumé du jeu de données fourni par l'utilisateur
			br(),
			br(),
			h4(textOutput("texte_resultats")), # cet élément est calculé dans server.R seulement après le lancement du calcul : il ne s'affiche donc qu'à ce moment
			tableOutput("resultats"), # idem
			uiOutput("bouton_telechargement") # le bouton de téléchargement des résultats n'est calculé / affiché qu'au bout du processus (cf. server.R)
		)
	)

))
