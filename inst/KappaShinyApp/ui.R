shinyUI(fluidPage(theme = "kappa.css",
	
	titlePanel("KappaGUI — Cohen's and Fleiss' kappa calculator"),
	tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0098B6}")), # pour régler la couleur de la barre du slider

	sidebarLayout(
		# 1. Le menu de gauche, presentant les options de l'analyse :
		sidebarPanel(
			# a) Menu d'importation du fichier de donnees :
			fileInput("file", label=h3("Import dataset"), accept=c(".csv", ".txt")), # le widget de selection d'un fichier sur le PC. On n'accepte que les types TXT et CSV
			selectInput("fieldSep", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" ")),
			checkboxInput("checkboxRownames", label="The file contains the row names as its first column", value=TRUE),	
			checkboxInput("checkboxHeaders", label="The file contains the names of the variables as its first line", value=TRUE),
			actionButton("load_dataset", "Load dataset"), # un bouton pour lancer le chargement effectif du fichier avec read.csv, une fois tous les parametres definis
			actionButton("help_me", "Show/Hide help"), # un bouton d'affichae/masquage d'un texte d'aide...
			uiOutput("helpBox"), # ... qui s'affiche ici, en fonction de la valeur retournée par la valeur "help_me" du bouton d'aide (cf. server.R)
			br(),
			br(), # sauts de ligne
			br(),
	
			# b) Choix entre Kappa de Cohen ou de Fleiss :
			radioButtons("choiceKappa", label=h3("Number of raters"), choices=c("Two raters", "Three raters or more")),	
			conditionalPanel( # panneau ne s'affichant que si l'utilisateur choisit le kappa de Cohen !
				condition = "input.choiceKappa == 'Two raters'", # condition javascript. Cf. aide en ligne pour la syntaxe. 
				radioButtons("weightingScheme", label=strong("Cohen's kappa weighting scheme"), choices=c("Unweighted kappa", "Linear weighting", "Quadratic weighting"))
			),
			conditionalPanel( # panneau ne s'affichant que si l'utilisateur choisit le kappa de Fleiss !
				condition = "input.choiceKappa == 'Three raters or more'",
				sliderInput("nb_raters", label=strong("Number of raters for Fleiss' kappa calculation"), min=3, max=10, value=3)
			),
			actionButton("go_kappa", "Execute") # bouton pour lancer le calcul effectif
		),

		# 2. Le panneau principal, pour l'affichage des resultats :
		mainPanel(
			h3(textOutput("texte_resume")), # cet element est calcul\'e dans server.R seulement apr\`es l'importation du fichier : il ne s'affiche donc qu'a ce moment
			h5(textOutput("texte_conseil")), # idem
			tableOutput("resume"), # on affiche le resum\'e du jeu de donnees fourni par l'utilisateur
			br(),
			br(),
			h3(textOutput("texte_resultats")), # cet element est calcul\'e dans server.R seulement apr\`es le lancement du calcul : il ne s'affiche donc qu'a ce moment
			tableOutput("resultats"), # idem
			uiOutput("bouton_telechargement") # le bouton de telechargement des resultats n'est calcul\'e / affich\'e qu'au bout du processus (cf. server.R)
		)
	)

))
