library(irr)
#source("kappaCohen.R")
#source("kappaFleiss.R")

shinyServer(function(input, output) {

	myenvg = new.env() # environnement privé au package ; contiendra le jeu de données (vu comme une variable globale)
	
	###########################################################
	# 1. Charger le fichier de données fourni par l'utilisateur
	observeEvent(input$load_dataset, { # on attend que l'utilisateur ait cliqué sur le bouton "Load dataset"
		if (! is.null(input$file$datapath)) { # si l'utilisateur a bien choisi un fichier sur son disque dur
			dat <- read.table(input$file$datapath, sep=as.character(input$fieldSep), na.strings=as.character(input$charNA), header=TRUE) # on charge le fichier *sans l'argument row.names*
			if (input$checkboxRownames) { # si l'utilisateur a fourni des noms d'individus,
				if (!any(duplicated(dat[,1]))) { # et qu'il n'y a pas de duplicats parmi eux,
					rownames(dat) <- dat[,1] # on définit la première colonne comme nom des individus,
					dat[,1] <- NULL # puis on la supprime.
				} else { # il y a des duplicats parmi les noms d'individus
					showModal(modalDialog(title = "Warning", "There are duplicates in row names. Row names will be ignored, but please check your data.", easyClose = TRUE))
					dat[,1] <- NULL
				}
			}
			output$resume <- renderTable(head(dat)) # on renvoie les premières lignes du tableau de données chargé par l'utilisateur
			output$texte_resume <- renderText("Your dataset has been successfully loaded.")
			output$texte_conseil <- renderText("Here are the first six rows.")
			assign("dat", dat, envir=myenvg) # on place le jeu de données dans un environnement global pour qu'il puisse etre réutilisé dans la fonction qui suit
		} else { # l'utilisateur a oublié de choisir un fichier
			showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
		}
    	})

	# Texte du bouton d'aide :
	observeEvent(input$helpKappa, { # bouton d'aide pour le MDS
		showModal(modalDialog(
			title="Input file",
			h4("Format"),
			p("KappaGUI accepts only CSV or TXT files."),
			p("If there are", em("p"), "variables observed by", em("k"), "raters on", em("n"), "individuals, the input file should have", em("n"), "rows and", em("(k"), "x", em("p)"), "columns. The first", em("k"), "columns represent the scores attributed by the", em("k"), "raters for the first variable ; the next", em("k"), "columns represent the scores attributed by the", em("k"), "raters for the second variable ; etc. KappaGUI returns Cohen's or Fleiss' kappa for each variable."),
			p("Headers (i.e., column names) are mandatory, cf. the help page of StartKappa() for more details about them."),
			h4("Example"),
			p("An example of valid input file can be downloaded here: http://www.pacea.u-bordeaux.fr/IMG/csv/data_Kappa_Cohen.csv"),
			easyClose=TRUE,
			size="l"
		))
	})

	###########################################
	# 2. Calcul des Kappa de Cohen ou de Fleiss
	observeEvent(input$go_kappa, { # on attend que l'utilisateur ait cliqué sur le bouton "Execute"
		if (exists("dat", envir=myenvg)) { # si un jeu de données a bien été chargé !
			dat <- get("dat", envir=myenvg) # alors on le récupère depuis l'environnement global
			if (input$choiceKappa == "Two raters") { # Kappa de Cohen
				if (ncol(dat) %% 2 != 0) { # on vérifie que le nombre de colonnes du jeu de données est bien un multiple de 2
					showModal(modalDialog(title = "Error", "Invalid datafile: the number of columns is not a multiple of the number of raters.", easyClose = TRUE))
				} else { # le jeu de données est valide
					MatRes <- kappaCohen(dat, weight=input$weightingScheme) # calcul de la table de résultats
					output$texte_resultats <- renderText("Cohen's kappa values for each variable")
					output$resultats <- renderTable(MatRes, rownames=TRUE, digits=3) # on renvoie le tableau de kappas
					output$bouton_telechargement <- renderUI({ downloadButton("telech", "Download the results [CSV file]") }) # ce bouton n'est generé que maintenant, lorsque l'utilisateur a uploadé les donnees et lancé le calcul
					output$telech <- downloadHandler(filename='results_KappaCohen.csv', content=function(file) {write.csv(MatRes, file)}) # la fonction déclenchée par le bouton de téléchargement
				}
			
			} else { # Kappa de Fleiss
				if (ncol(dat) %% input$nb_raters != 0) { # on vérifie que le nombre de colonnes du jeu de données est bien divisible par le nombre de juges
					showModal(modalDialog(title = "Error", "The number of columns is not a multiple of the number of raters. Please check your data, and be sure you selected the correct number of raters.", easyClose = TRUE))
				} else { # le jeu de données est valide
					MatRes <- kappaFleiss(dat, nb_raters=input$nb_raters) # calcul de la table de résultats
					output$texte_resultats <- renderText("Fleiss' kappa values for each variable")
					output$resultats <- renderTable(MatRes, rownames=TRUE, digits=3) # on renvoie le tableau de kappas
					output$bouton_telechargement <- renderUI({ downloadButton("telech", "Download the results [CSV file]") })
					output$telech <- downloadHandler(filename='results_KappaFleiss.csv', content=function(file) {write.csv(MatRes, file)})
				}
			}
		} else { # il n'y a pas de jeu de données chargé !
			showModal(modalDialog(title = "Error", "Please import a dataset first.", easyClose = TRUE))
		}
	})
})
