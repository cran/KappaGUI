library(irr)

shinyServer(function(input, output) {

	myenvg = new.env() # environnement priv\'e au package ; contiendra le jeu de donnees (vu comme une variable globale)
	
	output$helpBox <- renderUI({ # la definition du texte qui doit s'afficher *si l'utilisateur clique sur "Help"*
 		if (input$help_me %% 2 == 1){ # le bouton "Help" a initialement une valeur 0, incrémentée à chaque clic. L'aide doit donc s'afficher si la valeur est un nb impair, et être masquée sinon.
   			helpText("The dataset should be a CSV or TXT file.", br(), "If there are", em("p"), "variables observed by", em("k"), "raters on", em("n"), "individuals, the input file should have", em("n"), "rows and", em("(k"), "x", em("p)"), "columns. The first", em("k"), "columns represent the scores attributed by the", em("k"), "raters for the first variable ; the next", em("k"), "columns represent the scores attributed by the", em("k"), "raters for the second variable ; etc. KappaGUI returns Cohen's or Fleiss' kappa for each variable.", br(), "Missing values are allowed and must be indicated by an empty cell.")
  		} else {
    			return() # si la valeur du bouton "Help" est paire, on masque le texte d'aide
 		}
	})

	###########################################################
	# 1. Charger le fichier de donnees fourni par l'utilisateur
	observeEvent(input$load_dataset, { # on attend que l'utilisateur ait cliqué sur le bouton "Load dataset"
		if (! is.null(input$file$datapath)) { # si l'utilisateur a bien choisi un fichier sur son disque dur
			dat <- read.table(input$file$datapath, sep=as.character(input$fieldSep), na.strings="", header=input$checkboxHeaders) # on charge le fichier
			if (input$checkboxRownames) { # si l'utilisateur a fourni des noms d'individus
				if (!any(duplicated(dat[,1]))) { # et qu'il n'y a pas de duplicats parmi eux
					rownames(dat) <- dat[,1]
					dat[,1] <- NULL
				} else { # il y a des duplicats parmi les noms d'individus
					showModal(modalDialog(title = "Warning", "There are duplicates in row names. Row names will be ignored, but please check your data.", easyClose = TRUE))
					dat[,1] <- NULL
				}
			}
			output$resume <- renderTable(head(dat)) # on renvoie les premieres lignes du tableau de donnees charg\'e par l'utilisateur
			output$texte_resume <- renderText("Your dataset has been loaded!")
			output$texte_conseil <- renderText("Here are the first six lines.")
			assign("dat", dat, envir=myenvg) # on place le jeu de donnees dans un environnement global pour qu'il puisse etre reutilis\'e dans la fonction qui suit
			} else { # l'utilisateur a oubli\'e de choisir un fichier
			showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
		}
    	})

	###########################################
	# 2. Calcul des Kappa de Cohen ou de Fleiss
	observeEvent(input$go_kappa, { # on attend que l'utilisateur ait cliqué sur le bouton "Execute"
		if (exists("dat", envir=myenvg)) { # si un jeu de donnees a bien ete charg\'e !
			dat <- get("dat", envir=myenvg) # alors on  le charge
			if (input$choiceKappa == "Two raters") { # Kappa de Cohen
				if (ncol(dat) %% 2 != 0) { # on verifie que le nb de colonnes du jeu de donnees est bien divisible par 2
					showModal(modalDialog(title = "Error", "Invalid datafile: the number of columns is not a multiple of the number of raters.", easyClose = TRUE))
				} else { # le jeu de donnees est valide
					MatRes <- matrix(nrow=ncol(dat)/2 , ncol=3)
					rownames(MatRes) <- substr(colnames(dat)[seq(from=1, to=ncol(dat), by=2)], 1, nchar(colnames(dat)[seq(from=1, to=ncol(dat), by=2)])-2)
					colnames(MatRes) <- c("Kappa", "Subjects", "p-value")
  
					for (j in 1:nrow(MatRes)) { # pour chaque paire de variables...
						z <- data.frame(dat[,2*j], dat[,2*j-1]) # ... isoler cette paire dans un dataframe...
						zz <- na.omit(z) # ... enlever les donnees manquantes
						if (nrow(zz)>1) { # s'il y a encore des donnees...
							ponderation <- switch(input$weightingScheme, # on renomme, pour etre conforme avec les arguments de irr::kappa2
							"Unweighted kappa" = "unweighted",
							"Linear weighting" = "equal",
							"Quadratic weighting" = "squared")
							kappacohen <- kappa2(zz, weight=ponderation) # calcul du Kappa de Cohen
							MatRes[j,1] <- kappacohen$value 
							MatRes[j,2] <- nrow(zz)
							MatRes[j,3] <- kappacohen$p.value
						} else { # cette paire de variables n'a aucune donnee en commun
							MatRes[j,1] <- NA
							MatRes[j,2] <- 0
							MatRes[j,3] <- NA
						}
					}
					MatRes <- as.data.frame(MatRes)
					MatRes[,2] <- as.integer(MatRes[,2]) # les effectifs ne devront pas etre affich\'es avec des decimales dans le rendu final
					output$texte_resultats <- renderText("Cohen's kappa values for each variable")
					output$resultats <- renderTable(MatRes, rownames=TRUE, digits=3) # on renvoie le tableau de kappas
					output$bouton_telechargement <- renderUI({ downloadButton("telech", "Download the results [CSV file]") }) # ce bouton n'est gener\'e que maintenant, lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul
					output$telech <- downloadHandler(filename='results_KappaCohen.csv', content=function(file) {write.csv(MatRes, file)}) # la fonction declenchee par le bouton de telechargement
				}
			
			} else { # Kappa de Fleiss
				if (ncol(dat) %% input$nb_raters != 0) { # on verifie que le nb de colonnes du jeu de donnees est bien divisible par le nombre de juges
					showModal(modalDialog(title = "Error", "The number of columns is not a multiple of the number of raters. Please check your data, and be sure you selected the correct number of raters.", easyClose = TRUE))
				} else { # le jeu de donnees est valide
					MatRes <- matrix(nrow=ncol(dat)/input$nb_raters, ncol=2)
					rownames(MatRes) <- substr(colnames(dat)[seq(from=1, to=ncol(dat), by=input$nb_raters)], 1, nchar(colnames(dat)[seq(from=1, to=ncol(dat), by=input$nb_raters)])-2)
					colnames(MatRes) = c("Kappa", "Subjects")
					MatRes <- as.data.frame(MatRes)
					MatRes[,2] <- as.integer(MatRes[,2]) # les effectifs ne devront pas etre affich\'es avec des decimales dans le rendu final
					for (j in 1:nrow(MatRes)) {
						MatRes[j,1] = kappam.fleiss(data.frame(dat[ , (1:input$nb_raters)+(j-1)*input$nb_raters]))$value
						MatRes[j,2] = nrow(na.omit(data.frame(dat[ , (1:input$nb_raters)+(j-1)*input$nb_raters])))
					} 
					output$texte_resultats <- renderText("Fleiss' kappa values for each variable")
					output$resultats <- renderTable(MatRes, rownames=TRUE, digits=3) # on renvoie le tableau de kappas
					output$bouton_telechargement <- renderUI({ downloadButton("telech", "Download the results [CSV file]") })
					output$telech <- downloadHandler(filename='results_KappaFleiss.csv', content=function(file) {write.csv(MatRes, file)})
				}
			}
		} else { # il n'y a pas de jeu de donnees charg\'e !
			showModal(modalDialog(title = "Error", "Please import a dataset first.", easyClose = TRUE))
		}
	})
})
