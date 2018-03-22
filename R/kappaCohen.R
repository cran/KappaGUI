kappaCohen <- function(data, weight="unweighted") {
# data : le dataframe chargé par l'utilisateur dans l'interface graphique
# weight : chaîne de caractères, "unweighted", "equal" ou "squared", correspondant aux paramètres d'entrée de irr::kappa2
# output -> tableau final des valeurs de Kappa pour chaque trait, à afficher dans l'UI

	# 1. Déclaration et initialisation de la structure du tableau de résultats :
	MatRes <- matrix(nrow=ncol(data)/2 , ncol=3)
	rownames(MatRes) <- substr(colnames(data)[seq(from=1, to=ncol(data), by=2)], 1, nchar(colnames(data)[seq(from=1, to=ncol(data), by=2)])-2)
	colnames(MatRes) <- c("Kappa", "Subjects", "p-value")
  
	# 2. Calcul des valeurs de Kappa :
	for (j in 1:nrow(MatRes)) { # pour chaque paire de variables...
		z <- data.frame(data[,2*j], data[,2*j-1]) # ... isoler cette paire dans un dataframe...
		zz <- na.omit(z) # ... enlever les données manquantes
		if (nrow(zz)>1) { # s'il y a encore des données...
			kappacohen <- kappa2(zz, weight=weight) # calcul du Kappa de Cohen
			MatRes[j,1] <- kappacohen$value 
			MatRes[j,2] <- nrow(zz)
			MatRes[j,3] <- kappacohen$p.value
		} else { # cette paire de variables n'a aucune donnée en commun
			MatRes[j,1] <- NA
			MatRes[j,2] <- 0
			MatRes[j,3] <- NA
		}
	}
	
	# 3. Mise en forme définitive et renvoi des résultats :
	MatRes <- as.data.frame(MatRes)
	MatRes[,2] <- as.integer(MatRes[,2]) # les effectifs ne devront pas etre affichés avec des décimales dans le rendu final
	return(MatRes)
}
