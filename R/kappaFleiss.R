kappaFleiss <- function(data, nb_raters=3) {
# data : le dataframe chargé par l'utilisateur dans l'interface graphique
# nb_raters : integer, récupéré depuis l'UI
# output -> tableau final des valeurs de Kappa pour chaque trait, à afficher dans l'UI

	# 1. Déclaration et initialisation de la structure du tableau de résultats :
	MatRes <- matrix(nrow=ncol(data)/nb_raters, ncol=2)
	rownames(MatRes) <- substr(colnames(data)[seq(from=1, to=ncol(data), by=nb_raters)], 1, nchar(colnames(data)[seq(from=1, to=ncol(data), by=nb_raters)])-2)
	colnames(MatRes) = c("Kappa", "Subjects")
	MatRes <- as.data.frame(MatRes)
  
	# 2. Calcul des valeurs de Kappa :
	MatRes[,2] <- as.integer(MatRes[,2]) # les effectifs ne devront pas être affichés avec des décimales dans le rendu final
	for (j in 1:nrow(MatRes)) {
		MatRes[j,1] = kappam.fleiss(data.frame(data[ , (1:nb_raters)+(j-1)*nb_raters]))$value
		MatRes[j,2] = nrow(na.omit(data.frame(data[ , (1:nb_raters)+(j-1)*nb_raters])))
	} 
	
	# 3. Renvoi des résultats :
	return(MatRes)
}
