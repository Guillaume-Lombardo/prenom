library(dplyr)
library(data.table)
library(ggplot2)
library(rgeos)
library(ggmap)
library(maptools)


donnees_insee <- rio::import('./data/dpt2017.txt', encoding = 'UTF-8', setclass = 'data.table') 
map_dep <- rgdal::readOGR("./data/departements-20140306-50m-shp/departements-20140306-50m.shp")
map_dep <- fortify (map_dep, region="code_insee")

annee2int <- function(x) {
	if (length(x) == 1) {
		if (x == 'XXXX') {
			return(1800)
		} else {
			return(as.integer(x))
		}
	}
	x[x == 'XXXX'] <- '1800'
	return(as.integer(x))
}

proportionneur <- function(dt = donnees_insee, sexe = 'F', annee = NULL, departement = NULL){
	sexeN <- 1L * (toupper(sexe) == 'F')
	if (is.null(annee)) {
		filtre_annee <- sort(unique(dt$annais))
	} else {
		filtre_annee <- annee
	}
	if (is.null(departement)) {
		filtre_departement <- sort(unique(dt$dpt))
	} else {
		filtre_departement <- departement
	}
	dts <- dt[sexe > sexeN & annais %in% filtre_annee & dpt %in% filtre_departement,.(total = 1.0 * sum(nombre, na.rm = TRUE)), by = .(annais, dpt)]
	dts_annee <- dt[sexe > sexeN & annais %in% filtre_annee & dpt %in% filtre_departement,.(total = 1.0 * sum(nombre, na.rm = TRUE)), by = .(annais)]
	dts_dpart <- dt[sexe > sexeN & annais %in% filtre_annee & dpt %in% filtre_departement,.(total = 1.0 * sum(nombre, na.rm = TRUE)), by = .(dpt)]
	return(list(total = dts,
							annee = dts_annee,
							dptot = dts_dpart))
}

prop_defaut <- proportionneur(dt = donnees_insee, sexe = 'F')

selecteur <- function(dt = donnees_insee, sexe = 'F', annee = NULL, deparatement = NULL) {
	sexeN <- 1L * (toupper(sexe) == 'F')
	if (is.null(annee)) {
		filtre_annee <- sort(unique(dt$annais))
	} else {
		filtre_annee <- annee
	}
	if (is.null(deparatement)) {
		filtre_departement <- sort(unique(dt$dpt))
	} else {
		filtre_departement <- departement
	}
	
	dts <- dt[sexe > sexeN & annais %in% filtre_annee & dpt %in% filtre_departement & annais != 'XXXX' & dpt != 'XX']
	dts_annee <- dts[,.(nombre = sum(nombre, na.rm = TRUE)), by = .(preusuel, annais)]
	dts_departement <- dts[,.(nombre = sum(nombre, na.rm = TRUE)), by = .(preusuel, departement)]
	return(list(total = dts,
							annee = dts_annee,
							dptot = dts_departement))
}

histoire_prenom <- function(dt = donnees_insee, prenom = 'ELENA') {
	return(dt[preusuel %in% prenom & annais != 'XXXX' & dpt != 'XX'])
}

graph_annee <- function(dt = histoire_prenom(), sexe = 'F', departement = NULL) {
	sexeN <- 1L * (toupper(sexe) == 'F')
	titre <- paste0(unique(dt$preusuel), collapse = ', ')
	if (!is.null(departement)) {
		donnees <- dt[dpt %in% departement & annais != 'XXXX' & dpt != 'XX', .(nombre = sum(nombre,na.rm = TRUE)), by = .(sexe, annais)]
		prop <- proportionneur(donnees_insee, sexe, departement = departement)$annee
	} else {
		donnees <- dt[annais != 'XXXX' & dpt != 'XX', .(nombre = sum(nombre,na.rm = TRUE)), by = .(sexe, annais)]
		prop <- prop_defaut$annee
	}
	# donnees$Sexe <- factor(x = c('Garçon','Fille'))[donnees$sexe]
	donnees <- merge(x = donnees, y = prop, by = c('annais'), all.x = TRUE)[, ratio := (1.0 * nombre) / total ]
	gg_abs <- ggplot(data = donnees, aes(x = annee2int(annais), y = nombre)) +
		geom_bar(stat = 'identity') +
		xlab('annee') +
		ylab('nombre de naissances') +
		ggtitle(titre)
	gg_rel <- ggplot(data = donnees, aes(x = annee2int(annais), y = ratio)) +
		geom_bar(stat = 'identity') +
		xlab('annee') +
		ylab('part des naissances') +
		ggtitle(titre)
	return(list(abs = gg_abs,
							rel = gg_rel))
}

graph_departement <- function(dt = histoire_prenom(), annee = NULL, sexe = 'F', map = map_dep) {
	if (toupper(sexe) != 'F') {.l <- 0} else {.l <- 1}
	if (!is.null(annee)) {
		donnees <- dt[annais %in% annee & dpt != 'XX' & annais != 'XXXX' & sexe >.l, .(nombre = sum(nombre,na.rm = TRUE)), by = .(dpt)]
		prop <- proportionneur(donnees_insee, sexe, annee = annee)$dptot
	} else {
		donnees <- dt[dpt != 'XX' & annais != 'XXXX' & sexe >.l, .(nombre = sum(nombre,na.rm = TRUE)), by = .(dpt)]
		prop <- prop_defaut$dptot
	}
	# donnees$Sexe <- factor(x = c('Garçon','Fille'))[donnees$sexe]
	# browser()
	donnees <- merge(x = donnees, y = prop, by = c('dpt'), all.x = TRUE)[, ratio := nombre / total ]
	titre <- paste0(paste0(unique(dt$preusuel), collapse = ', '), ifelse(.l == 1, ' : Filles', ' : Garçons et filles'))
	gg_abs <- ggplot(data = donnees) +
		geom_map(aes(map_id = dpt, fill = nombre),
						 map = map) +
		coord_map() +
		expand_limits(x=map$long[nchar(map$id)<3], 
									y=map$lat[nchar(map$id)<3]) +
		scale_fill_gradient(name="Nombre", low="LightYellow", high="Firebrick") +
		theme_void() +
		theme(legend.title = element_blank())+
		ggtitle(titre)
	gg_rel <- ggplot(data = donnees) +
		geom_map(aes(map_id = dpt, fill = ratio),
						 map = map) +
		coord_map() +
		expand_limits(x=map$long[nchar(map$id)<3], 
									y=map$lat[nchar(map$id)<3]) +
		scale_fill_gradient(name="Nombre", low="LightYellow", high="Firebrick") +
		theme_void() +
		theme(legend.title = element_blank())+
		ggtitle(titre)
	return(list(abs = gg_abs,
							rel = gg_rel))
}

graph_annee()
graph_departement()
graph_annee(departement = paste0(c(75,77,78,92,93,94,95)))
graph_departement(annee = paste0(1980:2017))
graph_annee(dt = histoire_prenom(prenom = c('ELENA','HELENE', 'HELENA')))
graph_departement(dt = histoire_prenom(prenom = c('ELENA','HELENE', 'HELENA')),annee = paste0(1980:2017))
