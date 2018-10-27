
shinyServer(function(input, output) {
	# finesse
	annee <- reactive({paste0(input$annee[1]:input$annee[2])})
	departement <- reactive({
		if (is.null(input$departement)) {
			res <- c('XX',formatC(1:95, width = 2, flag = '0'), paste0(971:974))
		} else {
			deparsed <- deparseur(input$departement, pre_dep = 'dep')
			if (length(deparsed) > 0) {
				res <- deparsed
			} else {
				res <- c('XX',formatC(1:95, width = 2, flag = '0'), paste0(971:974))
			}
		}
		res
	})
	prenom2 <- reactive({
		deparsed <- deparseur(input$prenom2, pre_dep = 'pre')
		if (length(deparsed) > 0) {
			res <- deparsed
		} else {
			res <- 'ELENA'
		}
	})
	annee2 <- reactive({paste0(input$annee2[1]:input$annee2[2])})
	departement2 <- reactive({
		if (is.null(input$departement2)) {
			res <- c('XX',formatC(1:95, width = 2, flag = '0'), paste0(971:974))
		} else {
			deparsed <- deparseur(input$departement2, pre_dep = 'dep')
			if (length(deparsed) > 0) {
				res <- deparsed
			} else {
				res <- c('XX',formatC(1:95, width = 2, flag = '0'), paste0(971:974))
			}
		}
		return(res)
	})
	graph_anneeR <- reactive({graph_annee(dt = histoire_prenom(dt = donnees_insee, 
																														prenom = prenom2()), 
																			 sexe = 'F', 
																			 departement = departement2())})
	graph_dpartR <- reactive({graph_departement(dt = histoire_prenom(dt = donnees_insee, 
																																	prenom = prenom2()), 
																						 sexe = 'F',
																						 annee = annee2())})
	
	output$table_prenom <- renderDataTable({
		switch (input$finesse,
						'fin' = setorder(donnees_insee[preusuel != '_PRENOMS_RARES' & annais %in% annee() & dpt %in% departement() & sexe == 2,
																					 .(nombre = sum(nombre, na.rm = TRUE)), 
																					 by = .(prenoms = preusuel, annee = annais, departement = dpt)], -nombre)[],
						'annee' = setorder(donnees_insee[preusuel != '_PRENOMS_RARES' & annais %in% annee() & dpt %in% departement() & sexe == 2,
																						 .(nombre = sum(nombre, na.rm = TRUE)), 
																						 by = .(prenoms = preusuel, annee = annais)], -nombre)[],
						'depart' = setorder(donnees_insee[preusuel != '_PRENOMS_RARES' & annais %in% annee() & dpt %in% departement() & sexe == 2,
																							.(nombre = sum(nombre, na.rm = TRUE)), 
																							by = .(prenoms = preusuel, departement = dpt)], -nombre)[],
						'rien' = setorder(donnees_insee[preusuel != '_PRENOMS_RARES' & annais %in% annee() & dpt %in% departement() & sexe == 2,
																						.(nombre = sum(nombre, na.rm = TRUE)), 
																						by = .(prenoms = preusuel)], -nombre)[]
		)})
	output$graph_annee_abs <- renderPlotly({graph_anneeR()$abs})
	output$graph_annee_rel <- renderPlotly({graph_anneeR()$rel})
	output$graph_dpart_abs <- renderPlotly({graph_dpartR()$abs})
	output$graph_dpart_rel <- renderPlotly({graph_dpartR()$rel})
	
})
