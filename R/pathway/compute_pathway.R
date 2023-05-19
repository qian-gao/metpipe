compute_pathway <-
  function(
    x = NULL,
    contrast.to.use = NULL,
    path.result = NULL,
    p.thres = NULL
    
  ){
    
    # x = result
    # contrast.to.use = contrast.to.use
    # path.result = getwd()
    # p.thres = 0.5
    
    # pathway.class <- c( "Metabolic;primary_pathway", "Physiological;primary_pathway", 
    #                     "Protein;primary_pathway"
    #                     #"Signaling;primary_pathway", 
    #                     #"Disease;primary_pathway", 
    #                     #"Drug Action;primary_pathway", "Drug Metabolism;primary_pathway"
    # )
    
    library(metpath)
    library(openxlsx)
    #remotes::install_gitlab("tidymass/metpath", dependencies = TRUE)
    
    path.list <- list()
    wb <- createWorkbook()
    
    for (i in 1: length(contrast.to.use)){
      
      cont.i <- contrast.to.use[i]
      
      var.sel <-
        result$ph.tbl.all %>%
        filter(contrast %in% cont.i) %>%
        filter(adj.p.value < p.thres) %>%
        left_join(feature.info, by = c("variable" = "Name")) %>%
        mutate(hmdb = substr(HMDB, 1, 11))
      
      if (nrow(var.sel) > 0){
        
        path.i <-
          enrich_hmdb(query_id = var.sel$hmdb, 
                      query_type = "compound", 
                      id_type = "HMDB",
                      pathway_database = hmdb_pathway,
                      only_primary_pathway = TRUE,
                      p_cutoff = 1, 
                      p_adjust_method = "BH", 
                      threads = 3)
        
        if (!is.null(path.i)){
          
          path.list[[cont.i]] <- path.i
          
          addWorksheet(wb, sheetName = make.names(cont.i) )
          writeData(wb, sheet = make.names(cont.i), path.i@result)
          
          png( file = paste0(path.result, "/pathway_", make.names(cont.i), ".png"), width = 2500, height = 1800, res = 300)
          
            p <-
              enrich_bar_plot(
                object = path.i,
                x_axis = "p_value_adjust",
                cutoff = p.thres,
                top = 10
              ) +
              ggtitle("Enriched pathway")
          
            print(p)
            
          dev.off()
          
        }
      }
      
    }
    
    saveWorkbook(wb, file = paste0( path.result, "/pathway_analysis.xlsx"), overwrite = TRUE)
    
    return(path.list)
    
  }