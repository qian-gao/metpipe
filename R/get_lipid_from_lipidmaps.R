#' @title get_lipid_from_lipidmaps
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param lipids = NULL,
#' @param type = "abbrev" # "abbrev" "abbrev_chains"
#' @param keep.all = FALSE # keep all records for isomers
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples

#' @export
#' @importFrom dplyr "%>%" mutate
#' @import httr


get_lipid_from_lipidmaps <-
  function( lipids = NULL,
            type = "abbrev", # "abbrev" "abbrev_chains"
            keep.all = FALSE
  ){

    df <-
      data.frame(lipid = lipids) %>%
      mutate( query = gsub(" ", "(", lipid),
              query = paste0(query, ")") )

    temp <- paste0("www.lipidmaps.org/rest/compound/", type, "/")

    info <-
      apply(df, 1, function(x){

        all <- httr::content(httr::GET(url = paste0(temp, x[["query"]], "/all")))

        if ("input" %in% names(all)) {

          df.i <-
            as.data.frame(all) %>%
            mutate(isomer_label = FALSE)

        } else {

          if (keep.all){

            df.i <-
              data.table::rbindlist(all, fill = TRUE) %>%
              mutate(isomer_label = TRUE)

          } else {

            df.i <-
              data.table::rbindlist(all, fill = TRUE)[1, ]

            df.i$isomer_label = TRUE

          }

        }

        return(df.i)

      })

    result <- data.table::rbindlist(info, fill = TRUE)

    output <-
      df %>%
      left_join(result, by = c("query" = "input"))

    lip.info <-
      output %>%
      select( abbrev, pubchem_cid, hmdb_id, formula, exactmass, kegg_id,
              smiles, inchi_key, core, main_class, sub_class,
              isomer_label, abbrev_chains, query, lipid) %>%
      mutate( Compound.name = ifelse(is.na(abbrev), lipid, abbrev)) %>%
      rename( PC.CID = pubchem_cid,
              HMDB.ID = hmdb_id,
              Molecular.formula = formula,
              Monoisotopic.mass = exactmass,
              KEGG.ID = kegg_id,
              Smiles = smiles,
              Inchikey = inchi_key) %>%
      select(Compound.name, PC.CID, HMDB.ID, Molecular.formula, Monoisotopic.mass, KEGG.ID,
             Smiles, Inchikey, core, main_class, sub_class, isomer_label, abbrev_chains, query, lipid)

    return(lip.info)

  }
