#' @title select_precursor
#'
#' @description Select precursors from a XCMSnExp object
#'
#' @param XCMSnExp A XCMSnExp object from XCMS
#' @param method The method for annotate and group features, 'camera' or 'pmd'
#' @param mode Polarity, 'pos' or 'neg'
#' @param remove_frag_addu Indicator for removing the fragments and adducts. TRUE or FALSE
#' @param path.result Path to output folder
#'
#' @return A data frame object that contains the precusors extracted from the 
#'    XCMSnExp object
#'
#' @export
#' @importFrom dplyr "%>%" mutate select
#' @impot CAMERA pmd

select_precursor <-
  function( XCMSnExp = NULL,
            method = NULL,
            mode = NULL,
            remove_frag_addu = FALSE,
            path.result = NULL
  ){

    if ( file.exists(paste0(path.result, "precursor_", mode, ".rds")) ){

      precursor <-
        readRDS( paste0(path.result, "precursor_", mode, ".rds"))

    } else {

      if (mode == "pos"){
        polarity = "positive"
      } else if (mode == "neg"){
        polarity = "negative"
      } 
      
      if (method == "camera"){
        xset <- as(XCMSnExp, 'xcmsSet')
        xsa <- CAMERA::annotate(srmnxset, perfwhm=0.7, cor_exp_th=0.85,
                                ppm=10, polarity=polarity)
        peaklist <- CAMERA::getPeaklist(xsa)
        
        # xsa <- xsAnnotate(xset)
        # xsaF <- groupFWHM(xsa, perfwhm = 0.6)
        # xsaC <- groupCorr(xsaF, cor_eic_th = 0.8)
        # xsaFI <- findIsotopes(xsaC)
        # xsaFA <- findAdducts(xsaFI, polarity = polarity)
        # peaklist <- getPeaklist(xsaFA)
        precursor2 <-
          peaklist[grepl("[M+H]+", peaklist$adduct, fixed = TRUE), ]
        
      } else if (method == "pmd") {
        mzrt <-
          XCMSnExp_mzrt( XCMSnExp = XCMSnExp,
                         mzdigit = 4,
                         rtdigit = 1,
                         method = "medret",
                         value = "into")
        
        ### PMD
        pmd <- pmd::globalstd( mzrt,
                               sda = F,
                               ng = NULL)
        
        pmd.cluster <- pmd::getcluster( pmd,
                                        corcutoff = 0.9)
        
        precursor <-
          cbind.data.frame( mz = pmd.cluster$mz[ pmd.cluster$stdmassindex2 ],
                            rt = pmd.cluster$rt[ pmd.cluster$stdmassindex2 ],
                            mzrt$data[ pmd.cluster$stdmassindex2, ]) %>%
          mutate( id = row_number()) %>%
          select( id, rt, mz, colnames(mzrt$data))  %>%
          mutate( rt = rt / 60)
      }
      
      saveRDS( precursor,
               file = paste0(path.result, "precursor_", mode, ".rds") )

    }

    return(precursor)

  }
