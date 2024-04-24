#' @title select_precursor
#'
#' @description Select precursors from a XCMSnExp object
#'
#' @param XCMSnExp A XCMSnExp object from XCMS
#' @param method The method for annotate and group features, 'camera' or 'pmd'
#' @param mode Polarity, 'pos' or 'neg'
#' @param path.result Path to output folder
#'
#' @return A data frame object that contains the precusors extracted from the 
#'    XCMSnExp object
#'
#' @export
#' @importFrom dplyr "%>%" mutate select
#' @import CAMERA pmd
#' 
select_precursor <-
  function( XCMSnExp = NULL,
            method = NULL,
            mode = NULL,
            path.result = NULL,
            BPPARAM = NULL
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
        xsa <- 
          XCMSnExp %>% 
          as("xcmsSet") %>% 
          xsAnnotate(polarity = polarity
                     #nSlaves = BPPARAM
                     )
        
        # xsa <- CAMERA::annotate(xset, perfwhm=0.7, cor_eic_th=0.85,
        #                         ppm=10, polarity=polarity)
        # peaklist <- CAMERA::getPeaklist(xsa)
        
        xsaF <- groupFWHM(xsa, perfwhm = 0.6, intval = "into", sigma = 6)
        xsaC <- groupCorr(xsaF,
                          calcIso = FALSE, 
                          calcCiS = FALSE, 
                          calcCaS = TRUE, 
                          #cor_eic_th=0.7,
                          cor_exp_th=0.8,
                          pval= 0.000001, 
                          graphMethod="lpc",
                          intval="into")
        xsaFI <- findIsotopes(xsaC, ppm = 10, mzabs= 0.01,  intval = "into")
        xsaFA <- findAdducts(xsaFI, ppm=10, mzabs=0.01, multiplier=4, polarity=polarity)
        peaklist <- getPeaklist(xsaFA)
        precursor <-
          peaklist %>% 
          mutate(id = row_number(),
                 rt = rt/60,
                 pcgroup = as.numeric(pcgroup)) %>% 
          relocate(c(id, mz, rt, adduct, pcgroup, isotopes), .before = mz)
        
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
          select( id, mz, rt, colnames(mzrt$data))  %>%
          mutate( rt = rt / 60)
      }
      
      saveRDS( precursor,
               file = paste0(path.result, "precursor_", mode, ".rds") )

    }

    return(precursor)

  }
