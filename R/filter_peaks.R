#' @title filter_peaks
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param XCMSnExp = NULL,
#' @param peaktable = NULL,
#' @param sample.type = NULL,
#' @param bl.thres = NULL,
#' @param rsd.po.thres = NULL,
#' @param rt.range = NULL,
#' @param mean.thres = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export

filter_peaks <-
  function( XCMSnExp = NULL,
            peaktable = NULL,
            sample.type = NULL,
            bl.thres = NULL,
            rsd.po.thres = NULL,
            rt.range = NULL,
            mean.thres = NULL
  ){
    
    if ( !is.null(XCMSnExp) ){
      
      mzrt <- 
        XCMSnExp_mzrt( XCMSnExp = XCMSnExp, 
                       mzdigit = 4, 
                       rtdigit = 1, 
                       method = "medret", 
                       value = "into")
      
      data <- t(mzrt$data) %>%
        as.data.frame()
      
      rt.range.s <- rt.range*60
      
    } else if ( !is.null(peaktable) ){
      
      mzrt <-
        list( peaktable = peaktable,
              rt = peaktable$rt )
      
      data <-
        t( peaktable[, !colnames(peaktable) %in% c("mz", "rt", "Identity")] ) %>%
        as.data.frame()
      
      colnames(data) <- mzrt$peaktable$Identity
      rt.range.s <- rt.range
      
    }
    
    rsd <-
      calculate_rsd( data = data, 
                     type = sample.type,
                     impute = TRUE)
    
    mzrt <- c(mzrt, rsd)
    index.list <- list()
    
    # Filter based on BL and RSD
    if (!is.null(bl.thres)){
      
      index <-
        apply( mzrt$type.mean,
               MAR = 1,
               function(x){ eval( parse( text = paste0( bl.thres[1], "*", x[bl.thres[2]], bl.thres[3], x[bl.thres[4]]) ))
               })
      
      index.list$index.bl.thres <- index
      
      mzrt <-
        mzrt_filter( mzrt = mzrt, 
                     index = index)
      
      print( paste0( "Only keep features having mean intensity ", 
                     bl.thres[1], "*", bl.thres[2], " ", bl.thres[3], " ", bl.thres[4],
                     ": ", sum(!index), " features have been removed" ), 
             quote = FALSE)
      
    }
    
    if (!is.null(rsd.po.thres)){
      
      index <-
        apply( mzrt$type.rsd,
               MAR = 1,
               function(x){ eval( parse( text = paste0( x[rsd.po.thres[1]], rsd.po.thres[2], rsd.po.thres[3], "/100") ))
               })
      
      index.list$index.rsd.po.thres <- index
      
      mzrt <-
        mzrt_filter( mzrt = mzrt, 
                     index = index)
      
      print( paste0( "Only keep features having RSD in ", 
                     rsd.po.thres[1], " ", rsd.po.thres[2], " ", rsd.po.thres[3],
                     ": ", sum(!index), " features have been removed" ),
             quote = FALSE)
      
    }
    
    if (!is.null(rt.range)){
      
      index <-
        mzrt$rt > rt.range.s[1] & mzrt$rt < rt.range.s[2]
      
      index.list$index.rt.range <- index
      
      mzrt <-
        mzrt_filter( mzrt = mzrt, 
                     index = index)
      
      print( paste0( "Only keep features having retention time ",
                     rt.range[1], " - ", rt.range[2], " min",
                     ": ", sum(!index), " features have been removed" ), 
             quote = FALSE)
      
    }
    
    
    if (!is.null(mean.thres)){
      
      for (i in 1:length(mean.thres)){
        
        m.thres <- mean.thres[[i]]
        
        index <-
          apply( mzrt$type.mean,
                 MAR = 1,
                 function(x){ eval( parse( text = paste0( x[m.thres[1]], m.thres[2], m.thres[3], "*", x[m.thres[4]]) ))
                 })
        
        index.list$mean.thres[[i]] <- c(index)
        
        mzrt <-
          mzrt_filter( mzrt = mzrt,
                       index = index)
        
        print( paste0( "Only keep features having mean intensity ", 
                       mean.thres[[i]][1], " ", mean.thres[[i]][2], " ", mean.thres[[i]][3], "*", mean.thres[[i]][4],
                       ": ", sum(!index), " features have been removed" ), 
               quote = FALSE)
        
      }
      
    }
    
    mzrt$index.list <- index.list
    
    tm <- mzrt$type.mean
    colnames(tm) <- paste0( "mean.", colnames(tm))
    tsd <- mzrt$type.sd
    colnames(tsd) <- paste0( "sd.", colnames(tsd))
    trsd <- mzrt$type.rsd
    colnames(trsd) <- paste0( "rsd.", colnames(trsd)) 
    
    summary <- 
      data.frame(tm, trsd[, -1], tsd[, -1])
    
    mzrt$summary <- summary
    
    return(mzrt)
    
  }

mzrt_filter <-
  function( mzrt = NULL,
            index = NULL){
    
    obj.names <- names(mzrt)
    obj.names <- obj.names[ !grepl( "sample.info", obj.names ) ]
    
    for ( i in 1:length(obj.names) ){
      
      name <- obj.names[i]
      
      if ( !is.null(dim(mzrt[[name]])) ) mzrt[[name]] <- mzrt[[name]][index, ]
      else mzrt[[name]] <- mzrt[[name]][index]
      
      
    }
    
    return(mzrt)
    
  }