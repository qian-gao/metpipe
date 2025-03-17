#' @title filter_peaks
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param XCMSnExp A XCMSnExp object. If null, peaktable should be mandatory
#' @param peaktable A dataframe of peaktable, feature x sample. If null, XCMSnExp
#'    should be mandatory
#' @param sample.type A vector indicating sample types
#' @param bl.thres Threshold for keeping features based on their values in one
#'    type of sample compared to blanks. e.g. c( 3, "BL", "<", "PO")
#' @param rsd.po.thres Threshold for keeping features based on their rsd in pools.
#'    e.g. c( "PO", "<", 40)
#' @param mean.po.thres Threshold for keeping features based on their mean in pools.
#'    e.g. c( "PO", ">", 1000)
#' @param rt.range Retention time range for keeping features
#' @param mean.thres Threshold for keeping features based on their mean values in
#'    certain types. e.g. list( c( "PO100", ">", 1, "PO25"),
#'                              c( "PO25", ">", 1, "PO12.5") )
#'
#' @return A data frame object that contains filtered peaktable and summary statistics
#' @examples
#'
#' @export

filter_peaks <-
  function( XCMSnExp = NULL,
            peaktable = NULL,
            sample.type = NULL,
            #bl.thres = NULL,
            rsd.filter = NULL,
            #mean.po.thres = NULL,
            rt.range = NULL,
            mean.filter = NULL
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
    # if (!is.null(mean.filter)){
    #
    #   index <-
    #     apply( mzrt$type.mean,
    #            MAR = 1,
    #            function(x){ eval( parse( text = paste0( mean.filter[1], "*", x[mean.filter[2]], mean.filter[3], x[mean.filter[4]]) ))
    #            })
    #
    #   index.list$index.mean.filter <- index
    #
    #   mzrt <-
    #     mzrt_filter( mzrt = mzrt,
    #                  index = index)
    #
    #   print( paste0( "Only keep features having mean intensity ",
    #                  mean.filter[1], "*", mean.filter[2], " ", mean.filter[3], " ", mean.filter[4],
    #                  ": ", sum(!index), " features have been removed" ),
    #          quote = FALSE)
    #
    # }

    if (!is.null(mean.filter)){

      for (i in 1:ifelse(is.list(mean.filter), length(mean.filter), 1)){

        if (i == 1 & !is.list(mean.filter)){
          m.thres <- mean.filter
        } else {
          m.thres <- mean.filter[[i]]
        }

        if (!is.na(as.numeric(m.thres[4]))){
          index <-
            apply( mzrt$type.mean,
                   MAR = 1,
                   function(x){
                       eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], m.thres[4]) ))
                   })
          } else {
            index <-
              apply( mzrt$type.mean,
                     MAR = 1,
                     function(x){
                       eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], x[m.thres[4]]) ))
                     })            
          }

        index[is.na(index)] <- FALSE
        index.list$mean.filter[[i]] <- c(index)

        mzrt <-
          mzrt_filter( mzrt = mzrt,
                       index = index)

        print( paste0( "Only keep features having mean intensity ",
                       m.thres[1], "*", m.thres[2], " ", m.thres[3], " ", m.thres[4],
                       ": ", sum(!index), " features have been removed" ),
               quote = FALSE)

      }

    }

    if (!is.null(rsd.filter)){

      for (i in 1:ifelse(is.list(rsd.filter), length(rsd.filter), 1)){

        if (i == 1 & !is.list(rsd.filter)){
          m.thres <- rsd.filter
        } else {
          m.thres <- rsd.filter[[i]]
        }

        if (!is.na(as.numeric(m.thres[4]))){
          index <-
            apply( mzrt$type.rsd,
                   MAR = 1,
                   function(x){
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], m.thres[4]) ))
                   })
        } else {
          index <-
            apply( mzrt$type.rsd,
                   MAR = 1,
                   function(x){
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], x[m.thres[4]]) ))
                   })          
        }

        index[is.na(index)] <- FALSE
        index.list$rsd.filter[[i]] <- c(index)

        mzrt <-
          mzrt_filter( mzrt = mzrt,
                       index = index)

        print( paste0( "Only keep features having RSD in ",
                       m.thres[1], "*", m.thres[2], " ", m.thres[3], " ", m.thres[4],
                       ": ", sum(!index), " features have been removed" ),
               quote = FALSE)

      }

    }

    # if (!is.null(mean.po.thres)){
    #
    #   index <-
    #     apply( mzrt$type.mean,
    #            MAR = 1,
    #            function(x){ eval( parse( text = paste0( x[mean.po.thres[1]], mean.po.thres[2], mean.po.thres[3]) ))
    #            })
    #
    #   index.list$index.mean.po.thres <- index
    #
    #   mzrt <-
    #     mzrt_filter( mzrt = mzrt,
    #                  index = index)
    #
    #   print( paste0( "Only keep features having mean in ",
    #                  mean.po.thres[1], " ", mean.po.thres[2], " ", mean.po.thres[3],
    #                  ": ", sum(!index), " features have been removed" ),
    #          quote = FALSE)
    #
    # }

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

#' @title mzrt_filter
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param mzrt A list of dataframe or vectors containing peaktable, feature.info
#' @param index Index of features to remove
#'
#' @return A list of filtered dataframe or vectors
#' @examples
#'
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
