#' @title normalize_with_best_internal_standard
#'
#' @description Normalize each feature using the most correlated internal standard.
#'
#' @param x Feature intensity matrix/data frame (sample x feature).
#' @param istds Internal-standard matrix/data frame (sample x IS features).
#' @param batch Batch vector aligned with samples.
#' @param batch.wise Logical; perform selection independently per batch.
#' @param cor.method Correlation method used in IS selection.
#' @param type Optional sample-type vector.
#' @param use.type Optional sample type to restrict IS selection.
#' @param verbose Logical; print progress information.
#'
#' @return A list with normalized matrix (`x`), selected IS map (`best.istd`),
#' and logical matrix of normalized cells (`is.normalized`).
#' @examples
#' \dontrun{
#' out <- normalize_with_best_internal_standard(x, istds, batch = rep("B1", nrow(x)))
#' }
#' @export
#'
normalize_with_best_internal_standard <-
  function(
    x,
    istds,
    batch,
    batch.wise = TRUE,
    cor.method = "pearson",
    type = NULL,
    use.type = NULL,
    verbose = FALSE
  ) {

    if (!is.data.frame(x) && !is.matrix(x)) {
      stop("x must be a data.frame or matrix")
    }
    if (!is.data.frame(istds) && !is.matrix(istds)) {
      stop("istds must be a data.frame or matrix")
    }
    if (nrow(x) != nrow(istds)) {
      stop("x and istds must have the same number of rows")
    }
    if (length(batch) != nrow(x)) {
      stop("batch must have the same length as nrow(x)")
    }

    if ( verbose ) {

      print( "normalize_with_best_internal_standard was created by Tommi Suvitaival" )
      print( "tommi.raimo.leo.suvitaival@regionh.dk" )
      print( "2019-07-10" )

    }

    y <- x

    # TODO: Check that samples match in x and istds.

    if ( !batch.wise ) {

      batch = rep( x = "No batches", times = nrow( x ) )

    }

    batches = unique( batch )

    best.istd.mat <-
      array( dim =
               c(
                 ncol( x ),
                 length( batches )
               )
      )

    colnames( best.istd.mat ) <- batches

    rownames( best.istd.mat ) <- colnames( x )

    use <- rep( x = TRUE, times = nrow( x ) )

    if ( !is.null( use.type ) ) {

      if ( is.null( type ) ) {

        print( "WARNING: Cannot use samples only -- type not provided" )

      } else {

        use <- ( type == use.type )

      }

    }

    is.normalized <- y
    is.normalized[] <- FALSE

    for ( j in seq_along( batches ) ) {

      idx.j <- which( batch == batches[ j ] )

      cor.j <-
        cor( x = x[ idx.j[ which( use[ idx.j ] ) ], ],
             y = istds[ idx.j[ which( use[ idx.j ] ) ], ],
             use = "pairwise.complete.obs",
             method = cor.method )

      # cor.j <-
      #   cor( x = x[ idx.j, ],
      #        y = istds[ idx.j, ],
      #        use = "pairwise.complete.obs",
      #        method = cor.method )

      cor.j[ which( cor.j <= 0 ) ] <- NA

      best.istd <-
        apply(
          X = cor.j,
          MAR = 1,
          FUN =
            function( x ) {
              ifelse(
                test = all( is.na( x ) ),
                yes = NA,
                no = which.max( x )
              )
            }
        )

      best.istd <- colnames( istds )[ best.istd ]

      best.istd.mat[ , batches[ j ] ] <- best.istd

      names.istds.j <- base::unique( best.istd[ !is.na( best.istd ) ] )

      if ( length( names.istds.j ) > 0 ) {

        for ( i in seq_along( names.istds.j ) ) { # Go through selected istds.

          istd.i <- unlist( istds[ idx.j, names.istds.j[ i ] ] )

          mean.istd.i <- mean( x = istd.i, na.rm = TRUE )

          if ( is.na( mean.istd.i ) ) {

            mean.istd.i <- 1

          }

          istd.i[ is.na( istd.i ) ] <- mean.istd.i

          idx.peaks.i <- which( best.istd == names.istds.j[ i ] )

          normalizer.i <-
            matrix(
              data = istd.i,
              nrow = length( idx.j ),
              ncol = length( idx.peaks.i ),
              byrow = FALSE
            )

          y[ idx.j, idx.peaks.i ] <-
            y[ idx.j, idx.peaks.i ] /
            normalizer.i *
            mean.istd.i

          is.normalized[ idx.j, idx.peaks.i ] <- TRUE

        }

      }

    }

    output <-
      list(
        x = y,
        best.istd = best.istd.mat,
        is.normalized = is.normalized
      )

    return( output )

  }
