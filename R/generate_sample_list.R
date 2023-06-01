### Usage
#Output includes a list and a excel file named "sample_list.xlsx"

# example <- generate_sample_list( "Sample_list_test.xlsx",
#                                   sample.initial = c("sol", "BL", "NIST", "CP", "PO"),
#                                   sample.cycle = c("sol", "BL", "CP", "PO"),
#                                   sample.final = c("sol", "BL", "NIST", "CP", "PO"),
#                                   cycle.size = 8,
#                                   rand.method = "block", # "none" "complete" "block"
#                                   plate.x = as.character(1:9),
#                                   plate.y = c('A', 'B', 'C', 'D', 'E', 'F'),
#                                   plate.nr = 6,
#                                   one.vial = c("sol"),
#                                   plate.separate = c("PO"),
#                                   nr.subj.per.block = 8,
#                                   sample.position.fixed = FALSE
# )
# # Test
# template = "Test_01_01_Sample_list_test.xlsx"
# template = "Test_01_02_Sample_list_fixed_position_test.xlsx"
# sample.initial = c("sol", "BL", "NIST", "CP", "PO")
# sample.cycle = c("sol", "BL", "CP", "PO")
# sample.final = c("sol", "BL", "NIST", "CP", "PO")
# cycle.size = 8
# rand.method = "none" # "none" "complete" "block
# plate.x = as.character(1:9)
# plate.y = c('A', 'B', 'C', 'D', 'E', 'F')
# plate.nr = 6
# one.vial = c("sol")
# plate.separate = c("PO")
# nr.subj.per.block = 4
# sample.position.fixed = FALSE
# number.side = FALSE
# number.reverse = FALSE

#' @title generate_sample_list
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#' @import tidyverse

generate_sample_list <-
  function( template,
            sample.initial = c("sol", "BL", "NIST", "CP", "PO"),  # if null, ""
            sample.cycle = c("sol", "BL", "CP", "PO"),  # if null, ""
            sample.final = c("sol", "BL", "NIST", "CP", "PO"), # if null, ""
            cycle.size = 8,
            rand.method = "none",  # "none" "complete" "block"
            plate.x = as.character(1:9),
            plate.y = c('A', 'B', 'C', 'D', 'E', 'F'),
            plate.nr = 6,
            one.vial = c("sol"),  # if null, NULL
            plate.separate = NULL,  # if null, NULL or ""
            nr.subj.per.block = 0,
            adjust.thres = 0.5, # blocks contains more than such as 50 % of the groups wouldn't get more imbalanced samples
            sample.position.fixed = FALSE,
            number.reverse = FALSE,
            number.side = FALSE
){

  if (!is.data.frame(template)){

    input.list <- openxlsx::read.xlsx(template, sheet = 1)

  } else {

    input.list <- template

  }

  group.var <- names(input.list)
  group.var <- group.var[!group.var %in% c("Name", "Project", "Method", "Date", "Position")]

  if ( length(group.var) == 0 ){
      input.list$Group <- NA
      group.var <- "Group"
  }

  block.initial <- data.frame( Name = sample.initial,
                               Project = NA,
                               Method = NA,
                               Date = NA)

  block.cycle <- data.frame( Name = sample.cycle,
                             Project = NA,
                             Method = NA,
                             Date = NA)

  block.final <- data.frame( Name = sample.final,
                             Project = NA,
                             Method = NA,
                             Date = NA)

  rand.list <- input.list[, !names(input.list) %in% c("Project", "Method", "Date")]
  info.list <- input.list[, c("Project", "Method", "Date")]

  if (rand.method == "none") {

    rand.result <- rand.list
    rand.subject.blocks <- data.frame()

  } else if (rand.method == "complete") {

    set.seed(1)

    rand <- block_random( S = rand.list, #[, "Name", drop = FALSE]
                          group.var = NULL)

    rand.result <- rand$result
    rand.subject.blocks <- rand$subject.blocks

  } else if (rand.method == "block") {

    set.seed(1)
    rand.result <- rand.list

    rand <-
      block_random( S = rand.list,
                    group.var = group.var,
                    nr.subj.per.block = nr.subj.per.block,
                    adjust.thres = adjust.thres)

    rand.result <- rand$result
    rand.subject.blocks <- rand$subject.blocks

  }

  raw.list <- cbind(Name = rand.result$Name, info.list)
  cycle.n <- floor( nrow(raw.list) / cycle.size )

  new.list <- block.initial
  for (i in 1:(cycle.n + 1) ) {

    if (i <= cycle.n) {

      list.i <- raw.list[ c( (cycle.size*(i-1) + 1): (cycle.size*i) ),  ]
      new.list <- rbind(new.list, list.i, block.cycle)

    } else {

      list.i <- raw.list[ c( (cycle.size*(i-1) + 1): nrow(raw.list) ),  ]
      new.list <- rbind(new.list, list.i, block.final)

    }
  }

  new.list <-
    new.list %>%
      left_join(rand.result, by = "Name") %>%
      filter(Name != "")

  sample.list <-
    new.list %>%
    fill(Project, Method, Date, .direction = "downup") %>%
    mutate(Run_N = formatC(row_number(), width=3, flag="0")) %>%
    group_by(Name) %>%
    mutate(count = as.numeric(row_number())) %>%
    ungroup() %>%
    add_plate_position( x = plate.x,
                        y = plate.y,
                        one.vial = one.vial,
                        plate.separate = plate.separate,
                        sample.position.fixed = sample.position.fixed) %>%
    rowwise() %>%
    mutate( bio_rep = case_when( Name %in% one.vial ~ 1,
                                 TRUE               ~ count ),
            tech_rep = case_when( Name %in% one.vial ~ count,
                                  TRUE                       ~ 1 ),
    Vial = paste0("P", (plate - 1) %% plate.nr + 1, "-", y, "-", x),
    Vial_impact = paste0((plate - 1) %% plate.nr + 1, "-", ( match( tolower(y), letters) - 1 )*max(as.numeric(plate.x)) + as.numeric(x)),
    Method_pos = paste0(Method, "p"),
    Method_neg = paste0(Method, "n"))

  sample.list.pos <-
    sample.list %>%
    mutate(Sample_name = paste(Project, Method_pos, Date, Run_N, Name, bio_rep, tech_rep,
                               sep = "_")) %>%
    select(Name, Sample_name, Vial, Vial_impact)

  sample.list.neg <-
    sample.list %>%
    mutate(Sample_name = paste(Project, Method_neg, Date, Run_N, Name, bio_rep, tech_rep,
                               sep = "_")) %>%
    select(Name, Sample_name, Vial, Vial_impact)

  plate.n <- length(unique(sample.list$plate))

  tbls <- data.frame()
  for (i in 1:plate.n) {

    if (number.side){
      plate.i.sample <-
        sample.list[sample.list$plate == i, c("Name", "x", "y")] %>%
        unique() %>%
        spread(y, Name) %>%
        arrange(as.numeric(x))

      plate.i <-
      data.frame(x = plate.x) %>%
      left_join(plate.i.sample, by = "x", suffix = c("", ""))

      plate.nr.i <- (i - 1) %% plate.nr + 1
      col_names <- data.frame(t(c( paste0("TP", i, " / P", plate.nr.i), plate.y )))
      colnames(col_names) <- c("x", plate.y)

    } else {

      plate.i.sample <-
        sample.list[sample.list$plate == i, c("Name", "x", "y")] %>%
        unique() %>%
        spread(x, Name) %>%
        rename(x = y)

      # Sort numbers
      plate.i.colnames <- colnames(plate.i.sample)[-1]
      cols.n <- sort(as.numeric(plate.i.colnames))
      ind <- match(cols.n, as.numeric(plate.i.colnames))
      plate.i.colnames <- plate.i.colnames[ind]
      plate.i.sample <-
        plate.i.sample %>%
        select(x, all_of(plate.i.colnames))


      plate.i <-
        data.frame(x = plate.y) %>%
        left_join(plate.i.sample, by = "x", suffix = c("", ""))

      plate.nr.i <- (i - 1) %% plate.nr + 1
      col_names <- data.frame(t(c( paste0("TP", i, " / P", plate.nr.i), plate.x )))
      colnames(col_names) <- c("x", plate.x)
    }

    if (number.reverse){
      plate.i <-
        plate.i %>%
          arrange(desc(row_number()))
    }

    tbls <- bind_rows(tbls, plate.i) %>%
      plyr::rbind.fill(col_names) %>%
      rbind("") %>%
      replace(is.na(.), "")

  }

  rand.check <-
    sample.list[, "Name", drop = FALSE] %>%
      left_join(rand.result, by = "Name")

  result <- list(sample.list.pos = sample.list.pos,
                 sample.list.neg = sample.list.neg,
                 rack.position = tbls,
                 rand.check = rand.check,
                 rand.subject.blocks = rand.subject.blocks
                )

  # xlsx::write.xlsx( sample.list.pos,
  #                   file = "Sample_list.xlsx",
  #                   sheetName = "Sample_list_POS")
  #
  # xlsx::write.xlsx( sample.list.neg,
  #                   file = "Sample_list.xlsx",
  #                   sheetName = "Sample_list_NEG",
  #                   append = TRUE)
  #
  # xlsx::write.xlsx( as.data.frame(tbls),
  #                   file = "Sample_list.xlsx",
  #                   sheetName = "Rack_position",
  #                   col.names = FALSE,
  #                   row.names = FALSE,
  #                   append = TRUE)

  return(result)
}

#' @title block_random
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
block_random <- function( S,
                          group.var = NULL,
                          nr.subj.per.block = 0,
                          adjust.thres = 0.5){

  raw <- S

  # Create blocks of subjects
  if ("Subject" %in% group.var){

    grp.var <- group.var[group.var != "Subject"]

    if (length(grp.var) > 1){
      raw <-
        raw %>%
          mutate( grp = apply(raw[, grp.var], 1, paste, collapse = "_")
          )
    } else {
      raw <-
        raw %>%
        mutate( grp = grp.var )
    }

    # Split into groups
    subject.grps <-
      lapply( unique(raw[, "grp"]),
              function(x){ grp.subj <- unique(raw[raw[, "grp"] == x, "Subject"]);
                           grp.subj[sample(length(grp.subj))]}
      )

    # Create blocks of subjects
    n <- max(lengths(subject.grps))
    m <- length(subject.grps)

    if (nr.subj.per.block == 0){

      nr.subj.per.block <- m

    } else {

      rem <- nr.subj.per.block %% m == 0

      if (rem == FALSE) {

        text <- paste0("Number of the subject per unit should be the multiple of ", m)
        stop(text)

      }
    }

    subject.blocks <-
      base::Reduce(rbind, lapply(1:n,
                                 function(i){base::Reduce(rbind, lapply(1:m,
                                                                        function(j){
                                                                          if (i <= length(subject.grps[[j]]))
                                                                            subject.grps[[j]][i]
                                                                          else{
                                                                            z <- subject.grps[[j]][1]
                                                                            z[1] <- NA
                                                                            z[1]
                                                                          }
                                                                        }))}))

    suppressWarnings(length(subject.blocks) <- prod(dim(matrix( subject.blocks,
                                               ncol = nr.subj.per.block))) )

    subject.blocks <- matrix( subject.blocks,
                              ncol = nr.subj.per.block,
                              byrow = TRUE)

    # Adjust for unbalanced design

    block.length <- apply(subject.blocks, 1, function(x){ length(x[!is.na(x)]) })

    block.long <- subject.blocks[block.length > nr.subj.per.block*adjust.thres, ]
    block.long.n <- nrow(block.long)

    block.short <- subject.blocks[block.length <= nr.subj.per.block*adjust.thres, , drop = FALSE]
    block.short.length <- block.length[block.length <= nr.subj.per.block*adjust.thres]

    block.short <- block.short[sort(block.short.length, index.return = TRUE)$ix, , drop = FALSE]

    block.short.grouped <-
      lapply(split(block.short, (seq_along(1:nrow(block.short)) - 1) %% block.long.n + 1),
             function(x){ data.frame(matrix(x, nrow = 1)) } )

    block.short.combined <-
      data.table::rbindlist(block.short.grouped, fill = TRUE)

    nrow.diff <- block.long.n - nrow(block.short.combined)
    if (nrow.diff > 0){
      fill.lines <- data.frame(matrix(nrow = nrow.diff, ncol = ncol(block.short.combined)))
      block.short.combined <- rbind(block.short.combined, fill.lines)
    }

    subject.blocks <- cbind(block.long, block.short.combined)

  } else{

    raw$Subject <- rep("S0", nrow(raw))

    subject.blocks <- matrix(rep("S0", nrow(raw)), nrow = 1)

  }


  # Randomize within each block

  rand.result <- data.frame()
  for (i in 1: nrow(subject.blocks)){

    rand.i <- raw[raw$Subject %in% subject.blocks[i, ], ]

    grp.var <- group.var[group.var != "Subject"]

    if (length(grp.var) > 1){
      rand.i <-
        rand.i %>%
        mutate( grp = apply(rand.i[, grp.var], 1, paste, collapse = "_")
        )
    } else if (length(grp.var) == 1){
      rand.i <-
        rand.i %>%
        mutate( grp = grp.var )
    } else {
      rand.i <-
        rand.i %>%
        mutate( grp = "None" )
    }

    # Split into groups
    rand.grps <-
      lapply( unique(rand.i[, "grp"]),
              function(x){ grp.subj <- unique(rand.i[rand.i[, "grp"] == x, ]);
              grp.subj[sample(nrow(grp.subj)), ]}
      )

    # Randomization
    n <- max( sapply(rand.grps,nrow) )
    m <- length(rand.grps)

    rand.blocks <-
      base::Reduce(rbind, lapply(1:n,
                                 function(i){base::Reduce(rbind, lapply(1:m,
                                                                        function(j){
                                                                          if (i <= nrow(rand.grps[[j]]))
                                                                            rand.grps[[j]][i, ]
                                                                          else{
                                                                            z <- rand.grps[[j]][1, ]
                                                                            z[1, ] <- NA
                                                                            z[1, ]
                                                                          }
                                                                        }))[sample(m), ]}))




    rand.result <- rbind(rand.result, rand.blocks)

  }

  result <- list()
  result$result <-
    rand.result %>%
      filter(!is.na(Name))

  result$subject.blocks <- subject.blocks

  return(result)
}

#' @title add_plate_position
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
add_plate_position <- function(S,
                               x = as.character(1:9),
                               y = c('A', 'B', 'C', 'D', 'E', 'F'),
                               plate = 1:10000,
                               one.vial = c("sol"),
                               plate.separate = NULL,
                               sample.position.fixed = FALSE){

  stopifnot(is.data.frame(S))

  plate.position <- length(x) * length(y)
  one.vial.n <- length(one.vial)

  if (plate.position * length(plate) < nrow(S)){
    stop("Not enough plates!")
  }

  if (sample.position.fixed == TRUE & !"Position" %in% names(S)){
    stop("Fixed position need to be provided!")
  }

  if (sample.position.fixed == TRUE){

    S.others <-
      S[ !S$Name %in% c(one.vial, plate.separate), ] %>%
        filter(is.na(Position))

  } else {

    S.others <-
      S[ !S$Name %in% c(one.vial, plate.separate), ]

  }

  S.others <-
    S.others %>%
    mutate( run = row_number() + one.vial.n*(floor( (row_number() - 1) / (plate.position - one.vial.n)) + 1) )

  plate.n <- floor(max(S.others$run) / plate.position) + 1

  # one.vial only shows up once in each plate, take the first in each plate
  S.one.vial <-
    S[ S$Name %in% one.vial, ] %>%
    group_by(Name) %>%
    filter(row_number() == 1) %>%
    slice(rep(1:n(), each = plate.n)) %>%
    mutate( run = match(Name, one.vial) + (row_number() - 1)*plate.position ) %>%
    ungroup()

  S.all <-
    rbind(S.one.vial, S.others) %>%
    arrange(run)

  # plate.separate are arranged together in single plate, take out to put in the end
  S.separate <-
    S[S$Name %in% plate.separate, ] %>%
    mutate(run = row_number())

  # Add plate position
  S.all$x <- x[((S.all$run - 1) %% length(x)) + 1]
  S.all$y <- y[(floor((S.all$run - 1) / length(x)) %% length(y))  + 1]
  S.all$plate <- plate[floor((S.all$run - 1 ) / plate.position) + 1]

  # Add plate position for plate.separate
  S.separate$x <- x[((S.separate$run - 1) %% length(x)) + 1]
  S.separate$y <- y[(floor((S.separate$run - 1) / length(x)) %% length(y))  + 1]
  S.separate$plate <- plate[floor((S.separate$run - 1 ) / plate.position) + 1 + max(S.all$plate)]
  S.separate <-
    S.separate %>%
      select(-run)

  # Put one.vial, plate.separate into the main list
  S.others.position <-
    S.all[ !S.all$Name %in% one.vial, ] %>%
    select(-run)

  S.one.vial <-
    S[ S$Name %in% one.vial, ]

  S.one.vial.position <-
    S.all[ S.all$Name %in% one.vial, ]

  S.position <-
    bind_rows(S.one.vial, S.others.position) %>%
    arrange(Run_N) %>%
    fill(plate, .direction = "downup") %>%
    left_join(S.one.vial.position[, c("Name", "x", "y", "plate")], by = c("Name", "plate"), suffix = c("", ".2")) %>%
    mutate( x = if_else( is.na(x), x.2, x),
            y = if_else( is.na(y), y.2, y)) %>%
    select(-x.2, -y.2) %>%
    bind_rows(S.separate) %>%
    arrange(Run_N)

  if (sample.position.fixed == TRUE){

    plate.max <- max(S.position$plate)

    S.fixed <-
      S[!is.na(S$Position), ] %>%
        rowwise() %>%
        mutate( x = strsplit(Position, "-")[[1]][3],
                y = strsplit(Position, "-")[[1]][2],
                plate = as.numeric(
                          gsub( "[^0-9]", "",
                                strsplit(Position, "-")[[1]][1])
                        ) + plate.max )

    S.position <-
      bind_rows(S.fixed, S.position) %>%
      arrange(Run_N)

  }

  return(S.position)
}
