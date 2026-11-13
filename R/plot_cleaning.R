#' @title parse_filter_rule
#'
#' @description Parse a mean/RSD filter rule such as `c("1", "PO", "<", "0.4")`
#'   (rules read from YAML are character vectors).
#'
#' @param rule A length-4 vector: multiplier, sample type, operator, and either a
#'   numeric threshold or a second sample type.
#'
#' @return A list with `k`, `lhs`, `op`, `rhs`, `rhs_num` (NA when `rhs` is a
#'   sample type), and `text` (the rule as written).
#' @keywords internal
parse_filter_rule <- function(rule) {
  rule <- as.character(unlist(rule))
  if (length(rule) != 4) {
    stop("Filter rule must have 4 elements: multiplier, type, operator, threshold/type")
  }
  list(
    k = as.numeric(rule[1]),
    lhs = rule[2],
    op = rule[3],
    rhs = rule[4],
    rhs_num = suppressWarnings(as.numeric(rule[4])),
    text = paste0(rule[1], "*", rule[2], " ", rule[3], " ", rule[4])
  )
}

#' @title plot_cleaning_steps
#'
#' @description Build ggplot figures that show what each cleaning filter did to
#'   one ionization mode. All sample types, multipliers, operators and
#'   thresholds are read from the filter rules recorded by [clean_peaktable()],
#'   so the figures follow whatever `mean.filter`, `rsd.filter`,
#'   `dilution.filter`, `rt.range` and `filter_by_missing_*` were configured.
#'
#'   Each per-step figure shows only the features that reached that step
#'   (filters run in order), coloured by whether the step removed them, for
#'   all features and per `Feature_type` (Known / Unknown). Mean, RSD and
#'   dilution steps are violin plots with jittered features; the threshold is
#'   drawn as a dotted line.
#'
#' @param mode_data A cleaned mode-level datalist (e.g. `datalist$pos`)
#'   containing `clean_trace`, or the `clean_trace` list itself.
#' @param mode Unused; kept so existing calls keep working. Plot titles do not
#'   include the mode (the report shows it in the tab).
#' @param bins Number of histogram bins.
#'
#' @return A named list of ggplot objects: `funnel`, then one per configured
#'   step (`mean_<i>`, `rsd_<i>`, `dilution`, `rt`, `missing`), then
#'   `rsd_before_after_<i>` for each RSD rule. Attribute `"tooltip"` gives the
#'   recommended `ggplotly()` tooltip per plot; see [cleaning_plotly()].
#' @examples
#' \dontrun{
#' plots <- plot_cleaning_steps(datalist$pos, mode = "pos")
#' cleaning_plotly(plots)
#' }
#' @export
#' @import ggplot2
plot_cleaning_steps <- function(mode_data, mode = NULL, bins = 60) {

  ct <- if (!is.null(mode_data$clean_trace)) mode_data$clean_trace else mode_data
  if (is.null(ct$features) || is.null(ct$steps)) {
    stop("mode_data must contain clean_trace; run clean_peaktable() first")
  }

  tr <- as.data.frame(ct$features, stringsAsFactors = FALSE)
  steps <- ct$steps
  step_ids <- vapply(steps, function(s) s$id, character(1))
  step_labels <- vapply(steps, function(s) s$label, character(1))
  removed_idx <- match(tr$removed_at, step_ids)

  col_status <- c("Passed" = "#4682B4", "Removed" = "#B47846")
  col_before_after <- c("Before cleaning" = "#9A9A9A", "After cleaning" = "#4682B4")

  hover <- function(d, value_label, value) {
    paste0(d$Identity,
           "<br>m/z: ", round(d$mz, 4),
           "<br>RT: ", round(d$rt, 2),
           "<br>", value_label, ": ", signif(value, 3),
           "<br>", if (is.null(d$Feature_type)) "" else paste0(d$Feature_type, ", "), d$step_status)
  }

  # Features that reached step i, with status relative to that step
  entering <- function(i) {
    keep <- is.na(removed_idx) | removed_idx >= i
    d <- tr[keep, , drop = FALSE]
    d$step_status <- factor(ifelse(!is.na(removed_idx[keep]) & removed_idx[keep] == i,
                                   "Removed", "Passed"),
                            levels = c("Passed", "Removed"))
    d
  }

  # Feature groups shown side by side: All, then Known/Unknown (and any other
  # non-IS Feature_type present). Older traces without Feature_type: All only.
  feature_types <- if ("Feature_type" %in% colnames(tr)) {
    ft <- unique(tr$Feature_type[!is.na(tr$Feature_type)])
    c(intersect(c("Known", "Unknown"), ft), sort(setdiff(ft, c("Known", "Unknown"))))
  } else {
    character(0)
  }
  groups <- c("All", feature_types)
  col_groups <- stats::setNames(
    c("#5F6B77", "#3B75AF", "#E39B2D", rep("#9A9A9A", max(0, length(groups) - 3)))[seq_along(groups)],
    groups
  )

  # Stack the data once per group (All + each Feature_type)
  expand_groups <- function(d) {
    out <- d
    out$group <- "All"
    if (length(feature_types) > 0) {
      sub <- d[!is.na(d$Feature_type), , drop = FALSE]
      sub$group <- sub$Feature_type
      out <- rbind(out, sub)
    }
    out$group <- factor(out$group, levels = groups)
    out
  }

  # Facet labels carrying per-group "removed of entered" counts
  add_step_panels <- function(d) {
    d <- expand_groups(d)
    n <- tapply(rep(1, nrow(d)), d$group, sum)
    rem <- tapply(d$step_status == "Removed", d$group, sum)
    n[is.na(n)] <- 0
    rem[is.na(rem)] <- 0
    lab <- paste0(groups, ": ", rem[groups], " of ", n[groups], " removed")
    d$panel <- factor(lab[match(as.character(d$group), groups)], levels = lab)
    d
  }

  facet_groups <- function(scales = "fixed") {
    facet_wrap(~panel, nrow = 1, scales = scales)
  }

  # Squish extreme values into the upper edge so the threshold stays readable
  clip_upper <- function(v, threshold = NA_real_) {
    v_ok <- v[is.finite(v)]
    if (length(v_ok) == 0) return(list(v = v, upper = NA_real_, n = 0))
    upper <- max(stats::quantile(v_ok, 0.99, names = FALSE),
                 if (is.finite(threshold)) threshold * 1.5 else -Inf)
    n <- sum(v_ok > upper)
    list(v = pmin(v, upper), upper = upper, n = n)
  }

  hist_plot <- function(d, value, xlab, title, threshold = NULL, caption = NULL) {
    d$value <- value
    d <- d[is.finite(d$value), , drop = FALSE]
    # Discrete-valued metrics (e.g. correlations over few dilution levels,
    # missing % over few samples): one bar per value instead of gappy bins
    # Bin over the data and the threshold together, so a threshold far from
    # the data does not shrink the bars to hairlines
    rng <- if (nrow(d) > 0) range(c(d$value, threshold[is.finite(threshold)])) else c(0, 0)
    u <- sort(unique(round(d$value, 10)))
    discrete <- length(u) > 1 && length(u) <= bins &&
      min(diff(u)) >= diff(rng) / bins
    hist_layer <- if (discrete) {
      geom_histogram(binwidth = min(diff(u)), center = u[1], position = "stack", colour = NA)
    } else {
      if (diff(rng) > 0) {
        geom_histogram(binwidth = diff(rng) / bins, boundary = rng[1],
                       position = "stack", colour = NA)
      } else {
        geom_histogram(bins = bins, position = "stack", colour = NA)
      }
    }
    d <- add_step_panels(d)
    p <- ggplot(d, aes(x = value, fill = step_status)) +
      hist_layer +
      facet_groups(scales = "free_y") +
      scale_fill_manual(values = col_status, name = NULL, drop = FALSE) +
      labs(x = xlab, y = "Number of features", title = title, caption = caption) +
      theme_bw()
    for (t in threshold) {
      if (is.finite(t)) p <- p + geom_vline(xintercept = t, linetype = "dotted", linewidth = 0.8)
    }
    p
  }

  # Violin per feature group (All / Known / Unknown on the x axis) with jittered
  # features coloured by whether this step removed them; threshold as a
  # horizontal dotted line
  violin_plot <- function(d, value, ylab, title, threshold = NA_real_,
                          subtitle = NULL, caption = NULL, text = NULL) {
    d$value <- value
    d$text <- text
    d <- d[is.finite(d$value), , drop = FALSE]
    d <- add_step_panels(d)
    n_panel <- stats::ave(rep(1, nrow(d)), d$panel, FUN = sum)
    # `text` (plotly hover) is a plot-wide aesthetic; the violin layer does not
    # inherit it, otherwise every feature would become its own violin group
    p <- ggplot(d, aes(x = panel, y = value, text = text)) +
      geom_violin(data = d[n_panel >= 2, , drop = FALSE], aes(x = panel, y = value),
                  inherit.aes = FALSE,
                  fill = NA, colour = "grey55", scale = "width", width = 0.8) +
      geom_jitter(aes(colour = step_status),
                  position = position_jitter(width = 0.25, height = 0, seed = 1),
                  size = 0.7, alpha = 0.5) +
      scale_colour_manual(values = col_status, name = NULL, drop = FALSE) +
      guides(colour = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
      labs(x = NULL, y = ylab, title = title, subtitle = subtitle, caption = caption) +
      theme_bw()
    if (is.finite(threshold)) {
      p <- p + geom_hline(yintercept = threshold, linetype = "dotted", linewidth = 0.8)
    }
    p
  }

  out <- list()
  tooltip <- list()

  ## Funnel -------------------------------------------------------------------
  funnel <- do.call(rbind, lapply(groups, function(g) {
    in_g <- if (g == "All") rep(TRUE, nrow(tr)) else tr$Feature_type %in% g
    ri <- removed_idx[in_g]
    data.frame(
      step = c("Detected", step_labels),
      group = g,
      remaining = c(sum(in_g), vapply(seq_along(steps), function(i) {
        sum(is.na(ri) | ri > i)
      }, numeric(1))),
      removed = c(0, vapply(seq_along(steps), function(i) sum(ri == i, na.rm = TRUE),
                            numeric(1))),
      stringsAsFactors = FALSE
    )
  }))
  funnel$step <- factor(funnel$step, levels = c("Detected", step_labels))
  funnel$group <- factor(funnel$group, levels = groups)
  funnel$text <- paste0(funnel$step, " (", funnel$group, ")",
                        "<br>Remaining: ", funnel$remaining,
                        "<br>Removed at this step: ", funnel$removed)
  dodge <- position_dodge(width = 0.85)
  out$funnel <-
    ggplot(funnel, aes(x = step, y = remaining, fill = group, text = text)) +
    geom_col(position = dodge, width = 0.8) +
    geom_text(aes(label = remaining), position = dodge, vjust = -0.3, size = 3) +
    scale_fill_manual(values = col_groups, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Features remaining",
         title = "Features remaining after each cleaning step",
         subtitle = "Internal standards are filtered separately and not counted") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  tooltip$funnel <- "text"

  ## Per-step plots -------------------------------------------------------------
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    d <- entering(i)

    if (s$kind %in% c("mean", "rsd")) {
      r <- parse_filter_rule(s$rule)
      lhs_col <- make.names(paste0(s$kind, ".", r$lhs))
      if (!lhs_col %in% colnames(d)) {
        warning("Column ", lhs_col, " not found in clean_trace; skipping plot for ", s$label)
        next
      }

      if (s$kind == "mean" && !is.na(r$rhs_num)) {
        # Mean of one type against a constant: k*mean op t  ->  mean op t/k
        v <- d[[lhs_col]]
        n_nonpos <- sum(!is.finite(v) | v <= 0)
        thr <- r$rhs_num / r$k
        out[[s$id]] <- violin_plot(
          d, value = ifelse(is.finite(v) & v > 0, log10(v), NA),
          ylab = paste0("log10 mean intensity in ", r$lhs),
          title = paste0("Mean intensity filter (keep ", r$text, ")"),
          threshold = if (thr > 0) log10(thr) else NA_real_,
          caption = if (n_nonpos > 0) paste0(n_nonpos, " features with zero/missing mean not shown") else NULL,
          text = hover(d, paste0("mean ", r$lhs), v)
        )
        tooltip[[s$id]] <- "text"

      } else if (s$kind == "mean") {
        # Mean of one type against another: k*mean(A) op mean(B)
        rhs_col <- make.names(paste0("mean.", r$rhs))
        if (!rhs_col %in% colnames(d)) {
          warning("Column ", rhs_col, " not found in clean_trace; skipping plot for ", s$label)
          next
        }
        # One value per feature: log10(mean B / mean A); the rule k*A < B is
        # the same as log10(B/A) > log10(k), so the threshold is log10(k).
        a <- d[[lhs_col]]
        b <- d[[rhs_col]]
        a_ok <- is.finite(a) & a > 0
        b_ok <- is.finite(b) & b > 0
        ratio <- ifelse(a_ok & b_ok, log10(b / a), NA_real_)
        fin <- ratio[is.finite(ratio)]
        hi <- if (length(fin)) max(fin, log10(r$k)) + 0.5 else log10(r$k) + 1
        lo <- if (length(fin)) min(fin, log10(r$k)) - 0.5 else log10(r$k) - 1
        n_no_a <- sum(!a_ok & b_ok)
        n_no_b <- sum(!b_ok)
        ratio[!a_ok & b_ok] <- hi  # not detected in A (e.g. absent from blanks)
        ratio[!b_ok] <- lo         # no signal in B
        text <- paste0(d$Identity,
                       "<br>m/z: ", round(d$mz, 4),
                       "<br>RT: ", round(d$rt, 2),
                       "<br>mean ", r$lhs, ": ", signif(a, 3),
                       "<br>mean ", r$rhs, ": ", signif(b, 3),
                       "<br>", if (is.null(d$Feature_type)) "" else paste0(d$Feature_type, ", "),
                       d$step_status)
        notes <- c(
          if (n_no_a > 0) paste0(n_no_a, " features not detected in ", r$lhs, " shown at the top"),
          if (n_no_b > 0) paste0(n_no_b, " features without ", r$rhs, " signal shown at the bottom")
        )
        out[[s$id]] <- violin_plot(
          d, value = ratio,
          ylab = paste0("log10(mean ", r$rhs, " / mean ", r$lhs, ")"),
          title = paste0("Mean intensity filter (keep ", r$text, ")"),
          threshold = log10(r$k),
          subtitle = paste0("dotted line: ", r$rhs, " = ", r$k, " x ", r$lhs),
          caption = if (length(notes)) paste(notes, collapse = "; ") else NULL,
          text = text
        )
        tooltip[[s$id]] <- "text"

      } else {
        # RSD filter (threshold or type-vs-type)
        v <- d[[lhs_col]]
        thr <- if (!is.na(r$rhs_num)) r$rhs_num / r$k else NA_real_
        cl <- clip_upper(v, thr)
        out[[s$id]] <- violin_plot(
          d, value = cl$v,
          ylab = paste0("RSD in ", r$lhs),
          title = paste0("RSD filter (keep ", r$text, ")"),
          threshold = thr,
          caption = if (cl$n > 0) paste0(cl$n, " features with RSD > ", signif(cl$upper, 3),
                                         " shown at the upper edge") else NULL,
          text = hover(d, paste0("RSD in ", r$lhs), v)
        )
        tooltip[[s$id]] <- "text"
      }

    } else if (s$kind == "dilution") {
      out$dilution <- violin_plot(
        d, value = d$dilution.cor,
        ylab = paste0(s$rule$method, " correlation with dilution level"),
        title = paste0("Dilution-series filter (keep cor >= ", s$rule$min.cor, ")"),
        threshold = s$rule$min.cor,
        subtitle = paste0("Dilution levels: ", paste(s$rule$dilution.types, collapse = ", ")),
        caption = if (any(is.na(d$dilution.cor)))
          paste0(sum(is.na(d$dilution.cor)), " features without enough data for a correlation (removed) not shown") else NULL,
        text = hover(d, "correlation", d$dilution.cor)
      )
      tooltip$dilution <- "text"

    } else if (s$kind == "rt") {
      d$text <- hover(d, "RT", d$rt)
      d <- add_step_panels(d)
      out$rt <-
        ggplot(d, aes(x = rt, y = mz, colour = step_status, text = text)) +
        geom_point(alpha = 0.5, size = 1.2) +
        geom_vline(xintercept = s$rule, linetype = "dotted", linewidth = 0.8) +
        facet_groups() +
        scale_colour_manual(values = col_status, name = NULL, drop = FALSE) +
        labs(x = "Retention time (min)", y = "m/z",
             title = paste0("Retention time filter (keep ", s$rule[1], " - ", s$rule[2], " min)")) +
        theme_bw()
      tooltip$rt <- "text"

    } else if (s$kind == "missing") {
      out$missing <- hist_plot(
        d, value = d$missing.pct,
        xlab = paste0("Missing values (%) in ", paste(s$rule$sample.type, collapse = ", ")),
        title = paste0("Missing-value filter (keep < ", s$rule$threshold, "% missing)"),
        threshold = as.numeric(s$rule$threshold)
      )
      tooltip$missing <- c("x", "count", "fill")
    }
  }

  ## RSD before vs after --------------------------------------------------------
  rsd_steps <- which(vapply(steps, function(s) s$kind == "rsd", logical(1)))
  for (j in seq_along(rsd_steps)) {
    s <- steps[[rsd_steps[j]]]
    r <- parse_filter_rule(s$rule)
    col <- make.names(paste0("rsd.", r$lhs))
    if (!col %in% colnames(tr)) next
    thr <- if (!is.na(r$rhs_num)) r$rhs_num / r$k else NA_real_

    cl <- clip_upper(tr[[col]], thr)
    base <- tr
    base$rsd <- pmin(tr[[col]], cl$upper)
    before <- base
    before$set <- "Before cleaning"
    after <- base[is.na(base$removed_at), , drop = FALSE]
    after$set <- if (nrow(after) > 0) "After cleaning" else character(0)
    d <- expand_groups(rbind(before, after))
    d$set <- factor(d$set, levels = c("Before cleaning", "After cleaning"))
    d <- d[is.finite(d$rsd), , drop = FALSE]

    # Panel labels as in the other plots: features removed by cleaning (any
    # step) out of all detected features in the group
    lab <- vapply(groups, function(g) {
      in_g <- if (g == "All") rep(TRUE, nrow(tr)) else tr$Feature_type %in% g
      paste0(g, ": ", sum(in_g & !is.na(tr$removed_at)), " of ", sum(in_g), " removed")
    }, character(1))
    d$panel <- factor(lab[match(as.character(d$group), groups)], levels = lab)
    # A density needs at least two points per curve
    n_curve <- stats::ave(rep(1, nrow(d)), d$panel, d$set, FUN = sum)
    d <- d[n_curve >= 2, , drop = FALSE]

    id <- paste0("rsd_before_after_", j)
    p <- ggplot(d, aes(x = rsd, fill = set, colour = set)) +
      geom_density(alpha = 0.35) +
      facet_groups() +
      scale_fill_manual(values = col_before_after, name = NULL, drop = FALSE) +
      scale_colour_manual(values = col_before_after, name = NULL, drop = FALSE) +
      labs(x = paste0("RSD in ", r$lhs), y = "Density",
           title = paste0("RSD in ", r$lhs, " before and after cleaning"),
           caption = if (cl$n > 0) paste0("RSD values > ", signif(cl$upper, 3), " squished to the upper edge") else NULL) +
      theme_bw()
    if (is.finite(thr)) p <- p + geom_vline(xintercept = thr, linetype = "dotted", linewidth = 0.8)
    out[[id]] <- p
    tooltip[[id]] <- c("x", "fill")
  }

  attr(out, "tooltip") <- tooltip
  out
}

#' @title cleaning_plotly
#'
#' @description Convert the plots from [plot_cleaning_steps()] to interactive
#'   plotly widgets, using a suitable tooltip for each plot.
#'
#' @param plots The list returned by [plot_cleaning_steps()].
#'
#' @return An `htmltools::tagList` of plotly widgets, which renders in
#'   R Markdown / Quarto when it is the value of a chunk.
#' @export
#' @import plotly htmltools
cleaning_plotly <- function(plots) {
  tooltip <- attr(plots, "tooltip")
  widgets <- lapply(names(plots), function(nm) {
    p <- plots[[nm]]
    tt <- if (!is.null(tooltip[[nm]])) tooltip[[nm]] else "all"

    # ggplotly() drops subtitle and caption; fold them into the title instead
    labs_p <- if (exists("get_labs", envir = asNamespace("ggplot2"), inherits = FALSE)) {
      ggplot2::get_labs(p)
    } else {
      p$labels
    }
    notes <- c(labs_p$subtitle, labs_p$caption)
    notes <- notes[!vapply(notes, is.null, logical(1))]
    if (length(notes) > 0) {
      p <- p + ggplot2::labs(
        title = paste0(labs_p$title, "<br><sup>", paste(unlist(notes), collapse = "; "), "</sup>"),
        subtitle = NULL, caption = NULL
      )
    }

    plotly::layout(plotly::ggplotly(p, tooltip = tt), margin = list(t = 80))
  })
  do.call(htmltools::tagList, widgets)
}
