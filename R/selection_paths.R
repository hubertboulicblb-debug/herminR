# Re-express a fitted model with one formula term per monomial ---------------
#
# rsm's SO()/FO() wrap many coefficients in a single formula term, so terms
# cannot be dropped individually and the model cannot be refitted on
# resampled data from its model frame. This helper rebuilds an equivalent lm
# with explicit terms (x1, I(x1^2), x1:x2, ...) plus the original nuisance
# terms (e.g. blocks), on a data frame of raw variables.

.monomial_label <- function(p) {
  p <- p[order(names(p))]
  paste(vapply(names(p), function(k) if (p[[k]] == 1L) k else sprintf("I(%s^%d)", k, p[[k]]), character(1)),
        collapse = ":")
}

.as_monomial_lm <- function(model) {
  if (!inherits(model, c("lm", "rsm"))) {
    cli::cli_abort("{.arg model} must be an {.cls lm} or {.cls rsm} object.")
  }
  tm <- .term_map(model, max_order = 10L)
  f <- stats::formula(model)
  resp <- f[[2]]
  tt <- stats::terms(model)
  fac_mat <- attr(tt, "factors")
  dc <- attr(tt, "dataClasses")
  labels <- attr(tt, "term.labels")
  is_num <- function(v) !is.na(dc[v]) && (dc[v] == "numeric" || grepl("^nmatrix", dc[v]))
  nuis_labels <- labels[vapply(seq_along(labels), function(j) {
    vars <- rownames(fac_mat)[fac_mat[, j] > 0]
    !all(vapply(vars, is_num, logical(1)))
  }, logical(1))]

  vars <- unique(all.vars(f))
  dd <- .raw_data(model, vars)
  if (is.null(dd)) cli::cli_abort("Cannot recover the data used to fit {.arg model}.")

  mono <- vapply(tm$powers, .monomial_label, character(1))
  rhs <- c(nuis_labels, unname(mono))
  fml <- stats::reformulate(if (length(rhs)) rhs else "1", response = resp)
  environment(fml) <- environment(f)
  fit <- stats::lm(fml, data = dd)
  list(fit = fit, data = dd, response = resp, nuisance = nuis_labels,
       monomials = stats::setNames(unname(mono), names(tm$powers)), powers = tm$powers,
       factors = tm$factors)
}

.refit_terms <- function(base, terms) {
  rhs <- c(base$nuisance, terms)
  fml <- stats::reformulate(if (length(rhs)) rhs else "1", response = base$response)
  stats::lm(fml, data = base$data)
}

# A monomial term may be dropped only if no remaining term contains it
# (well-formed polynomial hierarchy).
.droppable <- function(current, powers_by_label, keep) {
  cand <- setdiff(current, keep)
  cand[vapply(cand, function(t) {
    p <- powers_by_label[[t]]
    !any(vapply(setdiff(current, t), function(o) {
      q <- powers_by_label[[o]]
      all(names(p) %in% names(q)) && all(p <= q[names(p)])
    }, logical(1)))
  }, logical(1))]
}

#' Model-selection path sensitivity for MPI rankings
#'
#' Runs backward elimination along several paths — AIC and partial-F
#' p-value elimination at each `alphas` level — and records each factor's MPI
#' and rank on every path (referee W5).
#'
#' Selection works on individual monomial terms (`x1`, `I(x1^2)`, `x1:x2`),
#' including models fitted with \pkg{rsm}'s `SO()`. With
#' `hierarchy = "well_formed"` (default) a term is only removed when no
#' remaining term contains it (e.g. `x1` stays while `I(x1^2)` or `x1:x2`
#' remains). Nuisance terms (blocks) and `keep` terms are never removed.
#'
#' This is a sensitivity analysis over a small set of plausible selection
#' procedures, not proof of model stability.
#'
#' @param model An `lm` or `rsm` object (coded units).
#' @param alphas Significance levels for p-value elimination paths.
#' @param direction Only `"backward"` is implemented.
#' @param keep Monomial term labels that must be retained, e.g. `"x1"` or
#'   `"I(x2^2)"`.
#' @param criteria Which paths to run: any of `"aic"`, `"pvalue"`.
#' @param hierarchy `"well_formed"` or `"none"`.
#' @param top A factor is "stable" on a path if its rank is `<= top`.
#' @param norm Norm passed to [mpi()].
#' @param seed Unused (selection is deterministic); kept for API stability.
#'
#' @return A tibble of class `herminR_selection` with columns `factor`,
#'   `path`, `alpha`, `estimate` (|MPI|, 0 when all of a factor's terms were
#'   removed), `rank`, `in_model`, `stability`. Attribute `paths` lists the
#'   retained terms and status (including errors) of every path.
#' @export
mpi_selection_paths <- function(model,
                                alphas = c(0.01, 0.05, 0.10),
                                direction = "backward",
                                keep = NULL,
                                criteria = c("aic", "pvalue"),
                                hierarchy = c("well_formed", "none"),
                                top = 3L,
                                norm = "l1",
                                seed = NULL) {
  direction <- rlang::arg_match(direction, "backward")
  hierarchy <- rlang::arg_match(hierarchy)
  criteria <- match.arg(criteria, c("aic", "pvalue"), several.ok = TRUE)
  base <- .as_monomial_lm(model)
  labels <- unname(base$monomials)
  pw <- stats::setNames(base$powers, labels)
  bad_keep <- setdiff(keep, labels)
  if (length(bad_keep)) cli::cli_abort("Unknown {.arg keep} term(s): {.val {bad_keep}}. Available: {.val {labels}}.")
  if (length(base$factors) <= top) {
    cli::cli_warn("Only {length(base$factors)} factor{?s}: with {.arg top} = {top} every factor is trivially 'stable'.")
  }

  run_path <- function(crit, alpha) {
    current <- labels
    fit <- base$fit
    steps <- 0L
    repeat {
      cand <- if (hierarchy == "well_formed") .droppable(current, pw, keep) else setdiff(current, keep)
      if (!length(cand)) break
      d1 <- stats::drop1(fit, scope = cand, test = "F")
      d1 <- d1[rownames(d1) != "<none>", , drop = FALSE]
      if (crit == "aic") {
        j <- which.min(d1$AIC)
        if (!(d1$AIC[j] < stats::extractAIC(fit)[2])) break
      } else {
        pv <- d1[["Pr(>F)"]]
        if (all(is.na(pv))) break
        j <- which.max(pv)
        if (pv[j] <= alpha) break
      }
      current <- setdiff(current, rownames(d1)[j])
      fit <- .refit_terms(base, current)
      steps <- steps + 1L
    }
    list(fit = fit, terms = current, steps = steps)
  }

  specs <- list()
  if ("aic" %in% criteria) specs <- c(specs, list(list(path = "aic", crit = "aic", alpha = NA_real_)))
  if ("pvalue" %in% criteria) {
    specs <- c(specs, lapply(alphas, function(a) list(path = paste0("p", a), crit = "pvalue", alpha = a)))
  }

  path_info <- list()
  rows <- list()
  for (s in specs) {
    r <- tryCatch(run_path(s$crit, s$alpha), error = function(e) e)
    if (inherits(r, "error")) {
      path_info[[s$path]] <- list(status = "failed", message = conditionMessage(r), terms = NULL)
      cli::cli_warn("Selection path {.val {s$path}} failed: {conditionMessage(r)}")
      next
    }
    path_info[[s$path]] <- list(status = "ok", steps = r$steps, terms = r$terms)
    m <- suppressWarnings(mpi(r$fit, norm = norm, abs = TRUE, factors = base$factors))
    present <- vapply(base$factors, function(f) any(vapply(pw[r$terms], function(p) f %in% names(p), logical(1))), logical(1))
    rows[[s$path]] <- tibble::tibble(
      factor = m$factor, path = s$path, alpha = s$alpha, estimate = m$estimate,
      rank = rank(-m$estimate, ties.method = "min"),
      in_model = unname(present[m$factor])
    )
  }
  if (!length(rows)) cli::cli_abort("All selection paths failed.")
  combined <- dplyr::bind_rows(rows)
  stab <- stats::ave(as.numeric(combined$rank <= top), combined$factor, FUN = mean)
  combined$stability <- stab
  class(combined) <- c("herminR_selection", "tbl_df", "tbl", "data.frame")
  attr(combined, "paths") <- path_info
  attr(combined, "top") <- top
  attr(combined, "norm") <- norm
  combined
}

#' @export
plot.herminR_selection <- function(x, ...) {
  df <- tibble::as_tibble(x)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$factor, y = .data$path, fill = .data$estimate)) +
    ggplot2::geom_tile(colour = "grey80") +
    ggplot2::geom_text(ggplot2::aes(label = .data$rank,
                                    colour = .data$estimate > 0.55 * max(.data$estimate, na.rm = TRUE)),
                       size = 3.5, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white")) +
    ggplot2::scale_fill_gradient(low = "white", high = "grey15") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Factor", y = "Selection path",
                  fill = paste0("|", .quantity_label(attr(x, "norm"))$short, "|"),
                  title = paste(.quantity_label(attr(x, "norm"))$short, "across selection paths (labels = rank)"),
                  subtitle = .norm_note(attr(x, "norm")))
}
