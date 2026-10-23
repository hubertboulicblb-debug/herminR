#' Response correlation helper
#'
#' Correlation matrix among responses, kept *aside* from the MPI heatmap as
#' the Discussion of Otava and Mylona (2020) recommends.
#'
#' With a list of models, each response vector is taken from its model frame
#' and observations are **matched by row name** (the original data's row
#' names survive `na.omit`). Only observations used by every model are kept;
#' a message reports how many were dropped. Models fitted to *different* data
#' frames that share default row names (`"1"`, `"2"`, ...) cannot be told
#' apart, so fit all responses to one data frame.
#'
#' @param models_or_data A named list of `lm`/`rsm` objects, or a data frame /
#'   matrix whose columns are the responses.
#' @param method Correlation method passed to [stats::cor()].
#' @param plot If `TRUE`, return a ggplot heatmap instead of the matrix.
#'
#' @return A correlation matrix, or a ggplot object. Attribute `n_obs` is a
#'   matrix of the number of observations used for each pair: for a model
#'   list every entry equals the number of shared observations; for a data
#'   frame, the pairwise-complete counts.
#' @export
response_cor <- function(models_or_data, method = "pearson", plot = FALSE) {
  if (.is_model_list(models_or_data)) {
    models <- models_or_data
    if (is.null(names(models)) || any(names(models) == "")) names(models) <- paste0("Y", seq_along(models))
    resp <- lapply(models, function(m) {
      mf <- stats::model.frame(m)
      stats::setNames(as.numeric(stats::model.response(mf)), rownames(mf))
    })
    common <- Reduce(intersect, lapply(resp, names))
    if (length(common) < 3L) {
      cli::cli_abort("Fewer than 3 observations are shared by all models (matched by row name).")
    }
    dropped <- length(unique(unlist(lapply(resp, names)))) - length(common)
    if (dropped > 0) {
      cli::cli_inform("{dropped} observation{?s} not used by every model {?was/were} excluded.")
    }
    mat <- vapply(resp, function(v) v[common], numeric(length(common)))
    if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(common))
    colnames(mat) <- names(models)
    C <- stats::cor(mat, method = method)
  } else if (is.data.frame(models_or_data) || is.matrix(models_or_data)) {
    mat <- as.matrix(models_or_data)
    C <- stats::cor(mat, use = "pairwise.complete.obs", method = method)
  } else {
    cli::cli_abort("{.arg models_or_data} must be a list of models or a data frame.")
  }
  ok <- !is.na(mat)
  n_pair <- crossprod(ok)
  storage.mode(n_pair) <- "integer"
  attr(C, "n_obs") <- n_pair
  if (!plot) return(C)

  df <- as.data.frame(as.table(C))
  names(df) <- c("response1", "response2", "correlation")
  ggplot2::ggplot(df, ggplot2::aes(x = .data$response1, y = .data$response2,
                                   fill = .data$correlation)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$correlation)), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = NULL, fill = "r", title = "Response correlations")
}
