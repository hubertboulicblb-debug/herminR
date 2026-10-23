#' @details herminR implements the Maximal Possible Influence (MPI) of Otava
#'   and Mylona (2020) and extends it with uncertainty intervals, rank
#'   stability, alternative norms and feasible-region MPI. Run
#'   `citation("herminR")` for how to cite the package and the method.
#' @references Otava, M. and Mylona, K. (2020). Communicating statistical
#'   conclusions of experiments to scientists. *Quality and Reliability
#'   Engineering International*, 36(8), 2688-2698. \doi{10.1002/qre.2697}
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
## usethis namespace: end
NULL

utils::globalVariables(c("pastrydough", "pastry_table1", "dss_sim"))
