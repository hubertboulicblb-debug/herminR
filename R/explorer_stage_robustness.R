# Stage 4: robustness (selection paths and norm comparison) ----------------------------

#' UI of the robustness stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_robustness_ui <- function() {
  shiny::tabPanel(
    "4 Robustness",
    guide_box_ui(4),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("rob_response", "Response", NULL),
        shiny::conditionalPanel("output.cap_selection",
          shiny::textInput("alphas", "p-value paths (alphas)", "0.01, 0.05, 0.10"),
          shiny::numericInput("top", "Stable if rank <= top", 2, min = 1, step = 1),
          shiny::actionButton("run_sel", "Run selection paths", class = "herminR-run"),
          shiny::uiOutput("sel_last")),
        shiny::conditionalPanel("!output.cap_selection",
          explorer_unavailable_ui("Selection paths re-fit simplified models, so they need raw data (step 1). The norm comparison below works for any source.",
                                  "goto_data_4"))
      ),
      shiny::mainPanel(
        shiny::conditionalPanel("output.cap_selection",
          shiny::h4("Model-selection paths"),
          explorer_plot_output("sel_plot", vh = 42, min = 280, max = 460),
          explorer_details("Terms kept on each path", shiny::tableOutput("sel_terms"))),
        shiny::h4("Norm comparison"),
        explorer_plot_output("norm_plot", vh = 42, min = 280, max = 460)
      )
    )
  )
}

#' Server of the robustness stage
#' @param input,output,session Shiny objects.
#' @param st Shared state.
#' @return A list: `sel_state`, `sel_res`, `sel_params`.
#' @keywords internal
#' @noRd
stage_robustness_server <- function(input, output, session, st) {
  sel_params <- shiny::reactive({
    list(response = input$rob_response, alphas = input$alphas, top = input$top, norm = input$norm)
  })
  sel_state <- explorer_run_on_click(input, session, "run_sel", sel_params, function(p) {
    a <- st$analysis()
    if (!identical(a$kind, "model")) stop("Selection paths need fitted models (choose raw data in step 1).")
    al <- suppressWarnings(as.numeric(strsplit(p$alphas, "[,; ]+")[[1]]))
    if (!length(al) || anyNA(al) || any(al <= 0 | al >= 1)) stop("Alphas must be numbers between 0 and 1, e.g. 0.01, 0.05.")
    shiny::withProgress(message = "Selection paths", value = 0.3,
      suppressWarnings(mpi_selection_paths(a$models[[p$response]], alphas = al, top = p$top, norm = p$norm)))
  })
  sel_res <- shiny::reactive(explorer_run_value(sel_state(), "Press 'Run selection paths'."))
  output$sel_last <- shiny::renderUI(explorer_last_run_ui(
    sel_state(), tryCatch(sel_params(), error = function(e) NULL),
    function(p) sprintf("response %s, alphas %s, top %s", p$response, p$alphas, p$top),
    "Run selection paths"))
  output$sel_plot <- shiny::renderPlot(
    explorer_fit_width(explorer_highlight_tiles(plot(sel_res()), st$focus()) + explorer_theme(), explorer_width(session, "sel_plot")),
    res = 96, alt = "MPI of each factor on each model-selection path; the table lists the terms kept")
  output$sel_terms <- shiny::renderTable({
    p <- attr(sel_res(), "paths")
    data.frame(path = names(p),
               status = vapply(p, `[[`, "", "status"),
               terms_kept = vapply(p, function(x) paste(x$terms, collapse = " "), ""),
               check.names = FALSE)
  })
  output$norm_plot <- shiny::renderPlot({
    a <- st$analysis()
    explorer_fit_width(explorer_highlight_tiles(plot(suppressWarnings(mpi_compare_norms(st$mpi_input(a, shiny::req(input$rob_response))[[1]], abs = TRUE))),
                                                st$focus()) + explorer_theme(), explorer_width(session, "norm_plot"))
  }, res = 96, alt = "Factor influence under the L1, L2 and L-infinity norms")

  list(sel_state = sel_state, sel_res = sel_res, sel_params = sel_params)
}
