# Stage 2: influence map (MPI) ---------------------------------------------------------

#' UI of the MPI stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_mpi_ui <- function() {
  shiny::tabPanel(
    "2 MPI explorer",
    guide_box_ui(2),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("norm", "Aggregation", c("L1 = MPI (attainable change)" = "l1",
                                                     "L2 score" = "l2", "L-infinity score" = "linf")),
        shiny::radioButtons("signed", "Values", c("Signed (direction)" = "signed", "Absolute" = "abs"),
                            selected = "abs"),
        shiny::selectInput("std", "Standardisation", c(
          "None" = "none", "Eq. (5): per response max" = "colmax_response",
          "Per factor max" = "colmax_factor", "Residual SD (fitted models)" = "residual_sd",
          "Rank within response" = "rank")),
        shiny::radioButtons("palette", "Palette", c("ermine (B/W)" = "ermine", "colour" = "colour"), inline = TRUE),
        shiny::checkboxInput("transpose", "Paper layout (responses on x)", TRUE),
        shiny::checkboxInput("annotate", "Show values", TRUE),
        shiny::checkboxInput("reorder", "Cluster rows/columns", FALSE),
        shiny::downloadButton("dl_mpi_csv", "Table (CSV)"),
        shiny::downloadButton("dl_mpi_png", "Heatmap (PNG)"),
        shiny::hr(),
        shiny::selectInput("highlight_select", "Highlight a factor (same as clicking it)", c("None" = "")),
        shiny::hr(),
        shiny::h5("Compare settings"),
        shiny::actionButton("snap_take", "Keep current results as snapshot", class = "btn-sm"),
        shiny::uiOutput("snap_info"),
        shiny::conditionalPanel("output.has_snapshot",
          shiny::checkboxInput("snap_mark", "Mark changes on the heatmap", TRUE),
          shiny::radioButtons("snap_mark_kind", NULL, c("Rank changes" = "rank", "Value changes" = "value"), inline = TRUE)),
        shiny::hr(),
        adv_settings_ui()
      ),
      shiny::mainPanel(
        shiny::uiOutput("norm_info"),
        shiny::radioButtons("mpi_view", "View", inline = TRUE, c(
          "Heatmap" = "heatmap", "Where it comes from (Sankey)" = "terms",
          "How factors interact (network)" = "network", "Factors to responses (Sankey)" = "responses",
          "What MPI means (contour)" = "contour")),
        shiny::conditionalPanel("input.mpi_view == 'heatmap'",
          shiny::p(class = "stage-question", "Where is influence largest?"),
          shiny::p(class = "plot-hint", "Hover a cell for its numbers; click it (or use 'Highlight a factor') to highlight the factor on every stage."),
          explorer_interactive_plot("heatmap", vh = 62, min = 360, max = 640)),
        shiny::conditionalPanel("input.mpi_view == 'terms' || input.mpi_view == 'network' || input.mpi_view == 'contour'",
          shiny::div(class = "view-controls",
            shiny::selectInput("view_response", "Response", NULL, width = "220px"),
            shiny::conditionalPanel("input.mpi_view == 'contour'", class = "view-controls",
              shiny::selectInput("contour_factor", "Factor (arrow)", NULL, width = "180px"),
              shiny::selectInput("contour_partner", "Second factor (vertical axis)", NULL, width = "220px")))),
        shiny::conditionalPanel("input.mpi_view == 'terms'",
          shiny::p(class = "stage-question", "What generates each factor's influence?")),
        shiny::conditionalPanel("input.mpi_view == 'network'",
          shiny::p(class = "stage-question", "Which factors amplify each other?")),
        shiny::conditionalPanel("input.mpi_view == 'terms' || input.mpi_view == 'network'",
          shiny::uiOutput("anatomy_note"),
          shiny::p(class = "note", "These diagrams show how the fitted model adds up; they are not causal pathways.")),
        shiny::conditionalPanel("input.mpi_view == 'contour'",
          shiny::p(class = "stage-question", "What does one factor's MPI mean on the fitted surface?"),
          shiny::uiOutput("contour_note"),
          explorer_plot_output("contour", vh = 62, min = 380, max = 600)),
        shiny::conditionalPanel("input.mpi_view == 'terms'",
          explorer_plot_output("sankey_terms", vh = 55, min = 340, max = 520),
          explorer_details("Contributions table", shiny::tableOutput("contrib_table"))),
        shiny::conditionalPanel("input.mpi_view == 'network'",
          shiny::p(class = "plot-hint", "Click a factor to highlight it on every stage."),
          explorer_plot_output("network", vh = 62, min = 380, max = 600, click = "network_click")),
        shiny::conditionalPanel("input.mpi_view == 'responses'",
          shiny::div(class = "view-controls",
            shiny::checkboxInput("flow_scale", "Scale each response to its largest factor (Eq. 5)", TRUE),
            shiny::sliderInput("flow_min", "Hide flows below", min = 0, max = 0.9, value = 0, step = 0.05,
                               width = "260px", ticks = FALSE)),
          explorer_plot_output("sankey_responses", vh = 70, min = 400, max = 680)),
        explorer_details("Values table (rows: responses, columns: factors)",
          shiny::tableOutput("mpi_table"),
          shiny::p(class = "note", "Download the CSV for the long table with direction and number of terms.")),
        shiny::conditionalPanel("output.has_snapshot",
          shiny::h4("What changed since the snapshot?"),
          shiny::uiOutput("compare_text"),
          explorer_plot_output("compare_plot", vh = 45, min = 300, max = 520),
          explorer_details("Comparison table", shiny::tableOutput("compare_table"), open = TRUE),
          shiny::p(class = "note", "Ranks are by absolute value within each response, so they can be compared across norms and standardisations; the values themselves are on the scale of each setting."))
      )
    )
  )
}

#' Server of the MPI stage
#' @param input,output,session Shiny objects.
#' @param st Shared state (from the data stage).
#' @return A list of reactives: `base_mpi`, `shown_mpi`, `heatmap_plot`.
#' @keywords internal
#' @noRd
stage_mpi_server <- function(input, output, session, st) {
  base_mpi <- shiny::reactive({
    a <- st$analysis()
    suppressWarnings(mpi_multi(st$mpi_input(a), norm = input$norm, abs = input$signed == "abs",
                               factors = a$factors))
  })

  shown_mpi <- shiny::reactive({
    m <- base_mpi()
    switch(input$std,
      none = m,
      colmax_response = standardize_mpi(m, "colmax", by = "response"),
      colmax_factor = standardize_mpi(m, "colmax", by = "factor"),
      residual_sd = {
        shiny::validate(shiny::need(st$analysis()$kind == "model",
          "Residual-SD scaling needs fitted models: choose another standardisation, or raw data in step 1."))
        standardize_mpi(m, "residual_sd")
      },
      rank = standardize_mpi(m, "rank"))
  })

  heatmap_plot <- shiny::reactive({
    n_cells <- nrow(shown_mpi())
    plot(shown_mpi(), transpose = isTRUE(input$transpose), annotate = isTRUE(input$annotate),
         reorder = isTRUE(input$reorder), palette = input$palette,
         signed = if (input$signed == "abs" || input$std == "rank") FALSE else NULL,
         text_size = if (n_cells > 60) 3 else 4.5) + explorer_theme()
  })
  # redraw at most every 300 ms while controls are being clicked
  heatmap_debounced <- shiny::debounce(heatmap_plot, 300)

  output$norm_info <- shiny::renderUI({
    note <- .norm_note(input$norm)
    if (is.null(note)) {
      shiny::p(class = "note", "L1 MPI: the largest change in the response from a one-unit increase of the factor, other factors set most favourably (paper Eq. 4).")
    } else shiny::p(class = "note warn", note)
  })
  output$heatmap <- shiny::renderPlot({
    p <- heatmap_debounced()
    s <- st$snapshot()
    if (!is.null(s) && isTRUE(input$snap_mark)) {
      p <- explorer_mark_changes(p, explorer_compare(s$data, shown_mpi()), kind = input$snap_mark_kind %||% "rank")
    }
    w <- explorer_width(session, "heatmap")
    p <- explorer_fit_width(explorer_fit_heatmap(p, w), w)
    explorer_draw_interactive(explorer_highlight_heatmap(p, st$focus()), st$geom, "heatmap")
  }, res = 96, alt = "Heatmap of MPI by factor and response; the values table below gives the same numbers")
  # flow views: built from the unstandardised result (contributions are on the response scale)
  shiny::observe({
    r <- st$responses()
    cur <- shiny::isolate(input$view_response)
    shiny::updateSelectInput(session, "view_response", choices = r, selected = if (!is.null(cur) && cur %in% r) cur else r[1])
  })
  view_resp <- shiny::reactive({
    r <- st$responses()
    if (!is.null(input$view_response) && input$view_response %in% r) input$view_response else r[1]
  })
  focus_factor <- function() { f <- st$focus(); if (is.null(f)) NULL else f$factor }
  output$anatomy_note <- shiny::renderUI({
    cc <- mpi_contributions(base_mpi(), response = view_resp())
    shiny::p(class = "note", guide_anatomy_text(cc, view_resp(), input$norm))
  })
  output$contrib_table <- shiny::renderTable({
    cc <- mpi_contributions(base_mpi(), response = view_resp())
    data.frame(Factor = cc$factor, Term = cc$term, Type = cc$type, Coefficient = cc$coef,
               Contribution = cc$contribution, `Share of factor` = sprintf("%.0f%%", 100 * cc$share), check.names = FALSE)
  }, digits = 4)
  # keyboard alternative to clicking: a select that drives the same highlight
  shiny::observe({
    fs <- tryCatch(st$analysis()$factors, error = function(e) character())
    f <- shiny::isolate(st$focus())
    shiny::updateSelectInput(session, "highlight_select", choices = c("None" = "", fs),
                             selected = if (!is.null(f) && f$factor %in% fs) f$factor else "")
  })
  shiny::observeEvent(input$highlight_select, {
    f <- st$focus()
    v <- input$highlight_select
    if (!nzchar(v %||% "")) {
      if (!is.null(f)) st$focus(NULL)
    } else if (is.null(f) || !identical(f$factor, v)) {
      st$focus(list(factor = v, response = view_resp()))
    }
  }, ignoreInit = TRUE)
  shiny::observe({
    f <- st$focus()
    cur <- shiny::isolate(input$highlight_select) %||% ""
    want <- if (is.null(f)) "" else f$factor
    if (!identical(cur, want)) shiny::updateSelectInput(session, "highlight_select", selected = want)
  })

  output$sankey_terms <- shiny::renderPlot({
    w <- explorer_width(session, "sankey_terms")
    explorer_fit_width(mpi_sankey(base_mpi(), view = "terms", response = view_resp(), highlight = focus_factor(),
                                  compact = isTRUE(w < 600)) + explorer_theme_diagram(), w)
  }, res = 96, alt = function() paste("Sankey diagram: model terms flowing into the factors' MPI for", view_resp()))
  output$network <- shiny::renderPlot({
    explorer_fit_width(mpi_network(base_mpi(), response = view_resp(), highlight = focus_factor()) + explorer_theme_diagram(),
                       explorer_width(session, "network"))
  }, res = 96, alt = function() paste("Network of factor interactions for", view_resp()))
  shiny::observeEvent(input$network_click, {
    ev <- input$network_click
    fs <- unique(base_mpi()$factor)
    k <- length(fs)
    ang <- pi / 2 - 2 * pi * (seq_len(k) - 1) / k
    d <- sqrt((cos(ang) - ev$x)^2 + (sin(ang) - ev$y)^2)
    if (min(d) > 0.35) return()
    cell <- list(factor = fs[which.min(d)], response = view_resp())
    st$focus(if (identical(st$focus(), cell)) NULL else cell)
  })
  # contour view of the MPI definition
  shiny::observe({
    fs <- st$analysis()$factors
    cur <- shiny::isolate(input$contour_factor)
    top <- tryCatch({
      m <- tibble::as_tibble(base_mpi())
      m <- m[m$response == view_resp(), , drop = FALSE]
      m$factor[which.max(abs(m$estimate))]
    }, error = function(e) fs[1])
    shiny::updateSelectInput(session, "contour_factor", choices = fs,
                             selected = if (!is.null(cur) && cur %in% fs) cur else top)
  })
  shiny::observe({
    fs <- st$analysis()$factors
    f <- input$contour_factor
    cur <- shiny::isolate(input$contour_partner)
    others <- setdiff(fs, f)
    shiny::updateSelectInput(session, "contour_partner", choices = c("Strongest interaction" = ".auto", others),
                             selected = if (!is.null(cur) && cur %in% c(".auto", others)) cur else ".auto")
  })
  contour_args <- shiny::reactive({
    fs <- st$analysis()$factors
    f <- shiny::req(input$contour_factor)
    shiny::validate(shiny::need(f %in% fs, "Choose a factor."),
                    shiny::need(length(fs) >= 2, "The contour view needs at least two factors."))
    pa <- input$contour_partner
    list(factor = f, partner = if (is.null(pa) || identical(pa, ".auto") || !pa %in% setdiff(fs, f)) NULL else pa,
         response = view_resp())
  })
  contour_plot <- shiny::reactive({
    ca <- contour_args()
    tryCatch(mpi_contour(base_mpi(), factor = ca$factor, partner = ca$partner, response = ca$response),
             error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
  }) |> shiny::bindCache(base_mpi(), contour_args())
  output$contour_note <- shiny::renderUI({
    s <- attr(contour_plot(), "mpi_step")
    shiny::p(class = "note", sprintf(
      "Starting at %s = %s with the other factors at their most favourable levels, one coded unit more of %s changes the fitted response by %s: exactly its MPI (%s).",
      s$factor, formatC(s$start[[s$factor]], format = "g"), s$factor, formatC(s$change, digits = 4, format = "g"),
      formatC(s$mpi, digits = 4, format = "g")))
  })
  output$contour <- shiny::renderPlot(explorer_fit_width(contour_plot() + explorer_theme(), explorer_width(session, "contour")),
                                      res = 96,
                                      alt = "Contour plot of the fitted surface with the one-unit MPI step")

  output$sankey_responses <- shiny::renderPlot({
    p <- tryCatch(mpi_sankey(base_mpi(), view = "responses", standardize = if (isTRUE(input$flow_scale)) "colmax" else "none",
                             min_value = input$flow_min %||% 0, highlight = focus_factor(),
                             compact = isTRUE(explorer_width(session, "sankey_responses") < 600)),
                  error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
    explorer_fit_width(p + explorer_theme_diagram(), explorer_width(session, "sankey_responses"))
  }, res = 96, alt = "Sankey diagram: factors flowing into the responses they drive")

  output$mpi_table <- shiny::renderTable({
    # compact wide table: one row per response, one column per factor
    m <- tibble::as_tibble(shown_mpi())
    fs <- unique(m$factor)
    rs <- unique(m$response)
    wide <- data.frame(response = rs, check.names = FALSE)
    for (f in fs) wide[[f]] <- m$estimate[match(paste(rs, f), paste(m$response, m$factor))]
    wide
  }, digits = 3)
  output$dl_mpi_csv <- shiny::downloadHandler(
    filename = function() sprintf("herminR_mpi_%s.csv", input$norm),
    content = function(file) utils::write.csv(tibble::as_tibble(shown_mpi()), file, row.names = FALSE))
  output$dl_mpi_png <- shiny::downloadHandler(
    filename = function() sprintf("herminR_heatmap_%s.png", input$norm),
    content = function(file) ggplot2::ggsave(file, heatmap_plot(), width = 8, height = 6, dpi = 150))

  list(base_mpi = base_mpi, shown_mpi = shown_mpi, heatmap_plot = heatmap_plot, view_resp = view_resp,
       contour_plot = contour_plot, contour_args = contour_args)
}
