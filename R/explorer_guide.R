# Guided experience for the explorer ------------------------------------------------
#
# A "Start here" page, a progress bar on every page, and on each stage a guide
# box: the question the stage answers, how to read it, and a plain-language
# summary of what was found (generated from the actual results).

#' Stages of the guided workflow
#' @return A list of stages, each with `tab` (navbar value), `short`, `title`,
#'   `question`, `what`, `read`, `get` and `optional`.
#' @keywords internal
#' @noRd
guide_stages <- function() {
  list(
    list(tab = "1 Data & model", short = "Data", optional = FALSE,
         title = "Choose your study",
         question = "Which experiment are we analysing?",
         what = paste("Pick one of the paper's studies, or upload your own data or a table of model",
                      "coefficients. With raw data the app fits one response-surface model per response."),
         read = c("Each response gets a model: main effects (x), curvature (x^2) and interactions (x1:x2).",
                  "Factors are put on a common 'coded' scale from -1 (low) to +1 (high), so a one-unit change means the same thing for every factor.",
                  "Block terms (e.g. day of the experiment) are kept in the model but are not factors you can set."),
         get = "One fitted model per response"),
    list(tab = "2 MPI explorer", short = "Influence", optional = FALSE,
         title = "Which factors matter most?",
         question = "If I change this factor, how much can this response change?",
         what = paste("For every factor and response the app computes the Maximal Possible Influence (MPI):",
                      "the largest change in the response from increasing the factor by one coded unit,",
                      "with the other factors set where they amplify the effect most."),
         read = c("Darker cell = more influence. The number is the MPI in the response's own units.",
                  "A white cell (0) means the factor has no effect on that response in the model.",
                  "'Eq. (5)' scaling divides each response by its largest MPI, so 1 marks the most influential factor of each response.",
                  "L2 and L-infinity are alternative summaries, not achievable changes; keep L1 (MPI) for decisions."),
         get = "A factor x response influence map"),
    list(tab = "3 Uncertainty", short = "Certainty", optional = TRUE,
         title = "How sure are we?",
         question = "Is each influence precisely estimated, and is the ranking stable?",
         what = paste("Experiments are noisy, so each MPI has uncertainty. The interval shows the plausible range",
                      "of each factor's influence; the bootstrap replays the experiment with new noise and checks",
                      "whether the order of the factors changes."),
         read = c("Bar = plausible range of the influence. Filled dot: the range stays above 0. Open dot: 'no influence' cannot be ruled out.",
                  "Rank plot: a factor ranked first in nearly all replays is a reliable leader.",
                  "Overlapping bars can still come with a stable ranking: the two questions are different."),
         get = "Intervals and rank stability"),
    list(tab = "4 Robustness", short = "Robustness", optional = TRUE,
         title = "Does it depend on our modelling choices?",
         question = "Would a different but reasonable model tell a different story?",
         what = paste("Models are usually simplified by removing weak terms, and different rules remove different",
                      "terms. The app re-runs that simplification several ways and checks whether the most",
                      "influential factors stay the same."),
         read = c("Each row is one way of simplifying the model; the number in a cell is the factor's rank on that path.",
                  "The same leader on every row = a conclusion that does not depend on the modelling choice.",
                  "The norm comparison shows whether the ranking depends on how terms are added up."),
         get = "Rankings across model choices"),
    list(tab = "5 Follow-up", short = "Next experiment", optional = TRUE,
         title = "What does it mean for the next experiment?",
         question = "What happens if I narrow a factor's range, drop factors, or exclude unsafe settings?",
         what = paste("Plan the next study: restrict a factor to a narrow window, keep only some factors, or",
                      "describe which combinations are feasible. The app shows how much influence remains."),
         read = c("Narrowing a factor mainly shrinks its interactions with the other factors.",
                  "Dropped factors are held at the centre of their range.",
                  "'Feasible region' compares the influence you can still reach under your constraints with the unrestricted one."),
         get = "Influence under your planned conditions"),
    list(tab = "6 Reproduce", short = "Reproduce", optional = FALSE,
         title = "Keep a record",
         question = "How do I reproduce or report these results?",
         what = "The app writes the R script that recreates what you see, with the settings you chose.",
         read = c("Download the script and keep it with your report.",
                  "Running it in R with herminR gives exactly the same numbers."),
         get = "A reproducible R script")
  )
}

.fmt <- function(x) format(signif(x, 3), trim = TRUE, scientific = FALSE, drop0trailing = TRUE)

.join_words <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

# Plain-language findings ----------------------------------------------------------

#' Plain-language summary of the data stage
#' @param kind `"model"`, `"table"` or `"coef"`.
#' @param label Description of the source.
#' @param responses,factors Character vectors.
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_data_text <- function(kind, label, responses, factors) {
  c(sprintf("Using %s: %d response%s (%s) and %d factor%s (%s).", label,
            length(responses), if (length(responses) == 1) "" else "s", .join_words(utils::head(responses, 6)),
            length(factors), if (length(factors) == 1) "" else "s", .join_words(utils::head(factors, 11))),
    switch(kind,
      model = "Models were fitted to the data, so every later step is available.",
      table = "Coefficients with standard errors were supplied: uncertainty intervals are available; bootstrap and selection paths need raw data.",
      coef = "Only coefficients were supplied: influence and follow-up are available; uncertainty and robustness need raw data or standard errors."),
    "Next: see which factors can move each response the most.")
}

#' Plain-language summary of MPI results
#' @param m An unstandardised `herminR_mpi` object (signed or absolute).
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_mpi_text <- function(m) {
  m <- tibble::as_tibble(m)
  l1 <- identical(unique(m$norm)[1], "l1")
  resp <- unique(m$response)
  out <- character(0)
  if (length(resp) <= 6) {
    for (r in resp) {
      d <- m[m$response == r, ]
      a <- abs(d$estimate)
      if (all(a == 0)) {
        out <- c(out, sprintf("%s: no factor has any influence in this model.", r))
        next
      }
      i <- which.max(a)
      dir <- switch(d$direction[i] %||% "", increase = " (the response goes up)",
                    decrease = " (the response goes down)", "")
      s <- if (l1) {
        sprintf("%s: %s has the largest influence; one coded unit more of %s can change %s by up to %s%s.",
                r, d$factor[i], d$factor[i], r, .fmt(a[i]), dir)
      } else {
        sprintf("%s: %s has the highest %s score (%s).", r, d$factor[i],
                c(l2 = "L2", linf = "L-infinity")[[unique(m$norm)[1]]], .fmt(a[i]))
      }
      second <- order(-a)[2]
      if (!is.na(second) && a[second] > 0) {
        gap <- (a[i] - a[second]) / a[i]
        s <- paste0(s, if (gap < 0.01) {
          sprintf(" %s is practically tied with it (%s, within 1%%): do not rank these two from this table alone.",
                  d$factor[second], .fmt(a[second]))
        } else if (gap < 0.05) {
          sprintf(" %s is close behind (%s, within 5%%): check the ranking in the Certainty step.", d$factor[second], .fmt(a[second]))
        } else sprintf(" Next: %s (%s).", d$factor[second], .fmt(a[second])))
      }
      z <- d$factor[a == 0]
      if (length(z)) s <- paste0(s, sprintf(" No influence: %s.", .join_words(z)))
      out <- c(out, s)
    }
  } else {
    leaders <- vapply(resp, function(r) {
      d <- m[m$response == r, ]
      if (all(d$estimate == 0)) NA_character_ else d$factor[which.max(abs(d$estimate))]
    }, character(1))
    tab <- sort(table(leaders), decreasing = TRUE)
    out <- c(out, sprintf("Across %d responses, %s is the most influential factor for %d of them%s.",
                          length(resp), names(tab)[1], tab[[1]],
                          if (length(tab) > 1) sprintf(", followed by %s (%d)", names(tab)[2], tab[[2]]) else ""))
    never <- unique(m$factor)[vapply(unique(m$factor), function(f) all(m$estimate[m$factor == f] == 0), logical(1))]
    if (length(never)) out <- c(out, sprintf("%s never influence%s any response.", .join_words(never),
                                             if (length(never) == 1) "s" else ""))
  }
  if (!l1) out <- c(out, "These are contribution scores, not achievable changes; switch to L1 (MPI) for decisions.")
  c(out, "Next: check how precisely these influences are estimated.")
}

#' Plain-language summary of uncertainty intervals
#' @param u A `herminR_mpi` object with `lower` / `upper`.
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_interval_text <- function(u) {
  u <- tibble::as_tibble(u)
  out <- character(0)
  for (r in utils::head(unique(u$response), 6)) {
    d <- u[u$response == r & !is.na(u$lower), ]
    d <- d[abs(d$estimate) > 0 | d$upper > 0, ]
    if (!nrow(d)) next
    clear <- d$factor[d$lower > 0]
    open <- d$factor[d$lower <= 0]
    s <- sprintf("%s: ", r)
    if (length(clear)) s <- paste0(s, sprintf("%s %s clearly influential (interval above 0)", .join_words(clear),
                                              if (length(clear) == 1) "is" else "are"))
    if (length(open)) s <- paste0(s, if (length(clear)) "; " else "",
                                  sprintf("for %s the data cannot rule out no influence", .join_words(open)))
    out <- c(out, paste0(s, "."))
  }
  if (length(unique(u$response)) > 6) out <- c(out, sprintf("(%d more responses in the table.)", length(unique(u$response)) - 6))
  out
}

#' Plain-language summary of bootstrap rank stability
#' @param br Output of [bootstrap_ranks()].
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_anatomy_text <- function(cc, response, norm = "l1") {
  if (!nrow(cc)) return(paste0("No term of the model involves the chosen factors for ", response, "."))
  tot <- tapply(cc$contribution, cc$factor, sum)
  f <- names(which.max(tot))
  d <- cc[cc$factor == f, , drop = FALSE]
  inter <- sum(d$share[d$type == "interaction"])
  biggest <- d[which.max(d$contribution), , drop = FALSE]
  partners <- unique(d$partners[d$type == "interaction" & d$share > 0.05])
  s1 <- if (identical(norm, "l1")) {
    paste0(response, ": the influence of ", f, " (", formatC(sum(d$contribution), digits = 3, format = "g"), ") comes ",
           round(100 * (1 - inter)), "% from its own terms and ", round(100 * inter), "% from interactions",
           if (length(partners)) paste0(" with ", paste(partners, collapse = ", ")) else "", "; ",
           "the largest single term is ", biggest$term, " (", round(100 * biggest$share), "%).")
  } else {
    paste0(response, ": ", f, " has the largest term components; ", round(100 * inter),
           "% of them come from interactions (the norm combines components, so shares are indicative).")
  }
  heavy <- names(tot)[vapply(names(tot), function(g) {
    dg <- cc[cc$factor == g, ]; sum(dg$share[dg$type == "interaction"]) > 0.5
  }, logical(1))]
  s2 <- if (length(heavy)) {
    paste0("Mostly driven by interactions: ", paste(heavy, collapse = ", "),
           ". Their effect depends on where the partner factors are set, so check them together.")
  } else "Every factor's influence comes mainly from its own terms, so each can be studied largely on its own."
  paste(s1, s2)
}

guide_rank_text <- function(br) {
  b <- tibble::as_tibble(br)
  n <- attr(br, "n_ok")
  o <- order(-b$top1_frac)
  lead <- b$factor[o[1]]
  p <- b$top1_frac[o[1]]
  s <- if (p >= 0.9) {
    sprintf("%s ranked first in %.0f%% of %d replays: a reliable leader.", lead, 100 * p, n)
  } else if (p >= 0.6) {
    sprintf("%s ranked first in %.0f%% of %d replays: a likely but not certain leader (next: %s, %.0f%%).",
            lead, 100 * p, n, b$factor[o[2]], 100 * b$top1_frac[o[2]])
  } else {
    sprintf("No stable leader: %s ranked first in %.0f%% of %d replays and %s in %.0f%%.",
            lead, 100 * p, n, b$factor[o[2]], 100 * b$top1_frac[o[2]])
  }
  moving <- b$factor[b$rank_upper > b$rank_lower]
  c(s, if (length(moving)) sprintf("Rank can change for %s.", .join_words(moving)) else
         "Every factor kept the same rank in all replays.")
}

#' Plain-language summary of selection paths
#' @param sp Output of [mpi_selection_paths()].
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_selection_text <- function(sp) {
  d <- tibble::as_tibble(sp)
  paths <- unique(d$path)
  leaders <- vapply(paths, function(p) {
    x <- d[d$path == p, ]
    x$factor[which.min(x$rank)]
  }, character(1))
  s <- if (length(unique(leaders)) == 1) {
    sprintf("%s is the most influential factor on all %d model choices: the conclusion does not depend on how the model is simplified.",
            leaders[1], length(paths))
  } else {
    sprintf("The leader depends on the model choice: %s.",
            paste(sprintf("%s on %s", leaders, paths), collapse = ", "))
  }
  dropped <- unique(d$factor[!d$in_model])
  c(s, if (length(dropped)) sprintf("%s %s removed entirely on at least one path.", .join_words(dropped),
                                    if (length(dropped) == 1) "is" else "are"))
}

#' Plain-language summary of a follow-up (range / subset) scenario
#' @param before Unstandardised absolute MPI before the change.
#' @param after The modified object (with `estimate_raw` if standardised).
#' @param max_items Maximum number of changes listed.
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_followup_text <- function(before, after, max_items = 5) {
  b <- tibble::as_tibble(before)
  a <- tibble::as_tibble(after)
  av <- if ("estimate_raw" %in% names(a)) a$estimate_raw else a$estimate
  key_a <- paste(a$response, a$factor)
  bv <- abs(b$estimate[match(key_a, paste(b$response, b$factor))])
  rel <- ifelse(bv > 0, (abs(av) - bv) / bv, 0)
  ch <- which(abs(rel) > 0.01)
  dropped <- setdiff(unique(b$factor), unique(a$factor))
  out <- character(0)
  if (length(dropped)) out <- c(out, sprintf("Dropped and held at the centre: %s.", .join_words(dropped)))
  if (!length(ch)) {
    out <- c(out, "No influence changes: the changed factors do not interact with the remaining ones.")
  } else {
    ch <- ch[order(-abs(rel[ch]))][seq_len(min(max_items, length(ch)))]
    out <- c(out, vapply(ch, function(i) sprintf("%s: influence of %s goes from %s to %s (%+.0f%%).",
                                                 a$response[i], a$factor[i], .fmt(bv[i]), .fmt(abs(av[i])),
                                                 100 * rel[i]), character(1)))
  }
  out
}

#' Plain-language summary of feasible-region results
#' @param f Output of [mpi_feasible()].
#' @return Character vector of sentences.
#' @keywords internal
#' @noRd
guide_feasible_text <- function(f) {
  d <- tibble::as_tibble(f)
  d <- d[d$mpi_unrestricted > 0, ]
  if (!nrow(d)) return("No factor has any influence on this response.")
  parts <- sprintf("%s keeps %.0f%%", d$factor, 100 * d$feas_ratio)
  c(paste0("Of its unrestricted influence, ", .join_words(parts), "."),
    if (identical(attr(f, "method"), "exact")) "Exact result for these ranges." else
      "Sampling result: a lower estimate; more samples give a closer value.")
}

# UI ------------------------------------------------------------------------------------

#' Guide box shown at the top of a stage
#' @param k Stage number (1-6).
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
guide_box_ui <- function(k) {
  st <- guide_stages()[[k]]
  shiny::div(
    class = "guide-box",
    shiny::div(class = "guide-head",
      shiny::div(
        shiny::span(class = "guide-step", sprintf("Step %d of 6", k)),
        if (st$optional) shiny::span(class = "guide-opt", "optional"),
        shiny::h3(st$title),
        shiny::p(class = "guide-q", st$question)
      ),
      shiny::div(class = "guide-nav",
        if (k > 1) shiny::actionButton(paste0("guide_back_", k), "\u2190 Back", class = "btn-sm"),
        if (k < 6) shiny::actionButton(paste0("guide_next_", k),
                                       paste("Next:", guide_stages()[[k + 1]]$short, "\u2192"),
                                       class = "btn-sm btn-primary"))
    ),
    shiny::p(st$what),
    shiny::tags$details(class = "guide-read", shiny::tags$summary("How to read this"),
                        shiny::tags$ul(lapply(st$read, shiny::tags$li))),
    shiny::div(class = "guide-find", shiny::uiOutput(paste0("guide_find_", k)))
  )
}

#' The "Start here" page
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
guide_start_ui <- function() {
  st <- guide_stages()
  cards <- lapply(seq_along(st), function(k) {
    shiny::div(class = "guide-card",
      shiny::div(class = "guide-num", k),
      shiny::h4(st[[k]]$title, if (st[[k]]$optional) shiny::span(class = "guide-opt", "optional")),
      shiny::p(class = "guide-q", st[[k]]$question),
      shiny::p(class = "note", shiny::strong("You get: "), st[[k]]$get))
  })
  shiny::tabPanel(
    "Start here", value = "start",
    shiny::div(class = "guide-start",
      shiny::h2("Which process factors really drive your results?"),
      shiny::p(class = "lead",
        "In a designed experiment you vary several process factors (temperature, speed, moisture...) and",
        "measure several responses (quality attributes). This app answers, for every factor and response:",
        shiny::strong("if I change this factor, how much can the response change?"),
        "Then it checks how sure and how robust that answer is, and what it means for your next experiment."),
      shiny::div(class = "guide-key",
        shiny::strong("The key number: MPI (Maximal Possible Influence). "),
        "The largest change in a response caused by increasing one factor by one coded unit (for example",
        "from the middle of its range to the top), with the other factors set where they amplify the",
        "effect most. A large MPI means the factor matters; MPI = 0 means it has no effect in the model."),
      shiny::h4("The journey, step by step"),
      shiny::div(class = "guide-cards", cards),
      shiny::div(class = "guide-actions",
        shiny::actionButton("guide_example", "Try the paper example \u2192", class = "btn-primary btn-lg"),
        shiny::actionButton("guide_begin", "Start with my own data", class = "btn-lg")),
      shiny::tags$details(class = "guide-read",
        shiny::tags$summary("Glossary"),
        shiny::tags$dl(
          shiny::tags$dt("Factor / response"), shiny::tags$dd("What you set in the experiment / what you measure."),
          shiny::tags$dt("Coded units"), shiny::tags$dd("Each factor rescaled to -1 (low) ... +1 (high), so one unit means the same for all factors."),
          shiny::tags$dt("Main, quadratic, interaction effect"), shiny::tags$dd("Straight-line effect of a factor; its curvature; how two factors change each other's effect."),
          shiny::tags$dt("MPI (L1)"), shiny::tags$dd("Largest achievable change in a response for a one-unit increase of a factor."),
          shiny::tags$dt("L2 / L-infinity score"), shiny::tags$dd("Alternative ways of adding up the effects; useful comparisons, not achievable changes."),
          shiny::tags$dt("Eq. (5) scaling"), shiny::tags$dd("Divide each response by its largest MPI, so 1 = most influential factor of that response."),
          shiny::tags$dt("Interval"), shiny::tags$dd("Plausible range of a factor's influence given the experimental noise."),
          shiny::tags$dt("Bootstrap"), shiny::tags$dd("Replaying the experiment many times with new noise to see if the ranking holds."),
          shiny::tags$dt("Selection path"), shiny::tags$dd("One way of simplifying the model by removing weak terms."),
          shiny::tags$dt("Feasible region"), shiny::tags$dd("The factor settings that are allowed (safe, practical) in the next experiment.")
        ))
    )
  )
}

#' CSS for the guided experience
#' @return A character string.
#' @keywords internal
#' @noRd
guide_css <- function() {
  "
  .guide-stepper{display:flex;gap:6px;flex-wrap:wrap;margin:0 0 14px;padding:8px 12px;background:#f7f7f7;border-bottom:1px solid #e3e3e3}
  .guide-pill{display:flex;align-items:center;gap:6px;padding:4px 10px;border-radius:16px;cursor:pointer;color:#555;font-size:90%;text-decoration:none}
  .guide-pill:hover{background:#eaeaea;text-decoration:none}
  .guide-pill .dot{display:inline-flex;align-items:center;justify-content:center;width:22px;height:22px;border-radius:50%;border:2px solid #aaa;font-weight:bold;font-size:85%}
  .guide-pill.done .dot{background:#2e7d32;border-color:#2e7d32;color:#fff}
  .guide-pill.current{background:#1f4e79;color:#fff}
  .guide-pill.current .dot{border-color:#fff;color:#fff}
  .guide-pill .opt{font-size:75%;opacity:.75}
  .guide-sep{color:#bbb;align-self:center}
  .guide-box{border:1px solid #d9e2ec;border-left:5px solid #1f4e79;border-radius:4px;padding:10px 16px;margin:0 0 16px;background:#fbfdff}
  .guide-head{display:flex;justify-content:space-between;align-items:flex-start;gap:12px}
  .guide-head h3{margin:2px 0 2px}
  .guide-step{font-size:80%;text-transform:uppercase;letter-spacing:.05em;color:#1f4e79;font-weight:bold}
  .guide-opt{font-size:75%;margin-left:6px;padding:1px 6px;border-radius:8px;background:#eee;color:#666;font-weight:normal}
  .guide-q{font-style:italic;color:#333;margin:0 0 6px}
  .guide-nav{white-space:nowrap}
  @media (max-width: 600px) {
    .guide-head{flex-direction:column-reverse;gap:6px}
    .guide-nav{align-self:flex-end}
    .guide-box{padding:8px 10px}
  }
  .guide-read>summary{cursor:pointer;color:#1f4e79;margin:4px 0}
  .guide-find{background:#eef6ee;border-radius:4px;padding:6px 12px;margin-top:8px}
  .guide-find h5{margin:4px 0;font-weight:bold;color:#2e7d32}
  .guide-find ul{margin:0 0 4px;padding-left:20px}
  .guide-start{max-width:1100px;margin:0 auto}
  .guide-key{background:#fff8e1;border-left:5px solid #f9a825;padding:10px 14px;margin:12px 0 18px}
  .guide-cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:12px;margin-bottom:18px}
  .guide-card{border:1px solid #e3e3e3;border-radius:6px;padding:10px 14px;position:relative;background:#fff}
  .guide-card h4{margin:2px 0 4px 36px}
  .guide-card .guide-q{margin-left:36px}
  .guide-card .note{margin-left:36px}
  .guide-num{position:absolute;left:12px;top:12px;width:26px;height:26px;border-radius:50%;background:#1f4e79;color:#fff;text-align:center;line-height:26px;font-weight:bold}
  .guide-actions{display:flex;gap:10px;margin-bottom:16px}
  "
}

# Server ------------------------------------------------------------------------------------

#' Server logic of the guided experience
#'
#' @param input,output,session Shiny server arguments.
#' @param r A list of the explorer's reactives and helpers: `analysis`,
#'   `responses`, `mpi_input`, `unc_mpi`, `boot_res`, `sel_res`,
#'   `followup_mpi`, `feas_res`.
#' @return A list with the `visited` reactive value, for testing.
#' @keywords internal
#' @noRd
guide_server <- function(input, output, session, r) {
  st <- guide_stages()
  tabs <- vapply(st, `[[`, character(1), "tab")
  visited <- shiny::reactiveVal(character(0))
  shiny::observeEvent(input$tabs, {
    if (!input$tabs %in% visited()) visited(c(visited(), input$tabs))
  })
  go <- function(tab) shiny::updateNavbarPage(session, "tabs", selected = tab)

  for (k in seq_along(st)) local({
    kk <- k
    shiny::observeEvent(input[[paste0("guide_next_", kk)]], go(tabs[kk + 1]), ignoreInit = TRUE)
    shiny::observeEvent(input[[paste0("guide_back_", kk)]], go(tabs[kk - 1]), ignoreInit = TRUE)
  })
  shiny::observeEvent(input$guide_goto, go(input$guide_goto))
  shiny::observeEvent(input$guide_begin, go(tabs[1]))
  for (b in c("goto_data_3", "goto_data_4")) local({
    bb <- b
    shiny::observeEvent(input[[bb]], go(tabs[1]), ignoreInit = TRUE)
  })
  shiny::observeEvent(input$guide_example, {
    shiny::updateRadioButtons(session, "source", selected = "pastry_table1")
    shiny::updateSelectInput(session, "std", selected = "colmax_response")
    go(tabs[2])
  })

  data_ok <- shiny::reactive(!inherits(try(r$analysis(), silent = TRUE), "try-error"))
  caps <- shiny::reactive(tryCatch(r$capabilities(), error = function(e) explorer_capabilities("none")))

  # availability of each stage for the current data
  stage_status <- shiny::reactive({
    cp <- caps()
    c("ok", "ok", if (cp$intervals) "ok" else "unavailable", if (cp$selection) "ok" else "limited", "ok", "ok")
  })

  output$guide_stepper <- shiny::renderUI({
    cur <- input$tabs %||% "start"
    v <- visited()
    status <- stage_status()
    tips <- c(unavailable = "Needs fitted models or standard errors - change the data source in step 1.",
              limited = "Selection paths need raw data; only the norm comparison is available.")
    pills <- lapply(seq_along(st), function(k) {
      done <- tabs[k] %in% v && tabs[k] != cur && (k > 1 || data_ok()) && status[k] == "ok"
      cls <- paste("guide-pill", if (tabs[k] == cur) "current" else if (done) "done",
                   if (status[k] != "ok") status[k])
      shiny::tags$a(class = cls, title = if (status[k] != "ok") tips[[status[k]]] else st[[k]]$question,
                    onclick = sprintf("Shiny.setInputValue('guide_goto', '%s', {priority: 'event'})", tabs[k]),
                    shiny::span(class = "dot", if (done) "\u2713" else k),
                    shiny::span(st[[k]]$short),
                    if (status[k] == "unavailable") shiny::span(class = "opt", "(not available)") else
                      if (status[k] == "limited") shiny::span(class = "opt", "(limited)") else
                        if (st[[k]]$optional) shiny::span(class = "opt", "(optional)"))
    })
    seps <- lapply(seq_along(pills), function(i) if (i < length(pills)) shiny::span(class = "guide-sep", "\u203a"))
    start <- shiny::tags$a(class = paste("guide-pill", if (cur == "start") "current"),
                           onclick = "Shiny.setInputValue('guide_goto', 'start', {priority: 'event'})",
                           shiny::span(class = "dot", "?"), shiny::span("Start here"))
    shiny::div(class = "guide-stepper", start, shiny::span(class = "guide-sep", "\u203a"),
               do.call(shiny::tagList, as.list(rbind(pills, seps))))
  })

  # findings as data (named list: heading -> sentences), shared by UI and report
  fw <- "What we found"
  findings <- list(
    function() {
      a <- r$analysis()
      out <- stats::setNames(list(guide_data_text(a$kind, a$label, r$responses(), a$factors)), fw)
      if (length(a$notes)) out[["Data checks"]] <- a$notes
      out
    },
    function() {
      a <- r$analysis()
      m <- suppressWarnings(mpi_multi(r$mpi_input(a), norm = input$norm, abs = identical(input$signed, "abs"),
                                      factors = a$factors))
      out <- stats::setNames(list(guide_mpi_text(m)), fw)
      an <- tryCatch(guide_anatomy_text(mpi_contributions(m, response = r$view_resp()), r$view_resp(), input$norm),
                     error = function(e) NULL)
      if (length(an) && "flows" %in% r$used()) out[["Where it comes from"]] <- an
      ct <- if (!is.null(r$compare_text)) r$compare_text() else NULL
      if (length(ct)) out[["Compared with the snapshot"]] <- ct
      out
    },
    function() {
      if (!caps()$intervals) {
        return(list(`Not available for this data` =
          "Intervals need fitted models (raw data) or a coefficient table with standard errors."))
      }
      lines <- guide_interval_text(r$unc_mpi())
      fl <- tryCatch(r$int_flags(), error = function(e) NULL)
      if (!is.null(fl) && any(!fl$reliable)) {
        lines <- c(lines, if (identical(input$bounds, "se"))
          "Caution: SE soft-threshold intervals are a screening aid, not confidence intervals." else
          sprintf("Caution: the delta-method interval may be too narrow for %s (a term is not clearly non-zero); the ellipsoid interval keeps its coverage.",
                  .join_words(unique(fl$factor[!fl$reliable]))))
      }
      lines <- c(lines, if (!caps()$bootstrap) "The bootstrap needs raw data (step 1)." else
        if (is.null(r$boot_state())) "Press 'Run bootstrap' to check whether the ranking holds when the experiment is replayed." else
          guide_rank_text(r$boot_res()))
      stats::setNames(list(lines), fw)
    },
    function() {
      if (!caps()$selection) {
        return(list(`Limited for this data` =
          "Selection paths need raw data; the norm comparison shows whether rankings depend on how terms are added up."))
      }
      if (is.null(r$sel_state())) {
        return(list(`Not run yet` = "Press 'Run selection paths' to see whether the conclusions depend on how the model is simplified."))
      }
      stats::setNames(list(guide_selection_text(r$sel_res())), fw)
    },
    function() {
      a <- r$analysis()
      fu <- r$followup_mpi()
      changed <- length(attr(fu, "ranges")) > 0 || length(unique(fu$factor)) < length(a$factors)
      fu_lines <- if (changed) {
        before <- suppressWarnings(mpi_multi(r$mpi_input(a), abs = TRUE, factors = a$factors))
        guide_followup_text(before, fu)
      } else {
        "Nothing changed yet: narrow a factor's range or untick factors in 'Ranges & subset' to see the effect."
      }
      f <- tryCatch(r$feas_res(), error = function(e) e)
      fe_lines <- if (inherits(f, "error")) {
        conditionMessage(f)
      } else if (!(length(attr(f, "region")) > 0 || nzchar(trimws(input$constraint %||% "")))) {
        "No restriction set: give narrower ranges or a constraint (e.g. x1 + x2 <= 0) in 'Feasible region'."
      } else {
        cs <- tryCatch(r$conv_status(), error = function(e) list(applies = FALSE))
        c(paste0(input$feas_response, ": ", guide_feasible_text(f)),
          if (isTRUE(cs$applies)) {
            if (!isTRUE(cs$checked)) "Convergence not checked: press 'Check convergence' before reporting these sampled values." else
              if (length(cs$unstable)) sprintf("Not yet stable for %s: more samples are needed; report as lower estimates only.", .join_words(cs$unstable)) else
                "The sampled values are stable under this sampling (numerically stable lower estimates)."
          })
      }
      list(`Ranges & subset` = fu_lines, `Feasible region` = fe_lines)
    },
    function() {
      stats::setNames(list(c(
        "The script below recreates the influence map, the intervals, the bootstrap and the follow-up with the settings of the results on screen.",
        "Download it and keep it with your report; running it in R gives the same numbers.")), fw)
    }
  )
  findings_safe <- function(k) {
    tryCatch(findings[[k]](), error = function(e) {
      msg <- conditionMessage(e)
      list(`Not available yet` = if (nzchar(msg)) msg else "Complete the previous steps first.")
    })
  }
  render_groups <- function(g) {
    do.call(shiny::tagList, unlist(lapply(names(g), function(h) {
      list(shiny::h5(h), shiny::tags$ul(lapply(g[[h]], shiny::tags$li)))
    }), recursive = FALSE))
  }
  for (k in seq_along(st)) local({
    kk <- k
    output[[paste0("guide_find_", kk)]] <- shiny::renderUI(render_groups(findings_safe(kk)))
  })

  list(visited = visited, findings = findings_safe, stage_status = stage_status)
}
