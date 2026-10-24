
library(pdftools)
library(tidyverse)   # dplyr, tidyr, purrr (>= 1.0), stringr, lubridate (>= 2.0)

# =============================================================================
# Parser: one AcroCompanion "rankings" PDF -> one tidy table
#   - Qualification pages: 2 rows per athlete (exercise 1 and 2)
#   - Final pages:         1 row per athlete (exercise = NA)
#   - DNS / DNF / DSQ:     kept, flagged in `status`
# =============================================================================
parse_trampoline_pdf <- function(path) {
    
    # ---- 1. Every word with its position --------------------------------------
    words <- pdf_data(path) |>
        imap(\(df, i) mutate(as_tibble(df), page = i)) |>
        list_rbind() |>
        mutate(id     = row_number(),
               ym     = y + height / 2,
               is_dec = str_detect(text, "^\\d+\\.\\d{3}$"))
    
    # ---- 2. Header column positions, per page ---------------------------------
    # Pages without an "Rn" header word (legend-only pages) drop out here.
    rn <- words |>
        filter(text == "Rn") |>
        slice_min(ym, n = 1, by = page, with_ties = FALSE) |>
        select(page, rn_y = ym)
    
    hdr <- words |>
        inner_join(rn, by = "page") |>
        filter(abs(ym - rn_y) < 10) |>
        summarise(name_x  = x[text == "Name"][1],
                  birth_x = x[text == "Birth"][1],
                  club_x  = x[text == "Club"][1],
                  noc_x   = x[text == "NOC"][1],
                  q_x     = x[text == "Q"][1],        # NA on pages without a Q column
                  has_ex  = any(text == "Exercise"),  # FALSE on single-line (final) pages
                  rn_y    = first(rn_y),
                  .by = page)
    
    bottom <- words |>
        inner_join(rn, by = "page") |>
        filter(is_dec, ym > rn_y) |>
        summarise(bottom_y = max(ym) + 25, .by = page)
    
    hdr <- left_join(hdr, bottom, by = "page")
    
    # ---- 3. Rank cells (1, 2, ..., HC, or "-" for DNS/DNF) = one anchor each ---
    ranks <- words |>
        inner_join(hdr, by = "page") |>
        filter(str_detect(text, "^(\\d{1,3}|HC|-)$"),
               x < coalesce(q_x, name_x) - 5,
               ym > rn_y + 5) |>
        transmute(id, page, ry = ym, rank = text,
                  rank_num = suppressWarnings(as.integer(text)))
    
    legend <- words |>
        filter(str_starts(text, "Legend")) |>
        summarise(legend_y = min(ym), .by = page)
    
    # ---- 4. Give each word to the closest rank ABOVE it -----------------------
    aw <- words |>
        filter(!id %in% ranks$id) |>
        inner_join(hdr, by = "page") |>
        left_join(legend, by = "page") |>
        mutate(legend_y = replace_na(legend_y, Inf),
               ymk = ym + 4) |>
        filter(ym < legend_y, ym < bottom_y) |>
        inner_join(ranks |> select(page, ry, rank, rank_num),
                   by = join_by(page, closest(ymk >= ry)))
    
    # ---- 5. Which column is each word in? -------------------------------------
    aw <- aw |>
        mutate(col = case_when(
            is_dec                                              ~ "num",
            str_detect(text, "^(DNS|DNF|DSQ)$")                 ~ "status",
            str_detect(text, "^[A-Z]{3}$") &
                x > noc_x - 15 & x < noc_x + 20                   ~ "noc",
            x < name_x  - 2                                     ~ "q",
            x < birth_x - 2                                     ~ "name",
            x < club_x  - 2                                     ~ "birth_year",
            x < noc_x   - 15                                    ~ "club",
            TRUE                                                ~ "skip"))   # "Exercise" digits
    
    # ---- 6. Text columns: one row per athlete ---------------------------------
    info <- aw |>
        filter(!col %in% c("num", "skip")) |>
        arrange(page, ry, ym, x) |>
        summarise(text = str_c(text, collapse = " "),
                  .by = c(page, ry, rank, rank_num, col)) |>
        pivot_wider(names_from = col, values_from = text) |>
        mutate(name = str_replace_all(name, "-\\s+", "-"))   # "Matembele- Lyon" -> "Matembele-Lyon"
    
    if (!"status" %in% names(info)) info$status <- NA_character_
    if (!"q"      %in% names(info)) info$q      <- NA_character_
    info <- mutate(info, status = str_extract(status, "[A-Z]+$"))   # last token wins (DNF over DNS)
    
    # ---- 7. Score columns -----------------------------------------------------
    # Two-exercise pages: line 1 = d e h t pen score total, line 2 = d e h t pen score
    # Single-line pages:  d e h t pen total
    scores <- aw |>
        filter(col == "num") |>
        mutate(exercise = if_else(has_ex, 1L + as.integer(ym - ry > 9), NA_integer_)) |>
        arrange(page, ry, exercise, x) |>
        mutate(idx = row_number(), .by = c(page, ry, exercise)) |>
        mutate(k = case_when(
            has_ex ~ c("d", "e", "h", "t", "pen", "score", "total")[idx],
            TRUE   ~ c("d", "e", "h", "t", "pen", "total", NA)[idx])) |>
        filter(!is.na(k)) |>
        select(page, ry, exercise, k, text) |>
        pivot_wider(names_from = k, values_from = text) |>
        mutate(total = first(na.omit(total)), .by = c(page, ry))      # total printed once per athlete
    
    # ---- 8. Competition details per page --------------------------------------
    txt <- pdf_text(path)
    date_rx <- "^(\\d{2}/\\d{2}/\\d{4}) - (\\d{2}/\\d{2}/\\d{4}) / (.+)$"
    
    empty_header <- tibble(competition = NA_character_, start_date = as.Date(NA),
                           end_date = as.Date(NA), location = NA_character_,
                           discipline = NA_character_, stage = NA_character_,
                           event = NA_character_)
    
    get_header <- function(page_txt) {
        lines <- str_squish(str_split(page_txt, "\n")[[1]])
        lines <- lines[lines != "" & !str_starts(lines, "__Marker__")]
        d <- which(str_detect(lines, date_rx))[1]
        r <- which(str_starts(lines, "Results:"))[1]
        if (is.na(d) || is.na(r) || d < 2) return(empty_header)   # legend-only / empty page
        m <- str_match(lines[d], date_rx)
        tibble(competition = lines[d - 1],
               start_date  = dmy(m[2]),
               end_date    = dmy(m[3]),
               location    = m[4],
               discipline  = lines[d + 1],
               stage       = lines[r - 1],
               event       = str_remove(lines[r], "^Results: "))
    }
    
    page_info <- tibble(page = seq_along(txt), txt = txt) |>
        mutate(header = map(txt, get_header)) |>
        unnest(header) |>
        filter(!is.na(event)) |>
        left_join(ranks |>
                      summarise(first_rank = rank_num[which.min(ry)], .by = page),
                  by = "page") |>
        arrange(page) |>
        mutate(changed = stage != lag(stage) | event != lag(event) |
                   coalesce(first_rank == 1, FALSE),      # new block, or ranks restart at 1
               section = cumsum(replace_na(changed, TRUE))) |>
        select(page, competition, location, start_date, end_date,
               discipline, section, stage, event)
    
    # ---- 9. Final table -------------------------------------------------------
    info |>
        left_join(scores,    by = c("page", "ry")) |>
        left_join(page_info, by = "page") |>
        mutate(across(any_of(c("birth_year", "d", "e", "h", "t", "pen", "score", "total")),
                      as.numeric)) |>
        arrange(page, ry, exercise) |>
        select(any_of(c("competition", "location", "start_date", "end_date", "discipline",
                        "section", "stage", "event", "rank", "rank_num", "q", "name",
                        "birth_year", "club", "noc", "status", "exercise",
                        "d", "e", "h", "t", "pen", "score", "total")))
}

# =============================================================================
# Validation: uses the arithmetic printed in the PDFs. Every element should be
# an empty tibble if the parse is correct.
# =============================================================================
validate_results <- function(results, tol = 0.002) {
    list(
        # each score line = D + E + H + T - Pen
        score_arithmetic = results |>
            filter(!is.na(score)) |>
            filter(abs(d + e + h + t - pen - score) > tol) |>
            distinct(file, event, stage, name),
        
        # single-line (final) pages: total = D + E + H + T - Pen
        final_arithmetic = results |>
            filter(is.na(exercise), is.na(status), !is.na(total)) |>
            filter(abs(d + e + h + t - pen - total) > tol) |>
            distinct(file, event, stage, name),
        
        # two-exercise pages: every athlete without a status has exactly 2 rows
        exercise_count = results |>
            filter(!is.na(exercise), is.na(status)) |>
            count(file, event, stage, section, rank, name) |>
            filter(n != 2),
        
        # key fields present
        missing_fields = results |>
            filter(is.na(name) | is.na(noc) | is.na(club) | is.na(birth_year)) |>
            distinct(file, event, rank, name),
        
        # no rank used twice in a section (HC and "-" may repeat)
        duplicate_ranks = results |>
            filter(!rank %in% c("HC", "-")) |>
            distinct(file, section, rank, name) |>
            count(file, section, rank) |>
            filter(n > 1)
    )
}

# =============================================================================
# Run
# =============================================================================

 files <- list.files("Data/acro_companion", pattern = "\\.pdf$", full.names = TRUE)

results <- files |>
    set_names(basename(files)) |>
    map(parse_trampoline_pdf) |>
    list_rbind(names_to = "file")

checks <- validate_results(results)
print(map_int(checks, nrow))   # all zeros = parse is consistent with the PDFs

# Inspect anything that failed:
# checks$score_arithmetic
# results |> filter(!is.na(status))




