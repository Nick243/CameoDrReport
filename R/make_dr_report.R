#' Generate CAMEO Diet Recall Report
#'
#' This function merges CDART eligibility data with REDCap dietary recall data
#' to generate a tracking report for the CAMEO study.
#'
#' @param cdart_path String. Path to the CDART Excel export (e.g., "cdart_report.xlsx").
#' @param redcap_path String. Path to the REDCap raw CSV export.
#' @param out_xlsx String. Output path for the generated Excel report.
#' @param as_of Date. The reference date for the report. Defaults to Sys.Date().
#' @param lag_days Integer. Number of days to lag the reporting. Defaults to 30.
#' @param apply_lag Logical. Whether to apply the lag date logic. Defaults to TRUE.
#'
#' @return Invisible list containing the summary tables and processed dataframes.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @import readxl
#' @import openxlsx
#' @import janitor
#' @importFrom readr read_csv
make_dr_report <- function(
    cdart_path,
    redcap_path,
    out_xlsx,
    as_of = Sys.Date(),
    lag_days = 30,
    apply_lag = TRUE
) {
  
  cutoff_date <- as_date(as_of)
  if (apply_lag) cutoff_date <- cutoff_date - days(lag_days)
  
  # ----------------------------
  # 1) CDART eligibility
  # ----------------------------
  cdart <- read_excel(cdart_path, skip = 2) |>
    clean_names() |>
    rename(
      subjectid   = participant_id,
      ta_dt       = date_of_phase_2_ta,
      ta14_dt     = date_of_phase_2_ta14,
      ta52_dt     = date_of_phase_2_taea_52,
      withdraw_dt = date_of_study_withdrawal_if_applicable
    ) |>
    mutate(
      subjectid = as.character(subjectid),
      site = str_sub(subjectid, 1, 3),
      across(c(ta_dt, ta14_dt, ta52_dt, withdraw_dt), parse_mixed_date)
    )
  
  elig_due_long <- cdart |>
    pivot_longer(
      cols = c(ta_dt, ta14_dt, ta52_dt),
      names_to = "time",
      values_to = "visit_date"
    ) |>
    filter(!is.na(visit_date)) |>
    mutate(
      time = recode(time,
                    ta_dt = "TA",
                    ta14_dt = "TA_14",
                    ta52_dt = "TA_52"
      ),
      eligible = if_else(is.na(withdraw_dt) | withdraw_dt >= visit_date, 1L, 0L)
    ) |>
    filter(eligible == 1L) |>
    filter(visit_date <= cutoff_date) |>
    select(site, subjectid, time, visit_date, withdraw_dt)
  
  # ----------------------------
  # 2) REDCap recalls
  # ----------------------------
  redcap_raw <- readr::read_csv(redcap_path, show_col_types = FALSE)
  
  dr_scored <- redcap_raw |>
    select(
      record_id, study_id, redcap_event_name,
      dr1yn, dr2yn, dr3yn,
      dr1dat, dr2dat, dr3dat
    ) |>
    mutate(
      # Coalesce ID to handle inconsistent record_id usage
      subjectid = coalesce(as.character(study_id), as.character(record_id)) |> str_trim(),
      site = str_sub(subjectid, 1, 3),
      across(c(dr1dat, dr2dat, dr3dat), parse_mixed_date),
      across(c(dr1yn, dr2yn, dr3yn), ~ as.integer(replace_na(.x, 0L)))
    ) |>
    separate(redcap_event_name, into = c("time_raw", "rest"), sep = "__", fill = "right") |>
    mutate(time = event_to_time(time_raw)) |>
    filter(!is.na(time), !is.na(subjectid)) |>
    semi_join(
      elig_due_long |> distinct(subjectid, site, time),
      by = c("subjectid", "site", "time")
    ) |>
    left_join(cdart |> select(subjectid, withdraw_dt), by = "subjectid") |>
    mutate(
      dr1 = if_else(dr1yn == 1L & !is.na(dr1dat) & (is.na(withdraw_dt) | dr1dat <= withdraw_dt), 1L, 0L),
      dr2 = if_else(dr2yn == 1L & !is.na(dr2dat) & (is.na(withdraw_dt) | dr2dat <= withdraw_dt), 1L, 0L),
      dr3 = if_else(dr3yn == 1L & !is.na(dr3dat) & (is.na(withdraw_dt) | dr3dat <= withdraw_dt), 1L, 0L),
      dr_sum = dr1 + dr2 + dr3,
      dr_any = as.integer(dr_sum > 0L),
      dr_all = as.integer(dr_sum == 3L)
    ) |>
    group_by(subjectid, site, time) |>
    summarise(
      dr_any = as.integer(any(dr_any == 1L, na.rm = TRUE)),
      dr_all = as.integer(any(dr_all == 1L, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # ----------------------------
  # 3) Merge + summary + missing listing
  # ----------------------------
  df <- elig_due_long |>
    left_join(dr_scored, by = c("subjectid", "site", "time")) |>
    mutate(
      dr_any = replace_na(dr_any, 0L),
      dr_all = replace_na(dr_all, 0L)
    )
  
  summary_tbl <- df |>
    group_by(site, time) |>
    summarise(
      `Number Eligable` = n(),
      `Number with 1DR` = sum(dr_any == 1L),
      `Number with 3DR` = sum(dr_all == 1L),
      .groups = "drop"
    ) |>
    mutate(`Number Missed` = `Number Eligable` - `Number with 1DR`) |>
    select(
      Site = site,
      Time = time,
      `Number Eligable`,
      `Number with 1DR`,
      `Number Missed`,
      `Number with 3DR`
    ) |>
    arrange(factor(Time, levels = c("TA", "TA_14", "TA_52")), Site)
  
  missing_listing <- df |>
    filter(dr_any == 0L) |>
    transmute(
      Site = site,
      Time = time,
      ID = subjectid,
      `Visit Date` = visit_date
    ) |>
    arrange(Site, factor(Time, levels = c("TA", "TA_14", "TA_52")), ID)
  
  ta    <- summary_tbl |> filter(Time == "TA")
  ta_14 <- summary_tbl |> filter(Time == "TA_14")
  ta_52 <- summary_tbl |> filter(Time == "TA_52")
  
  # ----------------------------
  # 4) Write workbook
  # ----------------------------
  wb <- createWorkbook()
  addWorksheet(wb, "TA"); writeData(wb, "TA", ta)
  addWorksheet(wb, "TA_14"); writeData(wb, "TA_14", ta_14)
  addWorksheet(wb, "TA_52"); writeData(wb, "TA_52", ta_52)
  addWorksheet(wb, "missing_listing"); writeData(wb, "missing_listing", missing_listing)
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  
  invisible(list(
    cutoff_date = cutoff_date,
    summary = summary_tbl,
    missing_listing = missing_listing,
    person_time = df
  ))
}

# --- Internal Helper Functions ---

parse_mixed_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as_date(x))
  x <- as.character(x)
  parse_date_time(x, orders = c("dby", "dmy", "ymd", "mdy"), quiet = TRUE) |>
    as_date()
}

event_to_time <- function(x) {
  # Helper to handle NULLs cleanly
  x <- toupper(if (is.null(x)) "" else x)
  case_when(
    x == "TA" ~ "TA",
    x %in% c("14", "TA14", "TA_14") ~ "TA_14",
    x %in% c("EA", "52", "TA52", "TA_52", "TAEA", "TAEA52", "TAEA/52") ~ "TA_52",
    TRUE ~ NA_character_
  )
}