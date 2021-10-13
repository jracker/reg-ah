prep_stn_data <- function(stn_data) {
  # stn_data <- qnat_posto
  #checkmate::assert_true(data.table::uniqueN(stn_data[["code_stn"]]) == 1)
  
  #dailyQ <- dplyr::select(stn_data, -code_stn) %>%
  dailyQ <- stn_data %>%
    dplyr::rename("discharge" = qnat) %>%
    dplyr::filter(!is.na(discharge)) %>%
    as.data.frame()
  
  x <- dailyQ %>%
    dplyr::mutate(
      year_val = EflowStats::get_waterYear(date, numeric = T),
      day = EflowStats::get_waterYearDay(date),
      leapYear = EflowStats:::is.leapyear(as.numeric(as.character(year_val)))
    )
  
  fullYearCheck <- dplyr::summarize(dplyr::group_by(x, year_val),
                                    completeYear =
                                      if (!any(leapYear)) {
                                        ifelse(length(day) == 365, T, F)
                                      } else if (any(leapYear)) {
                                        ifelse(length(day) == 366, T, F)
                                      }
  )
  
  incomplete_yrs <- fullYearCheck %>%
    dplyr::filter(!completeYear) %>%
    dplyr::pull(year_val)
  
  dailyQClean <- x %>%
    dplyr::filter(!year_val %in% incomplete_yrs)
  
  tibble::as_tibble(dailyQClean)
}


magnif7 <- function(stn_data) {
  stn_data %>%
    prep_stn_data() %>%
    as.data.frame() %>%
    calc_magnifSeven(yearType = "water", digits = 3) %>%
    #mutate(code_stn = stn_data$code_stn[1]) %>%
    as_tibble() #%>%
    #dplyr::relocate(code_stn)
}

