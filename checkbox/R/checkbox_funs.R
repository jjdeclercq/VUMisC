


gen_checkbox_data <- function(ID, rep, by.var = c("Site A", "Site B"),  n.boxes = 12, PROB = 0.2) {
  map(1:rep, \(R) data.frame(id = ID, checkbox = "checkbox",
                             x = sample(c(0,1),n.boxes, prob = c(1-PROB, PROB), replace = TRUE)) %>%
        mutate(nv = as.character(1:n()))%>%
        mutate(rri = R)) %>%
    list_rbind() %>% mutate(site = sample(by.var, 1))
}

gen_checkbox_df <- function(ids, reps, by.var = c("Site A", "Site B"),  n.boxes = 12, PROB = 0.2){

  trib.x <- tibble::tribble(
    ~nv,     ~check,
    "1",    "Mouse",
    "2",      "Rat",
    "3",      "Cat",
    "4",      "Dog",
    "5",   "Snake",
    "6",   "Monkey",
    "7",   "Turtle",
    "8", "Lizard",
    "9", "Blue jay",
    "10",    "Turkey",
    "11",    "Other",
    "12",     "None")

  map2(.x = ids, .y = reps,
       \(x, y) gen_checkbox_data(x, y, n.boxes = n.boxes, PROB = PROB, by.var = by.var ) )  %>%
    list_rbind()%>%
    left_join(., trib.x, by = "nv") %>%
    mutate(checkbox = paste(checkbox, nv, sep = "_"), check = ifelse(x==1, check, NA)) %>%
    select(-x, -nv) %>%
    pivot_wider(., names_from = "checkbox", values_from = "check", values_fill = NA)

}


#' Lookup Names
#'
#' This function retrieves labels from a data frame based on a selection criteria.
#'
#' @param dat The data frame from which to retrieve labels.
#' @param selection The selection criteria to filter labels.
#' @param simple Logical indicating whether to simplify the labels.
#' @param add.zero Logical indicating whether to add zero selections.
#' @param tidy.labels Logical indicating whether to tidy the labels.
#' @param str.rm String to remove from labels (optional).
#'
#' @return A named character vector with retrieved labels.
#'
#' @examples
#' # Basic usage
#' lookup_names(my_data, "criteria")
#'
#' # Specify additional options
#' lookup_names(my_data, "criteria", simple = TRUE, add.zero = TRUE, tidy.labels = TRUE)
#'
#' @export
lookup_names <- function(dat, selection, simple = FALSE, add.zero = FALSE, tidy.labels = FALSE,
                         str.rm = NULL){

  # Check if 'dat' is a data frame
  if (!is.data.frame(dat)) {
    stop("'dat' must be a data frame.")
  }

  # Check if 'selection' is a character string
  if (!is.character(selection) || length(selection) != 1) {
    stop("'selection' must be a single character string.")
  }

  # Check if 'simple', 'add.zero', and 'tidy.labels' are logical
  if (!is.logical(simple) || !is.logical(add.zero) || !is.logical(tidy.labels)) {
    stop("'simple', 'add.zero', and 'tidy.labels' must be logical values.")
  }

  dat_labels <- collect.labels(dat) %>%
    filter(grepl(selection, variable)) %>%
    { if(isTRUE(tidy.labels)) mutate(., label = gsub('.{1}$', '', gsub(".*=", "", label))) else .} %>%
    { if(!is.null(str.rm)) mutate(., label = gsub(str.rm, '',label)) else .} %>%
    { if(isTRUE(add.zero)) bind_rows(., data.frame(variable = paste0(selection, "_zero"), label = "[Zero selected]")) else .} %>%
    mutate(
      any = paste(label, "(any)"),
      total = paste(label, "(total)"),
      any_var = paste0("any_", variable),
      total_var = paste0("total_", variable)
    )

  lookup.any <- dat_labels$any_var
  names(lookup.any) <- case_when(isTRUE(simple)~ dat_labels$label, TRUE ~ dat_labels$any)
  lookup.total <- dat_labels$total_var
  names(lookup.total) <- case_when(isTRUE(simple)~ dat_labels$label, TRUE ~ dat_labels$total)

  return(c(lookup.any, lookup.total))
}





#' Reorder columns in a data frame based on frequency of data.
#'
#' This function reorders the columns in a data frame based on a set of patterns
#' provided in the `selection` argument. Columns matching the patterns are moved
#' to the front of the data frame, while other columns remain in their original order.
#'
#' @param dat A data frame.
#' @param selection Character vector of patterns to match column names.
#'
#' @return A data frame with columns reordered based on the patterns in `selection`.
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' dat <- data.frame(a = 1:5, b_x = 6:10, c_x_y = 11:15, d_z = 16:20)
#' reordered_dat <- reorder_cols(dat, c("x", "z"))
#' # Columns matching "x" and "z" patterns are moved to the front.
#'
reorder_cols <- function(dat, selection) {
  # Check if the input is a data frame
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a data frame.")
  }

  # Check if the selection is a character vector
  if (!is.character(selection)) {
    stop("Input 'selection' must be a character vector.")
  }

  # Check if the selection patterns match any columns
  matching_cols <- select(dat, contains(selection)) %>% names()
  if (length(matching_cols) == 0) {
    stop("No columns match the provided selection patterns.")
  }

  static.cols <- dat %>% select(-contains(selection)) %>% names()

  col.order <- dat %>%
    summarise(across(contains(selection), ~ sum(!is.na(.x))), x = 1) %>%
    pivot_longer(., -x) %>%
    arrange(desc(value))

  return(dat %>% select(all_of(static.cols), all_of(col.order$name)))
}



#' Summaries of checkbox variables
#'
#' This function performs many common manipulations needed to adequately summarize
#'
#' @param dat A data frame.
#' @param selection Character string to identify set of checkbox variables for inclusion (usually a prefix)
#' @param id.var ID variable name
#' @param by.var Stratification variable (optional)
#' @param count One of "any", "total" or "both". With non-repeating data, 'any' and 'total' are equivalent. However, setting `count = "total"` returns numeric columns for the total number of times the box was selected within each ID.
#' @param repeat.var Variable capturing the repeat instance.
#' @param stats Setting to "only" will produce of ID-level summaries of the checkbox selections. Any other character string will include both the `count` designation as well as the stats.
#' @param tidy.labels Often redcap variable labels for checkboxes contain a lot of extraneous information. Adjusting `tidy.labels = TRUE` should fix this assuming the pattern is of "Long description (choice=DATA ITEM)"
#' @param toggle.names Logical.Setting to `TRUE` will use the variable labels rather than the variable names.
#' @param simple.names Logical. Changing to `TRUE` will clean up the trailing descriptor. Note, this feature does not work if `count = "both"`.
#' @param add.zero Even if there is a checkbox specifically for "none" or "none of the above", there is still room for data entry errors. Setting `add.zero = TRUE` will append a new row to the data, and any record with no other selections will be counted here.
#' @param output Either 'df' or 'summary'. Setting `output = 'df'` returns a data frame that can be used for further analysis. Columns may be more suited to other summary tables or modeling.
#' @param reorder.cols Logical. Reorder the data columns based on frequency of selections.
#' @param str.rm If the label pattern is not consistent with the pattern described in `tidy.labels` Utilize the `str.rm` argument by passing the exact string you want removed. This utilizes regular expressions via `gsub`. If too cumbersome to do within the function call, relabeling the data is also an option.
#' @param ... Additional arguments passed to `jgt()`
#'

checkbox_gather <- function(dat, selection, id.var = NULL, by.var = NULL, repeat.var = NULL, count = "any",
                            stats = "none", tidy.labels = FALSE,
                            toggle.names = FALSE, simple.names = FALSE, add.zero = FALSE,
                            output = "summary",reorder.cols = FALSE, str.rm= NULL,
                            remove.zero.selected = FALSE, ...){

  if(isTRUE(remove.zero.selected)) {add.zero <- TRUE}

  ## For matching variable class
  d.class <- dat %>% select(any_of(c(id.var, repeat.var, by.var)) ) %>%
    sapply(., class)

  int <- dat %>% unite(.,"ID", c(id.var, repeat.var, by.var), sep = "_", remove = FALSE)
  key <- int %>% select(ID, any_of(c(id.var, repeat.var, by.var))) %>% distinct()

  ## transform factors to character
  int %<>% rowwise() %>% mutate(across(any_of(c(id.var, repeat.var, by.var, contains(selection))),
                        ~ifelse(is.factor(.x), as.character(.x), .x))) %>% ungroup()




  if(reorder.cols == TRUE){
    int %<>% reorder_cols(., selection = selection)
  }

  if(!is.null(str.rm)){
    tidy.labels <- FALSE
  }


  ## For toggle.names data must be labelled
  lookup <- lookup_names(dat = int, selection = selection, add.zero = add.zero, tidy.labels = tidy.labels,
                         str.rm = str.rm,
                         simple = (case_when(count %in% "both" ~ FALSE,
                                             TRUE ~ simple.names)))

  ## account for rows with nothing selected
  int <- int %>% group_by(ID) %>%
        mutate(zero = sum(!is.na(across(contains(selection)))), zero = ifelse(zero ==0, "[Zero selected]", NA))


    int[, paste0(selection, "_zero")] <- int$zero

    stat_names <- c('Number of observations (rows)' = "n.obs",
                    'No selections made across all observations' = "n.all.none",
                    'Number of selections made' = "n.selected",
                    'Number of unique selections made' = "n.unique")


  int <- int %>%
    group_by(ID) %>%
    summarise(across(contains(selection), ~sum(!is.na(.x)), .names = "total_{.col}"),
              n.obs =n(),
              n.all.none = all(!is.na(zero))) %>%
    rowwise() %>%
    mutate(across(contains(selection), ~yesno(.x>0), .names = 'any{stringr::str_remove(.col, "total")}'),
           n.selected = sum(across(c(contains("total"), -any_of(paste0("total_", selection, "_zero"))))),
           n.unique = sum(across(c(contains("total"), -any_of(paste0("total_", selection, "_zero"))))>0)) %>% ungroup() %>%
    {if(count %in% c( "any")) select(., -contains("total")) else .} %>%
    {if(count %in% c("total")) select(., -contains("any")) else .} %>%
    {if(nrow(.) == nrow(dat)) select(., -n.unique, -n.obs) else .} %>%
    {if(stats %in% c("none")) select(., -starts_with("n.")) else .} %>%
    {if(stats %in% c("only")) select(., ID, starts_with("n.")) else .} %>%
    {if(isTRUE(toggle.names)) rename(., any_of(c(lookup, stat_names))) else .} %>%
    {if(!isTRUE(add.zero)) select(., -contains("zero"), -contains("Zero")) else .}


  ## Put back in original order
  int <- key %>% left_join(., int, by = "ID") %>% select(-ID)%>%
    {if(isTRUE(remove.zero.selected)) filter(., `[Zero selected]` == "No") %>%
        select(., -contains("zero"), -contains("Zero")) else .}

  if(output == "df"){
     return(int)
  } else {


  jgt(int %>% select(-any_of(c(id.var, repeat.var))),
      by = by.var,
      overall = 1*(!is.null(by.var)),
      one_row_binary = TRUE,
      add.n = FALSE, ...)

  }

}

checkbox_tally <- function(dat, selection, id.var = NULL, by.var = NULL, col.name = "Selection", add.zero = FALSE, ...){

    int <- dat %>%
      {if(is.null(by.var)) . else mutate(., byv = paste0("",!!sym(by.var)))} %>%
      select(any_of(c(id.var, by.var)), contains(selection)) %>%
      {if(isTRUE(add.zero)) rowwise(.) %>%
          mutate(zero = sum(!is.na(across(contains(selection)))), zero = ifelse(zero ==0, "[Zero selected]", NA)) else .} %>%
      pivot_longer(., -any_of(c(id.var, by.var)), values_drop_na = TRUE) %>%
      select(value, by.var)

    names(int)[1] <- col.name

    jgt(int, order.cat = TRUE, by = by.var, overall = 1*(!is.null(by.var)),  ...)

}


checkbox_intersect <- function(dat, selection, id.var, repeat.var = NULL, toggle.names = TRUE, add.zero = FALSE, ...){

  checkbox_gather(dat = dat, selection = selection, id.var = id.var, repeat.var = repeat.var,
                  count = "any", stats = "none", simple.names = TRUE,
                  toggle.names = toggle.names, add.zero = add.zero, output = "df", ...) %>%
    select(-any_of(c(id.var, repeat.var)))%>%
    mutate(across(everything(), ~ifelse(.x == "Yes", 1, 0))) %>%
    as.data.frame() %>% UpSetR::upset(., nsets = 20, nintersects = 30)

}

checkbox_merge <- function(dat, selection, list_vars){

  for(i in 1:length(list_vars)){

    new_var <- paste0(selection, "m",paste0(unlist(list_vars[i]), collapse = ""))
    merge_vars <- paste0(selection, paste0(unlist(list_vars[i])))

    dat[,new_var] <- ifelse(rowSums(!is.na(dat[,merge_vars]))>0, names(list_vars)[i], NA)

    Hmisc::label(dat[[new_var]]) <- names(list_vars)[i]

    dat <- dat %>% select(-any_of(merge_vars))
  }

  dat %>% clear.label.class()
}
