
ac <- as.character
lw <- function(x) length(which(x))
lu <- function(x) length(unique(x))
med.iqr <- function(x) paste0(median(x)," (", quantile(x, 0.25), ", ",quantile(x, 0.75), ")")
mean.sd <- function(x) paste0(round(mean(x),2)," (", round(sd(x), 2), ")")
## easy yes-no of binary variables
yesno <- function(logic) {ifelse(logic, "Yes", "No")}

j.scribe <- function(x, desc, scroll = TRUE) {
  if(compile=="latex"){
    latex(Hmisc::describe(x, descript = desc), file = "")
  }
  else{Hmisc::html(Hmisc::describe(x, descript = desc), file = "", scroll = scroll)}
}


#' Helper function to add factor labels
#'
#' Given a data frame and a vector of variable names
#' this function will return a block of code that can be copied and pasted into
#' the console for manual editing.
#'
#' @param df The input data frame.
#' @param var A character vector specifying the columns to be factorized.
#'
#' @return Code formatted for user input of factor labels.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' label_factor(iris, c("Species"))
#' }
#'
#' @export
label_factor <- function(df, var){
  open <- paste(deparse(substitute(df)), "<- within(", deparse(substitute(df)), ",{\n")
  close <- "})"
  
  I <- length(var)
  X <- LL <- rep(NA, I)
  
  for(i in 1:I){
    V <- var[i]
    LL[i] <- paste(deparse(substitute(df)), V, sep = "$")
    X[i] <- (paste0(var[i], " %<>% factor(., levels = c('", paste(sort(unique(eval(parse(text = LL[i])))), collapse = "','"), "'), labels = c( ))\n"))
  }
  
  cat(open, X, close)
}

recode_factor <- function(df, var){
  open <- paste0(deparse(substitute(df)), " %>% mutate(", substitute(var), " = forcats::fct_recode(", substitute(var), ",\n")
  
  lvs <- sort(unique(eval(parse(text = paste(deparse(substitute(df)), var, sep = "$")))))
  ll <- data.frame(l = rep('\t\t\t"" = "', length(lvs)), lvs, r = '",\n') %>% mutate(p = paste0(l, lvs, r))
  ll[nrow(ll), "p"] <- gsub(",\n", "))",ll[nrow(ll), "p"])
  
  cat(open, paste0(ll$p) )
}


j.label5 <- function(df, all = FALSE){
  
  prev <- collect.labels(df) %>% mutate(combined = paste0("\t\t", variable, ' = "', label, '"', ',\n'))
  prev[nrow(prev), "combined"] <- gsub(",\n", ")",prev[nrow(prev), "combined"])
  
  if(!all) prev %<>% filter(label == "")
  
  cat(deparse(substitute(df)), "<- labelVector::set_label(", deparse(substitute(df)), ",\n",
      paste0(prev$combined))
}

j.label6 <- function(df, all = FALSE){
  
  prev <- collect.labels(df) %>% mutate(combined = paste0("\t\t", variable, ' = "', label, '"', ',\n'))
  
  if(!all) prev %<>% filter(label == "")
  prev[nrow(prev), "combined"] <- gsub(",\n", ")\n\n",prev[nrow(prev), "combined"])
  
  
  
  output <- capture.output(cat(deparse(substitute(df)), "<- labelVector::set_label(", deparse(substitute(df)), ",\n",
                               paste0(prev$combined)))
  
  # output
  rstudioapi::insertText(output)
}


### Store labels in new dataframe ###
collect.labels <- function(x){
  require(Hmisc)
  require(tibble)
  x <- tibble::rownames_to_column(data.frame(Hmisc::label(x)))
  names(x) <- c("variable", "label")
  x
}

### Remove labels from entire dataframe ###
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

## Remove "labelled" class but preserves attributes
clear.label.class <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    # for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    # attr(x, "label") <- NULL
  }
  return(x)
}


remove.blank.columns2 <- function(dat) {
  names <- dat %>% dplyr::summarise(across(everything(), .fns = ~sum(.x==""|is.na(.x))==nrow(dat)))%>% 
    mutate(i = 1) %>% pivot_longer(.,-i) %>% filter(value %in% c(FALSE, NA)) %$% name
  
  dat[,names]
}


n_pct <- function(x){
  paste(x, " (",round(100*x/sum(x), 1), "%)", sep = "")
}

pc.x <- function(x,...){
  x = sprintf( '%#.1f%%',100*x)
  gsub("NaN%", ".", gsub("NA%", ".", x))
  
}



####### gtsummary custom theme
library(gtsummary)
library(gt)

# my_theme <- list(
#   # default continuous summary type
#   "tbl_summary-str:default_con_type" = "continuous2",
#   
#   # continuous stats
#   "tbl_summary-arg:statistic" = list(all_continuous() ~ 
#     c("{mean} ({sd})",
#     "{median} ({p25} - {p75})",
#     "{min} - {max}"),
#     all_categorical() ~ "{n} ({p})"
#   ),
#   
#   # number style
#   "style_number-arg:big.mark" = "",
#   
#   # percent function
#   "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1),
#   
#   # gt options (use 'as_gt-additional_cmds')
#   "as_gt-lst:addl_cmds" = list(
#     rlang::expr(gt::tab_options(table.font.size = 'small')),
#     rlang::expr(gt::opt_row_striping()),
#     rlang::expr(gt::opt_table_lines("default")),
#     rlang::expr(gt::tab_options(
#       table_body.hlines.color = "white",
#       row.striping.background_color = "#fafafa",
#       source_notes.font.size = 12,
#       table.font.size = 12,
#       # table.width = px(700),
#       heading.align = "left",
#       heading.title.font.size = 16,
#       table.border.top.color = "transparent",
#       table.border.top.width = px(3),
#       data_row.padding = px(4)
#     ))
#   )
# )
theme_gtsummary_compact()
theme_gtsummary_continuous2()
theme_gtsummary_eda()

## They changed the way themes work. How rude!
# set_gtsummary_theme(my_theme)


my_theme2 <-list(
  "tbl_summary-str:default_con_type" = "continuous2",
  "tbl_summary-str:continuous_stat" = c("{median} ({p25} - {p75})"),
  "tbl_summary-str:categorical_stat" = c("{n} ({p})"),
  "style_number-arg:big.mark" = "",
  "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 0),
  "as_gt-lst:addl_cmds" = list(
    tab_spanner = rlang::expr(gt::tab_options(table.font.size = 'small')),
    user_added1 = rlang::expr(gt::opt_row_striping()),
    user_added2 = rlang::expr(gt::opt_table_lines("default")),
    user_added3 = rlang::expr(gt::tab_options(table_body.hlines.color = "white",
                                              row.striping.background_color = "#fafafa",
                                              source_notes.font.size = 12,
                                              table.font.size = 12,
                                              # table.width = px(700),
                                              heading.align = "left",
                                              heading.title.font.size = 16,
                                              table.border.top.color = "transparent",
                                              table.border.top.width = px(3),
                                              data_row.padding = px(4)))
  )
)

## gtsummary updates means this version doesn't work anymore

# jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, 
#                 force.continuous = FALSE, missing = "ifany", missing_text = "Missing",
#                 spanner.size = NULL, spanner.text = NULL, add.n = TRUE, percent = "column", 
#                 one_row_binary = FALSE, ...){
#   
#   if(!is.null(by)){#dat[,by] <- clear.labels(dat[,by])
#     ST <- ifelse(Hmisc::label(dat[,by])=="", by, Hmisc::label(dat[,by]))
#     
#                 spanner.size.actual <- n_distinct(na.omit(dat[,by]))
#                 spanner.text.actual <- paste0("**",ST ,"**")  }
#   
#   {if(!is.null(spanner.size)) spanner.size.actual <- spanner.size}
#   {if(!is.null(spanner.text)) spanner.text.actual <- spanner.text}
#   
#   sort <- NULL
#   {if(order.cat) sort = all_categorical() ~ "frequency"}
#   
#   
#   
#   if(one_row_binary) {TYPE <- list( all_dichotomous() ~ "dichotomous")} 
#   else{
#     TYPE <-  list( all_dichotomous() ~ "categorical")
#   }
#   if(force.continuous) {TYPE[[2]] <- where(is.numeric) ~ "continuous2"}
#   
#   
#  tab <- dat  %>%
#     tbl_summary(type = TYPE,
#                 sort = sort,
#                 digits = list(all_continuous() ~ c(Digits, Digits)),
#                 by = !!by,
#                 missing = missing,
#                 missing_text = missing_text,
#                 percent = percent,
#                 ...) %>%
#     bold_labels() %>%
#    {if(add.n) add_n(.) else .}  %>% 
#     {if(add.p) add_p(., pvalue_fun = function(x) style_pvalue(x, digits = 3)) else .}  %>% 
#     {if(overall) add_overall(., last = TRUE) else .}
#   
#     if(!is.null(by)) tab %<>% modify_spanning_header(c(paste0("stat_", 1:spanner.size.actual)) ~ paste0("**",spanner.text.actual ,"**"))
#  
#   return(tab)
# }

jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, 
                force.continuous = FALSE, missing = "ifany", missing_text = "Missing",
                spanner.size = NULL, spanner.text = NULL, add.n = TRUE, percent = "column", 
                one_row_binary = FALSE, ...) {
  
  # spanner label/size
  if (!is.null(by)) {
    ST <- ifelse(Hmisc::label(dat[, by]) == "", by, Hmisc::label(dat[, by]))
    spanner.size.actual <- dplyr::n_distinct(stats::na.omit(dat[, by]))
    spanner.text.actual <- paste0("**", ST, "**")
  }
  if (!is.null(spanner.size)) spanner.size.actual <- spanner.size
  if (!is.null(spanner.text)) spanner.text.actual <- spanner.text
  
  # sort categorical vars by frequency if requested
  sort <- NULL
  if (order.cat) sort <- all_categorical() ~ "frequency"
  
  # variable type handling
  if (one_row_binary) {
    TYPE <- list(all_dichotomous() ~ "dichotomous")
  } else {
    TYPE <- list(all_dichotomous() ~ "categorical")
  }
  
  if (force.continuous) {
    TYPE[[length(TYPE) + 1]] <- where(is.numeric) ~ "continuous2"
  }
  
  # build table
  tab <- dat %>%
    tbl_summary(
      type = TYPE,
      sort = sort,
      digits = list(all_continuous() ~ c(Digits, Digits)),
      by = !!by,
      missing = missing,
      missing_text = missing_text,
      percent = percent,
      statistic = list(
        all_continuous() ~ c(
          "{mean} ({sd})",
          "{median} ({p25} - {p75})",
          "{min} - {max}"
        )
      ),
      ...
    ) %>%
    bold_labels() %>%
    {if (add.n) add_n(.) else .} %>%
    {if (add.p) add_p(., pvalue_fun = ~style_pvalue(.x, digits = 3)) else .} %>%
    {if (overall) add_overall(., last = TRUE) else .}
  
  # apply spanning header if `by` given
  if (!is.null(by)) {
    tab <- tab %>%
      modify_spanning_header(
        c(paste0("stat_", 1:spanner.size.actual)) ~ paste0("**", spanner.text.actual, "**")
      )
  }
  
  # apply gt styling
  tab <- tab %>%
    as_gt() %>%
    gt::tab_options(table.font.size = 'small') %>%
    gt::opt_row_striping() %>%
    gt::opt_table_lines("default") %>%
    gt::tab_options(
      table_body.hlines.color = "white",
      row.striping.background_color = "#fafafa",
      source_notes.font.size = 12,
      table.font.size = 12,
      # table.width = gt::px(700),
      heading.align = "left",
      heading.title.font.size = 16,
      table.border.top.color = "transparent",
      table.border.top.width = gt::px(3),
      data_row.padding = gt::px(4)
    )
  
  return(tab)
}


jgtt <- function(dat, col.names = TRUE) {
  
  # Generate a table_id
  table_id <- paste(sample(letters, 5), collapse = "_")
  
  # Create the gt table
  gt_tbl <- dat %>%
    {if(isTRUE(col.names)) sjlabelled::label_to_colnames(.) else .} %>%
    gt(., id = table_id) %>%
    opt_row_striping() %>%
    gt::tab_options(table_body.hlines.color = "white",
                    row.striping.background_color = "#fafafa",
                    source_notes.font.size = 12,
                    table.font.size = 12,
                    heading.align = "left",
                    column_labels.font.weight = 'bold',
                    heading.title.font.size = 16,
                    table.border.top.color = "transparent",
                    table.border.top.width = px(3),
                    data_row.padding = px(4))
  
  # Assign the table_id as an attribute of the gt object
  attr(gt_tbl, "table_id") <- table_id
  
  # Return the gt table
  gt_tbl
}

# Function to retrieve the table_id from the gt object
get_table_id <- function(gt_tbl) {
  return(attr(gt_tbl, "table_id"))
}


# recat <- function(dat, omit.vars = ""){
#   
#   cat2 <-  dat %>% select(-any_of(omit.vars)) %>% 
#     mutate(across(where(is.character), as.factor)) %>% 
#     summarise(across(names(.), ~n_distinct(.x, na.rm = TRUE), .names = "{col};n"),
#               across(names(.), ~levels(.x)[1], .names = "{col};l1"),
#               across(names(.), ~levels(.x)[2], .names = "{col};l2"),
#               across(names(.), ~sum(.x == levels(.x)[1], na.rm = TRUE), .names = "{col};f1"),
#               across(names(.), ~sum(.x == levels(.x)[2], na.rm = TRUE), .names = "{col};f2")) %>% 
#     mutate(x = "x") %>% pivot_longer(., -x, names_to = c("variable", ".value"), names_sep = ";" ) %>% 
#     filter(n == 2) %>% select(-x) %>% left_join(., collect.labels(dat), by = "variable") %>%
#     mutate(l = case_when(l1 %in% c("Yes","yes" ) ~ l1,
#                          l2 %in% c("Yes","yes" ) ~ l2,
#                          f1>=f2 ~ l1, 
#                          TRUE ~ l2)) %>% 
#     mutate(label = ifelse(is.na(label), variable, label)) %>% 
#     mutate(new_label = paste0(label, ": ", l)) 
#   
#   
#   lp <- cat2$variable
#   names(lp) <- cat2$new_label
#   
#   
#   dat %>% 
#     mutate(across(c(where(is.character), -any_of(omit.vars)), as.factor))  %>% 
#     mutate(across(any_of(cat2$variable), ~.x == levels(.x)[1])) %>% 
#     rename(any_of(lp)) 
#   
# }

## Recategorize binary variables for single line summaries
recat <- function(dat, omit.vars= NULL){
  
  ## Turn binary variables into TRUE/FALSE
  ## Re-level 'Yes' as the default if not set as a factor
  ## Re-level to most frequent category if not set as a factor
  ## Assign new label to variable affixing primary factor level
  
  cat2 <- dat %>%
    mutate(across(where(is.character), as.factor)) %>%
    summarise(across(names(.), ~n_distinct(.x, na.rm = TRUE), .names = "{col};n"),
              across(names(.), ~levels(.x)[1], .names = "{col};l1"),
              across(names(.), ~levels(.x)[2], .names = "{col};l2"),
              across(names(.), ~sum(.x == levels(.x)[1], na.rm = TRUE), .names = "{col};f1"),
              across(names(.), ~sum(.x == levels(.x)[2], na.rm = TRUE), .names = "{col};f2")) %>%
    mutate(x = "x") %>% pivot_longer(., -x, names_to = c("variable", ".value"), names_sep = ";" ) %>%
    filter(n == 2) %>% select(-x) %>% left_join(., collect.labels(dat), by = "variable") %>%
    mutate(l = case_when(l1 %in% c("Yes","yes" ) ~ l1,
                         l2 %in% c("Yes","yes" ) ~ l2,
                         f1>=f2 ~ l1,
                         TRUE ~ l2)) %>%
    mutate(label = ifelse(is.na(label), variable, label)) %>%
    mutate(new_label = paste0(label, ": ", l)) %>%
    filter(variable %nin% omit.vars)
  
  lp <- cat2$variable
  names(lp) <- cat2$new_label
  
  for(i in 1:nrow(cat2)){
    
    dat[,cat2$variable[i]] <- dat[,cat2$variable[i]] == cat2$l[i]
    
  }
  
  # relabel variables
  dat %<>% rename(any_of(lp))
  
  return(dat)
}


pgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, digit.cont = 0 , digit.cat = c(0,0), 
                force.continuous = FALSE, missing = "ifany", missing_text = "Missing",
                add.n = FALSE, percent = "column", stats = "brief",
                one_row_binary = TRUE, ...){
  
  if(stats=="full"){ 
    statistic <- list(all_continuous() ~ c( "{mean} ({sd})",
                                            "{median} ({p25} - {p75})",
                                            "{min} - {max}") )
  }else{
    statistic <- list(all_categorical() ~ "{n} ({p})", 
                      all_continuous() ~ "{median} ({p25} - {p75})" )}
  
  
  {if(!is.null(by)) 
    spanner.size <- n_distinct(na.omit(dat[,by]))
    spanner.text <- paste0("**",ifelse(Hmisc::label(dat[,by])=="", by, Hmisc::label(dat[,by])) ,"**")  }
  sort <- NULL
  {if(order.cat) sort = all_categorical() ~ "frequency"}
  
  if(one_row_binary) {TYPE <- list( all_dichotomous() ~ "dichotomous")} 
  else{
    TYPE <-  list( all_dichotomous() ~ "categorical")
  }
  if(force.continuous) {TYPE[[2]] <- where(is.numeric) ~ "continuous2"}
  
  tab <- dat  %>%
    tbl_summary(type = TYPE,
                sort = sort,
                digits = list(all_categorical() ~ digit.cat,
                              all_continuous() ~ digit.cont),
                by = !!by,
                missing = missing,
                statistic = statistic,
                missing_text = missing_text,
                percent = percent,
                ...) %>%
    bold_labels() %>%
    {if(add.n) add_n(.) else .}  %>% 
    {if(add.p) add_p(., pvalue_fun = function(x) style_pvalue(x, digits = 3)) else .}  %>% 
    {if(overall) add_overall(., last = TRUE) else .}
  
  if(!is.null(by)) tab %<>%  modify_spanning_header(c(paste0("stat_", 1:spanner.size)) ~ spanner.text)
  
  return(tab)
}


### Interaction plot

expand.int <- function(a.seq, b.seq, A, B, fit){
  
  xxdd <- expand_grid(
    a1 = a.seq, 
    a2 = a.seq,
    b1 = b.seq, 
    b2 = b.seq)
  
  CBs <-bind_cols(xxdd, 
                  xxdd%>%rowwise() %>% do(get.contrast.fun(fit, .$a1, .$a2, .$b1, .$b2, A, B)))
  
  
  output<- CBs %>% rowwise() %>% filter(contrast != 0) %>% arrange(p, a1, b1) %>%
    select(Effect = contrast, Lower.0.95 = lower, Upper.0.95 = upper, p,  a1, a2, b1, b2) %>% 
    mutate(p2 = star.p(p)) 
  output
}



int.tile.plot <- function(dat, var1, var2){
  ## log-hazard tile plot
  
  title = paste(var1, " * ", var2, " contrasts", sep = "")
  
  dat %>% 
    mutate(b2 = str_wrap(b2, 12)) %>%
    group_by(p)%>% 
    arrange(b2, b1, a1, a2) %>% 
    summarise(across(everything(), first)) %>%
    mutate(a2 = fct_rev(factor(a2) )) %>% 
    ggplot(., aes(x = a2, y = a1, fill =Effect)) + geom_raster() +
    geom_point(aes(size = p2)) + 
    theme_bw() + 
    labs(x = "Group 2", y = "Group 1", title = title) +
    facet_grid(b1~b2, scales = "free", space = "free", switch= "both")+ 
    scale_size_discrete(range = c(.0,2.5)) +
    scale_fill_gradient2()+
    theme_classic() + 
    theme(strip.text.y.left = element_text(angle = 0))+ 
    theme(strip.background  = element_blank()) + 
    theme(strip.placement = 'outside') +
    theme(legend.direction = "vertical", 
          legend.position = c(0.8, 0.8),
          legend.box = "horizontal") +
    labs(size = "Significance level", fill = "log(HR)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))}

int.react <- function(dat){
  dat %>% 
    mutate(across(c(Effect, `Lower.0.95`, `Upper.0.95`), ~round(exp(.x),2)), 
           p = format.pval(p, digits = 4, eps = 0.0001)) %>%
    select(a1,b1, a2,b2, Effect, Lower.0.95, Upper.0.95, p) %>% 
    reactable::reactable(., filterable = TRUE, minRows = 10)}

star.p <- function(p){
  factor(case_when(is.na(p) ~ "",
                   p >= 0.05 ~ "[.05, 1]",
                   p >= 0.01 ~ "[.01, .05)",
                   p >= 0.001 ~ "[.001, .01)",
                   p >= .0001 ~ "[.0001, .001)",
                   TRUE ~"< .0001"),
         levels = c("[.05, 1]", "[.01, .05)", "[.001, .01)", "[.0001, .001)", "< .0001"))}

get.contrast.fun <- function(fit, s1, s2, p1, p2, var1, var2){
  
  l1 <- list(v1=s1, v2 = p1)
  l2 <- list(v1=s2, v2 = p2)
  names(l1) <- c(var1, var2)
  names(l2) <- c(var1, var2)
  
  C <- contrast(fit, l1,l2)
  data.frame(contrast = C$Contrast, SE = C$SE, lower = C$Lower, upper = C$Upper, p = C$Pvalue)
}


### Create a tribble to easily recode data
j.trib <- function(dat, col){
  
  var2 <- sym(col)
  trib.title <- paste0("trib.", deparse(substitute(dat)))
  tt <- dat %>% group_by(!!var2) %>% tally() %>% select(-n) %>% 
    mutate(replace = ", '',", paste = paste0("'",!!var2,"'", replace, "\n             ")) %$% paste 
  
  tt[length(tt)] <- gsub(",\n", ")", tt[length(tt)])
  
  lj <- paste0(deparse(substitute(dat)), " %>% left_join(., ", 
               trib.title, ", by = '", col, "') %>% select(-", col, ", ", col, " = replace)")
  
  cat(c(paste0(trib.title, "<- tribble(~",col,", ~replace,\n             ") ,tt, "\n\n", lj))
  
}



#### ESPN theme for reactable
# https://github.com/kcuilla/reactablefmtr/blob/main/R/themes.R
j.espn <- function(font_family = "Albertus",
                 font_size = 12,
                 font_color = "#6C6D6F",
                 header_font_family = "Albertus",
                 header_font_size = 11,
                 header_font_color = "#48494a",
                 cell_padding = 7) {
  
  reactableTheme(
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#fafafa",
    stripedColor = "#fafafa",
    highlightColor = "#fafafa",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size,
                      fontFamily = font_family),
    headerStyle = list(
      borderTop = "1px solid #e1e2e4",
      borderBottom = "1px solid #e1e2e4",
      padding = "4px",
      background = "#ffffff",
      borderColor = "#ffffff",
      color = header_font_color,
      textTransform = "uppercase",
      "&:hover" = list(color = "#004D9A"),
      fontSize = header_font_size,
      fontFamily = header_font_family
    ),
    groupHeaderStyle = list(
      borderTop = "1px solid #e1e2e4",
      borderLeft = "1px solid #e1e2e4",
      borderRight = "1px solid #e1e2e4",
      backgroundColor = "#ffffff",
      textTransform = "uppercase",
      fontSize = "11px",
      color = font_color,
      fontSize = header_font_size,
      fontFamily = header_font_family
    ),
    searchInputStyle = list(color = "#6C6D6F",
                            fontSize = "13px"),
    inputStyle = list(backgroundColor = "#ffffff", color = "#6C6D6F"),
    rowSelectedStyle = list(backgroundColor = "#48494a"),
    selectStyle = list(color = "#48494a"),
    pageButtonStyle = list(color = "#48494a", fontSize = "13px"),
    paginationStyle = list(color = "#48494a", fontSize = "13px")
  )
}

j.reactable <- function(dat, col.names = TRUE, searchable = TRUE, resizable = TRUE, theme = j.espn(),
                        striped = TRUE, compact = TRUE, highlight = TRUE, sortable = TRUE, csv.file = NULL, ...){
  
  output <- dat %>% 
    {if(col.names) sjlabelled::label_to_colnames(.)else .}  %>% 
    reactable(data = ., searchable = searchable, resizable = resizable, sortable= sortable,
              striped = striped, compact = compact, highlight = highlight, theme = theme, elementId = csv.file, ...)
  
  if(!is.null(csv.file)){
    
    htmltools::browsable(
      tagList(
        tags$button(tagList(fontawesome::fa("download"), "Download as CSV"),
                    onclick = paste0("Reactable.downloadDataCSV('",csv.file,"', '",csv.file,".csv')")),
        output))
  }
  else{
    return(output)
  }
}

react.cap <- function(table,caption,font_size = 18, font_weight = "normal", ...){
  table %>% add_title(caption,
                      font_size = font_size, font_weight = font_weight, ...)
}



## TGS namify
#' Converts hogwash variable names to proper variable names.
#' 
#' @param x A character vector or a data.frame
#' @details The following changes are made to the text
#' * Change . to _
#' * Remove leading/trailing spaces, (, )
#' * Change \code{\%} to pct
#' * Change interior space to underscore
#' * Change all characters to lower case
#' * Remove punctuation
#' * If text begins with number, prefix with n
#' @md
#' @keywords names
#' @examples
#' df_with_proper_variable_names <- namify(iris)
#' @export
namify <- function(x){
  UseMethod("namify", x)
}

#' @export
namify.default <- function(txt){
  txt <- gsub("\\.", "_", txt)          # Change . to _
  txt <- gsub(" +$|^ +|\\(|\\)","",txt) # Remove leading/trailing spaces, (, )
  txt <- gsub("%","pct",txt)            # Change % to pct
  txt <- gsub(" ","_",txt)              # Change interior space to underscore
  txt <- tolower(txt)                   # to lower case
  txt <- gsub("[^_[:alnum:]]","",txt)   # Remove punctuation
  txt <- gsub("^([0-9]{1}.*)","n\\1", txt) # If text begins with number, prefix with n
  return(txt)
}

#' @export
namify.data.frame <- function(x){
  names(x) <- namify(names(x))
  return(x)
}



##### RMS summary
# Easily make tribble to label plot/ table

rms.trib <- function(rms.fit){
  # term = variable name
  # new = plot/ table labels
  # level = order
  # label = label if interaction applied
  
  top <- "trib.cols.rms <- tribble(~term, ~new, ~level, ~label,\n"
  middle <- data.frame(term = rms.fit$Design$name, new = rms.fit$Design$label, 
                       level = 1:length(rms.fit$Design$name), label = rms.fit$Design$label) %>% 
    mutate(x = paste("            '", paste(term, new, level, label, sep = "', '"),"',\n", sep = "")) %$% x
  middle[length(middle)] <- gsub(",\n", ")", middle[length(middle)])
  cat(top, middle)
  
}



rms.int.adj <- function(summary){
  
  if(attr(summary, "adjust")==""){
    return(NULL)
  }
  
  cap <- attr(summary, "adjust") %>% 
    str_split(., "\\s") %>% unlist() %>% 
    data.frame(adjust = .) %>% 
    separate(adjust, into = c("term", "adjust"), sep = "=", fill = "right") %>% 
    left_join(., trib.cols.rms, by = "term") %>% na.omit() %>% arrange(level) %>% 
    mutate(L = paste(label, adjust, sep = ": ")) %$% L %>% 
    paste0(collapse = "; ") %>% paste("Interaction terms adjusted to:", ., sep = "\n")
  
  return(cap)
  
}


rms.sum.table3 <- function(summary, trib, anova = NULL, raw = FALSE){
  
  if("anova.rms" %in% class(anova)){
  rtab <- data.frame(anova) %>%rownames_to_column() %>%
    filter(rowname != "TOTAL") %>% rename(term = rowname) %>% 
    mutate(P = format.pval(P, digits = 3, eps = 0.001))
  }
  
  cap <- rms.int.adj(summary)
  
  res <- tibble::as_tibble(summary, rownames = "term")
  names(res) <- c("term", "low", "high", "diff", "effect", "std.error", "conf.low", "conf.high", "type")
  res$term <- trimws(res$term)
  
  res <- res %>% 
    ## Separate variable names from adjust-to values
    separate(term, into = c("term", "t2"), sep = " - ", fill = "right") %>%
    separate(t2, into = c("highc", "lowc"), sep = ":", fill = "right") %>%
    # separate(term, into = c("term", "highc", "lowc"), sep = " *[-:] *", fill = "right") %>%
    ## Create character versions of all low/high values for each row
    mutate(vt = ifelse(!is.na(diff), "Continuous", "Categorical"),
           highc = ifelse(!is.na(highc), highc,
                          ifelse(term %in% paste(c("Hazard", "Odds"), "Ratio"), NA,
                                 as.character(round(high, 2)))),
           highc = ifelse(is.na(highc), dplyr::lag(highc), highc),
           lowc = ifelse(!is.na(lowc), lowc,
                         ifelse(term %in% paste(c("Hazard", "Odds"), "Ratio"), NA,
                                as.character(round(low, 2)))),
           lowc = ifelse(is.na(lowc), dplyr::lag(lowc), lowc),
           ## "term" will temporarily be "variable, [coef or ratio]"
           term = ifelse(term %in% c("Hazard Ratio", "Odds Ratio"),
                         sprintf("%s, ratio", dplyr::lag(term)),
                         sprintf("%s, coef", term))) %>%
    dplyr::select(-type, -low, -high) %>%
    separate(term, into = c("term", "type"), sep = ", ") %>%
    group_by(term, highc, lowc) %>%  summarise_all(., last) %>% ungroup() %>% 
    left_join(., trib, by = "term" ) %>%
    mutate(facet = ifelse(vt == "Continuous", new, paste(new, lowc, sep = "\n Ref: ")),
           facet = fct_reorder(facet, level, min),
           axis = ifelse(vt == "Continuous", paste(highc, lowc, sep = " vs. "), highc))%>% 
    arrange(level)%>% 
    mutate(levels = paste(highc, lowc, sep = " vs. ")) %>% 
    {if(!is.null(anova)) left_join(., rtab, by = "term") else .} 
  
  est.type <- res$term[1]
  
  if(isTRUE(raw)){
    return(res)
  } else if(est.type == "ratio"){
  return(res%>%
           select(label, levels, ratio = effect, conf.low, conf.high, any_of(c('d.f.', 'P'))) %>% 
           mutate(across(where(is.numeric), ~round(.x, 3))) %>% 
           jgtt(.) %>% tab_footnote(., cap))
  } else {
    return(res%>%
             select(label, levels, effect, conf.low, conf.high, any_of(c('d.f.', 'P'))) %>% 
             mutate(across(where(is.numeric), ~round(.x, 3))) %>% 
             jgtt(.) %>% tab_footnote(., cap))
    
  }
}
  

rms.forest.plot <- function(summary, trib, breaks = seq(0.6, 2, .2)){
  
  cap <- rms.int.adj(summary)
  
  res <- rms.sum.table3(summary = summary, trib = trib, raw = TRUE)
  
  
  res %>% mutate(across(where(is.numeric), as.numeric)) %>% 
    ggplot(., aes(x = effect, y = axis)) + geom_point() + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
    theme_classic() + 
    # geom_vline(xintercept = 1, linetype = 2) + 
    labs(x = "Hazard ratio", y = NULL) +
    facet_grid(facet~., scales = "free_y", switch = "y")+
    theme(strip.text.y.left = element_text(angle = 0))+
    theme(strip.background  = element_blank()) +
    theme(panel.spacing.y = unit(0,"line")) +
    theme(strip.placement = 'outside') +
    theme(panel.grid.major.x = element_line(colour = "gray90"),
          panel.grid.minor.x = element_line(colour = "gray95",
                                            size = 0.3),
          plot.background = element_rect(fill = "ghostwhite", size = 1.3)) +
    theme(plot.caption = element_text(size = 7, hjust = 0)) +
    labs(caption = cap)
  
}


get_toc <- function(rmd_file) {
  ## Takes in a rmdfile name or path
  ## Returns a data frame of the table of contents
  
  parsermd::parse_rmd(rmd_file) %>%as.data.frame() %>% 
    filter(type == "rmd_heading") %>% as.data.frame() %>% select(contains("sec")) %>% mutate(n = 1:n()) %>%
    pivot_longer(., -n, values_drop_na = TRUE) %>% mutate(h_level = readr::parse_number(name)) %>%
    group_by(n)%>% filter(h_level == max(h_level)) %>% ungroup() %>%
    mutate(level_1 = cumsum(h_level == 1)) %>% group_by(level_1) %>%
    mutate(level_2 = cumsum(h_level == 2)) %>% group_by(level_1, level_2) %>%
    mutate(level_3 = cumsum(h_level == 3)) %>% group_by(level_1, level_2, level_3) %>%
    mutate(level_4 = cumsum(h_level == 4)) %>% group_by(level_1, level_2, level_3, level_4) %>%
    mutate(level_5 = cumsum(h_level == 5)) %>% group_by(level_1, level_2, level_3, level_4, level_5) %>%
    mutate(level_6 = cumsum(h_level == 6)) %>% group_by(level_1, level_2, level_3, level_4, level_5, level_6) %>%
    mutate(level_7 = cumsum(h_level == 7)) %>% group_by(level_1, level_2, level_3, level_4, level_5, level_6, level_7) %>%
    mutate(level_8 = cumsum(h_level == 8)) %>% ungroup() %>%
    select(title = value, contains("level_")) %>% mutate(order = 1:n()) %>%
    pivot_longer(., c(-title, -order)) %>%
    arrange(order, desc(name)) %>%
    group_by(title, order) %>% mutate(v = cumsum(value)) %>% filter(v > 0) %>%
    arrange(order, name) %>% group_by(title, order) %>%
    summarise(section = paste0(value, collapse = ".")) %>% arrange(order,section) %>%
    select(order, section, title)
}

save_toc <- function(rmd_file){
  get_toc(rmd_file) %>% write.csv(.,gsub(".Rmd", "_toc.csv",rmd_file), row.names = FALSE )
}

session_info <- function(rmd_file){
  ## Exports the file into temp file for session info
  knitr::purl(rmd_file, "session_info.R", documentation = 2, quiet = TRUE)
  
  ## Gets parse data (duh)
  tmp <- getParseData(parse("session_info.R", keep.source=TRUE))
  
  ## formats output
  tmp %>% 
    filter(token %in% c("SPECIAL", "SYMBOL_FUNCTION_CALL") ) %>% 
    group_by(text) %>% 
    tally() %>% 
    rowwise() %>%
    mutate(f = toString(find(text))) %>% 
    separate_rows(.,f, sep = ",") %>% 
    mutate(f = trimws(gsub("package:", "", f), "both")) %>% 
    group_by(f) %>% 
    summarise(n = n(), funs = toString(text)) %>% 
    filter(funs != "%>%") %>% 
    rowwise() %>%
    mutate(version = tryCatch(as.character(packageVersion(f)), error=function(e) "NA")) %>% 
    select(package = f, version, n, funs) %>% 
    arrange(desc(n)) %>%
    jgtt() %>% 
    tab_source_note(., paste0("Current as of ", file.mtime("session_info.R")))
  
}

save_session_info <- function(rmd_file, path = "session_info.R"){
  knitr::purl(rmd_file, path, documentation = 2, quiet = TRUE)
}

print_session_info <- function(path = "session_info.R"){
  
  ## Gets parse data 
  tmp <- getParseData(parse(path, keep.source=TRUE))
  
  ## formats output
  tmp %>% 
    filter(token %in% c("SPECIAL", "SYMBOL_FUNCTION_CALL") ) %>% 
    group_by(text) %>% 
    tally() %>% 
    rowwise() %>%
    mutate(f = toString(find(text))) %>% 
    separate_rows(.,f, sep = ",") %>% 
    mutate(f = trimws(gsub("package:", "", f), "both")) %>% 
    group_by(f) %>% 
    summarise(n = n(), funs = toString(text)) %>% 
    filter(funs != "%>%") %>% 
    rowwise() %>%
    mutate(version = tryCatch(as.character(packageVersion(f)), error=function(e) "NA")) %>% 
    select(package = f, version, n, funs) %>% 
    arrange(desc(n)) %>%
    jgtt() %>% 
    tab_source_note(., paste0("Current as of ", file.mtime("session_info.R")))
  
}
rca_checkbox_fix <- function(rcon, rdat){
  
  checkbox_prefixes <- rcon$metadata() %>% 
    filter(field_type == "checkbox") %>% 
    pull(field_name)
  
  outdat <- rdat %>% select(-any_of(checkbox_prefixes))
  
  for(i in 1:length(checkbox_prefixes)){
    outdat %<>% mutate(across(starts_with(checkbox_prefixes[i]), 
                              ~ifelse(.x == "", NA, as.character(.x)))) 
  }
  
  
  sjlabelled::copy_labels(outdat, rdat)
}

rca_instrument <- function(rdat, instrument = NA, label_src = NULL){
  
   if(is.null(label_src)) {label_src <- rdat}
  
  rdat %>% 
    filter(redcap_repeat_instrument %in% instrument) %>% 
    remove.blank.columns2() %>% droplevels() %>% 
    as.data.frame() %>% 
    sjlabelled::copy_labels(., label_src)
}

rca_dictionary <- function(rcon, included.ids = del$record_id, id.var = "record_id", repeat.var = NULL){
  
  select_missing <- c(repeat.var, "missing")
  
  missing_summary <- missingSummary(rcon) %>% filter(!!sym(id.var) %in% included.ids) %>% 
    filter(n_missing > 0) %>% select(id.var, any_of(select_missing)) 
  
  if(is.null(repeat.var)) {
    missing_summary$rv <- NA
  } else {
    missing_summary$rv <- missing_summary[,repeat.var]
  }
  
  
  missing_summary %<>% 
    separate_rows(., "missing", sep = ",") %>%
    mutate(missing = trimws(missing)) %>%
    group_by(!!sym(id.var), missing) %>%
    summarise(x =  ifelse(is.na(rv), NA, toString(rv))) %>%
    distinct() %>%
    group_by(missing) %>%
    dplyr::summarise(n.miss = n(), ids = ifelse(is.na(x), toString(!!sym(id.var)),
                                                toString(paste0(!!sym(id.var), " (",x,")")))) %>%
    distinct() %>% rename(field_name = missing) %>%
    mutate(field_name = trimws(field_name, "both"))
  
  rcon$metadata() %>%
    left_join(., missing_summary, by = "field_name") %>%
    mutate(Variable = paste(field_name, branching_logic, sep = "<br><br>"),
           Variable = gsub("NA", "", Variable),
           choice = gsub("\\|", "<br>", select_choices_or_calculations),
           minmax = ifelse(text_validation_min %nin% NA,
                           paste("Min: ", text_validation_min, ", Max:",
                                 text_validation_max), ""),
           
           attr = ifelse(minmax == "", field_type,
                         paste0(field_type, "<br><br>(", minmax, ")" ))) %>%
    mutate(attr = ifelse(is.na(text_validation_type_or_show_slider_number), attr,
                         paste(attr,"<br><br>",
                               text_validation_type_or_show_slider_number) )) %>%
    select(form_name,n.miss, Variable, field_label, attr, choice,  ids) %>%
    j.reactable(., columns = list(Variable = colDef(html = TRUE, width = 150),
                                  field_label = colDef(html = TRUE, width = 150),
                                  attr = colDef(html = TRUE, width = 150),
                                  choice = colDef(html = TRUE, width = 200),
                                  form_name = colDef(width = 100),
                                  ids = colDef(show = FALSE, searchable = TRUE)),
                groupBy = "form_name", defaultExpanded = FALSE,
                details = function(index) {
                  if(!is.na(.[index, "ids"])){
                    reactable(data.frame(ids = .[index, "ids"]), fullWidth = TRUE)
                  }
                  
                })
}



date_dists <- function(dat, included.ids = NULL, id.var = "record_id", n_include = 10, label.src = NULL){
  
  if(is.null(label.src)){
    label.df <- collect.labels(dat)
  } else {
    label.df <- collect.labels(label.src)
  }

  dat %>% clear.labels()%>% ungroup() %>% 
    {if(!is.null(included.ids)) filter(., record_id %in% included.ids) else . } %>%
    select(!!sym(id.var), where(is.Date))%>%
    pivot_longer(., c(-!!sym(id.var)), values_drop_na = TRUE) %>%
    arrange(name, value) %>% group_by(name) %>%
    mutate(n = n(), sequence = 1:n(), value = ymd(value))%>%
    mutate(date2 = if_else(between(sequence, (n_include + 1), max(sequence)-n_include), as.Date(NA),value)) %>%
    rename( variable = name, date = date2) %>%
    left_join(., label.df) %>%
    mutate(variable = paste0(label, "(", variable, ") [N = ",n, "]"))
}

print.work.logs <- function(proj){
  work.logs <- readxl::read_xlsx("/Users/joshdeclercq/Library/CloudStorage/OneDrive-VUMC/Work logs/work log.xlsx", sheet = "Todo")
  
  res <- work.logs %>% filter(project == proj) %>%
    select(date, request, "date done") %>%
    jgtt() %>% tab_header("Summary of project requests") 
  
  return(res)
  
}


archive <- function(DAT){
  
  NEWDAT <- DAT
  timestamp <- format(Sys.time(), "%Y.%m.%d %H.%M.%S")
  dat_name <- deparse(substitute(DAT))
  archive_path <- paste0(getwd(), "/archive/", dat_name)
  file_path <- paste0(archive_path, "/",dat_name," " ,timestamp ,".rda")
  archive_file_details <- paste0(archive_path, "/archive_summary_", dat_name, ".rda")
  
  if(!dir.exists(paste0(getwd(), "/archive"))){
    dir.create(paste0(getwd(), "/archive"))
  }
  
  if(!dir.exists(archive_path)){
    
    dir.create(archive_path)
    
    ARCHIVE <- DAT
    save(ARCHIVE,
         file= file_path,
         compress='xz', compression_level=9)
    
    archive_details <- data.frame(root = archive_path, 
                                  file = list.files(archive_path)) %>%
      mutate(path = paste(root, file, sep = "/"), download.time = file.mtime(path),
             rows = dim(ARCHIVE)[1], cols = dim(ARCHIVE)[2], 
             n.diffs = 0)
    
    save(archive_details,
         file=archive_file_details,
         compress='xz', compression_level=9)
    
    return(paste("Archive directory created at", archive_path))
    
  }
  
  ## Get most recent data
  load(archive_file_details)
  recent_data_detail <- archive_details %>%
    filter(download.time == max(download.time))
  
  ## Check to see if data are the same between versions
  load(recent_data_detail$path) ##This loads ARCHIVE, which is the most recent saved data
  
  if(isTRUE(all.equal(ARCHIVE, NEWDAT))){
    return("No changes made to data")
  } else {
    ## Save new data
    ARCHIVE <- DAT
    save(ARCHIVE,
         file=file_path,
         compress='xz', compression_level=9)
    
    ## Save archive summary

    
    current_data_detail <- data.frame(root = archive_path, 
                                      file = list.files(archive_path)) %>%
      mutate(path = paste(root, file, sep = "/"), download.time = file.mtime(path),
             rows = dim(NEWDAT)[1], cols = dim(NEWDAT)[2], n.diffs = NA) %>% 
      filter(!grepl("archive_summary_|changelog_output", file)) %>%
      filter(download.time == max(download.time))
    
    archive_details <- rbind(archive_details, current_data_detail)
    
    save(archive_details,
         file=archive_file_details,
         compress='xz', compression_level=9)
    
    return(print("New copy of data saved"))
  }
  
}

changelog2 <- function(dat, id.vars) {
  
  ## get archive summary path
  dat_name <- deparse(substitute(dat))
  archive_path <- paste0(getwd(), "/archive/", dat_name)
  archive_file_details <- paste0(archive_path, "/archive_summary_", dat_name, ".rda")
  changelog_output_file <- paste0(archive_path, "/changelog_output_", dat_name, ".rda")
  
  
  ## load archive summary
  load(archive_file_details)
  
  ## determine if comparisons need to be run and terminate if no  
  comparisons_to_run <-  archive_details %>% 
    mutate(n = 1:n(), y = ifelse(is.na(n.diffs), n, NA)) %>% pull(y) %>% na.omit()
  
  if(length(comparisons_to_run)==0){
    return("No comparisons needed")
  }
  
  ## if no comparisons have be made yet, initialize DFs  
  
  if(!file.exists(changelog_output_file)){
    
    diffs_by_var <- data.frame()
    diffs_by_id <- data.frame()
    summary_comp_table <- data.frame()
    vars_not_shared <- data.frame()
    
  } else{
    ## If comparison data exists, load previous comparisons  
    
    load(changelog_output_file)
    
    diffs_by_var = changelog_output$diffs_by_var 
    diffs_by_id = changelog_output$diffs_by_id 
    summary_comp_table = changelog_output$summary_comp_table 
    vars_not_shared = changelog_output$vars_not_shared
    
  }
  
  ## Run 'compare' on new data only
  for(i in comparisons_to_run){
    
    load(archive_details$path[i])
    p1 <- ARCHIVE
    load(archive_details$path[i-1])
    p2 <- ARCHIVE
    
    
    Cc <- arsenal::comparedf(p1, p2, by = id.vars)
    sCc <- summary(Cc)
    archive_details$n.diffs[i] <- arsenal::n.diffs(Cc)
    
    
    diffs_by_var <- rbind(diffs_by_var,
                          diffs(Cc, by.var = TRUE) %>% filter(n>0|NAs>0)  %>%
                            mutate(x = ac(archive_details$download.time[i]),
                                   y = ac(archive_details$download.time[i-1]),
                                   comparison = paste0(i, " vs ", (i-1))) %>% 
                            select(comparison, var = var.x, n, NAs))
    
    diffs_by_id <- rbind(diffs_by_id,
                         diffs(Cc) %>%
                           mutate(x = archive_details$download.time[i],
                                  y = archive_details$download.time[i-1],
                                  comparison = paste0(i, " vs ", (i-1)))%>% 
                           select(all_of(id.vars), comparison, var = var.x, x, values.x, values.y)) 
    
    summary_comp_table <- rbind(summary_comp_table,
                                sCc$comparison.summary.table %>%
                                  mutate(comparison = paste0(i, " vs ", (i-1))))
    
    vars_not_shared <- rbind(vars_not_shared,
                             sCc$vars.ns.table %>%
                               mutate(comparison = paste0(i, " vs ", (i-1))))
    
    
  }     
  
  ## create and save new copy of changelog data  
  
  changelog_output <- list(diffs_by_var = diffs_by_var, diffs_by_id = diffs_by_id, 
                           summary_comp_table = summary_comp_table, vars_not_shared = vars_not_shared)
  
  save(changelog_output,
       file=changelog_output_file,
       compress='xz', compression_level=9)  
  
  ## Save n.diffs to archive summary
  save(archive_details,
       file=archive_file_details,
       compress='xz', compression_level=9)
  
  
  return(cat(length(comparisons_to_run), "comparisons run.\nSummaries saved to:", changelog_output_file))
  
}

retrieve_archive <- function(dat, wd = getwd()){
  fp <- paste0(wd, "/archive/", dat, "/archive_summary_", dat, ".rda")
  load(fp)
  ad <- archive_details
  return(ad)
}

reset_archive_details <- function(dat, wd = getwd()){
  
  ## If the number of data sets is too damn high
  ## Manually delete datasets from archive folder
  ## Run this function
  ## Re-run changelog2
  
  FP <- paste0(wd, "/archive/", dat)
  files <- list.files(FP)
  archive_details <- retrieve_archive(dat, wd = wd) %>%  
    mutate(n.diffs = c(0, rep(NA, nrow(.)-1))) %>% 
    filter(file %in% files)
  save(archive_details, file = paste0(FP, "/archive_summary_", dat, ".rda"))
  
}


j_dataChk <- function (d, checks, id, summary = TRUE) 
{   
  # Borrowed extensively from FHarrell's qreport::dataChk
  omit0 = TRUE
  byid = TRUE
  jj <- list()
  s <- NULL
  X <- Dat <- list()
  
  
  for (i in 1:length(checks)) {
    x <- checks[i]
    cx <- as.character(x)
    cx <- gsub("%between% c\\((.*?)\\)", "[\\1]", cx)
    form <- as.formula(paste("~", cx))
    vars.involved <- all.vars(form)
    z <- d[eval(x), c(id, vars.involved), with = FALSE]
    
    no <- nrow(z)
    if (byid && no > 0) {
      Da <- z[, id, with = FALSE]
      Da[, `:=`(Check, cx)]
      Da[, `:=`(Values, do.call(paste, z[, vars.involved, with = FALSE]))]
      Dat[[cx]] <- Da
    }
    s <- rbind(s, data.frame(Check = cx, n = no))
  }
  
  Dat <- rbindlist(Dat, fill = TRUE)
  
  return(list(s = s, d = Dat))
}

j_dataChk2 <- function (d, checks, id, summary = TRUE) 
{   
  # Borrowed extensively from FHarrell's qreport::dataChk
  omit0 = TRUE
  byid = TRUE
  jj <- list()
  s <- NULL
  X <- Dat <- list()
  
  
  for (i in 1:length(checks)) {
    x <- checks[i]
    nx <- names(x)
    cx <- as.character(x)
    if(is.null(nx)) nx <- cx
    else if(nx %in% '') nx <- cx
    cx <- gsub("%between% c\\((.*?)\\)", "[\\1]", cx)
    form <- as.formula(paste("~", cx))
    vars.involved <- all.vars(form)
    z <- d[eval(x), c(id, vars.involved), with = FALSE]
    
    no <- nrow(z)
    if (byid && no > 0) {
      Da <- z[, id, with = FALSE]
      Da[, `:=`(Check, cx)]
      Da[, `:=`(Name, nx)]
      Da[, `:=`(Values, do.call(paste,c(z[, vars.involved, with = FALSE], list(sep = ", "))))]
      Da$vars <- toString(vars.involved)
      Dat[[cx]] <- Da
    }
    s <- rbind(s, data.frame(Name = nx, Check = cx, n = no))
  }
  
  Dat <- rbindlist(Dat, fill = TRUE)
  
  if(nrow(Dat) > 0){
    Dat <- Dat %>% 
      separate_rows(., c("Values", "vars"), sep = ", ")  %>%
      mutate(X = paste(paste0("<b>",vars, "</b>"), Values, sep = ": ")) %>% 
      group_by(Name, !!!rlang::syms(id))  %>% 
      summarise(fields = paste(X, collapse = "<br>"), .groups = "drop") %>% 
      as.data.frame()
  }
  
  return(list(s = s, d = Dat))
}
format_data_checks <- function(L){
  
  L <- list_transpose(L)
  S <- bind_rows(L$s)
  D <- bind_rows(L$d)
  
  if(is.null(D)|nrow(D)==0){
    D <- data.frame(Name = "No failed data checks")
  }
  
  L <- list(Summary = S %>% jgtt(),
            Detail = j.reactable(D, groupBy = c("Name"), 
                                 columns = list(fields = colDef(html = TRUE, width = 350)),
                                 csv.file = "data_checks"))
  
  return(L)
  
}

inline_recode <- function(dat, var){
  
  open <- paste0( "mutate(", substitute(var), " = forcats::fct_recode(", substitute(var), ",\n")
  
  lvs <- dat %>% pull(var) %>% unique()
  ll <- data.frame(l = rep('\t\t\t"" = "', length(lvs)), lvs, r = '",\n') %>% mutate(p = paste0(l, lvs, r))
  ll[nrow(ll), "p"] <- gsub(",\n", "))\n",ll[nrow(ll), "p"])
  
  output <- capture.output(cat(open, paste0(ll$p) ))
  rstudioapi::insertText(output)
}

vsp_palette <- c("#0C3D77" ,"#4A8166", "#267F81", "#4762A6")

declutter_df <- function(dat, rcon){
  dat %>% remove.blank.columns2() %>% droplevels() %>% sjlabelled::copy_labels(., rcon)
}


step_exclude <- function(table = NULL, step, data, id.var = "record_id"){
  bind_rows(table,
            data.frame(step = step, IDs = n_distinct(data[,id.var]), observations = nrow(data))) %>% 
    mutate(n_excluded_ids = lag(IDs, default = NA) - IDs, 
           n_excluded_obs = lag(observations, default = NA) - observations) %>% 
    mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    mutate(cumulative_excluded_ids = cumsum(n_excluded_ids),
           cumulative_excluded_obs = cumsum(n_excluded_obs))
}



jreport <- function(IN){
  if(!is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))){
    cat(qreport::maketabs(IN)%>% compact())
  } else {
    IN
  }
}


inline_label <- function(dat, ...){
  
  labels <- as.list(substitute(list(...)))[-1] # get the names of the named arguments
  
  for(i in 1:length(labels)){
    Hmisc::label(dat[[names(labels)[[i]]]]) <- labels[[i]] # assign label from named argument
  }
  return(clear.label.class(dat))
}

label_pivoted <- function(dat, src){
  left_join(dat, collect.labels(src), by = c("name"= "variable"))
}

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

na_omit <- function(dat) {
  out <- dat %>% na.omit() %>% sjlabelled::copy_labels(dat)
  return(out)
}

get_table_id <- function(gt_tbl) {
  return(attr(gt_tbl, "table_id"))
}

scrollify <- function(tab, height = 400, width = 500, table_id = NULL) {
  
  # Set table ID based on object type
  if (inherits(tab, "gtsummary")) {
    table_id <- ifelse(is.null(table_id), paste(sample(words, 2), collapse = "_"), table_id)
    gt_tbl <- as_gt(tab, id = table_id)
    default_col_width <- "auto"  # gtsummary's recommended width
  } else if (inherits(tab, "gt_tbl")) {
    gt_tbl <- tab
    table_id <- ifelse(is.null(table_id), get_table_id(tab), table_id)
    default_col_width <- "auto"  # Allow automatic column width
  } else {
    stop("The input must be a gtsummary or gt object.")
  }
  
  # Freeze first column only, with flexible width based on type
  freeze_css <- glue::glue("
    #{table_id} td:nth-child(1),
    #{table_id} th:nth-child(1) {{
      position: sticky;
      left: 0;
      z-index: 2;
      background-color: white;
      min-width: {default_col_width};
      max-width: {default_col_width};
      white-space: nowrap;
    }}
    #{table_id} th:nth-child(1) {{
      top: 0;
      z-index: 3;
    }}
  ")
  
  # gtsummary-specific header fix
  header_css <- if (inherits(tab, "gtsummary")) glue::glue("
    #{table_id} th.gt_col_heading {{
      position: sticky;
      top: 0;
      z-index: 3;
      background-color: white;
      white-space: nowrap;
      min-width: 150px;
      max-width: 150px;
    }}
            #{table_id} .gt_col_headings th.gt_left {{
    position: -webkit-sticky; 
    position: sticky;
    left: 0; 
    background-color: white; 
    z-index: 10; 
        }}
  ") else ""
  
  # Main CSS for scrollable, responsive table layout
  css <- glue::glue("
    #{table_id} .gt_table {{
      display: block;
      height: {height}px;
      width: {width}px;
      overflow-y: scroll;
      overflow-x: auto;
      table-layout: auto;
    }}
    #{table_id} thead {{
      position: sticky;
      top: 0;
      background-color: white;
      z-index: 3;
    }}
    #{table_id} .gt_col_headings {{
      border-top-style: none;
    }}
    {freeze_css}
    {header_css}
    

  ")
  
  # Apply CSS to the table
  gt_tbl %>%
    opt_css(css = css)
}
#' Bidirectional setdiff
#' 
#' @param x,y As in \code{setdiff}
#' @return A list of the two items with setdiff run in both 
#'   directions.  
#' @examples
#' setdiff2(1:5, 3:8)
#' @export
setdiff2 <- function(x, y) {
  list("In 'x' but not 'y'"=setdiff(x, y), "In 'y' but not 'x'"=setdiff(y, x))
}


ggplot_to_ppt_custom <- function(test_plot, HT = 6, WD = 8, EDIT = TRUE) {
  
  # Define the output file path (saved in the working directory)
  ppt_file <- file.path(getwd(), "ppt_fig.pptx")
  
  # Create a new PowerPoint object
  ppt <- officer::read_pptx()
  
  # Add a slide with "Title and Content" layout from "Office Theme"
  ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  
  # Ensure the plot is valid before adding it
  testgg <- try(invisible(ggplot2::ggplot_build(test_plot)), silent = TRUE)
  
  if (!"try-error" %in% class(testgg)) {
    
    # Add the plot to the slide using dml (editable graphics)
    ppt <- officer::ph_with(
      ppt, 
      rvg::dml(code = print(test_plot), 
               editable = EDIT  # Keep it editable in PowerPoint
      ), 
      location = officer::ph_location(width = WD, height = HT, newlabel = "hello")  # Set width and height for the plot
    )
  }
  
  # Save the PowerPoint file in the working directory with the specified name
  print(ppt, target = ppt_file)
  
  # Open the PowerPoint file
  browseURL(ppt_file)
}

jjsave <- function(figure, file.name = NULL , publish.dir = NULL,CAP = NULL, DPI = 300, UNITS = "in",device = "png",preview = FALSE, ...){
  
  local.dir = "/figures"
  
  if(is.null(file.name)){
    file.name <- paste0(deparse(substitute(figure)), ".",device)
  }
  
  local.dir <- paste0(getwd(),"/figures")
  
  if(!dir.exists(local.dir)){
    dir.create(local.dir)
  }
  
  FN <- paste0(local.dir,"/",file.name)
  
  # Save file to local directory
  ggsave(filename = FN, plot = figure, dpi = DPI, units = UNITS, device = device, ...) %>% suppressMessages()
  
  if(!is.null(publish.dir)){
    publish.dir <- paste0(publish.dir,"/figures")
    
    if(!dir.exists(publish.dir)){
      dir.create(publish.dir)
    }
    
    file.copy(FN, paste0(publish.dir,"/",file.name), overwrite = TRUE)
    
  }
  
  FNx <- paste0("figures/",file.name)
  
  
  
  if(!is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))){
    cat(paste0("[![",CAP, "](",FNx,")](",FNx,")\n"))
  } else if(isTRUE(preview)){
    browseURL(FNx)
  }else{
    figure
  }
}

jj_index <- function(dat, qmd, omit = "", checkbox_sep = "___", crosslink = FALSE, src.dat = NULL){
  
  if(is.data.frame(dat)){dat = list(dat)}
  
  vars <- map(dat, ~setdiff(names(.x), omit)) %>% unlist() %>% unique()
  labs <-  map(dat, collect.labels) %>% list_rbind() %>% mutate(label = ifelse(label == "", variable, label))
  chkbx <- labs %>% separate("variable",c("variable","b"), sep = checkbox_sep, fill = "right") %>% 
    group_by(variable) %>% 
    filter(sum(!is.na(b))>1) %>% 
    mutate(label = sub("\\(.*", "", label)) %>% 
    select(-b) %>% distinct()
  vars <- c(vars, chkbx$variable)
  
  labs <- bind_rows(labs, chkbx)
  
  if(!is.null(src.dat)){
    labs <- labs %>% mutate(derived = ifelse(variable %in% c(chkbx$variable, names(src.dat)), "", "Yes"))
  }
  
  
  
  prp <- parsermd::parse_rmd(qmd) %>%as.data.frame() %>% 
    mutate(order = cumsum(type == "rmd_heading")) %>% rowwise() %>% 
    mutate(x = ifelse(type == "rmd_chunk", paste0(parsermd::as_document(ast), collapse = ""), "")) %>% 
    mutate(y = "") %>% 
    filter(order >0)
  
  for(i in 1:length(vars)){
    prp %<>% mutate(y = ifelse(grepl(vars[i], x), paste(vars[i], y, sep = ", "), y))
  }
  
  toc <- get_toc(qmd)%>% 
    separate(title, into = c("title", "slug"), sep = "{#", fill = "right") %>% 
    mutate(slug = gsub("\\}.*$", "", slug)) %>% tidyr::fill(slug, .direction = "down") 
  
  
  ndf <- prp %>% select(order, variable = y) %>% separate_rows(variable, sep = ", ") %>% 
    filter(variable != "") %>% 
    left_join(., labs, by = c("variable")) %>% 
    left_join(.,toc , by = "order") %>% 
    mutate(c_link = glue::glue('<a href="#{slug}" class="quarto-xref" aria-expanded="false">§{section}</a>') )
  
  if(isTRUE(crosslink)){ndf$section <- ndf$c_link}
  
  
  ndf <- ndf %>%
    {if(!is.null(src.dat)) group_by(., variable, label, derived) else group_by(., variable, label)} %>%
    distinct() %>%
    summarise(sections = toString(section))
  
  return(ndf)
  
}

jpub <- function(qmd, output, directory){
  ## qmd = "filename.Rmd" (was originally part of my quarto conversion)
  ## Output is the quoted name for the output file, e.g. "Project X"
  ## directory is the path to the VSP biostats report folder for the project
  
  ## Publish report to OneDrive for VSP (copied from vpn directory)
  
  local.dir <- paste0(getwd(),"/reports")
  
  if(!dir.exists(local.dir)){
    dir.create(local.dir)
  }
  
  
  of <- paste0(Sys.Date()," ", output, ".html" )
  orig <- gsub("qmd|Rmd", "html", qmd)
  
  if(grepl("qmd", qmd)){
    quarto::quarto_render(input = qmd)
  }else{
    # rmarkdown::render
    rmarkdown::render(input = qmd)
  }
  
  # # Quarto does not yet support rendering to different directory -- bleh
  file.copy(from = orig, to = paste0(directory,"/",of), overwrite = TRUE)
  file.copy(from = orig, to = paste0(local.dir,"/",of), overwrite = TRUE)
}

slugify <- function(text) {
  text <- tolower(text)
  text <- gsub(" ", "-", text)
  text <- gsub("[^a-z0-9-]", "", text)
  return(text)
}

process_line <- function(line, inside_chunk, inside_tabset, used_slugs) {
  # If we're inside a code chunk or a tabbed section, return the line as is
  if (inside_chunk || inside_tabset) {
    return(list(line = line, used_slugs = used_slugs))
  }
  
  # Match headers (e.g., "# Section 1", "## Section 1.1", etc.)
  if (grepl("^#{1,6} +[^#]", line)) {
    # Extract existing slug if present
    existing_slug <- regmatches(line, regexpr("\\{#([^}]*)\\}$", line))
    if (length(existing_slug) > 0) {
      # Remove curly braces and add to used_slugs
      used_slugs <- c(used_slugs, gsub("\\{#([^}]*)\\}$", "\\1", existing_slug))
    } else {
      # Extract the header text and trim whitespace
      header_text <- trimws(gsub("^#{1,6} +", "", line))
      # Generate the slug
      slug <- paste0(slugify(header_text), "-",Sys.Date())
      # Ensure the slug is unique
      original_slug <- slug
      i <- 1
      while (slug %in% used_slugs) {
        i <- i + 1
        slug <- paste0(original_slug, "_x_", i)
      }
      # Add the slug to the set of used slugs
      used_slugs <- c(used_slugs, slug)
      # Add the cross-reference
      line <- paste0(line, " {#", slug, "}")
    }
  }
  return(list(line = line, used_slugs = used_slugs))
}


add_cross_links <- function(input_file, output_file = input_file) {
  # Read the content of the Quarto Markdown file
  lines <- readLines(input_file)
  
  # Create binary vectors to track if we're inside a code chunk or a tabbed section
  in_code_chunk <- rep(FALSE, length(lines))
  in_tabset <- rep(FALSE, length(lines))
  
  # Track if we are in a code chunk or a tabbed section
  inside_code_chunk <- FALSE
  inside_tabset <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^```", lines[i])) {
      inside_code_chunk <- !inside_code_chunk
    }
    in_code_chunk[i] <- inside_code_chunk
    
    if (grepl("^::: panel-tabset", lines[i])) {
      inside_tabset <- TRUE
    } else if (grepl("^:::", lines[i]) && inside_tabset) {
      inside_tabset <- FALSE
    }
    in_tabset[i] <- inside_tabset
  }
  
  # Track used slugs to ensure uniqueness
  used_slugs <- character(0)
  
  # Process each line and update used slugs
  processed_lines <- character(length(lines))
  for (i in seq_along(lines)) {
    result <- process_line(lines[i], in_code_chunk[i], in_tabset[i], used_slugs)
    processed_lines[i] <- result$line
    used_slugs <- result$used_slugs
  }
  
  # Write the modified content back to the file
  writeLines(processed_lines, output_file)
}

add_alert <- function(yaml_file = "_alerts.yml", alert_name = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required but not installed.")
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("Package 'rstudioapi' is required but not installed.")
  }
  
  # Default template
  default_alert <- list(
    title = "",
    type = "",
    content = "",
    icon = TRUE,
    collapse = FALSE,
    date_created = I(as.character(Sys.Date())),  # <- Force it to be written as string
    resolved = FALSE,
    date_resolved = "",
    resolution = "",
    include_extras = FALSE
  )
  
  # Custom handlers to force true/false instead of yes/no
  custom_handlers <- list(
    logical = function(x) if (x) "true" else "false"
  )
  
  if (!file.exists(yaml_file)) {
    if (is.null(alert_name)) {
      new_alert_name <- "alert001"
    } else {
      new_alert_name <- alert_name
    }
    alerts_data <- list(alerts_list = list())
    alerts_data$alerts_list[[new_alert_name]] <- default_alert
    yaml::write_yaml(alerts_data, yaml_file, handlers = custom_handlers)
  } else {
    alerts_data <- yaml::read_yaml(yaml_file)
    if (is.null(alerts_data$alerts_list)) {
      alerts_data$alerts_list <- list()
    }
    n <- length(alerts_data$alerts_list)
    if (is.null(alert_name)) {
      new_alert_name <- paste0("alert", sprintf("%03d", n + 1))
    } else {
      new_alert_name <- alert_name
    }
    alerts_data$alerts_list[[new_alert_name]] <- default_alert
    yaml::write_yaml(alerts_data, yaml_file, handlers = custom_handlers)
  }
  ## Fix quoting or boolean madness
  ## Having such trouble with true/false and yes/no quoted and not. Bah!
  x <- readLines(yaml_file)
  x <- gsub("'true'", "true", x, fixed = TRUE)
  x <- gsub("'false'", "false", x, fixed = TRUE)
  writeLines(x, yaml_file)
  
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentOpen(normalizePath(yaml_file))
  } else {
    message("RStudio API not available; please open ", yaml_file, " manually.")
  }
  
  cat(paste0(
    "To display this alert in your document, copy and paste the following shortcode into your text editor:\n\n",
    "::: {#ale-", new_alert_name, "}\n",
    "{{< alert '", new_alert_name, "' >}}\n",
    ":::"
  ))
}

read_alerts_df <- function(yaml_file = "_alerts.yml") {
  # Load required packages (install if necessary)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required but not installed.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required but not installed.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }
  
  # Read the YAML file
  alerts_data <- yaml::read_yaml(yaml_file)
  
  # Extract the alerts_list
  alerts_list <- alerts_data$alerts_list
  
  # Convert the alerts_list to a dataframe
  # Each alert becomes a row; missing fields will become NA
  df <- purrr::map_df(names(alerts_list), function(alert_id) {
    alert <- alerts_list[[alert_id]]
    alert$id <- alert_id  # add an id column from the key
    tibble::as_tibble(alert)
  })
  
  # Optional: reorder columns to have 'id' first
  df <- df[, c("id", setdiff(names(df), "id"))]
  
  return(df)
}

grep_alerts <- function(input_string){
  
  pattern <- "\\{\\{<\\s*alert\\s+([^ >]+)\\s*>\\}\\}"
  matches <- str_match_all(input_string, pattern)[[1]]
  alert_names <- matches[,2]
  alert_names
}

format_alerts <- function(qmd, yaml){
  
  parsermd::parse_rmd(qmd) %>%as.data.frame() %>% 
    mutate(order = cumsum(type == "rmd_heading")) %>% rowwise() %>% 
    mutate(x = ifelse(type == "rmd_markdown", paste0(parsermd::as_document(ast), collapse = ""), "")) %>% 
    mutate(has_alert = grepl("\\{\\{< alert", x)) %>% 
    mutate(y = "") %>% 
    filter(order >0, has_alert) %>% 
    left_join(., get_toc(qmd), by= "order") %>% 
    mutate(x = gsub('\\"', "", x)) %>% mutate(x = gsub("\\'", "", x)) %>% 
    mutate(id = toString(grep_alerts(x))) %>% 
    separate_rows(., "id", sep = ",") %>% mutate(id= trimws(id)) %>% ungroup() %>% select(-title, -type) %>%  
    left_join(read_alerts_df(yaml), ., by = "id") %>% 
    mutate(section = case_when(is.na(section) ~ "Not included", 
                               !grepl("#ale", x) ~ glue::glue('§{section}'),
                               TRUE ~ glue::glue('<a href="#ale-{id}" class="quarto-xref"
                                                aria-expanded="false">§{section}</a>') 
    )) %>% 
    select(section, title, content ,resolved, any_of(c("date_created", "date_resolved")))
}
