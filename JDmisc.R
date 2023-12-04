

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
  
  cat(deparse(substitute(df)), "<- set_label(", deparse(substitute(df)), ",\n",
      paste0(prev$combined))
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
my_theme <-list(
  "tbl_summary-str:default_con_type" = "continuous2",
  "tbl_summary-str:continuous_stat" = c(
    "{mean} ({sd})",
    "{median} ({p25} - {p75})",
    "{min} - {max}"),
  "style_number-arg:big.mark" = "",
  "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1),
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
gtsummary::set_gtsummary_theme(my_theme)



jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, 
                force.continuous = FALSE, missing = "ifany", missing_text = "Missing",
                spanner.size = NULL, spanner.text = NULL, add.n = TRUE, percent = "column", 
                one_row_binary = FALSE, ...){
  {if(!is.null(by)) dat[,by] <- clear.labels(dat[,by])}
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
                digits = list(all_continuous() ~ c(Digits, Digits)),
                by = !!by,
                missing = missing,
                missing_text = missing_text,
                percent = percent,
                ...) %>%
    bold_labels() %>%
   {if(add.n) add_n(.) else .}  %>% 
    {if(add.p) add_p(., pvalue_fun = function(x) style_pvalue(x, digits = 3)) else .}  %>% 
    {if(overall) add_overall(., last = TRUE) else .}
  
    if(!is.null(spanner.size)) tab %<>% modify_spanning_header(c(paste0("stat_", 1:spanner.size)) ~ paste0("**",spanner.text ,"**"))
 
  return(tab)
}

jgtt <- function(dat, col.names = TRUE){
  
  dat %>% 
    {if(isTRUE(col.names)) sjlabelled::label_to_colnames(.)else .}  %>% 
    gt() %>%
    opt_row_striping() %>%
    gt::tab_options(table_body.hlines.color = "white",
                    row.striping.background_color = "#fafafa",
                    source_notes.font.size = 12,
                    table.font.size = 12,
                    # table.width = px(700),
                    heading.align = "left",
                    column_labels.font.weight = 'bold',
                    heading.title.font.size = 16,
                    table.border.top.color = "transparent",
                    table.border.top.width = px(3),
                    data_row.padding = px(4))
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

## Maybe break into 3 separate pieces?

rms.sum.frame <- function(summary, plot = FALSE, trib, breaks = seq(0.6, 2, .2), int = TRUE){
  
  cap <- attr(summary, "adjust") %>% 
    str_split(., "\\s") %>% unlist() %>% 
    data.frame(adjust = .) %>% 
    separate(adjust, into = c("term", "adjust"), sep = "=", fill = "right") %>% 
    left_join(., trib, by = "term") %>% na.omit() %>% arrange(level) %>% 
    mutate(L = paste(label, adjust, sep = ": ")) %$% L %>% 
    paste0(collapse = "; ") %>% paste("Interaction terms adjusted to:", ., sep = "\n")
  
  if(int == FALSE){cap <- ""}
  
  res <- tibble::as_tibble(summary, rownames = "term")
  names(res) <- c("term", "low", "high", "diff", "effect", "std.error", "conf.low", "conf.high", "type")
  res$term <- trimws(res$term)
  
  res <- res %>% 
    ## Separate variable names from adjust-to values
    separate(term, into = c("term", "highc", "lowc"), sep = " *[-:] *", fill = "right") %>%
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
    filter(type == "ratio") %>%
    left_join(., trib, by = "term" ) %>%
    mutate(facet = ifelse(vt == "Continuous", new, paste(new, lowc, sep = "\n Ref: ")),
           facet = fct_reorder(facet, level, min),
           axis = ifelse(vt == "Continuous", paste(highc, lowc, sep = " vs. "), highc))
  
  res.plot <- res %>%
    ggplot(., aes(x = effect, y = axis)) + geom_point() + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
    theme_classic() + 
    geom_vline(xintercept = 1, linetype = 2) + 
    labs(x = "Hazard ratio", y = NULL) +
    facet_grid(facet~., scales = "free_y", switch = "y")+
    theme(strip.text.y.left = element_text(angle = 0))+ 
    theme(strip.background  = element_blank()) + 
    theme(panel.spacing.y = unit(0,"line")) +
    theme(strip.placement = 'outside') +
    scale_x_continuous(breaks = breaks) +
    theme(panel.grid.major.x = element_line(colour = "gray90"), 
          panel.grid.minor.x = element_line(colour = "gray95", 
                                            size = 0.3), 
          plot.background = element_rect(fill = "ghostwhite", size = 1.3)) + 
    theme(plot.caption = element_text(size = 7, hjust = 0)) +
    labs(caption = cap)
  
  res.table <- res %>% arrange(level)%>% mutate(levels = paste(highc, lowc, sep = " vs. ")) %>% 
    select(label, levels, effect, conf.low, conf.high)%>% mutate(across(where(is.numeric), ~round(.x, 3))) %>% 
    jable(., caption = "Model summary") %>% kableExtra::collapse_rows(., columns = 1) 
  
  if(int == TRUE){ res.table <- res.table %>% kableExtra::add_footnote(., label = cap)}
  
  ifelse(plot, return(res.plot), return(res.table))
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

j.reactable <- function(dat, col.names = TRUE, searchable = TRUE, resizable = TRUE, theme = espn(),
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



rms.sum.table <- function(summary, trib){
  
  res <- tibble::as_tibble(summary, rownames = "term")
  names(res) <- c("term", "low", "high", "diff", "effect", "std.error", "conf.low", "conf.high", "type")
  res$term <- trimws(res$term)
  
  res <- res %>% 
    ## Separate variable names from adjust-to values
    separate(term, into = c("term", "highc", "lowc"), sep = " *[-:] *", fill = "right") %>%
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
    filter(type == "ratio") %>%
    left_join(., trib, by = "term" ) %>%
    mutate(facet = ifelse(vt == "Continuous", new, paste(new, lowc, sep = "\n Ref: ")),
           facet = fct_reorder(facet, level, min),
           axis = ifelse(vt == "Continuous", paste(highc, lowc, sep = " vs. "), highc))
  res
}

rms.sum.table2 <- function(summary, trib, anova){
  
  rtab <- data.frame(anova) %>%add_rownames() %>%
    filter(rowname != "TOTAL") %>% rename(term = rowname)
  
  res <- tibble::as_tibble(summary, rownames = "term")
  names(res) <- c("term", "low", "high", "diff", "effect", "std.error", "conf.low", "conf.high", "type")
  res$term <- trimws(res$term)
  
  res <- res %>% 
    ## Separate variable names from adjust-to values
    separate(term, into = c("term", "highc", "lowc"), sep = " *[-:] *", fill = "right") %>%
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
    filter(type == "ratio") %>%
    left_join(., trib, by = "term" ) %>%
    mutate(facet = ifelse(vt == "Continuous", new, paste(new, lowc, sep = "\n Ref: ")),
           facet = fct_reorder(facet, level, min),
           axis = ifelse(vt == "Continuous", paste(highc, lowc, sep = " vs. "), highc))%>% 
    arrange(level)%>% 
    mutate(levels = paste(highc, lowc, sep = " vs. ")) %>% 
    left_join(., rtab, by = "term") %>%
    select(label, levels, OR= effect, conf.low, conf.high, `d.f.`, P)%>% 
    mutate(across(where(is.numeric), ~round(.x, 3)))
  
  res
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

rca_instrument <- function(rdat, instrument = NA){
  rdat %>% 
    filter(redcap_repeat_instrument %in% instrument) %>% 
    remove.blank.columns2() %>% droplevels() %>% 
    sjlabelled::copy_labels(., rdat)
}

rca_dictionary <- function(rcon, included.ids, id.var = "record_id"){
  
  missing_summary <- missingSummary(rcon) %>% filter(!!sym(id.var) %in% included.ids) %>% 
    filter(n_missing > 0) %>% select(id.var, redcap_repeat_instance, missing) %>%
    separate_rows(., "missing", sep = ",") %>%
    group_by(record_id, missing) %>% summarise(x = ifelse(is.na(redcap_repeat_instance), NA, toString(redcap_repeat_instance))) %>%
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



