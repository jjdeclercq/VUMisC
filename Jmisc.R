# devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/Jmisc.R")

## To update R

# updateR::updateR(admin_password = 'Use your computer password!')

model.est.logistic <- function(FIT)
{ est <- coef(FIT)
ses <- sqrt(diag(vcov(FIT)))
z   <- est/ses
p   <- 2*pnorm(abs(z),lower.tail=FALSE)
cil <- format(round(exp(est - 1.96*ses),2),nsmall=2,scientific=FALSE,trim=TRUE)
ciu <- round(exp(est + 1.96*ses),2)
ciu <- format(ifelse(ciu>=100,100,ciu),nsmall=2,scientific=FALSE,trim=TRUE)
ci  <- paste(cil,ciu,sep=' - ')
OR=format(round(exp(est),2),nsmall=2)
p   <- format(round(p,3),nsmall=3,scientific=FALSE)
if(any(p=='0.000')) {p[p=='0.000']<-'< 0.0001'}
#              if(any(p<='.05')) {p[p<='0.05']<-paste(p[p<='0.05'],'*',sep="")}
p   <- format(p,trim=TRUE)
est <- format(round((est),3),nsmall=2,scientific=FALSE)
Std.Error <- format(round(ses,3),nsmall=3,scientific=FALSE)
Wald.Z<-format(round(z,3),nsmall=3,scientific=FALSE)
SP<-""
Table <- cbind(est,Std.Error,OR,ci,SP,p)
Table<-data.frame(Table)
Table <-upData(Table,rename=c(est="Coef",
                              Std.Error="S.E.",
                              ci="C.I.", 
                              SP="",
                              p="Pr(>|t|)"))
Table}


model.ols <- function(FIT)
{ est <- coefficients(FIT)
ses <- sqrt(diag(vcov(FIT)))
z   <- est/ses
degreeOfFreedom <- FIT$df.residual
p   <- 2*pt(abs(z),lower.tail=FALSE, df=degreeOfFreedom)
cil <- format(round((est - qt(0.975, df=degreeOfFreedom)*ses),2),nsmall=2,scientific=FALSE,trim=TRUE)
ciu <- format(round((est + qt(0.975, df=degreeOfFreedom)*ses),2),nsmall=2,scientific=FALSE,trim=TRUE)
ci  <- paste(cil,ciu,sep=' - ')
p   <- format(round(p,3),nsmall=3,scientific=FALSE)
if(any(p=='0.000')) {p[p=='0.000']<-'< 0.001'}
p   <- format(p,trim=TRUE)
est <- format(round((est),3),nsmall=2,scientific=FALSE)
Std.Error <- format(round(ses,3),nsmall=3,scientific=FALSE)
SP<-""
Table <- cbind(est,Std.Error,ci,SP,p)
Table<-data.frame(Table)
Table <-upData(Table,rename=c(est="Coef",
                              Std.Error="S.E.",
                              ci="C.I.", 
                              SP="",
                              p="Pr(>|t|)"))

Table}  

zip.table <- function(model){
  sum.mod <- summary(model)
  ROW.count <- length(model$coef$count)
  coefffs <- as.data.frame(exp(sum.mod$coefficients$count[c(2:ROW.count),1]))
  p_vals.count <- pnorm(abs(coef(sum.mod)$count[,'z value']),lower.tail=FALSE)*2
  conf.count <- matrix(exp(confint(model)[c(2:ROW.count),]), ncol = 2)
  
  p <- as.data.frame(p_vals.count[2:ROW.count])
  type <- rep("count", (ROW.count  -1))
  interp <- rep("coefficient", (ROW.count  -1))
  
  output.count <- cbind(type, interp, coefffs, conf.count, p)
  names(output.count) <- c("Model","Interpretation", "Estimate", "Lower", "Upper", "p-value")
  
  ROW.zero <- length(model$coef$zero)
  coefffs.zero <- as.data.frame(exp(sum.mod$coefficients$zero[c(2:ROW.zero),1]))
  p_vals.zero <- pnorm(abs(coef(sum.mod)$zero[,'z value']),lower.tail=FALSE)*2
  conf.zero <- matrix(exp(confint(model)[c((ROW.count+2):(ROW.zero+ROW.count)),]), ncol = 2)
  #upper.zero <- as.data.frame(exp(confint(model)[c(2:ROW.count),])["97.5 %"])
  p <- as.data.frame(p_vals.zero[2:ROW.zero])
  type.zero <- rep("zero inflated", (ROW.zero  -1))
  interp.zero <- rep("odds ratio", (ROW.zero  -1))
  
  output.zero <- cbind(type.zero, interp.zero, coefffs.zero, conf.zero, p)
  
  names(output.zero) <- c("Model","Interpretation", "Estimate", "Lower", "Upper", "p-value")
  
  output <- rbind(output.zero, output.count)
  output[,c(3:6)] <- round(output[,c(3:6)],3)
  output
}

lw <- function(x) length(which(x))
lu <- function(x) length(unique(x))

compile <- "html"


jable <- function(x, caption = "Needs a caption") {
  kable(x, compile, booktabs = TRUE, escape = TRUE, caption = caption) %>% kable_styling()
}



LATEX <- function(input){ 
  cat(capture.output(input)[-1], sep='\n')
} 

jatex <- function(X, caption = "Needs a caption"){
  if(compile=="latex"){
    LATEX(Hmisc::latex(X,file = "",digits = 2,exclude1 = FALSE, what = "%",pctdig = 1, long = TRUE, longtable = F, prmsd = TRUE, prn = TRUE, legend.bottom = T, caption = caption, lines.page=79,prtest="P", size="footnotesize",where="!htbp"))}
  else{{Hmisc::html(X,digits = 2,exclude1 = FALSE, what = "%",pctdig = 1, long = TRUE, longtable = F, prmsd = TRUE, prn = TRUE, legend.bottom = T, caption = caption, lines.page=79,prtest="P", size="footnotesize")}}
  
}

j.scribe <- function(x, desc, scroll = TRUE) {
  if(compile=="latex"){
    latex(Hmisc::describe(x, descript = desc), file = "")
  }
  else{Hmisc::html(Hmisc::describe(x, descript = desc), file = "", scroll = scroll)}
}

######### labeling factors#############
LAB <- function(df, var){
  open <- paste(deparse(substitute(df)), "<- within(",deparse(substitute(df)), ",{\n")
  close <- "})"
  I <- length(var)
  X <- rep(NA,I)
  LL <- rep(NA,I)
  for(i in 1:I){
    V <- var[i]
    LL[i] <- paste(deparse(substitute(df)), V, sep = "$")
    X[i] <- (paste0(var[i], " %<>% factor(., levels = c(", paste(sort(unique(eval(parse(text = LL[i])))), collapse = ","), "), labels = c( ))\n"))
  }
  # 
  cat(open,X, close)
}
#################################

# Code to help set labels

j.label <- function(df){
  cat(deparse(substitute(df)), "<- set_label(", deparse(substitute(df)), ",\n",
      paste0(names(df), sep = " = ,", collapse = "\n"), ")")
}

# cat("set_label(", "spec,",paste0(names(spec), sep = " = ,", collapse = "\n"), ")")
# cat("scrip <- set_label(", "scrip,",paste0(names(scrip), sep = " = ,", collapse = "\n"), ")")

## Interval-based PDC calculation
# this function appends extra columns to the data
prepare.dat <- function(dat, ident, supplied, dates.filled, end.date.rule = "given", given.end.dates, end.date.static = "12/31/2016"){
  
  XX <- nrow(dat)
  dat <- within(dat,{
    
    #wsd <- rep(NA, X)
    x <- rep(NA, XX)
    csa <- rep(NA, XX)
    csb <- rep(NA, XX)
    dsw <- rep(NA, XX)
    pdc.i <- rep(NA, XX)
    tbf <- rep(NA, XX)
    fn <- rep(NA, XX)
    fd <- rep(NA, XX)
    fdd <- dat[,dates.filled]
    fs <- dat[,supplied]
    id <- dat[,ident]
    #end.dt <- ifelse(end.date.rule == "given",as.Date(dat[,given.end.dates], "%m/%d/%Y", origin=origin),
    #                 as.Date(end.date.static, "%m/%d/%Y"))
    # end.dt <- as.Date(dat[,given.end.dates], "%m/%d/%Y", origin=origin)
    #end.dt <- dat[,given.end.dates]
  })
  return(dat)
}

PDC.interval <- function(sub.dat, plotting  = FALSE, remove.last.fill = FALSE, remove.supply.on.switch = FALSE){
  R <- nrow(sub.dat)
  sub.dat <- within(sub.dat,{
    for(i in 1:R){
      fd[i] <- ifelse(i==1, 0, as.numeric(difftime(fdd[i], fdd[i-1])) + fd[i-1] )
    }
    
    for(i in 1:R){  
      # time between fills
      tbf[i] <-  ifelse(i==R, as.numeric(difftime(end.dt[1],fdd[1])) - fd[i], fd[i+1] - fd[i])
      
      # gaps/ surplus at the beginning of an interval
      x[i] <- fs[i] - tbf[i]
      
      # carryover / gap supply at the end of an interval
      csa[i] <- ifelse(i==1, x[i], csb[i-1] + x[i])
      
      # days supply in the window
      dsw[i] <- tbf[i] + (csa[i] < 0)*csa[i]
      
      # carryover supply only at the beginning of an interval
      csb[i] <- csa[i]*(csa[i] > 0)
      if(remove.supply.on.switch == TRUE){ ## This is the latest addition to function, if old code doesn't work--look here
        if(switch[i]==1){csb[i] <- 0} # comment out if no switches
      }
      
      # pdc calculation
      pdc.i[i] <- dsw[i]/tbf[i]
      
      # fill number
      fn[i] <- i
      
    }
    # get denominator for whole window
    end.dt.num <- as.numeric(difftime(end.dt[1],fdd[1]))  
  })
  ## don't know how/why the 'i' column is initialized
  sub.dat$i <- NULL
  
  if(remove.last.fill==TRUE){
    sub.dat <- sub.dat[1:(nrow(sub.dat)-1),]
  }
  
  if(plotting== TRUE){
    
    sub.dat <- rbind(sub.dat, sub.dat[nrow(sub.dat),])
    sub.dat[nrow(sub.dat), "fn"] <- nrow(sub.dat)
    
  }  
  
  sub.dat
}

######## Format percent

pc = function(x,...){
  x = sprintf( '(%1.0f%%)',100*x)
  gsub("NaN%", ".", gsub("NA%", ".", x))
  
}

med.iqr <- function(x) paste0(median(x)," (", quantile(x, 0.25), ", ",quantile(x, 0.75), ")")
mean.sd <- function(x) paste0(round(mean(x),2)," (", round(sd(x), 2), ")")


# Create table of missing data with labels
miss.list <- function(dat, var){
  var2 <- sym(var)
  ids <- toString(dat %>% filter(is.na(!!var2)) %$% record_id)
  
  data.frame(variable = var,label = label(dat[,var]), IDs = ids)
}


# Create tidy survival times output
# Not sure why SEs are different
j.tidy.surv <- function(fit, times){
  FIT<- broom::tidy(fit)
  TIMES <- times[times %nin% FIT$time ]
  FIT %>% 
    bind_rows(., data.frame(time = TIMES,n.censor = rep(0, length(TIMES)), n.event = rep(0, length(TIMES)))) %>% 
    arrange(time) %>% 
    mutate(events = cumsum(n.event), censored = cumsum(n.censor)) %>% 
    fill(., c( "estimate", "std.error", "conf.high", "conf.low"), .direction = "down")%>% 
    fill(., "n.risk", .direction = "up") %>% filter(time %in% times) %>% 
    mutate(n.event = events - lag(events, default = 0)) %>% 
    select(time, n.risk,n.event, c.events = events, censored, survival = estimate, std.error, conf.low, conf.high) %>% 
    mutate_at(., c("survival","std.error","conf.high", "conf.low"), function(x) round(x, 3)) %>% 
    mutate(time = round(days(time)/months(1))) %>% rename(months = time)
}

j.tidy.surv_2 <- function(fit, times, strat){
  ## This function just can incorporate different strata
  FIT<- broom::tidy(fit) %>% filter(strata == strat)
  TIMES <- times[times %nin% FIT$time ]
  FIT %>% 
    bind_rows(., data.frame(time = TIMES,n.censor = rep(0, length(TIMES)), n.event = rep(0, length(TIMES)))) %>% 
    arrange(time) %>% 
    mutate(events = cumsum(n.event), censored = cumsum(n.censor)) %>% 
    fill(., c( "estimate", "std.error", "conf.high", "conf.low"), .direction = "down")%>% 
    fill(., "n.risk", .direction = "up") %>% filter(time %in% times) %>% 
    mutate(n.event = events - lag(events, default = 0)) %>% 
    select(time, n.risk,n.event, c.events = events, censored, survival = estimate, std.error, conf.low, conf.high) %>% 
    mutate_at(., c("survival","std.error","conf.high", "conf.low"), function(x) round(x, 3)) %>% 
    mutate(time = round(days(time)/months(1))) %>% rename(months = time)
}

numerify <- function(col){ 
  name <- toString(enquo(col)[2])
  ifelse(col==0, "", as.numeric(strsplit(grep("[0-9]", name, v = T), "\\.\\.\\.")[[1]][2]))
}

## Doesn't work well unless there is a new variable added into df
j.label4 <- function(df, label.df){
  needs.labels <- data.frame(label = Hmisc::label(df)) %>% filter(label == "") %>% rownames()
  new <- names(df)[names(df) %nin% label.df$variable]
  new <- new[new %in% needs.labels]
  top <- data.frame(variable = new, label = '')
  
  prev <- label.df[label.df$variable %in% needs.labels,]
  prev <- rbind(top, prev)
  prev$combined <- paste(prev$variable, " = '", prev$label, "',\n", sep = "")
  
  prev[nrow(prev), "combined"] <- gsub(",\n", ")",prev[nrow(prev), "combined"])
  
  cat(deparse(substitute(df)), "<- set_label(", deparse(substitute(df)), ",\n",
      paste0(prev$combined))}


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

## Data table helper function 
j.datatable <- function(X, caption = NULL, nrow = 10, width = 1000)   {
  datatable(X,caption = caption, options = list(pageLength = nrow,
      autoWidth = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().tables().body()).css({'font-size': '11px'});",
        "$(this.api().tables().header()).css({'font-size': '12px'});",
        "}")), rownames = FALSE, width = width, class = c('compact stripe' ))}

my.render.cont <- function(x) {
  gsub(":", "h", with(stats.apply.rounding(stats.default(x), digits=2),
    c(" ","mean (sd)"=sprintf("%s (&plusmn; %s)", MEAN, SD),
    "med (iqr) "=sprintf("%s [%s, %s]", MEDIAN, q25, q75),
    "min, max "=sprintf("[%s, %s]", MIN, MAX))))
}

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, 
            x)
  } else x
}


remove.blank.columns <- function(dat) {
  names <- dat %>% dplyr::summarise(across(everything(), .fns = ~sum(.x==""|is.na(.x))==nrow(dat)))%>% 
    mutate(i = 1) %>% pivot_longer(.,-i) %>% filter(!value) %$% name
  
  dat[,names]
}

## Fix dates
fix.dates <- function(dat) {dat %>% mutate(across(all_of(dates[dates %in% names(dat)]), ymd))}

n_pct <- function(x){
  paste(x, " (",round(100*x/sum(x), 1), "%)", sep = "")
}

pc.x <- function(x,...){
  x = sprintf( '%#.1f%%',100*x)
  gsub("NaN%", ".", gsub("NA%", ".", x))
  
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%0.00f%% (%d)", PCT, FREQ))))
}

stats.default2 <- function (x, useNA = NULL, quantile.type = 7) 
{
  
  if (is.factor(x) || is.character(x)) {
    y <- table(x, useNA = useNA)
    nn <- names(y)
    nn[is.na(nn)] <- "Missing"
    names(y) <- nn
    lapply(y, function(z) list(FREQ=z, PCT=100*z/length(x), PCTnoNA=100*z/sum(y)))
  }
  else {
    stop(paste("Unrecognized variable type:", class(x)))
  }
}

my.render.missing2b <- function (x, ...) 
{
  with(stats.apply.rounding(stats.default(!is.na(x)), ...)$Yes, 
       c(n = sprintf("%s (%s%%)", FREQ, PCT)))
}

my.render.cat2 <- function(x) {
  
  c("", sapply(stats.default2(x), function(y) with(y,sprintf("%.1f%% (%d)", PCTnoNA, FREQ))))
}


my.render.cont <- function(x) {
  gsub(":", "h", with(stats.apply.rounding(stats.default(x), digits=3), 
                      c(" ","mean (sd)"=sprintf("%s (&plusmn; %s)", MEAN, SD),
                        "med (iqr) "=sprintf("%s [%s, %s]", MEDIAN, q25, q75),
                        "min, max "=sprintf("[%s, %s]", MIN, MAX))))
}

jrm <- function (x, ...) 
{
  with(stats.apply.rounding(stats.default(is.na(x), ...), ...)$Yes, 
       c(Missing = sprintf("%s%% (%s)", PCT, FREQ)))
}

my.render.missing3 <- function (x, ...) 
{
  with(stats.apply.rounding(stats.default(is.na(x)), ...)$Yes, 
       c(Pending = sprintf("%s %% (%s)", PCT, FREQ)))
} 


## FUT2
my.render.missing <- function (x, ...) 
{
  with(stats.apply.rounding(stats.default(!is.na(x)), ...)$Yes, 
       c(n = sprintf("%s", FREQ)))
}


my.render.cont_2 <- function(x) {
  gsub(":", "h", with(stats.apply.rounding(stats.default(x), digits=4), 
                      c(" ","mean (sd)"=sprintf("%s (&plusmn; %s)", MEAN, SD),
                        "med (iqr) "=sprintf("%s [%s, %s]", MEDIAN, q25, q75),
                        "min, max "=sprintf("[%s, %s]", MIN, MAX))))
}


my.render.cont_D <- function(x, d) {
  gsub(":", "h", with(stats.apply.rounding(stats.default(x), digits=4, round.median.min.max = FALSE, round.integers = TRUE), 
                      c(" ","mean (sd)"=sprintf("%s (&plusmn; %s)", MEAN, SD),
                        "med (iqr) "=sprintf("%s [%s, %s]", MEDIAN, q25, q75),
                        "min, max "=sprintf("[%s, %s]", MIN, MAX))))
}

# ### This is a good start at listing the number of functions/ packages used in an r scriptc############
# ## Exports the file into temp file
# knitr::purl("encobini.Rmd", "test.R", documentation = 2)
# 
# ## Gets parse data (duh)
# tmp <- getParseData(parse("test.R", keep.source=TRUE))
# 
# ## formats output
# tmp %>% filter(token %in% c("SPECIAL", "SYMBOL_FUNCTION_CALL") )%>% group_by(text) %>% tally() %>% rowwise() %>% 
#   mutate(f = toString(find(text))) %>% separate_rows(.,f, sep = ",") %>% group_by(f) %>% tally()
# 
# ## This function creates a list of functions used by package (though it ignores magrittr)
# NCmisc::list.functions.in.file("test.R", alphabetic = TRUE)
#############################


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

require(captioner)
table_nums <- captioner()
jcap <- function(cap = "CAPTION"){
  j_cap = table_nums(name = sample(letters, 6, replace = TRUE) %>% paste0(., collapse = ""), caption = cap)
  return(j_cap)
}


# gtcap <- function(dat, cap = "CAPTION"){
#   j_cap = table_nums(name = sample(letters, 6, replace = TRUE) %>% paste0(., collapse = ""), caption = cap)
#   as_gt(dat) %>%
#     gt::tab_header(j_cap) 
# }

gtcap <- function(dat, cap = "CAPTION"){
  j_cap = table_nums(name = sample(letters, 6, replace = TRUE) %>% paste0(., collapse = ""), caption = cap)
  if("gtsummary" %in% class(dat)){dat <- as_gt(dat)}
  dat %>%
    gt::tab_header(j_cap) 
}


jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, force.continuous = FALSE){
  {if(!is.null(by)) dat[,by] <- clear.labels(dat[,by])}
  {if(order.cat) dat %<>% mutate(across(is.factor|is.character, ~fct_infreq(factor(.x))))}
  
  
  TYPE <-  list( all_dichotomous() ~ "categorical")
  if(force.continuous) {TYPE[[2]] <- where(is.numeric) ~ "continuous2"}
  
  dat  %>%
    tbl_summary(type = TYPE,
                digits = list(all_continuous() ~ c(Digits, Digits)),
                by = !!by,
                missing = "ifany",
                missing_text = "Missing")%>%
    bold_labels() %>%
    add_n() %>% 
    {if(add.p) add_p(.) else .}  %>% 
    {if(overall) add_overall(., last = TRUE) else .} 
}
  

jgtt <- function(dat){
  dat %>% gt() %>%
    opt_row_striping() %>%
    gt::tab_options(table_body.hlines.color = "white",
                    row.striping.background_color = "#fafafa",
                    source_notes.font.size = 12,
                    table.font.size = 12,
                    # table.width = px(700),
                    heading.align = "left",
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
    select(label, levels, effect, conf.low, conf.high)%>% mutate(across(is.numeric, ~round(.x, 3))) %>% 
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

## easy yes-no of binary variables
yesno <- function(logic) {ifelse(logic, "Yes", "No")}

#### ESPN theme for reactable
# https://github.com/kcuilla/reactablefmtr/blob/main/R/themes.R
espn <- function(font_family = "Arial",
                 font_size = 12,
                 font_color = "#6C6D6F",
                 header_font_family = "Arial",
                 header_font_size = 11,
                 header_font_color = "#48494a",
                 cell_padding = 7) {
  
  reactableTheme(
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#ededed",
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

checkbox.table2 <- function(dat, selection, id.vars = "study_id"){
  dat %>% select(id.vars, contains(selection)) %>% 
    pivot_longer(.,c(-all_of(id.vars)), values_drop_na = TRUE, values_transform = list(value = as.character ))%>%
    group_by(value) %>% tally() %>% 
    mutate( pct = paste(round(100*n/nrow(dat), 1), "%", sep = "")) %>% 
    select(value, n, pct) %>%
    arrange(desc(n)) %>%jgtt()
}

checkbox.upset2 <- function(dat, selection, id.vars = "study_id"){
  dat %>% select(id.vars, contains(selection)) %>% 
    pivot_longer(.,c(-all_of(id.vars)), values_drop_na = TRUE, values_transform = list(value = as.character ))%>%
    mutate(v = 1)%>% 
    pivot_wider(., names_from = value, values_from = v)%>% 
    mutate(across(everything(), ~ifelse(is.na(.x), 0, .x))) %>% 
    group_by(!!sym(id.vars)) %>% summarise_all(max) %>%
    select(-name, -id.vars) %>%
    as.data.frame() %>% UpSetR::upset(., nsets = 20, nintersects = 30)
}
