# devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/Jmisc.R")

## To update R

# installr::updateR(admin_password = 'Use your computer password!')

select <- dplyr::select
ac <- as.character

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

## Data table helper function 
j.datatable <- function(X, caption = NULL, nrow = 10, width = 1000)   {
  datatable(X,caption = caption, options = list(pageLength = nrow,
      autoWidth = TRUE, #scrollX = TRUE,
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

remove.blank.columns2 <- function(dat) {
  names <- dat %>% dplyr::summarise(across(everything(), .fns = ~sum(.x==""|is.na(.x))==nrow(dat)))%>% 
    mutate(i = 1) %>% pivot_longer(.,-i) %>% filter(value %in% c(FALSE, NA)) %$% name
  
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

# ### This is a good start at listing the number of functions/ packages used in an r script
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


# jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, force.continuous = FALSE){
#   {if(!is.null(by)) dat[,by] <- clear.labels(dat[,by])}
#   {if(order.cat) dat %<>% mutate(across(is.factor|is.character, ~fct_infreq(factor(.x))))}
#   
#   
#   TYPE <-  list( all_dichotomous() ~ "categorical")
#   if(force.continuous) {TYPE[[2]] <- where(is.numeric) ~ "continuous2"}
#   
#   dat  %>%
#     tbl_summary(type = TYPE,
#                 digits = list(all_continuous() ~ c(Digits, Digits)),
#                 by = !!by,
#                 missing = "ifany",
#                 missing_text = "Missing")%>%
#     bold_labels() %>%
#     add_n() %>% 
#     {if(add.p) add_p(.) else .}  %>% 
#     {if(overall) add_overall(., last = TRUE) else .} 
# }
#   
jgt <- function(dat, by = NULL, add.p = FALSE, overall = FALSE, order.cat = FALSE, Digits = 1, 
                force.continuous = FALSE, missing = "ifany", missing_text = "Missing",
                spanner.size = NULL, spanner.text = NULL, add.n = TRUE, percent = "column"){
  {if(!is.null(by)) dat[,by] <- clear.labels(dat[,by])}
  sort <- NULL
  {if(order.cat) sort = all_categorical() ~ "frequency"}
  
  
  TYPE <-  list( all_dichotomous() ~ "categorical")
  if(force.continuous) {TYPE[[2]] <- where(is.numeric) ~ "continuous2"}
  
 tab <- dat  %>%
    tbl_summary(type = TYPE,
                sort = sort,
                digits = list(all_continuous() ~ c(Digits, Digits)),
                by = !!by,
                missing = missing,
                missing_text = missing_text,
                percent = percent)%>%
    bold_labels() %>%
   {if(add.n) add_n(.) else .}  %>% 
    {if(add.p) add_p(., pvalue_fun = function(x) style_pvalue(x, digits = 3)) else .}  %>% 
    {if(overall) add_overall(., last = TRUE) else .}
  
    if(!is.null(spanner.size)) tab %<>% modify_spanning_header(c(paste0("stat_", 1:spanner.size)) ~ paste0("**",spanner.text ,"**"))
 
  return(tab)
}

jgtt <- function(dat, col.names = TRUE){
  
  dat %>% 
    {if(col.names) sjlabelled::label_to_colnames(.)else .}  %>% 
    gt() %>%
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

j.reactable <- function(dat,col.names = TRUE, searchable = TRUE, resizable = TRUE, 
                        striped = TRUE, compact = TRUE, highlight = TRUE,sortable = TRUE, csv.file = NULL, ...){
  
  output <- dat %>% 
    {if(col.names) sjlabelled::label_to_colnames(.)else .}  %>% 
    reactable(data = ., searchable = searchable, resizable = resizable, sortable= sortable,
              striped = striped, compact = compact, highlight = highlight, theme = espn(), elementId = csv.file, ...)
  
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

checkbox.table3 <- function(dat, selection, id.vars = "study_id", denom = NULL, col.name = "Value"){
  int <- dat %>% select(id.vars, contains(selection)) %>% 
    pivot_longer(.,c(-all_of(id.vars)), values_drop_na = TRUE, values_transform = list(value = as.character ))%>%
    group_by(value) %>% tally() 
  
  if(is.null(denom)) {denom <- sum(int$n)}
  
  int %<>%
    mutate( pct =100*n/denom) %>% 
    janitor::adorn_totals(., where = "row", name = "Total") %>%
    mutate( pct = paste(round(pct, 1), "%", sep = "")) %>% 
    select(col.name = value, n, pct) %>%
    arrange(desc(n)) 
  
  names(int)[1] <- col.name
  
  int  %>%jgtt() %>%
    tab_footnote(., paste0("Denominator = ", denom), locations = cells_column_labels(
      columns = pct
    ))
}



checkbox.by <- function(dat, selection, id.vars = "study_id", by.var = "Total", col.name = "Value", denom){
  
  
  dt <- dat %>% select(id.vars, contains(selection))
  
  dt[,by.var] <- "Total"
  
  int <- dt %>% 
    {if(by.var != "Total") bind_rows(., dat %>% select(id.vars,by.var, contains(selection))) else (.)} %>%
    pivot_longer(.,c(-all_of(c(id.vars, by.var))), 
                 values_drop_na = TRUE,
                 values_transform = list(value = as.character ))  %>% 
    group_by(!!sym(id.vars), !!sym(by.var)) %>%mutate(n = 1/n()) %>%
    group_by(!!sym(by.var)) %>%mutate(denom = sum(n)) %>%
    group_by(!!sym(by.var), value, denom) %>% tally() %>%
    mutate( pct =100*n/denom) %>%
    mutate( pct = paste0(n, " (", round(pct, 1), "%)", sep = "")) %>%
    mutate(byv = paste0(!!sym(by.var), "\n (N = ",denom, ")")) %>% ungroup() %>%
    mutate(byv = fct_reorder(byv, n, sum))%>%
    mutate(value = fct_reorder(value, n, sum))%>%
    select(byv, col.name = value, pct) %>%
    arrange(byv) %>%
    pivot_wider(., names_from = "byv", values_from = "pct", values_fill = "0 (0%)") %>% 
    arrange(desc(col.name))

  names(int)[1] <- col.name

  jgtt(int)
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
  
  
  
#Code by Will Gray - github.com/graywh ######################
redcapExportMeta <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/') {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  meta_data <- read.csv(text=postForm(uri=URI, token=APIKEY, content='metadata',
                                      format='csv',
                                      # RCurl options
                                      .opts=curlOptions(ssl.verifyhost=2)),
                        stringsAsFactors=FALSE, na.strings='')
  subset(meta_data, field_type != 'field_type')
}

redcapExportArms <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/') {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  read.csv(text=postForm(uri=URI, token=APIKEY, content='arm',
                         format='csv',
                         # RCurl options
                         .opts=curlOptions(ssl.verifyhost=2)),
           stringsAsFactors=FALSE, na.strings='')
}

redcapExportEvents <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/') {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  read.csv(text=postForm(uri=URI, token=APIKEY, content='event',
                         format='csv',
                         # RCurl options
                         .opts=curlOptions(ssl.verifyhost=2)),
           stringsAsFactors=FALSE, na.strings='')
}

redcapExportMappings <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/') {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  read.csv(text=postForm(uri=URI, token=APIKEY, content='formEventMapping',
                         format='csv',
                         # RCurl options
                         .opts=curlOptions(ssl.verifyhost=2)),
           stringsAsFactors=FALSE, na.strings='')
}

redcapExportRecords <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/', format='csv', type='flat', rawOrLabel='raw', exportCheckboxLabel='false', forms=NULL, fields=NULL, events=NULL) {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  read.csv(text=postForm(uri=URI, token=APIKEY, content='record',
                         format=format, type=type,
                         # Redcap API options
                         rawOrLabel=rawOrLabel,
                         exportCheckboxLabel=exportCheckboxLabel,
                         forms=forms, fields=fields, events=events,
                         # RCurl options
                         .opts=curlOptions(ssl.verifyhost=2)),
           stringsAsFactors=FALSE, na.strings='')
}

redcapExport <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/', labels=TRUE, checkboxLabels=FALSE, forms=NULL, fields=NULL, events=NULL) {
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  
  rmq <- function(x) gsub("['\"]", '', x)
  clean <- function(x) { gsub('[\n]', ' ', x) }
  
  # if Hmisc is available, apply labels
  Hmisc <- require('Hmisc')
  
  # Fetch metadata
  meta_data <- subset(redcapExportMeta(APIKEY, URI), field_type %in% c('text','notes','dropdown','radio','checkbox','calc','slider','yesno','truefalse'))
  
  form_field_names <- sprintf('%s_complete', unique(meta_data$form_name))
  if (!is.null(forms)) {
    forms <- intersect(forms, unique(meta_data$form_name))
    form_field_names <- sprintf('%s_complete', forms)
  }
  if (!is.null(fields)) {
    form_fields <- subset(meta_data, form_name %in% forms)$field_name                 # Select all the field (variable) names that are in the forms the user specified.
    if (!all(fields %in% form_fields)) {
      specific_fields <- intersect(fields, c(unique(meta_data$field_name), form_field_names))
      fields <- union(form_fields, specific_fields)
      if (!is.null(forms)) {
        fields <- c(fields, form_field_names)
        forms <- NULL
      }
      form_field_names <- intersect(form_field_names, fields)
    }
  }
  if (length(forms) > 0) {
    meta_data <- subset(meta_data, meta_data$form_name %in% forms)
  } else if (length(fields) > 0) {
    meta_data <- subset(meta_data, meta_data$field_name %in% fields)
  }
  
  # Fetch records
  data <- redcapExportRecords(APIKEY, URI=URI,
                              # Redcap API options
                              rawOrLabel=c('raw','label')[1 + labels], # real values or codes
                              exportCheckboxLabel=c('false','true')[1 + checkboxLabels], # real values or checked/unchecked
                              forms=paste(forms, collapse=','),
                              fields=paste(fields, collapse=','),
                              events=paste(events, collapse=','))
  
  for (i in seq_len(nrow(meta_data))) {
    fld <- as.list(meta_data[i,])
    choices <- redcapExtractChoices(fld$select_choices_or_calculations)
    nums <- choices$numbers
    choices <- choices$labels
    
    if (fld$field_type == 'checkbox') {
      
      for (j in seq(length(nums))) {
        checkbox_name <- sprintf('%s___%s', fld$field_name, nums[j])
        
        # advance to next field if not in dataset
        if (is.null(data[[checkbox_name]])) next
        
        if (labels) {
          if (checkboxLabels) {
            levels <- choices[j]
          } else {
            levels <- c('Unchecked', 'Checked')
          }
          data[[checkbox_name]] <- factor(data[[checkbox_name]], levels=levels)
        } else {
          data[[checkbox_name]] <- factor(data[[checkbox_name]], levels=c('0','1'))
        }
        if (Hmisc) {
          label(data[[checkbox_name]]) <- sprintf('%s (choice=%s)', clean(fld$field_label), rmq(choices[j]))
        }
      }
      
    } else {
      # advance to next field if not in dataset
      if (is.null(data[[fld$field_name]])) next
      
      if (fld$field_type %in% c('radio','dropdown','yesno','truefalse')) {
        if (labels) {
          if (fld$field_type == 'yesno') {
            levels <- c('No','Yes')
          } else if (fld$field_type == 'truefalse') {
            levels <- c('False','True')
          } else {
            levels <- choices
          }
          data[[fld$field_name]] <- factor(data[[fld$field_name]], levels=levels)
        } else {
          if (fld$field_type == 'yesno') {
            levels <- c('0','1')
          } else if (fld$field_type == 'truefalse') {
            levels <- c('0','1')
          } else {
            levels <- choices
          }
          data[[fld$field_name]] <- factor(data[[fld$field_name]], levels=levels)
        }
        
      } else if ((!is.na(fld$text_validation_type_or_show_slider_number) &&
                  fld$text_validation_type_or_show_slider_number %in% c('float','int') ) ||
                 fld$field_type %in% c('calc') ) {
        suppressWarnings(data[[fld$field_name]] <- as.numeric(data[[fld$field_name]]))
        
      }
      
      if (Hmisc) {
        label(data[[fld$field_name]]) <- fld$field_label
      }
    }
  }
  
  for (form_field_name in form_field_names) {
    data[[form_field_name]] <- factor(data[[form_field_name]], levels=c('Incomplete','Unverified','Complete'))
    if (Hmisc) {
      label(data[[form_field_name]]) <- 'Complete?'
    }
  }
  
  data
}

redcapExtractChoices <- function(choices) {
  # extract checkbox choices to identify sub-variables in data
  choices <- strsplit(choices, ' *[|] *')[[1]]
  choices <- sub('^[ ]+(.*)$', '\\1', choices)
  choices <- sub('(.*)[ ]+$', '\\1', choices)
  nums <- sub('^([^,]+).*$', '\\1', choices)
  choices <- sub('^[^,]+[, ]+(.*)$', '\\1', choices)
  list(numbers=nums, labels=choices)
}

# All preceding redcap code by Will Gray #########
  
  

j.redcap_extract <- function(rmd_file, data_path = "../data/", archive_path = "../data/archive/"){
  require(rstudioapi)
  require(RCurl)
  
  data <- redcapExport(APIKEY= key_get(rmd_file),
                       URI='https://redcap.vanderbilt.edu/api/', 
                       labels=TRUE, checkboxLabels=TRUE, forms=NULL, 
                       fields=TRUE, events=NULL)
  
  data <- sjlabelled::copy_labels(droplevels(data), data)
  data <- clear.label.class(data)
  
  save(data,
       file = paste0(data_path,rmd_file, ".rda"),
       compress ='xz', compression_level=9)
  
  save(data,
       file = paste0(archive_path,Sys.Date(), "_",rmd_file, ".rda"),
       compress ='xz', compression_level=9)
  
}


update.redcap.changelog <- function(id.vars){
  
  ## Could update function so that it only runs comparedf when new data is available
  
  require(arsenal)
  
  Ff <- data.frame(root = "../data/archive/", file = list.files("../data/archive")) %>%
    mutate(path = paste0(root, file), download.time = file.mtime(path)) %>%
    arrange(download.time) %>% mutate(n = 1:n(), n.diffs = NA)
  
  Dl <- data.frame()
  Dff <- data.frame()
  sct <- data.frame()
  vns <- data.frame()
  
  for(i in 2:nrow(Ff)){
    
    load(Ff$path[i])
    p1 <- data
    load(Ff$path[i-1])
    p2 <- data
    
    Cc <- arsenal::comparedf(p1, p2, by = id.vars)
    sCc <- summary(Cc)
    Ff$n.diffs[i] <- n.diffs(Cc)
    
    if(n.diffs(Cc) >0){
      Dl <- bind_rows(Dl,
                      diffs(Cc, by.var = TRUE)%>% filter(n>0|NAs>0)  %>%
                        mutate(x = ac(Ff$download.time[i]),
                               y = ac(Ff$download.time[i-1]),
                               comparison = paste0(i, " vs ", (i-1))))
      
      Dff <- bind_rows(Dff,
                       diffs(Cc) %>%
                         mutate(x = Ff$download.time[i],
                                y = Ff$download.time[i-1],
                                comparison = paste0(i, " vs ", (i-1))))
      
      sct <- bind_rows(sct,
                       sCc$comparison.summary.table %>%
                         mutate(comparison = paste0(i, " vs ", (i-1))))
      
      vns <- bind_rows(vns,
                       sCc$vars.ns.table %>%
                         mutate(comparison = paste0(i, " vs ", (i-1))))
    }
  }
  Ff %<>% select(file, download.time, n, n.diffs)
  sct %<>% 
    pivot_wider(., values_from = "value", names_from = "comparison")
  vns %<>% group_by(comparison, version) %>% summarise(n = n(), variable = toString(variable))
  Dl %<>% select(comparison, var = var.x, n, NAs)
  Dff %<>% select(comparison, var = var.x, id.vars, values.x, values.y)
  
  redcap_changelog <- list(Ff,sct, vns, Dl, Dff)
  
  save(redcap_changelog,
       file="../data/redcap_changelog.rda",
       compress='xz', compression_level=9)
}



update_redcap_notes <- function() {
  
  notes_data <- redcapExport(APIKEY= key_get("notes"),
                       URI='https://redcap.vanderbilt.edu/api/', 
                       labels=TRUE, checkboxLabels=TRUE, forms=NULL, 
                       fields=TRUE, events=NULL)
  
  
  save(notes_data,
       file = "/Users/joshdeclercq/Library/CloudStorage/OneDrive-VUMC/Work logs/notes/notes.rda",
       compress ='xz', compression_level=9)
  
  return(notes_data)
}

print.work.logs <- function(proj){
  work.logs <- readxl::read_xlsx("/Users/joshdeclercq/Library/CloudStorage/OneDrive-VUMC/Work logs/work log.xlsx", sheet = "Todo")
  
  res <- work.logs %>% filter(project == proj) %>%
    select(date, request, "date done") %>%
    jgtt() %>% tab_header("Summary of project requests") 
  
  return(res)
  
}

print.redcap.logs <- function(proj){
  
  load(file = "/Users/joshdeclercq/Library/CloudStorage/OneDrive-VUMC/Work logs/notes/notes.rda")
  res <- notes_data %>% filter(project == proj) %>% select(name, date, comment) %>% 
    jgtt() %>% tab_header("Redcap study notes")
  
  return(res)
}

redcap_dictionary <- function(ddict){
  
  
  out <- ddict %>% mutate(Variable = paste(variable___field_name, branching_logic__show_field_only_if____, sep = "<br><br>"),
                          choice = gsub("\\|", "<br>", choices__calculations__or_slider_labels),
                          # minmax = ifelse(text_validation_min != "", paste("Min: ", text_validation_min, ", Max:",  text_validation_max), ""),
                          minmax = "",
                          attr = ifelse(minmax == "", field_type, paste0(field_type, "<br><br>(",text_validation_type_or_show_slider_number, ", ", minmax, ")" ))) %>% 
    # mutate(attr = paste(field_type, text_validation_type_or_show_slider_number) )%>%
    select(form_name, Variable, field_label, attr, choice) %>%
    j.reactable(., columns = list(Variable = colDef(html = TRUE, width = 150),
                                  field_label = colDef(html = TRUE, width = 150),
                                  attr = colDef(html = TRUE, width = 150),
                                  choice = colDef(html = TRUE, width = 200),
                                  form_name = colDef(width = 100)),
                groupBy = "form_name", defaultExpanded = FALSE)
  
  return(out)
  
}


changelog <- function(DAT){
  
  NEWDAT <- DAT
  timestamp <- format(Sys.time(), "%Y.%m.%d %H.%M")
  dat_name <- deparse(substitute(DAT))
  changelog_path <- paste0("../data/archive/changelog_", dat_name)
  changelog_file <- paste0(changelog_path, "/changelog_", dat_name, ".rda")
  archive_path <- paste0(changelog_path, "/",dat_name," " ,timestamp ,".rda")
  
  
  ## First time running function will create directory and save a copy of the data there
  if(!dir.exists(changelog_path)){
    
    dir.create(changelog_path)
    ARCHIVE <- DAT
    save(ARCHIVE,
         file= archive_path,
         compress='xz', compression_level=9)
    
    Ffn <- data.frame(root = changelog_path, file = list.files(changelog_path)) %>%
      mutate(path = paste(root, file, sep = "/"), download.time = file.mtime(path)) %>%
      filter(!grepl("changelog", file)) %>%
      filter(download.time == max(download.time))
    
    save(Ffn,
         file=changelog_file,
         compress='xz', compression_level=9)
    
    return(print(paste("Archive directory created at", changelog_path)))
  }
  
  Ff.new <- data.frame(root = changelog_path, file = list.files(changelog_path)) %>%
    mutate(path = paste(root, file, sep = "/"), download.time = file.mtime(path)) %>%
    filter(!grepl("changelog", file)) %>%
    filter(download.time == max(download.time))
  
  # return(NEWDAT)
  
  ## Check to see if data are the same between versions
  load(Ff.new$path)
  
  # return(isTRUE(all.equal(ARCHIVE, NEWDAT)))
  if(isTRUE(all.equal(ARCHIVE, NEWDAT))){
    return("No changes made to data")
  } else {
    
    
    
    load(changelog_file)
    Ff <- bind_rows(Ffn, Ff.new) %>% mutate(n = 1:n())
    
    i <- nrow(Ff)
    
    Dl <- data.frame()
    Dff <- data.frame()
    sct <- data.frame()
    vns <- data.frame()
    
    Cc <- arsenal::comparedf(NEWDAT, ARCHIVE, by = "x")
    sCc <- summary(Cc)
    Ff$n.diffs[i] <- n.diffs(Cc)
    
    if(n.diffs(Cc) >0){
      ## Differences by variable
      Dl <- bind_rows(Dl,
                      diffs(Cc, by.var = TRUE)%>% filter(n>0|NAs>0)  %>%
                        mutate(x = ac(Ff$download.time[i]),
                               y = ac(Ff$download.time[i-1]),
                               comparison = paste0(i, " vs ", (i-1))))
      ## Individual differences
      Dff <- bind_rows(Dff,
                       diffs(Cc) %>%
                         mutate(x = Ff$download.time[i],
                                y = Ff$download.time[i-1],
                                comparison = paste0(i, " vs ", (i-1))))
      ## Summaru comparison table
      sct <- bind_rows(sct,
                       sCc$comparison.summary.table %>%
                         mutate(comparison = paste0(i, " vs ", (i-1))))
      
      ## variables not shared
      vns <- bind_rows(vns,
                       sCc$vars.ns.table %>%
                         mutate(comparison = paste0(i, " vs ", (i-1))))
      
      Ff %<>% select(file, download.time, n, n.diffs)
      # sct %<>%
      #   pivot_wider(., values_from = "value", names_from = "comparison")
      # vns %<>% group_by(comparison, version) %>% summarise(n = n(), variable = toString(variable), .groups = "drop")
      Dl %<>% select(comparison, var = var.x, n, NAs)
      Dff %<>% select(comparison, var = var.x, x, values.x, values.y)
    }
    
    
    
    changelog_output_file <- paste0(changelog_path, "/changelog_output_", dat_name, ".rda")
    if(!file.exists(changelog_output_file)){
      Ffi <- data.frame()
      Dli <- data.frame()
      Dffi <- data.frame()
      scti <- data.frame()
      vnsi <- data.frame()
    }else{
      load(changelog_output_file)
      Ffi = changelog_output$Ffi
      scti = changelog_output$scti 
      vnsi = changelog_output$vnsi 
      Dli = changelog_output$Dli 
      Dffi = changelog_output$Dffi
      
    }
    
    Ffi <- Ff
    scti <- bind_rows(scti, sct)
    vnsi <- bind_rows(vnsi, vns)
    Dli <- bind_rows(Dli, Dl)
    Dffi <- bind_rows(Dffi, Dff)
    
    changelog_output <- list(Ffi = Ffi,scti = scti, vnsi = vnsi, Dli = Dli, Dffi = Dffi)
    
    ARCHIVE <- DAT
    save(ARCHIVE,
         file=archive_path,
         compress='xz', compression_level=9)
    
    Ffn <- Ff
    save(Ffn,
         file=changelog_file,
         compress='xz', compression_level=9)
    
    
    save(changelog_output,
         file=changelog_output_file,
         compress='xz', compression_level=9)
    
    return(print("New copy of data saved"))
  }
  
  
}

publish_to_vsp <- function(qmd, output, directory){
  ## Publish report to OneDrive for VSP (copied from vpn directory)
  
  # Pull latest data from redcap
  notes <- update_redcap_notes()
  
  of <- paste0(Sys.Date()," ", output, ".aspx" )
  
  # rmarkdown::render
  quarto::quarto_render(input = qmd, 
                        output_file = of)
  
  # # Quarto does not yet support rendering to different directory -- bleh
  file.copy(from = of, to = directory, overwrite = TRUE)
  
}



render_toc <- function(qmd, output){
  ## Render blank html document with Table of contents only
  ## Requires eval_code parameter sent to knitr::opts_chunk$set
  
  rmarkdown::render(qmd, 
                    output_file = paste(output, "_toc.html"),
                    rmarkdown::html_document(toc = TRUE, 
                                             number_sections = TRUE, 
                                             pandoc_args = c("-V", 'body=""')),
                    params = list(eval_code = FALSE))
}



