#' ---
#' title: "MS Fumarates"
#' author:
#' - Josh DeClercq
#' - Department of Biostatistics
#' - Vanderbilt University Medical Center
#' date: today
#' format:
#'   html:
#'     grid: 
#'       body-width: 1250px
#'       sidebar-width: 250px
#'       margin-width: 200px
#'     toc: true
#'     mainfont: albertus
#'     theme: flatly
#' toc-location: left
#' toc-depth: 3
#' execute:
#'   echo: true
#'   eval: true
#'   warning: false
#' number-sections: true
#' monobackgroundcolor: "#DAE6DF"
#' code-fold: true
#' code-block-bg: "#DAE6DF"
#' code-block-border-left: "#BADBCB"
#' callout-appearance: minimal
#' self-contained: true
#' fig-align: center
#' ---
#' 
#| include: false

require(Hmisc)
require(rms)
require(ggplot2)
require(lubridate)
require(reshape2)
require(magrittr)
require(dplyr)
require(knitr)
require(kableExtra)
require(tidyr)
require(ggfortify)
require(stringr)
require(forcats)
require(labelVector)
require(plotly)
require(gtsummary)
require(gt)
require(reactable)
require(reactablefmtr)
require(htmltools)
require(keyring)
require(redcapAPI)
require(readxl)




devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/Jmisc.R")
devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/checkbox/R/checkbox_funs.R")
devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/VUMisC/main/JDmisc.R")

select <- dplyr::select


#' 

# Retreive data
getHdata(titanic3)

t3 <- titanic3 %>% clear.labels() %>% select(-c( boat:home.dest, ticket))

# Data cleaning/ Create new variables
t3 %<>% mutate(cabin = as.character(cabin), cabin = ifelse(cabin == "", NA, cabin))
t3 %<>% mutate(s.cat = yesno(survived == 1))
t3 %<>% mutate(missing.age = yesno(is.na(age)))

#' 
#' # Labels
#' 
#' ## Adding labels 
#' 
#| code-fold: show
j.label5(t3)

#' 

t3 <- set_label( t3 ,
                 name = "Passenger name",
 		             pclass = 'Passenger class',
                 survived = 'Survived',
                 sex = 'Sex',
                 age = 'Age',
                 sibsp = 'Number of Siblings/Spouses Aboard',
                 parch = 'Number of Parents/Children Aboard',
                 fare = 'Passenger Fare',
                 embarked = 'Port of Embarkation', 
                 cabin = "Cabin")

#' 
#| code-fold: show
j.label5(t3, all = TRUE)
j.label5(t3, all = FALSE)

#' 

t3 <- set_label( t3 ,
 		s.cat = "",
 		missing.age = "")

#' 
#' ## Collect labels
#' 

collect.labels(t3)

#' 
#' ## Removing labels
#' 
#| eval: false
## t3 %>% select(name, age, sibsp) %>% pivot_longer(., -1, names_to = "var")
## 

#' 
#' ```         
#' Error in `pivot_longer()`:
#' ! Can't combine `age` <labelled> and `sibsp` <labelled>.
#' ✖ Some attributes are incompatible.
#' ℹ The author of the class should implement vctrs methods.
#' ℹ See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
#' Backtrace:
#'  1. t3 %>% select(name, age, sibsp) %>% ...
#'  3. tidyr:::pivot_longer.data.frame(., -1, names_to = "var")
#' ```
#' 

t3.labelled <- t3
t3 <- clear.labels(t3)


t3 %>% select(name, age, sibsp) %>% pivot_longer(., -1, names_to = "var")

#' 
#' ## Adding labels back
#' 

t3 <- sjlabelled::copy_labels(t3, t3.labelled)

#' 
#' ## Clearing empty columns
#' 

t3 %<>% mutate(x = "", y = NA, z= as.Date(NA))

t3 %<>% remove.blank.columns2()

#' 
#' # Factors
#' 
#' ## label_factor
#' 

label_factor(t3, "sex")


#' 

t3 %>% select(where(is.factor)) %>% names() %>% 
label_factor(t3, .)

#' 
#' ## recode_factor
#' 

recode_factor(t3, "embarked")


#' 
#' ## j.trib
#' 

j.trib(t3, "embarked")

#' 

trib.t3<- tribble(~embarked, ~replace,
              'Cherbourg', '',
              'Queenstown', '',
              'Southampton', '',
              'NA', '')    

#' 
#' # Tables
#' 
#' ## jgt
#' 

t3 %>% select(age, sex, pclass, fare, embarked, sibsp, parch, s.cat) %>% 
  jgt(., by = "s.cat", spanner.size = 2, spanner.text = "Survived", overall = TRUE)

#' 
#' ## jgtt
#' 

t3  %>% select(name, age, sex, pclass) %>% head(20) %>% jgtt()

#' 
#' ## j.reactable
#' 

t3  %>% select(name, age, sex, pclass) %>% 
  j.reactable(., csv.file = "titanic")

#' 
#' # Dates
#' 

t3 %<>% 
  mutate(date1 = ymd("1888-01-01") + days(floor(rcauchy((nrow(t3)), 0, 300))),
         date2 = ymd("1898-01-01") + days(floor(rcauchy((nrow(t3)), 0, 200))),
         date3 = ymd("1911-01-01") + days(floor(rnorm((nrow(t3)), 0, 202))),
         date4 = ymd("1853-01-01") + days(floor(rnorm((nrow(t3)), 0, 11))))

j.label5(t3)

t3 <- set_label( t3 ,
 		date1 = "Date of birth",
 		date2 = "First time on boat",
 		date3 = "Ticket purchase date",
 		date4 = "Other date")

#' 
#' ## date_dists
#' 
#' ### Figure
#' 

t3_dates <- t3 %>% rename(record_id = name) %>% 
date_dists(., id.var = "record_id")

ggplot(t3_dates, aes(x = value)) + 
  geom_histogram(stat = "count", fill = "black", colour = "black", bins = 100) +  
  theme_classic() +labs(x = "Date", y = "Frequency") + 
  facet_wrap(variable~., scales = "free", ncol = 1)+
  theme(strip.background =element_rect(fill="honeydew2"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

#' 
#' ### Table
#' 

t3_dates %>%
  select(variable, sequence, date, record_id) %>%
  na.omit() %>%
   j.reactable(., groupBy = c("variable")) 

#' 
#' # Analysis
#' 
#' ## rms.trib
#' 

dd <- datadist(t3)
options(datadist = "dd", prType = "html", grType='plotly')
t3.mod <- lrm(s.cat ~ rcs(age, 5) + sex  + pclass + embarked + sibsp, data = t3)
t3.mod.int <- lrm(s.cat ~ rcs(age, 5) * sex  + pclass + embarked + sibsp, data = t3)
rms.trib(t3.mod)

trib.cols.rms <- tibble::tribble(
       ~term,               ~new, ~level,                              ~label,
       "age",              "Age",    "1",                               "Age",
       "sex",              "Sex",    "2",                               "Sex",
    "pclass",  "Passenger class",    "5",                   "Passenger class",
  "embarked",             "Port",    "3",               "Port of Embarkation",
     "sibsp", "Siblings/Spouses",    "4", "Number of Siblings/Spouses Aboard"
  )


#' 
#' ## rms.sum.table2
#' 


rms.sum.table3(summary = summary(t3.mod), trib.cols.rms, raw = TRUE) 

rms.sum.table3(summary = summary(t3.mod), trib.cols.rms, anova(t3.mod)) 

rms.sum.table3(summary = summary(t3.mod.int), trib.cols.rms, anova = anova(t3.mod.int)) 

rms.sum.table3(summary = summary(t3.mod.int, age = c(25, 50, 75), sex = "female"), 
               trib.cols.rms, anova = anova(t3.mod.int)) 


#' 

rms.forest.plot(summary(t3.mod), trib.cols.rms) +
  scale_x_continuous(trans='log', breaks = c(0.25, 0.5, 1, 2, 4, 8, 20))+
  labs(x = "Odds ratio")
  

rms.forest.plot(summary(t3.mod.int, age = c(50, 30, 15)), trib.cols.rms) +
  scale_x_continuous(trans='log', breaks = c(0.25, 0.5, 1, 2, 4, 8, 20)) +
  labs(x = "Odds ratio")

plot(anova(t3.mod.int))

#' 
#' ## Interaction contrasts
#' 
#' ### expand.int and int.react
#' 

t3_int <- expand.int(seq(10, 70, 10), c("male", "female"), "age", "sex", t3.mod)

int.react(t3_int)

#' 
#' ### int.tile.plot
#' 

int.tile.plot(t3_int, "age", "sex") +labs(fill = "Estimate")

#' 
#' 
#' # Appendix
#' ## get_toc

get_toc("JDmisc.qmd") %>% j.reactable(., csv.file = "JDmisc toc")

#' 
#' 
#' 
