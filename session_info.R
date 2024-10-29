#' ---
#' title: "[JDMisc functions](https://github.com/jjdeclercq/VUMisC)"
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
#' monobackgroundcolor: "#F5F5F5"
#' code-fold: show
#' code-block-bg: "#F5F5F5"
#' code-block-border-left: "#BADBCB"
#' callout-appearance: minimal
#' self-contained: true
#' fig-align: center
#' ---
#' 
## -----------------------------------------------------------------------------
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
require(arsenal)
require(data.table)


devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/Jmisc.R")
devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/Jmisc/main/checkbox/R/checkbox_funs.R")
devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/VUMisC/main/JDmisc.R")

select <- dplyr::select


#' 
#' # Objective
#' 
#' ## Background
#' 
#' Throughout my time as a biostatistician in the department, I've developed a handful of convenience functions. I think they're useful in my day-to-day work, and I think they can be useful for others. However, I usually write code in a way that's easy for me to understand, but my style unlikely follows best-practice guidelines, and there is potentially a lot of issues that may arise. This document will show the ideal use cases for these functions.
#' 
#' ## Goals
#' 
#' The goal of this document is to showcase some of this work, and to use it as a springboard into the development of a collaborative R package, wherein all members of the VUMC Department of Biostatistics can contribute functions to a central repository. Sharing code amongst the department should conceivably increase our individual coding skills as well as productivity.
#' 
#' This work is hosted on my [Github](https://github.com/jjdeclercq/VUMisC), but I would eagerly transfer ownership rights to anyone who is willing. (My git knowledge is quite limited at the moment.)
#' 
#' ## Data
#' 
#' This report uses the `titanic3` data. This report isn't intended to be a description of the best way to analyze this data.
#' 
## -----------------------------------------------------------------------------
# Retreive data
getHdata(titanic3)

t3 <- titanic3 %>% 
  select(-c( boat:home.dest, ticket)) %>%  
  mutate(id = 1:n()) ## Create a unique ID variable for each row

archive(t3) ## this will be discussed later, but essentially this takes an impression of the current data for later use

# Data cleaning/ Create new variables
t3 %<>% mutate(cabin = as.character(cabin), cabin = ifelse(cabin == "", NA, cabin))
t3 %<>% mutate(s.cat = yesno(survived == 1))
t3 %<>% mutate(missing.age = yesno(is.na(age)))

#' 
#' # Labels
#' 
#' 
## -----------------------------------------------------------------------------
t3 %<>% clear.labels()

#' 
#' ## Adding labels
#' 
#' `j.label5()` is code that outputs text suitable for pasting into your markdown document.
#' 
## -----------------------------------------------------------------------------
#| code-fold: show
j.label5(t3)

#' 
#' Labels can then be filled in by hand.
#' 
## -----------------------------------------------------------------------------
t3 <- labelVector::set_label( t3 ,
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
#' Subsequent execution of the code will have the added labels incorporated. Toggling the `all` argument determines whether all variables will be displayed or just those without labels
#' 
## -----------------------------------------------------------------------------
#| code-fold: show
j.label5(t3, all = TRUE)
j.label5(t3, all = FALSE)

#' 
## -----------------------------------------------------------------------------
t3 <- labelVector::set_label( t3 ,
 		s.cat = "Survived",
 		missing.age = "Missing age")

#' 
#' ## Collect labels
#' 
#' Create a data frame of variable names and the corresponding label.
#' 
## -----------------------------------------------------------------------------
collect.labels(t3)

#' 
#' ## Removing labels
#' 
#' Sometimes labelled data can present challenges as shown by the example below:
#' 
## -----------------------------------------------------------------------------
#| eval: false
t3 %>% select(name, age, sibsp) %>% 
  pivot_longer(., -1, names_to = "var")


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
#' `clear.labels`
#' 
#' I did not write this function ([stackoverflow](https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions)) but I find it immensely helpful in removing all labels from a data frame.
#' 
## -----------------------------------------------------------------------------
t3.labelled <- t3 ## create a second data frame for regaining labels later
t3 <- clear.labels(t3)


t3 %>% select(name, age, sibsp) %>% pivot_longer(., -1, names_to = "var") %>% head()

#' 
#' ## Adding labels back
#' 
#' Not my function, but I find this to be very helpful.
#' 
## -----------------------------------------------------------------------------
t3 <- sjlabelled::copy_labels(t3, t3.labelled)

#' 
#' `clear.label.class()` is a nice workaround for labelled data. It just removes the class "label" from the variable, but still retains the utility of labelled data.
#' 
## -----------------------------------------------------------------------------
t3 <- clear.label.class(t3)


#' 
#' ## Clearing empty columns
#' 
#' Sometimes there are columns in the data that don't carry any information. `remove.blank.columns2()` will remove all columns from the data that are either all `NA` or all empty characters (`""`).
#' 
## -----------------------------------------------------------------------------
t3 %<>% mutate(x = "", y = NA, z= as.Date(NA))

t3 %>% select(x, y, z) %>% head(10)
t3 %<>% remove.blank.columns2()

#' 
## -----------------------------------------------------------------------------
names(t3)

#' 
#' # Factors
#' 
#' ## label_factor
#' 
#' Produces code that can be pasted into your report for adding in variable labels:
#' 
## -----------------------------------------------------------------------------
label_factor(t3, "sex")


#' 
#' Can accommodate any number of factor variables:
#' 
## -----------------------------------------------------------------------------
t3 %>% select(where(is.factor)) %>% 
  names() %>% 
  label_factor(t3, .)

#' 
#' ## recode_factor
#' 
#' Produce editable code using `forcats::fct_recode()`
#' 
## -----------------------------------------------------------------------------
recode_factor(t3, "embarked")


#' 
#' ## j.trib
#' 
#' Create an editable `tribble` table that can be joined back into original data frame.
#' 
## -----------------------------------------------------------------------------
j.trib(t3, "embarked")

#' 
## -----------------------------------------------------------------------------
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
#' The package `gtsummary` provides a lot of great functions for summarizing data. The `jgt()` is a wrapper for `tbl_summary` that handles 99% of summary tables that I need to make.
#' 
## -----------------------------------------------------------------------------
t3 %>% select(age, sex, pclass, fare, embarked, sibsp, parch, s.cat) %>% 
  jgt(., by = "s.cat", spanner.size = 2, spanner.text = "Survived", overall = TRUE)

#' 
#' ## jgtt
#' 
#' Simply prints the data using the `gt` package, and stylized to match `jgt` output.
#' 
## -----------------------------------------------------------------------------
t3  %>% select(name, age, sex, pclass) %>% head(20) %>% jgtt()

#' 
#' ## j.reactable
#' 
#' Better at presenting lots of data: tabbed output, searchable text, filtering and embedding data as a csv file are available. The `reactable` package provides a ton a great functionality. `j.reactable` is a convenient wrapper that suits the majority of table needs.
#' 
## -----------------------------------------------------------------------------
t3  %>% select(name, age, sex, pclass) %>% 
  j.reactable(., csv.file = "titanic")

#' 
#' # Dates
#' 
#' Create dummy data data using a Cauchy distribution to generate some obvious outliers.
#' 
## -----------------------------------------------------------------------------
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
## -----------------------------------------------------------------------------
t3_dates <- t3 %>% 
  date_dists(., id.var = "id", label.src = t3)

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
## -----------------------------------------------------------------------------
t3_dates %>%
  select(variable, sequence, date, id) %>%
  na.omit() %>%
   j.reactable(., groupBy = c("variable")) 

#' 
#' # Analysis
#' 
#' Fit a logistic model with survival being the outcome. For illustrative purposes, fit one model with an age x sex interaction and one without.
#' 
## -----------------------------------------------------------------------------
dd <- datadist(t3)
options(datadist = "dd", prType = "html", grType='plotly')
t3.mod <- lrm(s.cat ~ rcs(age, 5) + sex  + pclass + embarked + sibsp, data = t3)
t3.mod.int <- lrm(s.cat ~ rcs(age, 5) * sex  + pclass + embarked + sibsp, data = t3)

#' 
#' ## rms.trib
#' 
#' Takes a model fit of class `rms` and returns an editable tribble for pasting into your code. The resulting saved tribble is used in later applications for summarizing models.
#' 
#' There are 4 columns for entry:
#' 
#' -   **term** - the raw variable name
#' 
#' -   **new** - the variable label, as it will appear in figures
#' 
#' -   **level -** the order in which the variable will appear in tables and figures
#' 
#' -   **label** - the variable label, as it will appear in tables
#' 
## -----------------------------------------------------------------------------
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
#' Note: the R package [datapasta](https://cran.r-project.org/web/packages/datapasta/vignettes/how-to-datapasta.html) can be really helpful in formatting tribbles, as well as other data formats.
#' 
#' ## rms.sum.table3
#' 
#' This function displays a tidy output of the model summary
#' 
#' -   **summary** - an rms.summary object
#' 
#' -   **trib** - the saved tribble from the `rms.trib` output
#' 
#' -   **anova** - Optional. an rms.anova object. Including this will add on the degrees of freedom and p-values from the anova table.
#' 
#' -   **raw** - Optional. Will output a raw data frame rather than a formatted table
#' 
#' ::: panel-tabset
#' ### Output
#' 
## -----------------------------------------------------------------------------
rms.sum.table3(summary = summary(t3.mod), trib.cols.rms, anova(t3.mod))

#' 
#' ### Raw output
#' 
## -----------------------------------------------------------------------------
rms.sum.table3(summary = summary(t3.mod), trib.cols.rms, raw = TRUE)

#' 
#' ### With interaction
#' 
## -----------------------------------------------------------------------------
rms.sum.table3(summary = summary(t3.mod.int), trib.cols.rms, anova = anova(t3.mod.int))

#' 
#' ### Adjustments via rms.summary()
#' 
## -----------------------------------------------------------------------------
rms.sum.table3(summary = summary(t3.mod.int, age = c(25, 50, 75), sex = "female"), 
               trib.cols.rms, anova = anova(t3.mod.int))

#' :::
#' 
#' ## rms.forest.plot
#' 
#' Similar to `rms.sum.table3`, this function takes an rms.summary input and the saved tribble from the `rms.trib` output, but instead produces a forest plot
#' 
#' ::: panel-tabset
#' ### Forest plot
#' 
## -----------------------------------------------------------------------------
rms.forest.plot(summary(t3.mod), trib.cols.rms) +
  scale_x_continuous(trans='log', breaks = c(0.25, 0.5, 1, 2, 4, 8, 20))+
  labs(x = "Odds ratio")

#' 
#' ### With interaction
#' 
## -----------------------------------------------------------------------------
rms.forest.plot(summary(t3.mod.int, age = c(50, 30, 15)), trib.cols.rms) +
  scale_x_continuous(trans='log', breaks = c(0.25, 0.5, 1, 2, 4, 8, 20)) +
  labs(x = "Odds ratio")


#' 
#' ### Possible extensions
#' 
## -----------------------------------------------------------------------------
bind_rows(
  rms.sum.table3(summary = summary(t3.mod, age = c(70, 55), sex = "female"), trib.cols.rms, raw = TRUE) ,
  rms.sum.table3(summary = summary(t3.mod, age = c(55, 40), sex = "female"), trib.cols.rms, raw = TRUE) ) %>% 
  bind_rows(.,
            rms.sum.table3(summary = summary(t3.mod, age = c(40, 25), sex = "female"), trib.cols.rms, raw = TRUE)) %>% 
    bind_rows(.,
            rms.sum.table3(summary = summary(t3.mod, age = c(25, 15), sex = "female"), trib.cols.rms, raw = TRUE)) %>% 
      bind_rows(.,
            rms.sum.table3(summary = summary(t3.mod, age = c(15, 10), sex = "female"), trib.cols.rms, raw = TRUE)) %>% 
        bind_rows(.,
            rms.sum.table3(summary = summary(t3.mod, age = c(10, 5), sex = "female"), trib.cols.rms, raw = TRUE)) %>% 
  distinct() %>% 
  mutate(across(where(is.numeric), as.numeric)) %>% 
  mutate(m = as.numeric(highc)) %>% 
  arrange(level, m, highc) %>% 
  mutate(axis = fct_inorder(axis) ) %>% 
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
    theme(panel.grid.major.x = element_line(colour = "gray90"),
          panel.grid.minor.x = element_line(colour = "gray95",
                                            size = 0.3),
          plot.background = element_rect(fill = "ghostwhite", size = 1.3)) +
    theme(plot.caption = element_text(size = 7, hjust = 0))+
  labs(x = "Odds ratio")


#' :::
#' 
#' ## Interaction contrasts
#' 
#' Explore any number of permutations of an interaction
#' 
#' ### expand.int
#' 
#' This function computes the contrasts for any set of two interacting variables. Five inputs are required:
#' 
#' -   **a.seq** - sequence of variable A
#' 
#' -   **b.seq** - sequence of variable B
#' 
#' -   **A** - name of variable A
#' 
#' -   **B** - name of variable B
#' 
#' -   **fit -** rms model object
#' 
## -----------------------------------------------------------------------------
t3_int <- expand.int(seq(10, 70, 10), c("male", "female"), "age", "sex", t3.mod.int)

head(t3_int)


#' 
#' ### int.react
#' 
#' This function just formats the output of `expand.int`
#' 
## -----------------------------------------------------------------------------
int.react(t3_int)

#' 
#' ### int.tile.plot
#' 
#' Produces a tile plot of all requested interaction contrasts
#' 
## -----------------------------------------------------------------------------
int.tile.plot(t3_int, "age", "sex") +labs(fill = "Estimate")

#' 
#' # Appendix
#' 
#' ## archive and changelog
#' 
#' These functions are in development still require some fine tuning to get the desired result. The two functions have complementary purpose:
#' 
#' `archive` creates a copy of the inputted data and saves it to an archive directory within your working directory. If it's your first time running `archive` from within the directory, a new folder will be created. If the data are unchanged from the last `archive` call, the data will not be re-saved and the user will be alerted. In addition to creating a time stamped copy of the data, a summary of all archives will be logged and updated. A helper function `retrieve_archive` can be used to access the stored summary file. 
#' 
#' Once archived data exists, `changelog2` will make comparisons across all consecutive archived versions of the data, e.g. 2 vs 1, 3 vs 2, etc. This relies heavily on `arsenal::comparedf()`. If there is only one archived data set or all comparisons have already been run, no new comparisons will be run. The function returns a message to the user with which action has been taken, and if necessitated by changes to the archive, will save a summary of changes from one version of the data to the next. 
#' 
#' The following example steps through the use of the functions with the expected output. Note: `changelog` was run at the very beginning on t3 data.
#' 
#' ### Example usage
#' ```
#' [1] "Archive directory created at /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false

changelog2(t3, "id") ## expected no action - yes

#' 
#' ```
#' [1] "No comparisons needed"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
archive(t3) ## expected save new data - yes

#' 
#' ```
#' [1] "New copy of data saved"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
ad0 <- retrieve_archive("t3") # expected 2 row - yes

changelog2(t3, "id") ## expected new comparison - yes


#' 
#' 
#' 
#' ```
#' [1] "1 comparisons run.\nSummaries saved to: /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
archive(t3) ## expect no action- yes

#' 
#' ```
#' [1] "No changes made to data"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
ad <- retrieve_archive("t3") # expected 2 rows - yes
changelog2(t3, "id")  ##expect no action - yes

#' 
#' ```
#' [1] "No comparisons needed"
#' ```
#' 
#' 
## -----------------------------------------------------------------------------
#| eval: false
t3 %<>% mutate(z = 12, w = 11)
archive(t3) ## expect new archive- yes

    ad2 <- retrieve_archive("t3")# expected 3 row - yes

#' 
#' ```
#' [1] "New copy of data saved"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
archive(t3) ## expect no action - yes

#' ```
#' [1] "No changes made to data"
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
changelog2(t3, "id") ## expect updates to logs - yes

#' 
#' ```
#' [1] "1 comparisons run.\nSummaries saved to: /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda"
#' ```
#' 
#' 
## -----------------------------------------------------------------------------
#| eval: false
ad4 <- retrieve_archive("t3") ##expected 3 rows - yes


#' 
#' 
## -----------------------------------------------------------------------------
#| eval: false
t3 %<>% mutate(ff = 23)
changelog2(t3, "id") # expect no action
archive(t3) ## expect new archive
changelog2(t3, "id") # expect new log
archive(t3) ## expect no action - yes
ad6 <- retrieve_archive("t3") # expect 4 rows - yes

#' ```
#' [1] "No comparisons needed"
#' [1] "New copy of data saved"
#' [1] "1 comparisons run.\nSummaries saved to: /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda"
#' [1] "No changes made to data"
#' 
#' ```
#' 
## -----------------------------------------------------------------------------
#| eval: false
t3 %<>% mutate(z = NULL, a = "b")
Sys.sleep(1)
changelog2(t3, "id")
archive(t3)
archive(t3)
t3 %<>% mutate(y = 122, w = 293, z = 111, a = "w")
Sys.sleep(1)
changelog2(t3, "id")
archive(t3)
changelog2(t3, "id")

ad7 <- retrieve_archive("t3") # expect 6 rows - yes

#' ```
#' [1] "No comparisons needed"
#' [1] "New copy of data saved"
#' [1] "No changes made to data"
#' [1] "1 comparisons run.\nSummaries saved to: /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda"
#' [1] "New copy of data saved"
#' [1] "1 comparisons run.\nSummaries saved to: /Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda"
#' ```
#' 
#' ### Archive summary
## -----------------------------------------------------------------------------
retrieve_archive("t3") %>% j.reactable()

#' 
#' 
#' ### Changelog output
#' ::: panel-tabset
## -----------------------------------------------------------------------------
load("/Users/joshdeclercq/Documents/GitHub/VUMisC/archive/t3/changelog_output_t3.rda")

#' 
#' #### Summary comparison
## -----------------------------------------------------------------------------
changelog_output$summary_comp_table %>% 
  pivot_wider(., names_from = "comparison", values_from = "value") %>% 
  j.reactable()

#' 
#' #### Differences by ID
## -----------------------------------------------------------------------------
changelog_output$diffs_by_id %>% j.reactable()

#' 
#' #### Variables not shared
#' 
## -----------------------------------------------------------------------------
changelog_output$vars_not_shared %>% j.reactable()

#' 
#' #### Differences by variable
## -----------------------------------------------------------------------------
changelog_output$diffs_by_var %>% j.reactable()

#' 
#' :::
#' 
#' ## j_dataChk
#' 
#' Borrowed extensively from Frank Harrell's `qreport::dataChk`
#' 
#' This function will check any number of logical expressions to find data errors or outliers. A data table is require. for the function input. The function outputs a summary of the number of checks found that meet the criteria, as well as a detailed data frame of each instance.
#' 
#' ### Summary
## -----------------------------------------------------------------------------


checks <- expression(date1 < ymd("1802-12-31"),
                      date2 > ymd("1910-01-01"),
                      date3  > ymd("1912-01-01"),
                      age > 75 )

B <- j_dataChk(data.table(t3), checks = checks, id= "id")

B$s

#' 
#' ### Detail
#' 
## -----------------------------------------------------------------------------
B$d %>% j.reactable(., groupBy = c("Check"), csv.file = "titanic data checks")

#' 
#' 
#' ## get_toc
#' 
#' Takes in a rmd (or qmd) file name or path and returns a data frame of the table of contents. When used in conjunction with the `csv.file` argument for `j.reactable`, it allows for a downloadable version of the current documents table of contents. I find this very useful in situations where collaborators want to provide large-scale annotations. 
#' 
#' 
## -----------------------------------------------------------------------------
get_toc("JDmisc.qmd") %>% j.reactable(., csv.file = "JDmisc toc")

#' 
#' 
#' ### Abbreviated report
#' 
#' The table of contents can also be leveraged to produce truncated reports where sections can be included/ excluded by simply toggling a field in the csv file. 
#' 
#' 
## -----------------------------------------------------------------------------
abbr <- read.csv("JDmisc toc.csv")

jgtt(abbr)

#' 
#' The `parsermd` package allows you to access you code in raw form. With some simple data manipulation, the undesired sections can be pruned out of the document. The resulting code can be saved as a different qmd or rmd file. (Note: I had some minor issues with the yaml header with the qmd format, so some minor tweaks are necessary. I have directly compiled an abbreviated report in the past using an rmd file, however.)
#' 
## -----------------------------------------------------------------------------
parsed <- parsermd::parse_rmd("JDmisc.qmd") %>% as.data.frame() %>%
  mutate(x = 1*(type=="rmd_heading"), n = cumsum(x)) %>%
  left_join(., abbr, by =c("n"= "order")) 

j.reactable(parsed, groupBy= c("sec_h1", "sec_h2"))


# parsed %>%
#   filter(include %in% c(NA, "y")) %>%
#   parsermd::as_document() %>% 
#   write(., file = "abbr_jdm.qmd")

#' 
#' 
#' ## session_info
#' 
#' This function takes the name of the current file as its input and returns a formatted table of the different functions used in the report and which packages they come from. It gives the current R version (alongside package = "base"). It's not perfect, as a lot of functions appear across multiple packages (e.g. `%>%` or `mutate`), but it is useful to know which packages needn't be attached to your report. 
#' 
#' Zooming out, one can envision how this type of function can be used to forge a reference for which functions are being routinely used, and in which report a certain function was called.
#' 
## -----------------------------------------------------------------------------
session_info("JDmisc.qmd")

#' 
#' 
## -----------------------------------------------------------------------------
#| eval: false

remotes::install_github('couthcommander/datastate', upgrade = 'never')
library(datastate)

Hmisc::getHdata(titanic3)
t3 <- titanic3 %>% mutate(cabin = as.character(cabin))
t3[[1]] <- c('first','second','third')[match(t3[[1]], c('1st','2nd','3rd'))]
t3 %<>% mutate(survived = as.factor(survived))

dd <- datadict(t3)

myfile2 <- paste0("jd_misc_datastate2", '.yaml')
dd2yaml(dd, file = myfile2)

dd1 <- makeFactor(dd, t3, 'pclass')
dd1 <- addRecode(dd1, t3, vars = 'survived')
myfile <- paste0("jd_misc_datastate", '.yaml')
dd2yaml(dd1, file = myfile)
sprintf('make changes to %s', myfile)
# directly edit file to do something like this
#dd1$variables[[1]]$factor[[1]]$label_output <- '1st'
#dd1$variables[[1]]$factor[[2]]$label_output <- '2nd'
#dd1$variables[[1]]$factor[[3]]$label_output <- '3rd'
#dd1$variables[[2]]$recode[[1]]$newvalue <- 'dead'
#dd1$variables[[2]]$recode[[2]]$newvalue <- 'alive'
#dd1$variables[[10]]$factor <- NULL
#dd1$variables[[12]]$factor <- NULL

upd <- yaml2dd(myfile)
out <- mod(upd)


collect.labels(out)

j.label5(t3) %>% datapasta::dpasta()
c("a", "b", "c", "d", "e", "f")
c("a", "b", "c", "d", "e", "f")

#' 
#' 
## -----------------------------------------------------------------------------
#| eval: false
#|
require(datapasta)
j.label6 <- function(df, all = FALSE){
  
  prev <- collect.labels(df) %>% mutate(combined = paste0("\t\t", variable, ' = "', label, '"', ',\n'))
  
  if(!all) prev %<>% filter(label == "")
  prev[nrow(prev), "combined"] <- gsub(",\n", ")\n\n",prev[nrow(prev), "combined"])
  
  
  
  output <- capture.output(cat(deparse(substitute(df)), "<- labelVector::set_label(", deparse(substitute(df)), ",\n",
      paste0(prev$combined)))
  
  # output
 rstudioapi::insertText(output)
}
j.label6(t3, all = TRUE)




t3 <- labelVector::set_label( t3 ,
 		pclass = "",
 		survived = "",
 		name = "Name",
 		sex = "",
 		age = "Age",
 		sibsp = "Number of Siblings/Spouses Aboard",
 		parch = "Number of Parents/Children Aboard",
 		ticket = "Ticket Number",
 		fare = "Passenger Fare",
 		cabin = "",
 		embarked = "",
 		boat = "",
 		body = "Body Identification Number",
 		home.dest = "Home/Destination")

#' 
