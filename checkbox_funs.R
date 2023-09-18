

check_data <- function(ID) {
  data.frame(id = ID, checkbox = "checkbox", 
             x = sample(c(0,1),12,prob = c(0.8, 0.2),  replace = TRUE)) %>% 
    mutate(nv = ac(1:n()))%>%
    mutate(site = sample(c("Site A", "Site B"), 1))
}


check_data_repeat <- function(ID, rep) {
  map(1:rep, ~check_data(ID) %>% mutate(rri = .x)) %>%bind_rows() %>%
    mutate(site = sample(c("Site A", "Site B"), 1))
}




tidy_labels <- function(dat, selection){
  X <- collect.labels(dat %>% select(contains(selection)))$label
  names(X) <- gsub('.{1}$', '', gsub(".*=", "", X))
  X
}




lookup_names <- function(dat, selection, simple = FALSE, add.zero = FALSE, tidy.labels = FALSE){
  dat_labels <- collect.labels(dat) %>% filter(grepl(selection, variable)) %>%
    { if(isTRUE(tidy.labels)) mutate(., label = gsub('.{1}$', '', gsub(".*=", "", label))) else .} %>%
    { if(isTRUE(add.zero)) bind_rows(., data.frame(variable = paste0(selection, "_zero"), label = "[Zero selected]")) else .} %>%
    mutate(any = paste(label, "(any)"),
           total = paste(label, "(total)"),
           any_var = paste0("any_", variable),
           total_var = paste0("total_", variable)) 
  
  
  lookup.any <- dat_labels$any_var
  names(lookup.any) <- case_when(isTRUE(simple)~ dat_labels$label, TRUE ~ dat_labels$any)
  lookup.total <-   dat_labels$total_var
  names(lookup.total) <- case_when(isTRUE(simple)~ dat_labels$label, TRUE ~ dat_labels$total)
  
  return(c(lookup.any, lookup.total))
  
}





checkbox_gather <- function(dat, selection, id.var = NULL, by.var = NULL, count = "any", tidy.labels = FALSE,
                            repeat.var = NULL, stats = "none", toggle.names = FALSE, simple.names = FALSE, add.zero = FALSE,
                            output = "df", ...){
  

  
  ## For matching variable class
  d.class <- dat %>% select(any_of(c(id.var, repeat.var, by.var)) ) %>%
    sapply(., class)
  
  ## transform factors to character
  dat %<>% rowwise() %>% mutate(across(any_of(c(id.var, repeat.var, by.var, contains(selection))), 
                        ~ifelse(is.factor(.x), as.character(.x), .x))) %>% ungroup()
  

  int <- dat %>% unite(.,"ID", c(id.var, repeat.var, by.var), sep = "_", remove = FALSE)
  key <- int %>% select(ID, any_of(c(id.var, repeat.var, by.var))) %>% distinct() 
  
  
  ## For toggle.names data must be labelled
  lookup <- lookup_names(dat = int, selection = selection, add.zero = add.zero, tidy.labels = tidy.labels,
                         simple = (case_when(count %in% "both" ~ FALSE,
                                             TRUE ~ simple.names)))
  

  ## account for rows with nothing selected 
  int <- int %>% rowwise(.) %>% 
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
           n.selected = sum(across(c(contains("total"), -any_of(paste0("total_", selection, "_none"))))),
           n.unique = sum(across(c(contains("total"), -any_of(paste0("total_", selection, "_none"))))>0)) %>% ungroup() %>%
    {if(count %in% c( "any")) select(., -contains("total")) else .} %>%
    {if(count %in% c("total")) select(., -contains("any")) else .} %>%
    {if(stats %in% c("none")) select(., -starts_with("n.")) else .} %>%
    {if(stats %in% c("only")) select(., ID, starts_with("n.")) else .} %>%
    {if(isTRUE(toggle.names)) rename(., any_of(c(lookup, stat_names))) else .} %>%
    {if(!isTRUE(add.zero)) select(., -contains("zero"), -contains("Zero")) else .} #%>%
    # separate(., "ID", into = c(id.var, repeat.var, by.var), sep = "_")
  
  # ## Coerce classes back to original
  # for(i in 1:length(names(d.class))){
  #   class(int[[names(d.class)[i]]]) <- class(dat[[names(d.class)[i]]])
  # }
  
  ## Put back in original order
  # int <- dat %>% select(names(d.class))%>% distinct() %>% left_join(., int, by = names(d.class))
  int <- key %>% left_join(., int, by = "ID") %>% select(-ID)
  
  if(output == "df"){
     return(int)
  } else {
  
  
  jgt(int %>% select(-any_of(c(id.var, repeat.var))), by = by.var, 
      overall = 1*(!is.null(by.var)), one_row_binary = TRUE, add.n = FALSE, ...)
    
  }
  
}

checkbox_tally <- function(dat, selection, id.var = NULL, by.var = NULL, col.name = "Selection", add.zero = FALSE, ...){
  
    int <- dat %>% 
      {if(is.null(by.var)) . else mutate(., byv = paste0("",!!sym(by.var)))} %>%
      select(any_of(c(id.var, by.var)), contains(selection)) %>%
      {if(isTRUE(add.zero)) rowwise(.) %>% 
          mutate(None = sum(!is.na(across(contains(selection)))), None = ifelse(None ==0, "[None]", NA)) else .} %>%
      pivot_longer(., -any_of(c(id.var, by.var)), values_drop_na = TRUE) %>% 
      select(value, by.var)
    
    names(int)[1] <- col.name
    
    jgt(int, order.cat = TRUE, by = by.var, overall = 1*(!is.null(by.var)),  ...)
  
}


checkbox_intersect <- function(dat, selection, id.var, repeat.var = NULL, toggle.names = TRUE, add.zero = FALSE){
  
  checkbox_gather(dat = dat, selection = selection, id.var = id.var, repeat.var = repeat.var,
                  count = "any", stats = "none", simple.names = TRUE,
                  toggle.names = toggle.names, add.zero = add.zero, output = "df") %>%
    select(-any_of(c(id.var, repeat.var)))%>%
    mutate(across(everything(), ~ifelse(.x == "Yes", 1, 0))) %>%
    as.data.frame() %>% UpSetR::upset(., nsets = 20, nintersects = 30)
  
}
