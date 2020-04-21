

# Create vectors of variable names by type --------------------------------

variable_names <- function(df, type){
  dfnames <- names(df)
  dfnames <- dfnames[!str_detect(dfnames, "functions_Context")]
  
  # build regex
  pattern <- str_c(type, collapse = '|')
  
  thevars <- dfnames[str_detect(dfnames, pattern)]
  names(thevars) <- var_names[thevars]
  return(thevars)
}

# df that links the ids of texts, docs, and cultures

text_doc_auth_cult_ID <- function(df_text = leader_text_original, df_doc = documents){
  df_text %>% 
    dplyr::select(cs_textrec_ID, doc_ID, author_ID) %>% 
    left_join(df_doc[c('d_ID', 'd_culture')], by = c("doc_ID" = "d_ID"))
}

# Merge data frames

merge_dfs <- function(
  leader_text2, 
  all_ids, 
  leader_cult,
  documents,
  threshold = 4,
  vars = c(
    "c_culture_code",
    "region", 
    "subsistence", 
    "c_cultural_complexity", 
    "settlement_fixity", 
    "pop_density", 
    "com_size",
    "ehraf_pages",
    "number_leader_records"
  )
){
  leader_text2 %>%
    select_if(
      ~ is.character(.) | (is.numeric(.) && sum(., na.rm = T) >= threshold) # remove low evidence
    ) %>% 
    left_join(all_ids, by = 'cs_textrec_ID') %>%
    left_join(
      leader_cult[vars], by = c('d_culture' = 'c_culture_code')
    ) %>% 
    left_join(
      documents[c('d_ID', 'd_publication_date', 'female_coauthor')], # One doc missing pub date
      by = c('doc_ID' = 'd_ID')
    ) %>% 
    rename(
      pub_date = d_publication_date
    ) %>% 
    mutate(
      pagesZ = (ehraf_pages - mean(ehraf_pages, na.rm=T))/sd(ehraf_pages, na.rm=T),
      pagesSD = ehraf_pages/sd(ehraf_pages, na.rm=T),
      pub_date = as.numeric(pub_date),
      pub_dateZ = (pub_date - mean(pub_date, na.rm=T))/sd(pub_date, na.rm = T)
    )
}

textrecord_support <- function(thedata, type){
  thevars <- variable_names(thedata, type)
  formulae <- glue_data(list(outcome = thevars), "{outcome} ~ pub_dateZ + (1|d_culture/author_ID)")
  
  models <-
    map(
      formulae,
      ~ glmer(
        as.formula(.x),
        family = binomial,
        data = thedata,
        nAGQ = 0
      )
    )
  
  tibble(
    Level = "Text records",
    Type = str_to_title(type),
    vars = thevars,
    Variable = names(thevars),
    Model = models,
    Tidy = map(Model, broom.mixed::tidy, conf.int = T),
    Estimate = map_dbl(Tidy, ~ logit.inv(.x$estimate[1])),
    lowerCI = map_dbl(Tidy, ~ logit.inv(.x$conf.low[1])),
    upperCI = map_dbl(Tidy, ~ logit.inv(.x$conf.high[1])),
    authorSD  = map_dbl(Tidy, ~ .x$estimate[3]),
    cultureSD = map_dbl(Tidy, ~ .x$estimate[4])
  )
}

culture_support <- function(df, type, n = 10){
  thevars <- variable_names(df, type)
  
  boots <- 
    df[c('d_culture', thevars)] %>% 
    nest(data = -d_culture) %>% 
    bootstraps(n)
  
  thesum <- function(thesplit, thevars){
    df <- 
      as_tibble(thesplit) %>% 
      unnest(cols = everything())
    map_dbl(thevars, ~ mean(tapply(df[[.]], df[['d_culture']], max)))
  }
  
  bootstats <- map_df(boots$splits, ~ thesum(.x, thevars))

  tibble(
    Level = 'Cultures',
    Type = str_to_title(type),
    vars = thevars,
    Variable = names(thevars),
    Estimate = map_dbl(thevars, ~ mean(tapply(df[[.]], df[['d_culture']], max))),
    lowerCI = map_dbl(bootstats, ~ quantile(.x, probs = 0.025)),
    upperCI = map_dbl(bootstats, ~ quantile(.x, probs = 0.975))
  )
}

func_qual_support_plot <- function(...){
  thedata <- 
    bind_rows(...) %>% 
    mutate(
      Variable = fct_reorder(Variable, Estimate)
    )
  ggplot(thedata, aes(Estimate, Variable, xmin = lowerCI, xmax = upperCI, colour = Level)) +
    geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.5) +
    geom_point() +
    scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent, limits=c(0,1)) +
    hagenutils::scale_color_binary() +
    facet_grid(Type~., scales = 'free_y', space = 'free_y') +
    labs(x = '', y = '') +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle=0))
}

benefit_cost_support_plot <- function(...){
  thedata <- 
    bind_rows(...) %>% 
    mutate(
      Variable = fct_reorder(Variable, Estimate)
    ) %>% 
    separate(Type, into = c('leader_follower', 'cost_benefit'), sep = '\\.') %>% 
    mutate(
      cost_benefit = str_to_title(cost_benefit)
    )
  
  ggplot(thedata, aes(Estimate, Variable, xmin = lowerCI, xmax = upperCI, colour = Level)) +
    geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.5) +
    geom_point() +
    scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent, limits=c(0,1)) +
    hagenutils::scale_color_binary() +
    facet_grid(cost_benefit~leader_follower, scales = 'free_y', space = 'free_y') +
    labs(x = '', y = '') +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle=0))
}

# Text analysis -----------------------------------------------------------

model_words <- function(all_data, leader_dtm, var, lam = 'lambda.min', exponentiate = T, title){
  
  df <- left_join(all_data[c('cs_textrec_ID', var)], leader_dtm)
  y <- df[[2]]
  x <- as.matrix(df[-c(1:2)])
  
  m_cv <- cv.glmnet(x, y, family = 'binomial', alpha = 1, standardize = F)
  plot(m_cv)
  
  if (lam == 'mid'){
    lmda <- m_cv$lambda.min + (m_cv$lambda.1se - m_cv$lambda.min)/2
  } else if (lam == 'min'){
    lmda <- m_cv$lambda.min
  } else if(lam == '1se'){
    lmda <- m_cv$lambda.1se
  } else {
    lmda = lam
  }
  
  c.min <- coef(m_cv, s = lmda)
  
  coefs <- c()
  for (i in 1:length(c.min)){
    if (c.min[i] != 0){
      coefs <- c(coefs, c.min[i])
      names(coefs)[length(coefs)] <- rownames(c.min)[i]
    }
  }
  
  if (exponentiate){
    coefs <- exp(coefs)
    xintrcpt <- 1
  } else {
    xintrcpt <- 0
  }
  
  coefs <- sort(coefs[-1]) # delete intercept
  
  df <-
    tibble(
      Word = factor(names(coefs), levels = names(coefs)),
      Coefficient = coefs,
      Sign = ifelse(coefs > xintrcpt, 'Increase', 'Decrease')
    )
  plot <- 
    ggplot(df, aes(Coefficient, Word, colour = Sign, shape=Sign)) + 
    geom_point(size=3) + 
    geom_vline(xintercept = xintrcpt, linetype = 'dotted') +
    hagenutils::scale_color_binary() +
    guides(colour=F, shape=F) +
    labs(title = title, x = '', y = '') +
    theme_minimal(15)
  
  return(plot)
}

# Sources of bias ---------------------------------------------------------

bias_models <- function(all_data, all_study_vars){
  
  df_cross <- 
    cross_df(list(x=c('pub_date', 'female_coauthor', 'pagesZ'), y=all_study_vars)) %>% 
    mutate(
      Variable = as.character(1:nrow(.)),
      Formula = glue("{y} ~ {x} + (1|d_culture/author_ID)")
    )
  
  df_allvars_uni <- 
    pmap_dfr(
      df_cross['Formula'],
      ~ tidy(
        glmer(
          as.formula(.),
          family = binomial,
          data = all_data,
          nAGQ = 0
        ),
        conf.int = T,
        exponentiate = T
      ),
      .id = 'Variable'
    ) %>% 
    left_join(df_cross) 
  
  df_allvars_uni2 <-
    df_allvars_uni %>% 
    mutate(
      term = case_when(
        term == 'all_data2[[.x]]TRUE' ~ x,
        term == 'all_data2[[.x]]' ~ x,
        TRUE ~ term
      ),
      Variable = var_names[y]
    )
  
  df_pubdate_uni <-
    df_allvars_uni2 %>%
    dplyr::filter(term == 'pub_date') %>% 
    mutate(
      Variable = fct_reorder(Variable, estimate),
      p_adj = p.adjust(p.value, method = 'BH'),
      Model = 'Univariate'
    ) %>% 
    dplyr::filter(p_adj < 0.05) %>% 
    dplyr::select(
      Model, Variable, estimate, p.value, conf.low, conf.high
    )
  
  # Fit model of each var vs. pub meta-data
  
  multiformula <- map(all_study_vars, ~ glue("{.} ~ pub_date + female_coauthor + pagesZ + (1|d_culture/author_ID)"))
  
  df_allvars_multi <- 
    map_df(
      multiformula, 
      ~ tidy(
        glmer(
          as.formula(.),
          family = binomial,
          data = all_data,
          nAGQ = 0
        ),
        conf.int = T,
        exponentiate = T
      ),
      .id = 'Variable'
    ) 
  
  df_pubdate_multi <-
    df_allvars_multi %>%
    dplyr::filter(term == 'pub_date') %>% 
    mutate(
      Variable = fct_reorder(Variable, estimate),
      p_adj = p.adjust(p.value, method = 'BH'),
      Model = 'Mulitvariate'
    ) %>% 
    dplyr::filter(p_adj < 0.05) %>% 
    dplyr::select(
      Model, Variable, estimate, p.value, conf.low, conf.high
    )
  
  bind_rows(df_pubdate_multi, df_pubdate_uni)
}

bias_plot <- function(df_pubdate_both){
  plot_pubdate <-
    ggplot(df_pubdate_both, aes(estimate, Variable, colour = Model)) + 
    geom_point(position=position_dodge(width = 0.3)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position=position_dodge(width = 0.2), height = 0, alpha=0.5) +
    geom_vline(xintercept = 1, linetype = 'dotted') +
    scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1, 1.5, 2, 3)) +
    labs(x = "\nCoefficient of publication year Z-score (odds ratio)", y = "") +
    theme_minimal(15)
}


