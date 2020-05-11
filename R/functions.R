

# Reverse variable name dict ----------------------------------------------

reverse_vars_dict <- names(var_names)
names(reverse_vars_dict) <- var_names

# Create vectors of variable names by type --------------------------------

# variable names encode their type
variable_names <- function(df, type){
  dfnames <- names(df)
  dfnames <- dfnames[!str_detect(dfnames, "functions_Context")]
  
  # build regex
  pattern <- str_c(type, collapse = '|')
  
  thevars <- dfnames[str_detect(dfnames, pattern)]
  names(thevars) <- var_names[thevars]
  return(thevars)
}

# df that links the ids of texts, docs, and cultures -----------------------

text_doc_auth_cult_ID <- function(df_text, df_doc){
  df_text %>% 
    dplyr::select(cs_textrec_ID, doc_ID, author_ID) %>% 
    left_join(df_doc[c('d_ID', 'd_culture')], by = c("doc_ID" = "d_ID"))
}

# Merge data frames --------------------------------------------------------

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


# Variable support in text records ----------------------------------------

textrecord_support <- function(thedata, type, formula_string){
  
  if (length(type == 1)){
    thevars <- variable_names(thedata, type)
  } else {
    thevars = type
  }
  
  formulae <- glue_data(list(outcome = thevars), formula_string)
  
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
    Type = ifelse(length(type) == 1, str_to_title(type), 'Multiple'),
    vars = thevars,
    Variable = names(thevars),
    Model = models,
    Tidy = map(Model, broom.mixed::tidy, conf.int = T),
    Estimate = map_dbl(Tidy, ~ logit.inv(.x$estimate[1])),
    lowerCI = map_dbl(Tidy, ~ logit.inv(.x$conf.low[1])),
    upperCI = map_dbl(Tidy, ~ logit.inv(.x$conf.high[1])),
    authorSD  = map_dbl(Tidy, ~ .x$estimate[2]), # Very fragile
    cultureSD = map_dbl(Tidy, ~ .x$estimate[3])  # Very fragile
  )
}


# Variable support by culture ---------------------------------------------

culture_support <- function(df, thetype, n = 10){

  if (length(thetype) == 1){
    thevars <- variable_names(df, thetype)
  } else {
    thevars <- thetype
  }

  boots <- 
    df[c('d_culture', thevars)] %>% 
    nest(data = -d_culture) %>% 
    bootstraps(n)

  thesum <- function(thesplit, thevars){
    df <- 
      as_tibble(thesplit) %>% 
      unnest(cols = everything())
    # return(df)
    map_dbl(thevars, ~ mean(tapply(df[[.]], df[['d_culture']], max)))
  }
  
  bootstats <- map_df(boots$splits, ~ thesum(.x, thevars))

  tibble(
    Level = 'Cultures',
    Type = str_to_title(thetype),
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
    separate(Type, into = c('leader_follower', 'cost_benefit'), sep = '\\.') %>% 
    mutate(
      cost_benefit = str_to_title(cost_benefit),
      leader_follower = factor(leader_follower, levels = c('Leader', 'Follower'))
    )

  lvls <-
    thedata %>% 
    dplyr::filter(leader_follower == 'Leader', Level == 'Cultures') %>% 
    arrange(
      Estimate
    ) %>% 
    dplyr::select(Variable)
  
  thedata$Variable <- factor(thedata$Variable, levels = lvls[[1]])
  
  ggplot(thedata, aes(Estimate, Variable, xmin = lowerCI, xmax = upperCI, colour = Level)) +
    geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.5) +
    geom_point() +
    scale_x_continuous(breaks=seq(0,1,.2), labels=scales::percent, limits=c(0,1)) +
    hagenutils::scale_color_binary() +
    facet_grid(cost_benefit~leader_follower, scales = 'free_y', space = 'free_y') +
    labs(x = '', y = '') +
    theme_bw(20) +
    theme(strip.text.y = element_text(angle=0)) # , axis.text=element_text(size=rel(1.3))
}

features_support_plot <- function(...){
  thedata <- 
    bind_rows(...) %>% 
    mutate(
      Variable = fct_reorder(vars, Estimate)
    )
  ggplot(thedata, aes(Estimate, Variable, xmin = lowerCI, xmax = upperCI, colour = Level)) +
    geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.5) +
    geom_point() +
    scale_x_continuous(breaks=seq(0,1, 0.2), labels=scales::percent, limits=c(0,1)) +
    hagenutils::scale_color_binary() +
    labs(x = '', y = '') +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle=0))
}


# Benefit cost ratio ------------------------------------------------------

bc_dict <- c(
  Mating = 'Mating',
  Territory = 'Territory',
  Fitness = 'Inclusive fitness',
  ResourceOther = 'Material resources',
  SocialServices = 'Social services',
  SocialStatusReputation = 'Social status',
  ResourceFood = 'Food',
  Other = 'Misc. non-material resources',
  RiskHarmConflict = 'Protection/harm'
)

benefit_cost_ratio <- function(d){
  m <-
    glmer(
      Evidence ~
        Benefit_cost * Variable +
        (1|d_culture/author_ID),
      family = binomial,
      data = d,
      nAGQ = 0
    )
  
  em_cb <- summary(emmeans(m, pairwise ~ Benefit_cost, type = 'response'))
  em_cb_var <- confint(emmeans(m, pairwise ~ Benefit_cost | Variable, type = 'response'))
  
  list(
    benefit_cost_OR = em_cb$contrasts$odds.ratio,
    benefit_cost_var_OR = em_cb_var$contrasts
  )
}

# Variable support by subsistence and region ------------------------------

textrecord_support_multi <- function(thedata, thevars){
  
  # thevars <- variable_names(thedata, type)
  uniformulae <- glue_data(list(outcome = thevars), "{outcome} ~ 1 + (1|d_culture/author_ID)")
  multiformulae <- glue_data(list(outcome = thevars), "{outcome} ~ subsistence + region + demo_sex + group.structure2 + (1|d_culture/author_ID)")
  
  unimodels <-
    map(
      uniformulae,
      ~ glmer(
        as.formula(.x),
        family = binomial,
        data = thedata,
        nAGQ = 0
      )
    )
  
  multimodels <-
    map(
      multiformulae,
      ~ glmer(
        as.formula(.x),
        family = binomial,
        data = thedata,
        nAGQ = 0
      )
    )
  
  tibble(
    Level = "Text records",
    var = thevars,
    Variable = names(thevars),
    Evidence = map_dbl(var, ~ sum(thedata[.])), # This won't work for 2xn matrices
    Unimodel = unimodels,
    Unistats = map(Unimodel, glance),
    interceptAIC = map_dbl(Unistats, 3),
    Multimodel = multimodels,
    Multistats = map(Multimodel, glance),
    multiAIC = map_dbl(Multistats, 3),
    AIC_diff = multiAIC - interceptAIC,
    Drop1 = map(Multimodel, drop1),
    AIC_full = map_dbl(Drop1, c(2,1)),
    AIC_subsis = map_dbl(Drop1, c(2,2)),
    AIC_region = map_dbl(Drop1, c(2,3)),
    AIC_female = map_dbl(Drop1, c(2,4)),
    AIC_groups = map_dbl(Drop1, c(2,5)),
    Anova = map(Multimodel, Anova),
    Tidy = map(Multimodel, ~broom.mixed::tidy(., conf.int = T)),
    pvalues = map(Anova, 'Pr(>Chisq)'),
    pvalue_subsis = map_dbl(pvalues, 1),
    pvalue_region = map_dbl(pvalues, 2),
    pvalue_sex    = map_dbl(pvalues, 3),
    pvalue_groups = map_dbl(pvalues, 4),
    adj_pvalue_subsis = p.adjust(pvalue_subsis, method = 'BH'),
    adj_pvalue_region = p.adjust(pvalue_region, method = 'BH'),
    adj_pvalue_sex    = p.adjust(pvalue_sex, method = 'BH'),
    adj_pvalue_groups = p.adjust(pvalue_groups, method = 'BH')
  )
}

hagenheat <- function(d, hc_method = 'ward.D', dist = 'euclidean', scale. = 'row'){
  # Assumes that first column is row labels, and that
  # remaining columns are numeric
  
  if (scale. == 'row'){
    d[-1] <- as_tibble(t(scale(t(d[-1]))))
  } else if (scale. == 'col'){
    d[-1] <- as_tibble(scale(d[-1])) 
  }
  
  hclustrows <- hclust(dist(d[-1], method = dist), method = hc_method)
  hclustcols <- hclust(dist(t(d[-1]), method = dist), method = hc_method)
  
  d[1] <- factor(d[[1]], levels = d[[1]][hclustrows$order])
  
  d %>%
    gather(key = key, value = value, -1) %>% 
    mutate(
      key = factor(key, levels = colnames(d[-1])[hclustcols$order]),
    ) %>% 
    ggplot(aes_string('key', colnames(.)[1], fill = 'value')) + geom_raster() +
    scale_fill_viridis() +
    scale_x_discrete(labels = scales::label_wrap(10)) +
    labs(x = "", y = "")
}

var_heatmap <- function(df_models, spec){
  d <- 
    df_models %>%
    select(Variable, Multimodel) %>% 
    mutate(
      emmeans = map(Multimodel, ~emmeans(., spec = spec, type = "response")),
      emm_summary = map(emmeans, summary)
    ) %>% 
    unnest(emm_summary) %>%
    select(Variable, prob, all_of(spec)) %>%
    spread(key=spec, value=prob) # worried about using char vec here, but it seems to work
  
  # mat <- as.matrix(d[-1])
  # rownames(mat) <- d$Variable
  # heatmap(mat, hclustfun = function(x) hclust(x, method = 'ward.D'), scale = 'row')
  # ggheatmap(mat, hclustmethod = 'ward.D', scale = 'row')
  hagenheat(d)
}


# Elasticnet models of one dimension by other dimensions ------------------

elastic_dimensions <- function(d, outcomevar, predictorvars, alpha = 1, lambda = 'lambda.min', threshold = 0){
  predvars <- variable_names(d, predictorvars)
  predvars <- predvars[predvars != outcomevar]
  
  y <- d[[outcomevar]]
  x <- d[predvars]
  x <- x[colSums(x)>threshold]
  x <- as.matrix(x)
  
  m <- glmnet::cv.glmnet(x, y, family = 'binomial', alpha = alpha)
  plot(m)
  coefs <- coef(m, s = m[[lambda]])[-1,1] # delete intercept
  names(coefs) <- var_names[names(coefs)] # var_names from leadershipdata
  ggdotchart(exp(coefs[coefs != 0]), threshold = 1) +
    geom_vline(xintercept = 1, linetype = 'dotted') +
    # hagenutils::scale_color_binary() +
    guides(colour=F, shape=F) +
    scale_x_log10()
}

# Create feature variables ------------------------------------------------

create_feature_vars <- function(d, m_pvclust_func, m_pvclust_qual){
  
  branch2df <- function(branch, name){
    lbls <- labels(branch)
    tibble(
      Feature = rep(name, length(lbls)),
      Label = lbls, 
      Variable = reverse_vars_dict[lbls]
    )
  }
  
  qual_dendro <- as.dendrogram(m_pvclust_qual)
  
  qual_branches <- list(
    'Cultural_conformity' = qual_dendro[[1]][[1]],
    'Prosocial_competencies' = qual_dendro[[1]][[2]],
    'Social_material_success' = qual_dendro[[2]][[1]],
    'Competencies' = qual_dendro[[2]][[2]]
    # 'Prestige' = qual_dendro[[2]][[2]][[2]]
  )
  
  clust_qual_vars <- map2_df(qual_branches, names(qual_branches), branch2df)
  
  func_dendro <- as.dendrogram(m_pvclust_func)
  
  func_branches <- list(
    'Prosociality' = func_dendro[[1]],
    'Strategize' = func_dendro[[2]][[1]],
    'Organize' = func_dendro[[2]][[2]]
  )
  
  clust_func_vars <- map2_df(func_branches, names(func_branches), branch2df)
  
  clust_vars <- bind_rows(clust_func_vars, clust_qual_vars)
  features <- unique(clust_vars$Feature)
  names(features) <- features
  
  # Feature analysis
  
  feature_var <- function(feature){
    featurevars <- clust_vars$Variable[clust_vars$Feature == feature]
    n <- length(featurevars)
    rs <- rowSums(d[featurevars])
    cbind(rs, n - rs) # successes, failures
  }
  
  # Add feature vars to d
  # feature vars are n x 2 matrices; col 1: successes, col 2: failures
  d <-
    d %>% 
    dplyr::select(demo_sex:pub_dateZ) %>% 
    bind_cols(map_dfc(features, feature_var))
  
  return(list(feature_vars = features, data = d))
  
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
  print(summary(df))
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
    cross_df(list(x=c('pub_dateZ', 'female_coauthor', 'pagesZ'), y=all_study_vars)) %>% 
    mutate(
      Variable = as.character(1:nrow(.)),
      Formula = glue("{y} ~ {x} + (1|d_culture/author_ID)")
    )
  
  df_allvars_uni <- 
    pmap_dfr(
      df_cross['Formula'],
      ~ broom.mixed::tidy(
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
    left_join(df_cross) %>% 
    mutate(
      term = case_when(
        term == 'all_data2[[.x]]TRUE' ~ x,
        term == 'all_data2[[.x]]' ~ x,
        TRUE ~ term
      ),
      Variable = var_names[y],
      Type = 'Univariate'
    )
  
  # Fit model of each var vs. pub meta-data
  
  multiformula <- map(all_study_vars, ~ glue("{.} ~ pub_dateZ + female_coauthor + pagesZ + (1|d_culture/author_ID)"))
  
  df_allvars_multi <- 
    map_df(
      multiformula, 
      ~ broom.mixed::tidy(
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
    mutate(
      Type = 'Multivariate'
    )
  
  bind_rows(df_allvars_uni, df_allvars_multi)
}

bias_plot <- function(d, theterm, fdr = 0.05){

  dterm <-
    d %>%
    dplyr::filter(term == theterm) %>% 
    mutate(
      Variable = fct_reorder(Variable, estimate),
      p_adj = p.adjust(p.value, method = 'BH')
    ) %>% 
    dplyr::filter(p_adj < fdr) %>% 
    dplyr::select(
      Type, Variable, term, estimate, p_adj, conf.low, conf.high
    )
  
 ggplot(dterm, aes(estimate, Variable, colour = Type)) + 
    geom_point(position=position_dodge(width = 0.3)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position=position_dodge(width = 0.2), height = 0, alpha=0.5) +
    geom_vline(xintercept = 1, linetype = 'dotted') +
    scale_x_log10() +
    labs(x = glue("\n\nCoefficient of {theterm}"), y = "") +
    theme_minimal(15)
}


