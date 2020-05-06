
plan <- drake_plan(
  
  # Prepare data
  leader_text_original = leadershipdata::leader_text_original,
  leader_text2 = leadershipdata::leader_text2,
  leader_cult = leadershipdata::leader_cult,
  leader_dtm = leadershipdata::leader_dtm,
  documents = leadershipdata::documents,
  all_ids = text_doc_auth_cult_ID(leader_text_original, documents),
  all_data = merge_dfs(leader_text2, all_ids, leader_cult, documents, threshold = 1),
  all_study_vars = variable_names(all_data, type = c('functions', 'qualities', 'leader.benefit', 'leader.cost', 'follower.benefit', 'follower.cost')),
  
  # Misc objects for Rmd file
  
  # Variable support at the text record level
  formula_string = "{outcome} ~ 1 + (1|d_culture/author_ID)",
  
  functions_support_txt = textrecord_support(readd(all_data), 'functions', formula_string),
  qualities_support_txt = textrecord_support(readd(all_data), 'qualities', formula_string),
  leader_benefits_txt = textrecord_support(readd(all_data), 'leader.benefits', formula_string),
  leader_costs_txt = textrecord_support(readd(all_data), 'leader.costs', formula_string),
  follower_benefits_txt = textrecord_support(readd(all_data), 'follower.benefits', formula_string),
  follower_costs_txt = textrecord_support(readd(all_data), 'follower.costs', formula_string),

  # Variable support at the culture level
  functions_support_cult = culture_support(all_data, 'functions', n=1000),
  qualities_support_cult = culture_support(all_data, 'qualities', n=1000),
  leader_benefits_cult = culture_support(all_data, 'leader.benefits', n=1000),
  leader_costs_cult = culture_support(all_data, 'leader.costs', n=1000),
  follower_benefits_cult = culture_support(all_data, 'follower.benefits', n=1000),
  follower_costs_cult = culture_support(all_data, 'follower.costs', n=1000),
  
  # Variable support plots
  plot_func_qual = 
    func_qual_support_plot(
      functions_support_txt, 
      qualities_support_txt, 
      functions_support_cult, 
      qualities_support_cult
    ),
  
  plot_costs_benefits = 
    benefit_cost_support_plot(
      leader_benefits_txt, 
      leader_costs_txt, 
      follower_benefits_txt, 
      follower_costs_txt,
      leader_benefits_cult,
      leader_costs_cult,
      follower_benefits_cult,
      follower_costs_cult
    ),
  
  # Variable support by subsistence strategy, region, leader sex, group structure
  multi_allvars = textrecord_support_multi(readd(all_data), all_study_vars),
  multi_aic = multi_allvars %>% dplyr::filter(AIC_diff < -2),
  # p_heatmap_subsis = var_heatmap(multi_aic, 'subsistence'),
  # p_heatmap_region = var_heatmap(multi_aic, 'region'),
  # p_heatmap_sex    = var_heatmap(multi_aic, 'demo_sex'),
  # p_heatmap_groups = var_heatmap(multi_aic, 'group.structure2'),

  # Prepare data for dimension reduction
  # Only use coded columns
  
  df_qual = 
    merge_dfs(leader_text2, all_ids, leader_cult, documents, threshold = 3) %>% 
    dplyr::select(all_of(variable_names(., 'qualities'))) %>% 
    dplyr::filter(rowSums(.)>0),

  df_func = 
    all_data %>% 
    dplyr::select(all_of(variable_names(., 'functions'))) %>% 
    dplyr::filter(rowSums(.)>0),
  
  df_all =
    all_data %>% 
    dplyr::select(all_of(unname(variable_names(., c('qualities', 'functions', 'leader.costs', 'leader.benefits', 'follower.costs', 'follower.benefits'))))) %>% 
    dplyr::filter(rowSums(.)>0),
  
  # Cluster analyses
  m_pvclust_qual = pvclust(
    df_qual, 
    method.hclust = 'ward.D2', 
    method.dist = 'correlation', 
    nboot = 10000,
    parallel = T
  ),
  
  m_pvclust_qual_plot = {
    pdf(file_out("Figures/m_pvclust_qual.pdf"), width=12, height=8)
    plot(m_pvclust_qual)
    pvrect(m_pvclust_qual, alpha = 0.9)
    dev.off()
  },
  
  m_pvclust_func = 
    pvclust(
      df_func, 
      method.hclust = 'ward.D2', 
      method.dist = 'correlation', 
      nboot = 10000,
      parallel = T
    ),
  
  m_pvclust_func_plot = {
    pdf(file_out("Figures/m_pvclust_func.pdf"), width=12, height=8)
    plot(m_pvclust_func)
    pvrect(m_pvclust_func, alpha = 0.9)
    dev.off()
  },
  
  # Add feature variables to all_data
  feature_data = create_feature_vars(all_data, m_pvclust_func, m_pvclust_qual),
  features = feature_data$feature_vars,
  all_data2 = feature_data$data,
  feature_models = textrecord_support_multi(readd(all_data2), readd(features)),
  feature_models_aic = feature_models %>% tidylog::filter(AIC_diff < -2),
  p_heatmap_feature_subsis = var_heatmap(feature_models_aic, 'subsistence'),
  p_heatmap_feature_region = var_heatmap(feature_models_aic, 'region'),
  p_heatmap_feature_sex    = var_heatmap(feature_models_aic, 'demo_sex'),
  p_heatmap_feature_groups = var_heatmap(feature_models_aic, 'group.structure2'),
  
  # Cross-validate logisticPCA for optimal params (30-40 minutes)
  qual_cvlpca = cv.lpca(df_qual, ks = 1:20, ms = 5:15),
  func_cvlpca = cv.lpca(df_func, ks = 1:20, ms = 5:15),
  
  m_lpca_qual = logisticPCA(df_qual, k = 2, m = which.min(qual_cvlpca[2,]) + 4),
  m_lpca_func = logisticPCA(df_func, k = 2, m = which.min(func_cvlpca[2,]) + 4),
  
  # NMF (about 90 minutes)
  # m_nmf = nmf(t(df_all), rank = 2:15),
  # m_nmfrandom = nmf(randomize(t(df_all), rank=2:15)),

  # Elasticnet regression of high status by word freq
  highstatus_plot = model_words(all_data, leader_dtm, 'qualities_HighStatus', lam = "1se", title = ''),
  plot_elastic_status = elastic_dimensions(all_data, 'qualities_HighStatus', c('functions', 'qualities'), alpha = 1, lambda = 'lambda.1se'),
  coercive_plot = model_words(all_data, leader_dtm, 'qualities_CoerciveAuthority', lam = "min", title = ''),
  plot_elastic_coercive = elastic_dimensions(all_data, 'qualities_CoerciveAuthority', c('functions', 'qualities'), alpha = 0.5, lambda = 'lambda.1se'),
  mm_bias = bias_models(readd(all_data), all_study_vars),
  plot_pubdate = bias_plot(mm_bias, 'pub_dateZ', fdr = 0.05),
  
  report = rmarkdown::render(
    knitr_in("leadership_across_cultures_contexts.Rmd"),
    output_file = file_out("leadership_across_cultures_contexts.html"),
    quiet = TRUE
  )
)
