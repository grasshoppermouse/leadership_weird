
plan <- drake_plan(
  
  # Prepare data
  leader_text2 = leadershipdata::leader_text2,
  leader_cult = leadershipdata::leader_cult,
  leader_dtm = leadershipdata::leader_dtm,
  documents = leadershipdata::documents,
  all_ids = text_doc_auth_cult_ID(),
  all_data = merge_dfs(leader_text2, all_ids, leader_cult, documents, threshold = 1),
  all_study_vars = variable_names(all_data, type = c('functions', 'qualities', 'leader.benefit', 'leader.cost', 'follower.benefit', 'follower.cost')),
  
  # Misc objects for Rmd file
  
  # Variable support at the text record level
  functions_support_txt = textrecord_support(readd(all_data), 'functions'),
  qualities_support_txt = textrecord_support(readd(all_data), 'qualities'),
  leader_benefits_txt = textrecord_support(readd(all_data), 'leader.benefits'),
  leader_costs_txt = textrecord_support(readd(all_data), 'leader.costs'),
  follower_benefits_txt = textrecord_support(readd(all_data), 'follower.benefits'),
  follower_costs_txt = textrecord_support(readd(all_data), 'follower.costs'),

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
  
  # Variable support by subsistence strategy and region
  functions_support_subsis_region = textrecord_support_subsis_region(readd(all_data), 'functions'),
  qualities_support_subsis_region = textrecord_support_subsis_region(readd(all_data), 'qualities'),
  
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
  
  # Cross-validate logisticPCA for optimal params (30-40 minutes)
  qual_cvlpca = cv.lpca(df_qual, ks = 1:20, ms = 5:15),
  func_cvlpca = cv.lpca(df_func, ks = 1:20, ms = 5:15),
  
  # NMF (about 90 minutes)
  # m_nmf = nmf(t(df_all), rank = 2:15),
  # m_nmfrandom = nmf(randomize(t(df_all), rank=2:15)),

  # Elasticnet regression of high status by word freq
  highstatus_plot = model_words(all_data, leader_dtm, 'qualities_HighStatus', lam = "1se", title = 'Leader quality: High status'),
  
  mm_bias = bias_models(readd(all_data), all_study_vars),
  plot_pubdate = bias_plot(mm_bias, 'pub_dateZ', fdr = 0.05),
  
  report = rmarkdown::render(
    knitr_in("leadership_across_cultures_contexts.Rmd"),
    output_file = file_out("leadership_across_cultures_contexts.html"),
    quiet = TRUE
  )
)
