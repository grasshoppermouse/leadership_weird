
function_vars = variable_names(all_data, type = 'functions')
quality_vars = variable_names(all_data, type = 'qualities')
leader_benefit_vars = variable_names(all_data, type = 'leader.benefits')
leader_cost_vars = variable_names(all_data, type = 'leader.costs')
follower_benefit_vars = variable_names(all_data, type = 'follower.benefits')
follower_cost_vars = variable_names(all_data, type = 'follower.costs')

# Coefficient table -------------------------------------------------------

all_coefs <- 
  bind_rows(
    functions_support_txt,
    qualities_support_txt,
    leader_benefits_txt,
    leader_costs_txt,
    follower_benefits_txt,
    follower_costs_txt
  ) %>% 
  dplyr::select(Type, Outcome = Variable, Tidy) %>% 
  unnest(Tidy) %>% 
  group_by(Type, Outcome) %>% 
  mutate(
    Type = c(Type[1], rep(NA, n() - 1)),
    Outcome = c(Outcome[1], rep(NA, n() - 1))
  ) %>% 
  ungroup

# Compute values -----------------------------------------------------------

group_sex_tbl <- xtabs(~demo_sex+group.structure2, all_data)
female_residential_pct <- signif(group_sex_tbl['female', 'residential subgroup']/sum(all_data$demo_sex == 'female'), 3)*100
male_residential_pct <- signif(group_sex_tbl['male', 'residential subgroup']/sum(all_data$demo_sex == 'male'), 3)*100

sub_sex_tbl <- xtabs(~demo_sex+subsistence, all_data)
female_hort_pct <- signif(sub_sex_tbl['female', 'horticulturalists']/sum(all_data$demo_sex == 'female'), 3)*100
female_hg_pct <- signif(sub_sex_tbl['female', 'hunter gatherers']/sum(all_data$demo_sex == 'female'), 3)*100


group_sub_tbl <- xtabs(~ subsistence + group.structure2, all_data)

groupXsubsis <- function(group, subsis){
  signif(group_sub_tbl[subsis, group]/sum(leader_text$subsistence == subsis), 3)*100
}

hg_residential_pct <- groupXsubsis('residential subgroup', 'hunter gatherers')
hg_kin_pct <- groupXsubsis('kin group', 'hunter gatherers')
hg_supra_pct <- groupXsubsis('political group (supracommunity)', 'hunter gatherers')
hort_kin_pct <- groupXsubsis('kin group', 'horticulturalists')
agri_supra_pct <- groupXsubsis('political group (supracommunity)', 'agriculturalists')
agri_residential_pct <- groupXsubsis('residential subgroup', 'agriculturalists')

final_record_count <- sum(rowSums(all_data[all_study_vars])>0)

male_leader_pct <- signif(100*sum(all_data$demo_sex=='male', na.rm=T)/nrow(all_data), 3)
female_leader_pct <- signif(100*sum(all_data$demo_sex=='female', na.rm=T)/nrow(all_data), 2)

intelltxts <- sum(all_data$qualities_KnowlageableIntellect)
polytxts <- sum(all_data$qualities_Polygynous)
statustxts <- sum(all_data$qualities_HighStatus)
intellpolytxts <- sum(all_data$qualities_Polygynous & all_data$qualities_KnowlageableIntellect)
statuspolytxts <- sum(all_data$qualities_Polygynous & all_data$qualities_HighStatus)


# Text analysis -----------------------------------------------------------

textstats <- text_records %>% 
  dplyr::select(cs_textrec_ID, raw_text) %>% 
  unnest_tokens(word, raw_text) %>% 
  # dplyr::filter(is.na(as.numeric(word))) %>% # filters out numbers, some of which are page numbers
  dplyr::group_by(cs_textrec_ID) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::summarise(min = min(count), max = max(count), mean = mean(count), median = median(count), sd = sd(count)) %>% 
  round(1)


# Subsistence by group mosaic plot ----------------------------------------

df_groups <- 
  all_data %>% 
  dplyr::select(
    group.structure2,
    demo_sex,
    subsistence
  ) %>% 
  dplyr::filter(group.structure2 != 'other') %>% 
  dplyr::mutate(
    demo_sex = factor(demo_sex, levels = c('male', 'female')),
    group = factor(
      group.structure2,
      levels = c(
        'residential subgroup',
        'kin group',
        'economic group',
        'religious group',
        'military group',
        'political group (community)',
        'political group (supracommunity)'
      )
    ),
    subsistence = factor(
      subsistence,
      levels = c("hunter gatherers",
                 "pastoralists",
                 "mixed",
                 "horticulturalists",
                 "agriculturalists"
      )
    )
  )

plot_group_subsis <-
  ggplot(df_groups) +
  geom_mosaic(aes(x = product(group, subsistence), fill = group)) +
  scale_fill_viridis(discrete = T) +
  labs(x="", y="", fill = "Group context") +
  guides(fill = guide_legend(reverse = T)) +
  theme_minimal(20) 

# Female leaders by ethnographer gender -----------------------------------

# leader_text2$demo_sex
# authorship
# text_records

female_coauthor <- function(cs_textrec_ID){
  document_ID <- text_records$document_d_ID[text_records$cs_textrec_ID == cs_textrec_ID]
  author_genders <- authorship$author_gender[authorship$document_ID == document_ID]
  'female' %in% author_genders
}

all_data$female_coauthor <- sapply(all_data$cs_textrec_ID, female_coauthor)

leader_text3 <- 
  all_data %>% 
  dplyr::select(
    cs_textrec_ID,
    demo_sex,
    female_coauthor
  ) %>% 
  dplyr::mutate(
    female_leader_present = dplyr::case_when(
      demo_sex == 'unknown' ~ 'unknown',
      demo_sex == 'male' ~ 'no',
      TRUE ~ 'yes'
    ),
    female_leader_present2 = female_leader_present == 'yes'
  ) %>% 
  dplyr::filter(
    demo_sex != 'unknown'
  ) %>% 
  dplyr::left_join(
    text_records[c("document_d_ID", "cs_textrec_ID")]
  )

mm_coauthor <- glmer(
  female_leader_present2 ~
    female_coauthor +
    (1|document_d_ID),
  family = binomial,
  data = leader_text3
)
mm_coauthorOR <- exp(fixef(mm_coauthor))[[2]]


# Total benefits ----------------------------------------------------------

leader_text4 <-
  all_data %>% 
  dplyr::mutate(
    leadertotalbenefits = rowSums(.[leader_benefit_vars]),
    leaderbenefitfailure = length(leader_benefit_vars) - leadertotalbenefits,
    leadertotalcosts = rowSums(.[leader_cost_vars]),
    leadercostfailure = length(leader_cost_vars) - leadertotalcosts,
    followertotalbenefits = rowSums(.[follower_benefit_vars]),
    followerbenefitfailure = length(follower_benefit_vars) - followertotalbenefits,
    followertotalcosts = rowSums(.[follower_cost_vars]),
    followercostfailure = length(follower_cost_vars) - followertotalcosts
  )

mat <- as.matrix(cbind(leader_text4$leadertotalbenefits, leader_text4$leaderbenefitfailure))

m_ldrtotben <-
  glmer(
    cbind(leadertotalbenefits, leaderbenefitfailure) ~
      subsistence +
      region +
      demo_sex +
      group.structure2 +
      (1|d_culture/doc_ID),
    family = binomial,
    data = leader_text4,
    nAGQ = 0
  )

# Reduced model to avoid severe overfitting
m_ldrtotcost <-
  glmer(
    cbind(leadertotalcosts, leadercostfailure) ~
      subsistence +
      # region +
      # demo_sex +
      group.structure2 +
      (1|d_culture/doc_ID),
    family = binomial,
    data = leader_text4,
    nAGQ = 0
  )

m_followben <-
  glmer(
    cbind(followertotalbenefits, followerbenefitfailure) ~
      group.structure2 +
      (1|d_culture/doc_ID),
    family = binomial,
    data = leader_text4,
    nAGQ = 0
  )

m_followcost <-
  glmer(
    cbind(followertotalcosts, followercostfailure) ~
      group.structure2 +
      (1|d_culture/doc_ID),
    family = binomial,
    data = leader_text4,
    nAGQ = 0
  )

# Benefit / cost ratio -------------------------------

lvls <- leader_bc_ratio$benefit_cost_var_OR$Variable[order(leader_bc_ratio$benefit_cost_var_OR$odds.ratio)]

bc_OR <- 
  bind_rows(
  Leader = leader_bc_ratio$benefit_cost_var_OR, 
  Follower = follower_bc_ratio$benefit_cost_var_OR,
  .id = 'Type'
  ) %>% 
  mutate(
    Variable = factor(Variable, levels = lvls)
  )

plot_bc_OR <-
  ggplot(bc_OR, aes(odds.ratio, Variable, xmin = asymp.LCL, xmax = asymp.UCL, colour=Type)) + 
  geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.7, position = position_dodge(0.7)) + 
  geom_point(position = position_dodge(0.7)) + 
  geom_vline(xintercept = 1, linetype = 'dotted') +
  hagenutils::scale_colour_binary() +
  scale_x_log10() +
  guides(colour = guide_legend(reverse=T)) +
  labs(title = 'Relative evidence of benefits vs. costs', x = '\nOdds ratio', y = '') +
  theme_minimal(15)

# Document years plots ----------------------------------------------------

documents$d_publication_date <- as.numeric(documents$d_publication_date)

doc_year_hist <- 
  ggplot(documents, aes(d_publication_date)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(1860,2000,20)) +
  geom_vline(xintercept = median(documents$d_publication_date, na.rm=T),
             linetype="dotted", 
             color = "grey", size=.5)+
  labs(x="\nDocument publication years", y="Count\n") +
  theme_minimal(15)

## Dumbbell plot of fieldwork time frames

documents$d_field_date_start <- as.numeric(documents$d_field_date_start)
documents$d_field_date_end <- as.numeric(documents$d_field_date_end)

documents2 <- 
  documents %>% 
  dplyr::select(d_ID, d_culture, d_field_date_start, d_field_date_end) %>% 
  dplyr::filter(
    !is.na(d_field_date_start),
    !is.na(d_field_date_end),
    d_field_date_start > 1799
  ) %>% 
  dplyr::left_join(leader_cult[c('c_culture_code', 'region')], by = c("d_culture" = "c_culture_code")) %>% 
  mutate(
    d_ID = reorder(d_ID, d_field_date_start)
  )

#[!is.na(documents2$d_field_date_start)==T,], aes(x=d_field_date_start, xend=d_field_date_end, y=reorder(d_ID, d_field_date_start)

doc_fielddates_plot <- 
  ggplot(documents2, aes(x = d_field_date_start, xend=d_field_date_end, y = d_ID)) + 
  geom_dumbbell() +
  facet_grid(region~., scales = "free_y", space = "free_y")+ 
  scale_x_continuous(breaks = seq(1850, 2000, 10), minor_breaks = seq(1870,2000,10)) +
  labs(x="\nSpan of earliest and latest field work year for each document", y="") +
  theme_minimal(15) +
  theme(
    axis.text.y = element_blank(),
    strip.text.y = element_text(angle=0, hjust=0),
    legend.position = "none",
    strip.text = element_text(colour = 'black')
  )

# Research effort ---------------------------------------------------------

plot_pages_tr <-
  ggplot(leader_cult, aes(ehraf_pages, number_leader_records, label = Name)) + 
  geom_point() + 
  geom_text_repel(size = 3, alpha = 0.5) +
  geom_smooth(span=1) +
  labs(x = '\nTotal number of pages of ethnography in the eHRAF', y = 'Number of text records on leadership\n') +
  theme_bw(15)


# Female leaders ----------------------------------------------------------

leader_text5 <- 
  all_data %>% 
  dplyr::select(
    cs_textrec_ID,
    demo_sex,
    female_coauthor
  ) %>% 
  dplyr::mutate(
    female_leader_present = case_when(
      demo_sex == 'unknown' ~ 'unknown',
      demo_sex == 'male' ~ 'no',
      TRUE ~ 'yes'
    ),
    female_leader_present2 = female_leader_present == 'yes'
  ) %>% 
  dplyr::filter(
    demo_sex != 'unknown'
  ) %>% 
  dplyr::left_join(
    text_records[c("document_d_ID", "cs_textrec_ID")]
  ) 

# Assuming each record is independent
tab_coauthor <- xtabs(~female_coauthor + female_leader_present, leader_text5)


# High status by subsistence and region ---------------------------------------

m_status_subsistence <-
  glmer(
    qualities_HighStatus ~
      subsistence +
      region +
      (1|d_culture/author_ID),
    family = binomial,
    data = all_data,
    nAGQ = 0
  )

plot_status_subsistence <-
  hagenutils::ggemmeans(emmeans(m_status_subsistence, 'subsistence', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.7)) +
  labs(title = '', x = '', y = '')

plot_status_region <-
  hagenutils::ggemmeans(emmeans(m_status_subsistence, 'region', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.7)) +
  labs(title = '', x = '', y = '')

# High status predictors --------------------------------------------------

# Simple proportions
# x <- table(leader_text2$group.structure2, leader_text2$qualities_HighStatus)
# x <- prop.table(x, margin = 1)[,2]
# plot_status_group <- ggdotchart(x) + scale_x_continuous(limits = c(0, 0.5))

# logistic model

m_status_group2 <-
  glmer(
    qualities_HighStatus ~
      group.structure2 +
      subsistence +
      region +
      demo_sex +
      (1|d_culture/author_ID),
    family = binomial,
    data = all_data,
    nAGQ = 0
  )

all_emms <- function(m, specs, upperlimit, title){
  
  theplots = list()
  for (spec in specs){
    p <- 
      hagenutils::ggemmeans(emmeans(m, spec, type = 'response')) +
      scale_x_continuous(limits = c(0, upperlimit)) +
      labs(title = '', x = '', y = '')
    theplots <- c(theplots, list(p))
  }
  return(theplots)
}

# Elasticnet


# func_qual_vars <- c(function_vars, quality_vars)
# nonhighstatus_vars <- func_qual_vars[func_qual_vars != 'qualities_HighStatus']
# 
# y <- all_data$qualities_HighStatus
# x <- as.matrix(all_data[nonhighstatus_vars])
# 
# m_elastic_status <- cv.glmnet(x, y, family = 'binomial', alpha = 1)
# plot(m_elastic_status)
# coefs <- coef(m_elastic_status, s = m_elastic_status$lambda.1se)[-1,1]
# names(coefs) <- var_names[names(coefs)]
# 
# plot_elastic_status <- 
#   ggdotchart(exp(coefs[coefs != 0])) +
#   geom_vline(xintercept = 1, linetype = 'dotted') +
#   hagenutils::scale_color_binary() +
#   guides(colour=F, shape=F) +
#   scale_x_log10()
# plot_elastic_status

# Comparing universal vs variable vars ------------------------------------

df_compare <-
  multi_allvars %>% 
  select(var, Variable, Evidence, Unimodel, Multimodel, Drop1, AIC_diff, contains('pvalue')) %>% 
  rowwise() %>% 
  mutate(
    AIC_table = list(bbmle::AICtab(Multimodel, Unimodel, weights = T)),
  ) %>% 
  ungroup %>% 
  mutate(
    AIC_table = map(AIC_table, as.data.frame),
    Intercept_weight = map(AIC_table, function(x) x['Unimodel', 'weight']),
    Multi_weight = map(AIC_table, function(x) x['Multimodel', 'weight'])
  )

# Adjust p-values across all variables simultaneously (rather than per variable)
x <- as.matrix(df_compare[9:12])
x2 <- p.adjust(x, method = 'BH')
x3 <- matrix(x, 109, 4)

# Need to loadd these
df_cult_support <-
  bind_rows(
    functions_support_cult, 
    qualities_support_cult,
    leader_benefits_cult,
    leader_costs_cult,
    follower_benefits_cult,
    follower_costs_cult
  )

df_compare2 <-
  df_compare %>% 
  select(-Variable) %>% 
  left_join(df_cult_support, by = c('var' = 'vars')) %>%
  select(var, Variable, Evidence, Estimate, Multimodel, Drop1, AIC_diff, Intercept_weight, Multi_weight, contains('pvalue_'))

df_compare3 <-
  df_compare2 %>% 
  filter(Estimate >= 0.6 & AIC_diff < -2 & adj_pvalue_groups < 0.05)

heatmap_top_context <- var_heatmap(df_compare3, spec = 'group.structure2')

#' To do:
#' 
#' We have 20 models that show AIC improvement, but don't have
#' any one variable with low p-values. Need to run drop1 to figure
#' out which variables are making a difference, and then maybe figure
#' out how to communicate those results.

df_drop1 <-
  df_compare2 %>% 
  filter(AIC_diff < -2) %>%
  mutate(
    Drop1 = map(Drop1, ~ as_tibble(., rownames = 'Term'))
  ) %>% 
  unnest(Drop1) %>% 
  group_by(Variable) %>% 
  mutate(
    aicdiff = AIC - AIC[1]
  ) %>% 
  select(Variable, Term, aicdiff) %>% 
  spread(key = Term, value = aicdiff) %>% 
  select(
    -`<none>`, 
    `Group context` = group.structure2, 
    `Leader sex` = demo_sex, 
    Region = region, 
    Subsistence = subsistence
    )

matdrop1 <- as.matrix(df_drop1[-1])
rownames(matdrop1) <- df_drop1$Variable
heatmap_drop1 <- hagenheat(df_drop1) + theme_minimal(15)

# Pick dimensions with large delta AIC

df_drop1_thresh <-
  bind_cols(
    Variable = df_drop1$Variable,
    df_drop1[-1] > 3.5
  )

thevars <- df_drop1_thresh$Variable[df_drop1_thresh$`Leader sex`]
multi_aic_groups <- multi_aic[multi_aic$Variable %in% thevars,]
p_heatmap_sex <- var_heatmap(multi_aic_groups, 'demo_sex')

thevars <- df_drop1_thresh$Variable[df_drop1_thresh$`Subsistence`]
multi_aic_groups <- multi_aic[multi_aic$Variable %in% thevars,]
p_heatmap_subsis <- var_heatmap(multi_aic_groups, 'subsistence')

thevars <- df_drop1_thresh$Variable[df_drop1_thresh$`Group context`]
multi_aic_groups <- multi_aic[multi_aic$Variable %in% thevars,]
p_heatmap_groups <- var_heatmap(multi_aic_groups, 'group.structure2')

thevars <- df_drop1_thresh$Variable[df_drop1_thresh$`Region`]
multi_aic_groups <- multi_aic[multi_aic$Variable %in% thevars,]
p_heatmap_region <- var_heatmap(multi_aic_groups, 'region')

# Features drop1 ----------------------------------------------------------

df_features_drop1 <-
  feature_models_aic %>% 
  mutate(
    Drop1 = map(Drop1, ~ as_tibble(., rownames = 'Term'))
  ) %>% 
  unnest(Drop1) %>% 
  group_by(Variable) %>% 
  mutate(
    aicdiff = AIC - AIC[1]
  ) %>% 
  select(Variable, Term, aicdiff) %>% 
  spread(key = Term, value = aicdiff) %>% 
  select(
    -`<none>`, 
    `Group context` = group.structure2, 
    `Leader sex` = demo_sex, 
    Region = region, 
    Subsistence = subsistence
  )

# matfeaturedrop1 <- as.matrix(df_features_drop1[-1])
# rownames(matfeaturedrop1) <- df_features_drop1$Variable

heatmap_features_drop1 <- hagenheat(df_features_drop1) + theme_minimal(15)

# Pick Features with large delta AIC

df_feature_drop1_thresh <-
  bind_cols(
    Variable = df_features_drop1$Variable,
    df_features_drop1[-1] > 4
  )

thevars <- df_feature_drop1_thresh$Variable[df_feature_drop1_thresh$`Leader sex`]
feature_models_aic_groups <- feature_models_aic[feature_models_aic$Variable %in% thevars,]
heatmap_feature_sex <- var_heatmap(feature_models_aic_groups, 'demo_sex')

# thevars <- df_feature_drop1_thresh$Variable[df_feature_drop1_thresh$`Subsistence`]
# feature_models_aic_groups <- feature_models_aic[feature_models_aic$Variable %in% thevars,]
# heatmap_feature_subsis <- var_heatmap(feature_models_aic_groups, 'subsistence')

thevars <- df_feature_drop1_thresh$Variable[df_feature_drop1_thresh$`Group context`]
feature_models_aic_groups <- feature_models_aic[feature_models_aic$Variable %in% thevars,]
heatmap_feature_groups <- var_heatmap(feature_models_aic_groups, 'group.structure2')

thevars <- df_feature_drop1_thresh$Variable[df_feature_drop1_thresh$`Region`]
feature_models_aic_groups <- feature_models_aic[feature_models_aic$Variable %in% thevars,]
heatmap_feature_region <- var_heatmap(feature_models_aic_groups, 'region')


# Treemaps -----------------------------------------------------------------

# docs nested in cultures nested in subsistence types

docsum <-
  all_data %>% 
  dplyr::select(d_culture, doc_ID, subsistence, region) %>% 
  left_join(leader_cult[c('c_culture_code', 'Name')], by = c('d_culture' = 'c_culture_code')) %>% 
  group_by(subsistence, Name, doc_ID) %>% 
  tidylog::summarise(record_num = n())

plot_cult_docs_subsis <-
  ggplot(docsum, aes(area = record_num, label = Name, subgroup = subsistence, subgroup2 = Name, fill=subsistence)) + 
  geom_treemap() +
  # geom_treemap_text(colour = 'gray') +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup2_border() +
  geom_treemap_subgroup2_text(colour = 'gray') +
  # geom_treemap_subgroup_text(colour = 'white') +
  scale_fill_viridis(discrete = T)


# Author and Culture random effects plots ---------------------------------

plot_fun_auth_cult_re <- 
  ggplot(functions_support_txt, aes(authorSD, cultureSD)) + 
  geom_point() + 
  geom_text_repel(aes(label = Variable), alpha = 0.5) + 
  coord_fixed() +
  labs(title = 'Function variable random effects', x = '\nAuthor standard deviation', y = 'Culture standard deviation\n') +
  theme_bw(15)

plot_qual_auth_cult_re <- 
  ggplot(qualities_support_txt, aes(authorSD, cultureSD)) + 
  geom_point() + 
  geom_text_repel(aes(label = Variable), alpha = 0.5) + 
  coord_fixed() +
  labs(title = 'Quality variable random effects', x = '\nAuthor standard deviation', y = 'Culture standard deviation\n') +
  theme_bw(15)


# Cluster objects to feature vars -------------------------------

# Create df with feature vars as 1-d vectors: successes/(successes + failures)
df_features <- 
  all_data2 %>%
  mutate_at(
    vars(Prosociality:Competencies), function(x) apply(x, MARGIN = 1, function(x) x[1]/sum(x))
  ) %>% 
  dplyr::select(Prosociality:Competencies)

plot_feature_cor <- ggcorrplot(
  cor(df_features), 
  show.diag = F, 
  hc.order = T, 
  hc.method = 'ward.D',
  lab = T
  )

# feature_formulae <- map_chr(features, ~glue("{.} ~ subsistence + region + demo_sex + group.structure2 + (1|d_culture/doc_ID)"))
# # Note that the outcome var is a n x 2 matrix of successes vs. failures
# feature_models <- textrecord_support_multi(all_data2, features)
# 
# feature_models_aic <- 
#   feature_models %>% 
#   tidylog::filter(AIC_diff < -2)

# Summarize by culture
# df_culture_sum <-
#   all_data2 %>%
#   dplyr::select(d_culture, subsistence, Prosociality:Prestige) %>% 
#   group_by(d_culture, subsistence) %>% 
#   summarise_all(list(mean=mean, N=length)) %>% # Is there a better way to get N?
#   ungroup %>% 
#   dplyr::select(-Mediate_N:-Prestige_N) %>% # All N vectors are the same
#   tidylog::rename(N = Prosociality_N) %>% 
#   dplyr::filter(N > 2) # Eliminate cultures with few text records because means are misleading
# 
# ggcorrplot(cor(df_culture_sum[3:10]), hc.order = T, hc.method = 'ward.D', lab=T)

# heatmap(
#   t(as.matrix(df_culture_sum[3:10])), 
#   scale = 'row', # Features comprise different numbers of vars, so scale
#   hclustfun = function(x) hclust(x, method = 'ward.D'),
#   col = viridis(256)
# )

# m_feature_pca_cult <- prcomp(df_culture_sum[3:10], scale. = F)
# autoplot(
#   m_feature_pca_cult, 
#   data = df_culture_sum, 
#   colour = 'subsistence',
#   loadings = T,
#   loadings.label = T
#   )
# pca_loadings_plot(m_feature_pca_cult)

# feature_model_plot <- function(d, term){
#   ggplot(
#     d, 
#     aes_string("prob", term, xmin = "asymp.LCL", xmax = "asymp.UCL")
#   ) +
#     geom_errorbarh(height = 0, lwd = 2.5, alpha = .2) + 
#     geom_point() + 
#     scale_x_continuous(limits = c(0, NA)) +
#     facet_grid(Feature~.) + 
#     labs(x = '\nProbability', y = '') +
#     theme_bw(15) + 
#     theme(strip.text.y = element_text(angle=0))
# }
# 
# plot_feature_models_subsis <- feature_model_plot(subsis_models_sig, 'subsistence')
# 
# region_models_sig <- 
#   feature_sub_models %>% 
#   # Deliberately filtering on subsis here
#   tidylog::filter(adj_pvalue_subsis < 0.05)
# 
# plot_feature_models_region <- feature_model_plot(region_models_sig, 'region')


# logisticPCA -------------------------------------------------------------

df_lpca_func <- as_tibble(m_lpca_func$PCs)
names(df_lpca_func) <- c("PC1", "PC2")

df_cultvars <-
  all_data %>% 
  dplyr::select(cs_textrec_ID, d_culture, demo_sex:pub_dateZ) %>% 
  left_join(leader_cult[c('c_culture_code', 'c_name')], by = c('d_culture' = 'c_culture_code'))

df_func2 = 
  all_data %>% 
  dplyr::select(cs_textrec_ID, all_of(variable_names(., 'functions'))) %>% 
  dplyr::filter(rowSums(.[-1])>0) %>% 
  bind_cols(df_lpca_func) %>% 
  left_join(df_cultvars)

df_func2$Record_type <- "Mixed"
for (i in 1:nrow(df_func2)){
  if (df_func2$`Organize cooperation`[i] + df_func2$`Resolve conflict`[i] + df_func2$`Military command`[i] + df_func2$`Misc. social functions`[i] == 1){
    if (df_func2$`Organize cooperation`[i] == 1) df_func2$Record_type[i] <- 'Organize cooperation'
    if (df_func2$`Resolve conflict`[i] == 1) df_func2$Record_type[i] <- 'Resolve conflict'
    if (df_func2$`Military command`[i] == 1) df_func2$Record_type[i] <- 'Military command'
    if (df_func2$`Misc. social functions`[i] == 1) df_func2$Record_type[i] <- 'Misc. social functions'
  }
}

plot_lpca_func <-
  ggplot(df_func2, aes(PC1, PC2, colour = Record_type)) + 
  geom_point() +
  stat_ellipse() +
  # scale_colour_viridis(discrete=T) +
  theme_bw(15)

# Qualities
df_lpca_qual <- as_tibble(m_lpca_qual$PCs)
names(df_lpca_qual) <- c("PC1", "PC2")

df_qual2 = 
  all_data %>% 
  dplyr::select(cs_textrec_ID, all_of(variable_names(., 'qualities'))) %>% 
  dplyr::filter(rowSums(.[-1])>0) %>% 
  bind_cols(df_lpca_qual) %>% 
  left_join(df_cultvars)

df_qual2$`Knowledge/Experience` <- pmax(df_qual2$`Knowledgeable/intelligent`, df_qual2$`Experienced/accomplished`)
df_qual2$Record_type <- "Mixed"
for (i in 1:nrow(df_qual2)){
  if (df_qual2$`Polygynous`[i] + df_qual2$`Knowledge/Experience`[i] + df_qual2$`Aggressiveness`[i] + df_qual2$`High status`[i] == 1){
    if (df_qual2$`Polygynous`[i] == 1) df_qual2$Record_type[i] <- 'Polygynous'
    if (df_qual2$`Knowledge/Experience`[i] == 1) df_qual2$Record_type[i] <- 'Knowledge/Experience'
    if (df_qual2$`Aggressiveness`[i] == 1) df_qual2$Record_type[i] <- 'Aggressiveness'
    if (df_qual2$`High status`[i] == 1) df_qual2$Record_type[i] <- 'High status'
  }
}

plot_lpca_qual <-
  ggplot(df_qual2, aes(PC1, PC2, colour = Record_type)) + 
  geom_point() +
  stat_ellipse() +
  theme_bw(15)


plot_lpca_qual_tmp <-
  ggplot(df_qual2, aes(PC1, PC2, colour = factor(`Knowledge/Experience`))) + 
  geom_point() +
  stat_ellipse() +
  facet_wrap(~`Coercive authority`)
  theme_bw(15)
  

# Shamanism ---------------------------------------------------------------

df_shaman <-
    all_data %>%
    left_join(text_records[c('cs_textrec_ID', 'raw_text')]) %>% 
    mutate(
      shaman = str_detect(raw_text, 'shaman'),
      shamanism = as.numeric(shaman | qualities_Supernatural == 1)
    )

nonsupervars <- all_study_vars[all_study_vars != 'qualities_Supernatural']
# nonsupervars <- c(nonsupervars, 'subsistence', 'region')

y <- df_shaman$shamanism
x <- as.matrix(df_shaman[nonsupervars])
# x <- model.matrix(~.-1, df_shaman[nonsupervars]) # To dummy code subsistence and region
  
m_shamanism <- cv.glmnet(x, y, family = 'binomial', alpha = 1)
plot(m_shamanism)
coefs <- coef(m_shamanism, s = m_shamanism$lambda.1se)[-1,1]
names(coefs) <- var_names[names(coefs)]
  
plot_elastic_shamanism <-
  ggdotchart(exp(coefs[coefs != 0])) +
  geom_vline(xintercept = 1, linetype = 'dotted') +
  guides(colour=F, shape=F) +
  scale_x_log10()
  
dtm_noshaman <-
  leader_dtm %>% 
  select(-shaman, -shamanism)

plot_shamanism_text <- model_words(df_shaman, dtm_noshaman, 'shamanism', lam='lambda.1se')


# Benefits vs functions ---------------------------------------------------

# y <- leader_text4$leadertotalbenefits
# x <- as.matrix(leader_text4[c(quality_vars, function_vars)])
# 
# m <- cv.glmnet(x, y, family = 'poisson')
# plot(m)
# coefs <- coef(m, s = 'lambda.1se')
# ggdotchart(coefs[-1,1][coefs[-1,1] != 0])


# The five-fold way -------------------------------------------------------

# Leaders vs. Status vs. Provide benefits vs. Impose costs vs. Information vs. Physical

df_fivefold <-
  df_shaman %>% 
  cbind(all_data2[features]) %>% #, by = c('cs_textrec_ID' = 'doc_ID')) %>% 
  select(
    shamanism,
    qualities_HighStatus,
    # Provide benefits
    Prosociality,
    # Impose costs
    functions_Punishment,
    qualities_Aggressive,
    qualities_Feared,
    qualities_Killer,
    #Information
    qualities_KnowlageableIntellect,
    qualities_ExpAccomplished,
    # Physical
    qualities_PhysicallyStrong,
    qualities_PhysicalHealth
  ) %>% 
  transmute(
    # Shamanism = shamanism,
    Status = qualities_HighStatus,
    `Provide benefits` = Prosociality[,1]>0,
    `Impose costs` = functions_Punishment | qualities_Aggressive | qualities_Feared | qualities_Killer,
    Information = qualities_KnowlageableIntellect | qualities_ExpAccomplished,
    Physical = qualities_PhysicallyStrong | qualities_PhysicalHealth
   ) %>% 
  mutate_all(as.numeric)
