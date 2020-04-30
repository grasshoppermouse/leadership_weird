loadd(all_data)
loadd(all_study_vars)

loadd(functions_support_txt)
loadd(qualities_support_txt)
loadd(leader_benefits_txt)
loadd(leader_costs_txt)
loadd(follower_benefits_txt)
loadd(follower_costs_txt)

loadd(m_pvclust_qual)
loadd(m_pvclust_func)
loadd(functions_support_subsis_region)
loadd(qualities_support_subsis_region)
loadd(m_lpca_qual)
loadd(m_lpca_func)
loadd(df_func)
loadd(df_qual)

function_vars = variable_names(all_data, type = 'functions')
quality_vars = variable_names(all_data, type = 'qualities')
leader_benefit_vars = variable_names(all_data, type = 'leader.benefits')
leader_cost_vars = variable_names(all_data, type = 'leader.costs')
follower_benefit_vars = variable_names(all_data, type = 'follower.benefits')
follower_cost_vars = variable_names(all_data, type = 'follower.costs')

reverse_vars_dict <- names(var_names)
names(reverse_vars_dict) <- var_names

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
female_residential_pct <- signif(group_sex_tbl['female', 'residential subgroup']/sum(leader_text$demo_sex == 'female'), 3)*100
male_residential_pct <- signif(group_sex_tbl['male', 'residential subgroup']/sum(leader_text$demo_sex == 'male'), 3)*100

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
  labs(x="", y="", fill = "Group type") +
  guides(fill = guide_legend(reverse = T)) +
  theme_minimal(15) 
plot_group_subsis

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

# Total benefits

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
      group.structure2 +
      (1|d_culture/doc_ID),
    family = binomial,
    data = leader_text4,
    nAGQ = 0
  )

m_ldrtotcost <-
  glmer(
    cbind(leadertotalcosts, leadercostfailure) ~
      group.structure2 +
      subsistence + 
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

plot_emm_status_group2 <-
  hagenutils::ggemmeans(emmeans(m_status_group2, 'group.structure2', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(title = '', x = '', y = '')

plot_emm_status_subsistence2 <-
  hagenutils::ggemmeans(emmeans(m_status_group2, 'subsistence', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(title = '', x = '', y = '')

plot_emm_status_region2 <-
  hagenutils::ggemmeans(emmeans(m_status_group2, 'region', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(title = '', x = '', y = '')

plot_emm_status_sex2 <-
  hagenutils::ggemmeans(emmeans(m_status_group2, 'demo_sex', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(title = '', x = '', y = '')

# Original
# m_status_group <-
#   glmer(
#     qualities_HighStatus ~
#       group.structure2 +
#       (1|d_culture/author_ID),
#     family = binomial,
#     data = all_data
#   )
# 
# plot_emm_status_group <-
#   hagenutils::ggemmeans(emmeans(m_status_group, 'group.structure2', type = 'response')) +
#   scale_x_continuous(limits = c(0, NA)) +
#   labs(title = '', x = '', y = '')

# All vars by subsistence and region --------------------------------------

sig_func_region <-
  functions_support_subsis_region %>% 
  tidylog::filter(adj_pvalue_region < 0.05)

plot_morality_region <-
  hagenutils::ggemmeans(emmeans(sig_func_region$emmeans_region[[1]], 'region', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.3)) +
  labs(title = sig_func_region$Variable[1], x = '\nProbability', y = '')

plot_prosocial_region <-
  hagenutils::ggemmeans(emmeans(sig_func_region$emmeans_region[[2]], 'region', type = 'response')) +
  scale_x_continuous(limits = c(0, 0.3)) +
  labs(title = sig_func_region$Variable[2], x = '\nProbability', y = '')



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
  'Mediate' = func_dendro[[2]][[1]],
  'Organize' = func_dendro[[2]][[2]]
)

clust_func_vars <- map2_df(func_branches, names(func_branches), branch2df)

clust_vars <- bind_rows(clust_func_vars, clust_qual_vars)
features <- unique(clust_vars$Feature)
names(features) <- features


# Feature analysis --------------------------------------------------------

feature_var <- function(feature){
  featurevars <- clust_vars$Variable[clust_vars$Feature == feature]
  n <- length(featurevars)
  rs <- rowSums(all_data[featurevars])
  cbind(rs, n - rs) # successes, failures
}

# Add feature vars to all_data
# feature vars are n x 2 matrices; col 1: successes, col 2: failures
all_data2 <-
  all_data %>% 
  dplyr::select(demo_sex:pub_dateZ) %>% 
  bind_cols(map_dfc(features, feature_var))

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

feature_formulae <- map_chr(features, ~glue("{.} ~ subsistence + region + (1|d_culture/doc_ID)"))

# Note that the outcome var is a n x 2 matrix of successes vs. failures
feature_sub_models <-
  tibble(
    Feature = features,
    Model = map(
      feature_formulae, 
      ~ glmer(as.formula(.),
        family = binomial,
        data = all_data2,
        nAGQ = 0
      )),
    Drop1 = map(Model, drop1),
    Anova = map(Model, Anova),
    pvalues = map(Anova, 'Pr(>Chisq)'),
    pvalue_subsis = map_dbl(pvalues, 1),
    pvalue_region = map_dbl(pvalues, 2),
    adj_pvalue_subsis = p.adjust(pvalue_subsis, method = 'BH'),
    adj_pvalue_region = p.adjust(pvalue_region, method = 'BH'),
    emmeans_subsis = map(Model, ~emmeans(., spec = 'subsistence', type = "response")),
    emmeans_region = map(Model, ~emmeans(., spec = 'region', type = "response"))
  )

subsis_models_sig <- 
  feature_sub_models %>% 
  tidylog::filter(adj_pvalue_subsis < 0.05)

feature_model_plot <- function(d, term){
  ggplot(
    d, 
    aes_string("prob", term, xmin = "asymp.LCL", xmax = "asymp.UCL")
  ) +
    geom_errorbarh(height = 0, lwd = 2.5, alpha = .2) + 
    geom_point() + 
    scale_x_continuous(limits = c(0, NA)) +
    facet_grid(Feature~.) + 
    labs(x = '\nProbability', y = '') +
    theme_bw(15) + 
    theme(strip.text.y = element_text(angle=0))
}

plot_feature_models_subsis <- feature_model_plot(subsis_models_sig, 'subsistence')

region_models_sig <- 
  feature_sub_models %>% 
  # Deliberately filtering on subsis here
  tidylog::filter(adj_pvalue_subsis < 0.05)

plot_feature_models_region <- feature_model_plot(region_models_sig, 'region')


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
  ggplot(df_qual2, aes(PC1, PC2, colour = pub_dateZ > 0)) + 
  geom_point() +
  stat_ellipse() +
  theme_bw(15)
plot_lpca_qual_tmp
