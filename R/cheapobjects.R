loadd(all_data)
loadd(all_study_vars)

function_vars = variable_names(all_data, type = 'functions')
quality_vars = variable_names(all_data, type = 'qualities')
leader_benefit_vars = variable_names(all_data, type = 'leader.benefits')
leader_cost_vars = variable_names(all_data, type = 'leader.costs')
follower_benefit_vars = variable_names(all_data, type = 'follower.benefits')
follower_cost_vars = variable_names(all_data, type = 'follower.costs')

# Compute values -----------------------------------------------------------

group_sex_tbl <- xtabs(~demo_sex+group.structure2, all_data)
female_residential_pct <- signif(group_sex_tbl['female', 'residential subgroup']/sum(leader_text$demo_sex == 'female'), 3)*100
male_residential_pct <- signif(group_sex_tbl['male', 'residential subgroup']/sum(leader_text$demo_sex == 'male'), 3)*100

group_sub_tbl <- xtabs(~ subsistence + group.structure2, all_data)
hg_residential_pct <- signif(group_sub_tbl['hunter gatherers', 'residential subgroup']/sum(leader_text$subsistence == 'hunter gatherers'), 3)*100
hg_kin_pct <- signif(group_sub_tbl['hunter gatherers', 'kin group']/sum(leader_text$subsistence == 'hunter gatherers'), 3)*100
hort_kin_pct <- signif(group_sub_tbl['horticulturalists', 'kin group']/sum(leader_text$subsistence == 'horticulturalists'), 3)*100

final_record_count <- sum(rowSums(all_data[all_study_vars])>0)

male_leader_pct <- signif(100*sum(all_data$demo_sex=='male', na.rm=T)/nrow(all_data), 3)
female_leader_pct <- signif(100*sum(all_data$demo_sex=='female', na.rm=T)/nrow(all_data), 2)

intelltxts <- sum(all_data$qualities_KnowlageableIntellect)
polytxts <- sum(all_data$qualities_Polygynous)
statustxts <- sum(all_data$qualities_HighStatus)
intellpolytxts <- sum(all_data$qualities_Polygynous & all_data$qualities_KnowlageableIntellect)
statuspolytxts <- sum(all_data$qualities_Polygynous & all_data$qualities_HighStatus)

# text analysis
# leader_text has 1000 rows
# need raw texts for all 1212 rows

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

# Group structure by subsistence --------------------------------------------------

# High status by subsistence groups

# Simple proportions
# x <- table(leader_text2$group.structure2, leader_text2$qualities_HighStatus)
# x <- prop.table(x, margin = 1)[,2]
# plot_status_group <- ggdotchart(x) + scale_x_continuous(limits = c(0, 0.5))

# logistic model

m_status_group <-
  glmer(
    qualities_HighStatus ~
      group.structure2 +
      (1|d_culture/author_ID),
    family = binomial,
    data = all_data
  )

emm_status_group <-
  emmeans(m_status_group, 'group.structure2', type = 'response') %>% 
  summary %>% 
  mutate(
    group.structure2 = fct_reorder(group.structure2, prob)
  )

plot_emm_status_group <-
  ggplot(emm_status_group, aes(prob, group.structure2, xmin = asymp.LCL, xmax = asymp.UCL)) +
  geom_errorbarh(height = 0, lwd=2.5, alpha = 0.3) +
  geom_point() +
  scale_x_continuous(limits = c(0, 0.7)) +
  labs(title="Proportion of evidence that leaders are high status", x = '\nProportion', y = "") +
  theme_minimal(15)

# Treemaps -----------------------------------------------------------------

# docs nested in cultures nested in subsistence types

docsum <-
  all_data %>% 
  dplyr::select(d_culture, doc_ID, subsistence, region) %>% 
  left_join(leader_cult[c('c_culture_code', 'Name')], by = c('d_culture' = 'c_culture_code')) %>% 
  group_by(subsistence, Name, doc_ID) %>% 
  summarise(record_num = n())

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
