# GlobalMix sensor analysis - Mozambique
## Machi Shiiba

# Load dataset ----
mo_part <- readRDS(here("Mozambique/mo_participant.RDS"))
mo_part_full <- readRDS(here("Mozambique/mo_participant_full.RDS"))
mo_contact <- readRDS(here("Mozambique/mo_contact.RDS"))
mo_cont_full <- readRDS(here("Mozambique/mo_contact_full.RDS"))
mo_sensor <- readRDS(here("Mozambique/mo_sensor.RDS"))

# Identify contact IDs ------------------------------------------------------------------------------------------
## Step 1. Check NA data ----
sum(is.na(mo_part$hh_id))
sum(is.na(mo_part$participant_age))
sum(is.na(mo_part$participant_sex))
# No NA in hh_id, age, and sex

## Step 2. Check duplicated data ----
mo.sub.participants <- subset(mo_part, select = c("rec_id", "hh_id", "participant_age", "participant_sex"))

# n_dup_contact <- nrow(unique(sub.contacts[duplicated(sub.contacts), ]))
# dup_contacts <- sub.contacts[duplicated(sub.contacts),]
mo.sub2.participants <- subset(mo.sub.participants, select = c("hh_id", "participant_age", "participant_sex"))
mo.dup_participants <- mo.sub2.participants[duplicated(mo.sub2.participants), ]
# 3 duplicated participants

mo.participants.temp <- subset(mo_part, select = c("rec_id"))


# Remove the duplicates -- need to check this
mo.day1_unique <- mo_contact%>%
  filter(Day1or2 == 1)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)
mo.day2_unique <- mo_contact%>%
  filter(Day1or2 == 2)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)

# There is no day 2

## Step 3. Identify contact ID and check multiple match ----
mo.day1_complete <- mo.day1_unique%>%
  rename(participant_id = rec_id)%>%
  filter(!is.na(hh_id) & !is.na(contact_age) & !is.na(contact_sex))

# Step 3. Check multiple match
ini.t = Sys.time()

mo.hh_membership_index <- which(names(mo.day1_complete) %in% c("participant_id", "study_site", "survey_date", "duration_contact","frequency_contact","Day1or2", "hh_member_relationship"))

mo.record = c()
mo.multi_match=c()
for (i in 1:nrow(mo.day1_complete)){
  n = 0
  for (j in 1:nrow(mo.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(mo.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    mo.hh_row_to_compare <- mo.day1_complete[i, -c(1, mo.hh_membership_index)]
    
    if (all(mo.hh_row_to_compare == mo.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (mo.day1_complete[i, 1] != mo.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        mo.record = rbind(mo.record, c(mo.day1_complete[i, ], mo.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    mo.multi_match = rbind(mo.multi_match,c(mo.day1_complete[i,],n))
  }else if (n==0){
    mo.record = rbind(mo.record,c(mo.day1_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)


# Running time is about 15 seconds


### Modify the output table ----
mo.record = data.frame(mo.record)
mo.record <- as.data.frame(lapply(mo.record, unlist))%>%
  rename(contact_id = V10)

mo.multi_match = data.frame(mo.multi_match)
mo.multi_match <- as.data.frame(lapply(mo.multi_match, unlist))%>%
  rename(num_matches = V10)

## Step 4. Clean the output data ----
mo.record_identified <- mo.record[!is.na(mo.record$contact_id), ]

mo.validation <- mo.multi_match %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(mo.multi_match, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

# Remove the duplicates
mo.record_day1 <- mo.record_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)

# Remove the observations with multiple match but no multiple observations in the dataset
mo.record_complete <- anti_join(mo.record_day1, mo.validation, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))

# 226/350 identified (65%)

# Table 1 ----
## Filter to unique contact
mo.diary <- mo.record_complete %>%
  filter(!is.na(contact_id))%>%
  distinct(participant_id, contact_id, .keep_all = T)%>%
  mutate(participant_id.diary = participant_id, contact_id.diary = contact_id)

## participant age ----
mo_part_full %>%
  group_by(#study_site,
    participant_age) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

mo.diary%>%
  distinct(participant_id)%>%
  left_join(mo_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_age)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## participant sex ----
mo_part_full %>%
  group_by(#study_site,
    participant_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

mo.diary%>%
  distinct(participant_id)%>%
  left_join(mo_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_sex)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Number of household
mo_part_full%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

mo.diary%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

# Median (IQR) household size
mo_part_full%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

mo.diary%>%
  left_join(mo_part_full%>%select(rec_id, hh_size), by = c("participant_id" = "rec_id"))%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))



# rural population
mo_part_full %>%
  group_by(study_site) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

mo.diary%>%
  distinct(participant_id)%>%
  left_join(mo_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(study_site)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Figure 1 -----------------------------------------------------------------------------------------
## 1A ----------------------------------------------------------------------------------------------
### Edit the diary dataset so that the count of rows will be only unique contacts ----
mo.unique.d <- mo.record_complete%>%
  distinct(participant_id, contact_id)%>%
  group_by(participant_id)%>%
  summarise(d_num_con = n())

### Filter out the participants in sensor to match diary data participants
mo_sensor <- mo_sensor%>%
  filter(participant_id %in% mo.record_complete$participant_id)

mo.unique.s <- mo_sensor%>%
  group_by(participant_id)%>%
  summarise(sen_num_con = n())

mo.unique.con <- mo.unique.s%>%
  full_join(mo.unique.d, by = "participant_id")%>%
  mutate(d_num_con = replace_na(d_num_con, 0),
         sen_num_con = replace_na(sen_num_con, 0))%>%
  rename(Diary = d_num_con,
         Sensor = sen_num_con)

mo.unique.con.long <- mo.unique.con %>%
  pivot_longer(cols = c(Diary, Sensor), names_to = "Method", values_to = "contacts")

### Calculate summary statistics ----
mo.con.sum <- mo.unique.con.long %>%
  group_by(Method) %>%
  summarise(
    mean_contacts = mean(contacts),
    sd_contacts = sd(contacts),
    total_contacts = sum(contacts)
  )

### Calculate p-value for paired t-test for mean number of contacts comparison ----
t_result <- t.test(mo.unique.con$Diary, mo.unique.con$Sensor, paired = T)
p_value <- t_result$p.value


### Create the plot ----
mo_num_plot <- ggplot(mo.con.sum, aes(x = Method, y = mean_contacts, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, show.legend = F) +
  geom_errorbar(aes(ymin = mean_contacts - sd_contacts, ymax = mean_contacts + sd_contacts), 
                position = position_dodge(width = 0.9), width = 0.25) +
  annotate("text", x = 1.5, y = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 1.1, 
           label = paste0("p = ", format(p_value, scientific = F, digits = 2)), vjust = 0) +
  annotate("segment", x = 1, xend = 2, y = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 1, 
           yend = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 1, xend = 1, y = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 0.5, 
           yend = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 2, xend = 2, y = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 0.5, 
           yend = max(mo.con.sum$mean_contacts + mo.con.sum$sd_contacts) + 1) +
  geom_text(aes(label = paste0("N = ", total_contacts), y = -0.3), 
            position = position_dodge(width = 0.9), vjust = 1) +
  labs(title = "Mozambique",
       x = "Method", y = "Mean Number of Contacts") +
  ylim(-0.3, 6) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, vjust = 1.5))

## 1B ---------------------------------------------------------------------------------------------------
mo.pair.s <- paste(mo_sensor$participant_id, mo_sensor$contact_id, sep = "-")
mo.pair.d <- paste(mo.record_complete$participant_id, mo.record_complete$contact_id, sep = "-")

mo_sensor_set <- unique(mo.pair.s)
mo_diary_set <- unique(mo.pair.d)

## Run the function for venn diagram first
mo_venn <- create_proportional_venn(mo_diary_set, mo_sensor_set)

# Figure 2 ----------------------------------------------------------------------------------------------
mo.record_complete.p <- mo.record_complete%>%
  left_join(mo_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

mo_sensor.p <- mo_sensor%>%
  left_join(mo_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

mo.method.comp <- mo.record_complete.p%>%
  full_join(mo_sensor.p, by = "participant_contact_id")%>%
  mutate(method = case_when(!is.na(participant_id.x) & !is.na(participant_id.y) ~ "Both",
                            !is.na(participant_id.x) & is.na(participant_id.y) ~ "Diary only",
                            is.na(participant_id.x) & !is.na(participant_id.y) ~ "Sensor only"),
         participant_age = case_when(is.na(participant_age.x) ~ participant_age.y,
                                     TRUE ~ participant_age.x),
         participant_age = factor(participant_age, levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-14y", "15-19y",
                                                              "20-29y", "30-39y", "40-59y", "60+y")),
         participant_sex = case_when(is.na(participant_sex.x) ~ participant_sex.y,
                                     TRUE ~ participant_sex.x))


age_levels <- c("<6mo", "6-11mo", "1-4y", "5-9y", "10-14y", "15-19y",
                "20-29y", "30-39y", "40-59y", "60+y")

ggplot(data = mo.method.comp, aes(x = participant_age, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = age_levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant age", y = "Proportion", title = "Mozambique", fill = "Method") -> mo.method.plot

ggplot(data = mo.method.comp, aes(x = participant_sex, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant sex", y = "Proportion", fill = "Method") -> mo.method.plot.sex

# Figure 3 ---------------------------------------------------------------------------------------------
## Combine the diary and sensor dataset for duration comparison ----
mo.sensor.dur <- mo_sensor %>%
  mutate(participant_id.sensor = participant_id, contact_id.sensor = contact_id)%>%
  left_join(mo_part, by = c("contact_id" = "rec_id"))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  mutate(hh_id = case_when(!is.na(hh_id.x) ~ hh_id.x,
                    !is.na(hh_id.y) ~ hh_id.y))%>%
  select(-hh_id.x, -hh_id.y)

mo.merged.ds <- full_join(mo.diary, mo.sensor.dur, by = c("participant_id", "contact_id"))%>%
  mutate(contact_age = case_when(!is.na(contact_age.x) ~ contact_age.x,
                                 !is.na(contact_age.y) ~ contact_age.y),
         contact_sex = case_when(!is.na(contact_sex.x) ~ contact_sex.x,
                                 !is.na(contact_sex.y) ~ contact_sex.y),
         hh_id = case_when(!is.na(hh_id.x) ~ hh_id.x,
                           !is.na(hh_id.y) ~ hh_id.y),
         study_site = case_when(!is.na(study_site.x) ~ study_site.x,
                                !is.na(study_site.y) ~ study_site.y))%>%
  select(-c(contact_age.x, contact_age.y, contact_sex.x, contact_sex.y,
            hh_id.x, hh_id.y, study_site.x, study_site.y))

mo.merged.ds <- mo.merged.ds %>%
  mutate(source = case_when(
    !is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "both",
    !is.na(participant_id.diary) & is.na(participant_id.sensor) ~ "only_diary",
    is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "only_sensor"
  ))

mo.dur.contact <- mo.merged.ds%>%
  mutate(duration_sensor = case_when(duration < 600 ~ "<5mins", #times 2 because sensor data is over two days
                                     duration >= 600 & duration <= 1800 ~ "5-15mins",
                                     duration > 1800 & duration <= 3600 ~ "16-30mins",
                                     duration > 3600 & duration <= 7200 ~ "31mins-1 hr",
                                     duration > 7200 & duration <= 28800 ~ "1-4 hrs",
                                     duration > 28800 ~ ">4 hrs"),
         duration_sensor = factor(duration_sensor, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))

## For duration, we want to compare only those that had records in both measurement
mo.dur.contact.common <- mo.dur.contact%>%
  filter(source == "both")

table(mo.dur.contact$source, mo.dur.contact$duration_contact)
table(mo.dur.contact$source, mo.dur.contact$duration_sensor)
# Cross tabulation
table(mo.dur.contact.common$duration_contact, mo.dur.contact.common$duration_sensor, useNA = "a")

### Plot proportional bar graph ----
mo.dur_count <- mo.dur.contact.common%>%
  group_by(duration_contact)%>%
  summarise(count = n())%>%
  rename(duration = duration_contact)%>%
  mutate(source = "Diary")
mo.dur_count_s <- mo.dur.contact.common%>%
  group_by(duration_sensor)%>%
  summarise(count = n())%>%
  rename(duration = duration_sensor)%>%
  mutate(source = "Sensor")

mo.dur_comb <- rbind(mo.dur_count, mo.dur_count_s)%>%
  filter(!is.na(duration))%>%
  mutate(duration = factor(duration, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))

mo_dur_fig <- ggplot(data = mo.dur_comb, aes(fill = duration, y = count, x = source))+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("<5mins" = "#fca3b7", "5-15mins" = "#e55b3c", "16-30mins" = "#f89c3a",  
                               "31mins-1 hr" = "#9fd89a", "1-4 hrs" = "#6ec6f1",">4 hrs" = "#187bcd")) +
  labs(title = "Mozambique", x = "Dataset", y = "Proportion", fill = "Contact Duration")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20),
        panel.background = element_rect(color = "white"))

# Supplemental Figure 2 ------
## Household member relationship ----
mo_cont_sub <- mo.record_complete%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id)

# Function to infer relationships within a household
infer_from_head <- function(hh_data) {
  # Identify the head of household
  head_row <- hh_data %>% filter(!is.na(hh_member_relationship))
  if (nrow(head_row) == 0) return(NULL)
  
  head_id <- head_row$participant_id[1]
  
  # Get all relationships reported by the head
  rels <- hh_data %>%
    filter(participant_id == head_id) %>%
    select(hh_id, contact_id, relationship_to_head = hh_member_relationship)
  
  # Step 1: Infer relationships between contacts of the head
  inferred_between_contacts <- rels %>%
    rename(from = contact_id, rel_from = relationship_to_head) %>%
    inner_join(rels %>% rename(to = contact_id, rel_to = relationship_to_head), by = "hh_id") %>%
    filter(from != to) %>%
    mutate(
      inferred_relationship = case_when(
        rel_from == "Spouse" & rel_to == "Child" ~ "Parents",
        rel_from == "Spouse" & rel_to == "Sibling" ~ "Other relative",
        rel_from == "Spouse" & rel_to == "Nephew" ~ "Niece/Nephew",
        rel_from == "Spouse" & rel_to == "Grandchild" ~ "Grandchild",
        rel_from == "Spouse" & rel_to == "Other relative" ~ "Other relative",
        rel_from == "Child" & rel_to == "Spouse" ~ "Parents",
        rel_from == "Child" & rel_to == "Child" ~ "Sibling",
        rel_from == "Child" & rel_to == "Sibling" ~ "Uncle/aunt",
        rel_from == "Child" & rel_to == "Nephew" ~ "Other relative",
        rel_from == "Child" & rel_to == "Grandchild" ~ "Child",
        rel_from == "Child" & rel_to == "Other relative" ~ "Other relative",
        rel_from == "Sibling" & rel_to == "Spouse" ~ "Other relative",
        rel_from == "Sibling" & rel_to == "Child" ~ "Niece/Nephew",
        rel_from == "Sibling" & rel_to == "Sibling" ~ "Sibling",
        rel_from == "Sibling" & rel_to == "Nephew" ~ NA_character_, # could be child or nephew
        rel_from == "Sibling" & rel_to == "Grandchild" ~ "Parents",
        rel_from == "Sibling" & rel_to == "Other relative" ~ "Other relative",
        rel_from == "Nephew" & rel_to == "Spouse" ~ "Uncle/aunt",
        rel_from == "Nephew" & rel_to == "Child" ~ "Other relative",
        rel_from == "Nephew" & rel_to == "Sibling" ~ NA_character_, # could be uncle/aunt or parents
        rel_from == "Nephew" & rel_to == "Nephew" ~ NA_character_, # could be sibling or cousin
        rel_from == "Nephew" & rel_to == "Grandchild" ~ "Other relative",
        rel_from == "Nephew" & rel_to == "Other relative" ~ "Other relative",
        rel_from == "Grandchild" & rel_to == "Spouse" ~ "Grandparents",
        rel_from == "Grandchild" & rel_to == "Child" ~ "Parents",
        rel_from == "Grandchild" & rel_to == "Sibling" ~ "Uncle/aunt",
        rel_from == "Grandchild" & rel_to == "Nephew" ~ "Other relative", # could be child or nephew
        rel_from == "Grandchild" & rel_to == "Grandchild" ~ "Other relative",
        rel_from == "Grandchild" & rel_to == "Other relative" ~ "Other relative",
        rel_from == "Other relative" ~ "Other relative",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(inferred_relationship)) %>%
    select(hh_id, from, to, inferred_relationship)
  
  # Step 2: Infer relationships from contacts back to the head
  inferred_to_head <- rels %>%
    mutate(
      from = contact_id,
      to = head_id,
      inferred_relationship = case_when(
        relationship_to_head == "Spouse" ~ "Spouse",
        relationship_to_head == "Child" ~ "Parents",
        relationship_to_head == "Sibling" ~ "Sibling",
        relationship_to_head == "Nephew" ~ "Uncle/aunt",
        relationship_to_head == "Grandchild" ~ "Grandparents",
        relationship_to_head == "Other relative" ~ "Other relative",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(inferred_relationship)) %>%
    select(hh_id, from, to, inferred_relationship)
  
  # Combine both
  bind_rows(inferred_between_contacts, inferred_to_head)
}

mo_cont_rel_id <- mo_cont_sub %>%
  group_split(hh_id) %>%
  map_dfr(infer_from_head)%>%
  distinct(from, to, hh_id, inferred_relationship)

mo_cont_relationship <- mo_cont_sub%>%
  left_join(mo_cont_rel_id, by = c("participant_id" = "from", "contact_id" = "to", "hh_id"))%>%
  mutate(inferred_relationship = case_when(is.na(hh_member_relationship) ~ inferred_relationship,
                                           TRUE ~ hh_member_relationship))%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Nephew" ~ "Niece/Nephew",
                                           TRUE ~ inferred_relationship))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Child" ~ "Child",
                                           inferred_relationship == "Parents" ~ "Parents",
                                           inferred_relationship == "Sibling" ~ "Sibling",
                                           inferred_relationship == "Spouse" ~ "Spouse",
                                           is.na(inferred_relationship) ~ NA,
                                           TRUE ~ "Other"))


# For sensor
mo_sensor_rel <- mo_sensor%>%
  left_join(mo_cont_sub, by = c("participant_id", "contact_id", "hh_id"))

mo_sensor_rel_id <- mo_sensor_rel %>%
  group_split(hh_id) %>%
  map_dfr(infer_from_head)%>%
  distinct(from, to, hh_id, inferred_relationship)

mo_sensor_relationship <- mo_sensor_rel%>%
  left_join(mo_sensor_rel_id, by = c("participant_id" = "from", "contact_id" = "to", "hh_id"))%>%
  mutate(inferred_relationship = case_when(is.na(hh_member_relationship) ~ inferred_relationship,
                                           TRUE ~ hh_member_relationship))%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Nephew" ~ "Niece/Nephew",
                                           TRUE ~ inferred_relationship))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, inferred_relationship, hh_id, participant_contact_id)%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Child" ~ "Child",
                                           inferred_relationship == "Parents" ~ "Parents",
                                           inferred_relationship == "Sibling" ~ "Sibling",
                                           inferred_relationship == "Spouse" ~ "Spouse",
                                           is.na(inferred_relationship) ~ NA,
                                           TRUE ~ "Other"))

# Figure
mo.rel.method.comp <- mo_cont_relationship%>%
  full_join(mo_sensor_relationship, by = "participant_contact_id")%>%
  mutate(method = case_when(!is.na(participant_id.x) & !is.na(participant_id.y) ~ "Both",
                            !is.na(participant_id.x) & is.na(participant_id.y) ~ "Diary only",
                            is.na(participant_id.x) & !is.na(participant_id.y) ~ "Sensor only"),
         inferred_relationship = case_when(is.na(inferred_relationship.x) ~ inferred_relationship.y,
                                           TRUE ~ inferred_relationship.x),
         inferred_relationship = factor(inferred_relationship, levels = c("Parents", "Spouse", "Child", "Sibling", "Other")))

rel.levels = c("Parents", "Spouse", "Child", "Sibling", "Other", NA)

ggplot(data = mo.rel.method.comp, aes(x = inferred_relationship, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rel.levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  labs(title = "Mozambique", x = "Contact's relationship to participant", y = "Proportion", fill = "Method") -> mo.method.plot.rel
