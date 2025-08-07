# GlobalMix sensor analysis - Guatemala
## Machi Shiiba

# Load dataset ----
gt_part <- readRDS(here("Guatemala/gt_participant.RDS"))
gt_contact <- readRDS(here("Guatemala/gt_contact.RDS"))
gt_part_full <- readRDS(here("Guatemala/gt_participant_full.RDS"))
gt_cont_full <- readRDS(here("Guatemala/gt_contact_full.RDS"))
gt_sensor <- readRDS(here("Guatemala/gt_sensor.RDS"))


# Identify contact IDs -----------------------------------------------------------------------------------
## Step 1. Check NA data ----
sum(is.na(gt_part$participant_age))
sum(is.na(gt_part$participant_sex))
sum(is.na(gt_part$hh_id))
sum(is.na(gt_contact$contact_age))
sum(is.na(gt_contact$contact_sex))
sum(is.na(gt_contact$hh_id))
## No NA in hh_id, age, and sex

## Step 2. Check duplicated data ----
gt.sub.participants <- subset(gt_part, select = c("rec_id", "hh_id", "participant_age", "participant_sex"))

# n_dup_contact <- nrow(unique(sub.contacts[duplicated(sub.contacts), ]))
# dup_contacts <- sub.contacts[duplicated(sub.contacts),]
gt.sub2.participants <- subset(gt.sub.participants, select = c("hh_id", "participant_age", "participant_sex"))
gt.dup_participants <- gt.sub2.participants[duplicated(gt.sub2.participants), ]
# 3 duplicated participants

gt.participants.temp <- subset(gt_part, select = c("rec_id"))


# Remove the duplicates -- need to check this
gt.day1_unique <- gt_contact%>%
  filter(Day1or2 == 1)%>%
  distinct(rec_id, hh_member_id, hh_id, contact_age, contact_sex, .keep_all = T)
gt.day2_unique <- gt_contact%>%
  filter(Day1or2 == 2)%>%
  distinct(rec_id, hh_member_id, hh_id, contact_age, contact_sex, .keep_all = T)

## Step 3. Identify contact ID and check multiple match ----
### Day 1 ----
gt.day1_complete <- gt.day1_unique%>%
  rename(participant_id = rec_id)


ini.t = Sys.time()

gt.hh_membership_index <- which(names(gt.day1_complete) %in% c("participant_id", "study_site", "survey_date", "hh_member_id", "duration_contact","Day1or2", "hh_member_relationship"))

gt.record = c()
gt.multi_match=c()
for (i in 1:nrow(gt.day1_complete)){
  n = 0
  for (j in 1:nrow(gt.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(gt.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    gt.hh_row_to_compare <- gt.day1_complete[i, -c(1, gt.hh_membership_index)]
    
    if (all(gt.hh_row_to_compare == gt.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (gt.day1_complete[i, 1] != gt.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        gt.record = rbind(gt.record, c(gt.day1_complete[i, ], gt.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    gt.multi_match = rbind(gt.multi_match,c(gt.day1_complete[i,],n))
  }else if (n==0){
    gt.record = rbind(gt.record,c(gt.day1_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)


# Running time is about 2 seconds

### Modify the output table ----
gt.record = data.frame(gt.record)
gt.record <- as.data.frame(lapply(gt.record, unlist))%>%
  rename(contact_id = rec_id)

gt.multi_match = data.frame(gt.multi_match)
gt.multi_match <- as.data.frame(lapply(gt.multi_match, unlist))%>%
  rename(num_matches = V11)

### Day 2 ----
gt.day2_complete <- gt.day2_unique%>%
  rename(participant_id = rec_id)

ini.t = Sys.time()

gt.hh_membership_index <- which(names(gt.day2_complete) %in% c("participant_id", "study_site", "survey_date", "hh_member_id", "duration_contact","Day1or2", "hh_member_relationship"))

gt.record2 = c()
gt.multi_match2=c()
for (i in 1:nrow(gt.day2_complete)){
  n = 0
  for (j in 1:nrow(gt.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(gt.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    gt.hh_row_to_compare <- gt.day2_complete[i, -c(1, gt.hh_membership_index)]
    
    if (all(gt.hh_row_to_compare == gt.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (gt.day2_complete[i, 1] != gt.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        gt.record2 = rbind(gt.record2, c(gt.day2_complete[i, ], gt.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    gt.multi_match2 = rbind(gt.multi_match2,c(gt.day2_complete[i,],n))
  }else if (n==0){
    gt.record2 = rbind(gt.record2,c(gt.day2_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)

# Running time is about 2 seconds


### Modify the output table ----
gt.record2 = data.frame(gt.record2)
gt.record2 <- as.data.frame(lapply(gt.record2, unlist))%>%
  rename(contact_id = rec_id)

gt.multi_match2 = data.frame(gt.multi_match2)
gt.multi_match2 <- as.data.frame(lapply(gt.multi_match2, unlist))%>%
  rename(num_matches = V11)


## Step 4. Clean the output data ----
gt.record_identified <- gt.record[!is.na(gt.record$contact_id), ]
gt.record2_identified <- gt.record2[!is.na(gt.record2$contact_id), ]

gt.validation <- gt.multi_match %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(gt.multi_match, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

gt.validation2 <- gt.multi_match2 %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(gt.multi_match2, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

# Remove the duplicates
gt.record_day1 <- gt.record_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)
gt.record_day2 <- gt.record2_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)

# Remove the observations with multiple match but no multiple observations in the dataset
gt.record_day1_comp <- anti_join(gt.record_day1, gt.validation, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))
gt.record_day2_comp <- anti_join(gt.record_day2, gt.validation2, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))

# Combined day 1 and day 2
gt.record_complete <- rbind(gt.record_day1_comp, gt.record_day2_comp)

## 200/234 identified (85%)

# Table 1 --------------------------------------------------------------------------------------------
## Compare the characteristics of participants in full and analytic dataset

## Filter to only unique contact
gt.diary <- gt.record_complete %>%
  distinct(participant_id, contact_id, .keep_all = T)%>%
  mutate(participant_id.diary = participant_id, contact_id.diary = contact_id)

## participant age ----
gt_part_full %>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

gt.diary%>%
  distinct(participant_id)%>%
  left_join(gt_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_age)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## participant sex ----
gt_part_full %>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

gt.diary%>%
  distinct(participant_id)%>%
  left_join(gt_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_sex)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## Number of household ----
gt_part_full%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

gt.diary%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

## Median (IQR) household size ----
gt_part_full%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

gt.diary%>%
  left_join(gt_part_full %>%select(rec_id, hh_size), by = c("participant_id" = "rec_id"))%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

## Proportion of rural population ----
gt_part_full %>%
  group_by(study_site) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

gt.diary%>%
  distinct(participant_id)%>%
  left_join(gt_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(study_site)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)


# Figure 1 ----------------------------------------------------------------------------------------
## 1A ---------------------------------------------------------------------------------------------
### Edit the diary dataset so that the count of rows will be only unique contacts ----
gt.unique.d <- gt.record_complete%>%
  distinct(participant_id, contact_id)%>%
  group_by(participant_id)%>%
  summarise(d_num_con = n())

### Filter out the participants in sensor to match diary data participants
gt_sensor <- gt_sensor%>%
  filter(participant_id %in% gt.record_complete$participant_id)

gt.unique.s <- gt_sensor%>%
  group_by(participant_id)%>%
  summarise(sen_num_con = n())

gt.unique.con <- gt.unique.s%>%
  full_join(gt.unique.d, by = "participant_id")%>%
  mutate(d_num_con = replace_na(d_num_con, 0),
         sen_num_con = replace_na(sen_num_con, 0))%>%
  rename(Diary = d_num_con,
         Sensor = sen_num_con)

gt.unique.con.long <- gt.unique.con %>%
  pivot_longer(cols = c(Diary, Sensor), names_to = "Method", values_to = "contacts")

### Calculate summary statistics ----
gt.con.sum <- gt.unique.con.long %>%
  group_by(Method) %>%
  summarise(
    mean_contacts = mean(contacts),
    sd_contacts = sd(contacts),
    total_contacts = sum(contacts)
  )

### Calculate p-value for paired t-test for mean number of contacts comparison ----
t_result <- t.test(gt.unique.con$Diary, gt.unique.con$Sensor, paired = T)
p_value <- t_result$p.value


### Create the plot ----------------------------
gt_num_plot <- ggplot(gt.con.sum, aes(x = Method, y = mean_contacts, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, show.legend = F) +
  geom_errorbar(aes(ymin = mean_contacts - sd_contacts, ymax = mean_contacts + sd_contacts), 
                position = position_dodge(width = 0.9), width = 0.25) +
  annotate("text", x = 1.5, y = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 1.1, 
           label = paste0("p = ", format(p_value, digits = 2)), vjust = 0) +
  annotate("segment", x = 1, xend = 2, y = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 1, 
           yend = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 1, xend = 1, y = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 0.5, 
           yend = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 2, xend = 2, y = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 0.5, 
           yend = max(gt.con.sum$mean_contacts + gt.con.sum$sd_contacts) + 1) +
  labs(title = "Guatemala",
       x = "Method", y = "Mean Number of Contacts") +
  geom_text(aes(label = paste0("N = ", total_contacts), y = -0.3), 
            position = position_dodge(width = 0.9), vjust = 1) +
  ylim(-0.3, 6) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, vjust = 1.5))

## 1B --------------------------------------------------------------------------------------------
gt.pair.s <- paste(gt_sensor$participant_id, gt_sensor$contact_id, sep = "-")
gt.pair.d <- paste(gt.record_complete$participant_id, gt.record_complete$contact_id, sep = "-")

gt_sensor_set <- unique(gt.pair.s)
gt_diary_set <- unique(gt.pair.d)

## Run the function for venn diagram first
gt_venn <- create_proportional_venn(gt_diary_set, gt_sensor_set)

# Figure 2 ---------------------------------------------------------------------------------------
gt.record_complete.p <- gt.record_complete%>%
  left_join(gt_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

gt_sensor.p <- gt_sensor%>%
  left_join(gt_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

gt.method.comp <- gt.record_complete.p%>%
  full_join(gt_sensor.p, by = "participant_contact_id")%>%
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

ggplot(data = gt.method.comp, aes(x = participant_age, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = age_levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant age", y = "Proportion", title = "Guatemala", fill = "Method") -> gt.method.plot

ggplot(data = gt.method.comp, aes(x = participant_sex, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant sex", y = "Proportion", fill = "Method") -> gt.method.plot.sex


ggplot(data = gt.method.comp, aes(x = participant_sex, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant sex", y = "Proportion", fill = "Method")


# Figure 3 ---------------------------------------------------------------------------------------
## Combine the diary and sensor dataset for duration comparison ----
gt.sensor.dur <- gt_sensor %>%
  mutate(participant_id.sensor = participant_id, contact_id.sensor = contact_id)%>%
  left_join(gt_part, by = c("contact_id" = "rec_id"))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)

gt.merged.ds <- full_join(gt.diary, gt.sensor.dur, by = c("participant_id", "contact_id"))%>%
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

gt.merged.ds <- gt.merged.ds %>%
  mutate(source = case_when(
    !is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "both",
    !is.na(participant_id.diary) & is.na(participant_id.sensor) ~ "only_diary",
    is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "only_sensor"
  ))

gt.dur.contact <- gt.merged.ds%>%
  mutate(duration_sensor = case_when(duration < 600 ~ "<5mins", #times 2 because sensor data is over two days
                                     duration >= 600 & duration <= 1800 ~ "5-15mins",
                                     duration > 1800 & duration <= 3600 ~ "16-30mins",
                                     duration > 3600 & duration <= 7200 ~ "31mins-1 hr",
                                     duration > 7200 & duration <= 28800 ~ "1-4 hrs",
                                     duration > 28800 ~ ">4 hrs"),
         duration_sensor = factor(duration_sensor, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))

## For duration, we want to compare only those that had records in both measurement
gt.dur.contact.common <- gt.dur.contact%>%
  filter(source == "both")

table(gt.dur.contact$source, gt.dur.contact$duration_contact)
table(gt.dur.contact$source, gt.dur.contact$duration_sensor)
# Cross tabulation
table(gt.dur.contact.common$duration_contact, gt.dur.contact.common$duration_sensor, useNA = "a")

### Plot proportional bar graph ----
gt.dur_count <- gt.dur.contact.common%>%
  group_by(duration_contact)%>%
  summarise(count = n())%>%
  rename(duration = duration_contact)%>%
  mutate(source = "Diary")
gt.dur_count_s <- gt.dur.contact.common%>%
  group_by(duration_sensor)%>%
  summarise(count = n())%>%
  rename(duration = duration_sensor)%>%
  mutate(source = "Sensor")

gt.dur_comb <- rbind(gt.dur_count, gt.dur_count_s)%>%
  filter(!is.na(duration))

gt_dur_fig <- ggplot(data = gt.dur_comb, aes(fill = duration, y = count, x = source))+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("<5mins" = "#fca3b7", "5-15mins" = "#e55b3c", "16-30mins" = "#f89c3a",  
                               "31mins-1 hr" = "#9fd89a", "1-4 hrs" = "#6ec6f1",">4 hrs" = "#187bcd")) +
  labs(title = "Guatemala", x = "Dataset", y = "Proportion", fill = "Contact Duration")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20),
        panel.background = element_rect(color = "white"))

# Supplemental Figure 2 ----
## Household member relationship ----
gt_cont_sub <- gt.record_complete%>%
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
        rel_from == "Spouse" & rel_to == "Child" ~ "Child",
        rel_from == "Child" & rel_to == "Spouse" ~ "Parents",
        rel_from == "Child" & rel_to == "Child" ~ "Sibling",
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
        relationship_to_head == "Parents" ~ "Child",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(inferred_relationship)) %>%
    select(hh_id, from, to, inferred_relationship)
  
  # Combine both
  bind_rows(inferred_between_contacts, inferred_to_head)
}



# Apply the function to each household
gt_cont_rel_id <- gt_cont_sub %>%
  group_split(hh_id) %>%
  map_dfr(infer_from_head)%>%
  distinct(from, to, hh_id, inferred_relationship)

gt_cont_relationship <- gt_cont_sub%>%
  left_join(gt_cont_rel_id, by = c("participant_id" = "from", "contact_id" = "to", "hh_id"))%>%
  mutate(inferred_relationship = case_when(is.na(hh_member_relationship) ~ inferred_relationship,
                                           TRUE ~ hh_member_relationship))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))

# For sensor
gt_cont_sub_unique <- gt.record_complete%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id)%>%
  distinct()

gt_sensor_rel <- gt_sensor%>%
  left_join(gt_cont_sub_unique, by = c("participant_id", "contact_id", "hh_id"))

gt_sensor_rel_id <- gt_sensor_rel %>%
  group_split(hh_id) %>%
  map_dfr(infer_from_head)%>%
  distinct(from, to, hh_id, inferred_relationship)

gt_sensor_relationship <- gt_sensor_rel%>%
  left_join(gt_sensor_rel_id, by = c("participant_id" = "from", "contact_id" = "to", "hh_id"))%>%
  mutate(inferred_relationship = case_when(is.na(hh_member_relationship) ~ inferred_relationship,
                                           TRUE ~ hh_member_relationship))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, inferred_relationship, hh_id, participant_contact_id)


# Figure
gt.rel.method.comp <- gt_cont_relationship%>%
  full_join(gt_sensor_relationship, by = "participant_contact_id")%>%
  mutate(method = case_when(!is.na(participant_id.x) & !is.na(participant_id.y) ~ "Both",
                            !is.na(participant_id.x) & is.na(participant_id.y) ~ "Diary only",
                            is.na(participant_id.x) & !is.na(participant_id.y) ~ "Sensor only"),
         inferred_relationship = case_when(is.na(inferred_relationship.x) ~ inferred_relationship.y,
                                           TRUE ~ inferred_relationship.x),
         inferred_relationship = factor(inferred_relationship, levels = c("Parents", "Spouse", "Child", "Sibling", "Other")))

rel.levels = c("Parents", "Spouse", "Child", "Sibling", "Other", NA)

ggplot(data = gt.rel.method.comp, aes(x = inferred_relationship, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rel.levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  labs(title = "Guatemala", x = "Contact's relationship to participant", y = "Proportion", fill = "Method") -> gt.method.plot.rel
