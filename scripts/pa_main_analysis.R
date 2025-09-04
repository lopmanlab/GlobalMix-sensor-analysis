# GlobalMix sensor analysis - Pakistan

# Load dataset ----
pa_part <- readRDS(here("Pakistan/pa_participant.RDS"))
pa_contact <- readRDS(here("Pakistan/pa_contact.RDS"))
pa_part_full <- readRDS(here("Pakistan/pa_participant_full.RDS"))
pa_sensor <- readRDS(here("Pakistan/pa_sensor.RDS"))


# Identify contact IDs -----------------------------------------------------------------------------------
## Step 1. Check NA data ----
sum(is.na(pa_part$participant_age))
sum(is.na(pa_part$participant_sex))
sum(is.na(pa_part$hh_id))
sum(is.na(pa_contact$contact_age))
sum(is.na(pa_contact$contact_sex))
sum(is.na(pa_contact$hh_id))
# No NA in age, sex, and hh_id

## Step 2. Check duplicated data ----
pa.sub.participants <- subset(pa_part, select = c("rec_id", "participant_age", "participant_sex", "hh_id"))

pa.sub2.participants <- subset(pa.sub.participants, select = c("participant_age", "participant_sex", "hh_id"))
pa.dup_participants <- pa.sub2.participants[duplicated(pa.sub2.participants), ]
# 3 duplicated participants

pa.participants.temp <- subset(pa_part, select = c("rec_id"))

# Remove the duplicates -- need to check this
pa.day1_unique <- pa_contact%>%
  filter(Day1or2 == 1)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)
pa.day2_unique <- pa_contact%>%
  filter(Day1or2 == 2)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)


## Step 3. Identify contact ID and check multiple match ----
pa.day1_complete <- pa.day1_unique%>%
  rename(participant_id = rec_id)%>%
  filter(!is.na(hh_id) & !is.na(contact_age) & !is.na(contact_sex))

# Step 3. Check multiple match
ini.t = Sys.time()

pa.hh_membership_index <- which(names(pa.day1_complete) %in% c("study_site", "survey_date", "Day1or2", "hh_member_relationship", "duration_contact"))

pa.record = c()
pa.multi_match=c()
for (i in 1:nrow(pa.day1_complete)){
  n = 0
  for (j in 1:nrow(pa.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(pa.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    pa.hh_row_to_compare <- pa.day1_complete[i, -c(1, pa.hh_membership_index)]
    
    if (all(pa.hh_row_to_compare == pa.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (pa.day1_complete[i, 1] != pa.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        pa.record = rbind(pa.record, c(pa.day1_complete[i, ], pa.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    pa.multi_match = rbind(pa.multi_match,c(pa.day1_complete[i,],n))
  }else if (n==0){
    pa.record = rbind(pa.record,c(pa.day1_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)


# Running time is about 5 seconds

### Modify the output table ----
pa.record = data.frame(pa.record)
pa.record <- as.data.frame(lapply(pa.record, unlist))%>%
  rename(contact_id = V10)

pa.multi_match = data.frame(pa.multi_match)
pa.multi_match <- as.data.frame(lapply(pa.multi_match, unlist))%>%
  rename(num_matches = V10)

### Day 2 ----
pa.day2_complete <- pa.day2_unique%>%
  rename(participant_id = rec_id)%>%
  filter(!is.na(hh_id) & !is.na(contact_age) & !is.na(contact_sex))

ini.t = Sys.time()

pa.hh_membership_index <- which(names(pa.day2_complete) %in% c("study_site", "survey_date", "Day1or2", "hh_member_relationship", "duration_contact"))

pa.record2 = c()
pa.multi_match2=c()
for (i in 1:nrow(pa.day2_complete)){
  n = 0
  for (j in 1:nrow(pa.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(pa.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    pa.hh_row_to_compare <- pa.day2_complete[i, -c(1, pa.hh_membership_index)]
    
    if (all(pa.hh_row_to_compare == pa.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (pa.day2_complete[i, 1] != pa.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        pa.record2 = rbind(pa.record2, c(pa.day2_complete[i, ], pa.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    pa.multi_match2 = rbind(pa.multi_match2,c(pa.day2_complete[i,],n))
  }else if (n==0){
    pa.record2 = rbind(pa.record2,c(pa.day2_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)

# Running time is about 5 seconds


### Modify the output table ----
pa.record2 = data.frame(pa.record2)
pa.record2 <- as.data.frame(lapply(pa.record2, unlist))%>%
  rename(contact_id = V10)

pa.multi_match2 = data.frame(pa.multi_match2)
pa.multi_match2 <- as.data.frame(lapply(pa.multi_match2, unlist))%>%
  rename(num_matches = V10)


## Step 4. Clean the output data ----
pa.record_identified <- pa.record[!is.na(pa.record$contact_id), ]
pa.record2_identified <- pa.record2[!is.na(pa.record2$contact_id), ]

pa.validation <- pa.multi_match %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(pa.multi_match, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

pa.validation2 <- pa.multi_match2 %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(pa.multi_match2, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

# Remove the duplicates
pa.record_day1 <- pa.record_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)
pa.record_day2 <- pa.record2_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)

# Remove the observations with multiple match but no multiple observations in the dataset
pa.record_day1_comp <- anti_join(pa.record_day1, pa.validation, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))
pa.record_day2_comp <- anti_join(pa.record_day2, pa.validation2, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))

# Combined day 1 and day 2
pa.record_complete <- rbind(pa.record_day1_comp, pa.record_day2_comp)

## 319/483 (66% identified)
## 335/483 (69% identified) - 7/30/2025

# Table 1 --------------------------------------------------------------------------------------------
## Compare the characteristics of participants in full and analytic dataset

## Filter to only unique contact
pa.diary <- pa.record_complete %>%
  distinct(participant_id, contact_id, .keep_all = T)%>%
  mutate(participant_id.diary = participant_id, contact_id.diary = contact_id)

## participant age ----
pa_part_full %>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

pa.diary%>%
  distinct(participant_id)%>%
  left_join(pa_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_age)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## participant sex ----
pa_part_full %>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

pa.diary%>%
  distinct(participant_id)%>%
  left_join(pa_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_sex)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## Number of household ----
pa_part_full%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

pa.diary%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

## Median (IQR) household size ----
pa_part_full%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

pa.diary%>%
  left_join(pa_part_full %>%select(rec_id, hh_size), by = c("participant_id" = "rec_id"))%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

## Proportion of rural population ----
### Pakistan is only for urban data so not applicable

# Figure 1 ----------------------------------------------------------------------------------------
## 1A ---------------------------------------------------------------------------------------------
### Edit the diary dataset so that the count of rows will be only unique contacts ----
pa.unique.d <- pa.record_complete%>%
  distinct(participant_id, contact_id)%>%
  group_by(participant_id)%>%
  summarise(d_num_con = n())

### Filter out the participants in sensor to match diary data participants
pa_sensor <- pa_sensor%>%
  filter(participant_id %in% pa.record_complete$participant_id)
  
pa.unique.s <- pa_sensor%>%
  group_by(participant_id)%>%
  summarise(sen_num_con = n())

pa.unique.con <- pa.unique.s%>%
  full_join(pa.unique.d, by = "participant_id")%>%
  mutate(d_num_con = replace_na(d_num_con, 0),
         sen_num_con = replace_na(sen_num_con, 0))%>%
  rename(Diary = d_num_con,
         Sensor = sen_num_con)

pa.unique.con.long <- pa.unique.con %>%
  pivot_longer(cols = c(Diary, Sensor), names_to = "Method", values_to = "contacts")

### Calculate summary statistics ----
pa.con.sum <- pa.unique.con.long %>%
  group_by(Method) %>%
  summarise(
    mean_contacts = mean(contacts),
    sd_contacts = sd(contacts),
    total_contacts = sum(contacts),
    n = n()
  )%>%
  mutate(se = sd_contacts / sqrt(n),
         lower = mean_contacts - 1.96 * se,
         upper = mean_contacts + 1.96 * se)

### Calculate p-value for paired t-test for mean number of contacts comparison ----
t_result <- t.test(pa.unique.con$Diary, pa.unique.con$Sensor, paired = T)
p_value <- t_result$p.value


### Create the plot ----------------------------
pa_num_plot <- ggplot(pa.con.sum, aes(x = Method, y = mean_contacts, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, show.legend = F) +
  geom_errorbar(aes(ymin = mean_contacts - sd_contacts, ymax = mean_contacts + sd_contacts), 
                position = position_dodge(width = 0.9), width = 0.25) +
  annotate("text", x = 1.5, y = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 1.1, 
           label = paste0("p = ", format(p_value, digits = 2)), vjust = 0) +
  annotate("segment", x = 1, xend = 2, y = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 1, 
           yend = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 1, xend = 1, y = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 0.5, 
           yend = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 2, xend = 2, y = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 0.5, 
           yend = max(pa.con.sum$mean_contacts + pa.con.sum$sd_contacts) + 1) +
  labs(title = "Pakistan",
       x = "Method", y = "Mean Number of Contacts") +
  geom_text(aes(label = paste0("N = ", total_contacts), y = -0.3), 
            position = position_dodge(width = 0.9), vjust = 1) +
  ylim(-0.3, 6) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, vjust = 1.5))

## 1B --------------------------------------------------------------------------------------------
pa.pair.s <- paste(pa_sensor$participant_id, pa_sensor$contact_id, sep = "-")
pa.pair.d <- paste(pa.record_complete$participant_id, pa.record_complete$contact_id, sep = "-")

pa_sensor_set <- unique(pa.pair.s)
pa_diary_set <- unique(pa.pair.d)

## Run the function for venn diagram first
pa_venn <- create_proportional_venn(pa_diary_set, pa_sensor_set)

# Figure 2 ---------------------------------------------------------------------------------------
pa.record_complete.p <- pa.record_complete%>%
  left_join(pa_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

pa_sensor.p <- pa_sensor%>%
  left_join(pa_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

pa.method.comp <- pa.record_complete.p%>%
  full_join(pa_sensor.p, by = "participant_contact_id")%>%
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

ggplot(data = pa.method.comp, aes(x = participant_age, fill = method)) +
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
  labs(x = "Participant age", y = "Proportion", title = "Pakistan", fill = "Method") -> pa.method.plot

ggplot(data = pa.method.comp, aes(x = participant_sex, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant sex", y = "Proportion", fill = "Method") -> pa.method.plot.sex


# Figure 3 ---------------------------------------------------------------------------------------------
## Combine the diary and sensor dataset for duration comparison ----
pa.sensor.dur <- pa_sensor %>%
  mutate(participant_id.sensor = participant_id, contact_id.sensor = contact_id)%>%
  left_join(pa_part, by = c("contact_id" = "rec_id"))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  mutate(hh_id = case_when(!is.na(hh_id.x) ~ hh_id.x,
                           !is.na(hh_id.y) ~ hh_id.y))%>%
  select(-hh_id.x, -hh_id.y)

pa.merged.ds <- full_join(pa.diary, pa.sensor.dur, by = c("participant_id", "contact_id"))%>%
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

pa.merged.ds <- pa.merged.ds %>%
  mutate(source = case_when(
    !is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "both",
    !is.na(participant_id.diary) & is.na(participant_id.sensor) ~ "only_diary",
    is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "only_sensor"
  ))

pa.dur.contact <- pa.merged.ds%>%
  mutate(duration_sensor = case_when(duration < 600 ~ "<5mins", #times 2 because sensor data is over two days
                                     duration >= 600 & duration <= 1800 ~ "5-15mins",
                                     duration > 1800 & duration <= 3600 ~ "16-30mins",
                                     duration > 3600 & duration <= 7200 ~ "31mins-1 hr",
                                     duration > 7200 & duration <= 28800 ~ "1-4 hrs",
                                     duration > 28800 ~ ">4 hrs"),
         duration_sensor = factor(duration_sensor, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))

## For duration, we want to compare only those that had records in both measurement
pa.dur.contact.common <- pa.dur.contact%>%
  filter(source == "both")

table(pa.dur.contact$source, pa.dur.contact$duration_contact)
table(pa.dur.contact$source, pa.dur.contact$duration_sensor)
# Cross tabulation
table(pa.dur.contact.common$duration_contact, pa.dur.contact.common$duration_sensor, useNA = "a")

### Plot proportional bar graph ----
pa.dur_count <- pa.dur.contact.common%>%
  group_by(duration_contact)%>%
  summarise(count = n())%>%
  rename(duration = duration_contact)%>%
  mutate(source = "Diary")
pa.dur_count_s <- pa.dur.contact.common%>%
  group_by(duration_sensor)%>%
  summarise(count = n())%>%
  rename(duration = duration_sensor)%>%
  mutate(source = "Sensor")

pa.dur_comb <- rbind(pa.dur_count, pa.dur_count_s)%>%
  filter(!is.na(duration))%>%
  mutate(duration = factor(duration, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))%>%
  filter(!is.na(duration))

pa_dur_fig <- ggplot(data = pa.dur_comb, aes(fill = duration, y = count, x = source))+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("<5mins" = "#fca3b7", "5-15mins" = "#e55b3c", "16-30mins" = "#f89c3a",  
                               "31mins-1 hr" = "#9fd89a", "1-4 hrs" = "#6ec6f1",">4 hrs" = "#187bcd")) +
  labs(title = "Pakistan", x = "Dataset", y = "Proportion", fill = "Contact Duration")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20),
        panel.background = element_rect(color = "white"))

# Supplemental Figure 2 --------
## Household member relationship ----
pa_cont_sub <- pa.record_complete%>%
  left_join(pa_part_full %>%select(rec_id, index_child), by = c("participant_id" = "rec_id"))%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id, "index_child")

pa.infer_relationship <- function(data) {
  data %>%
    group_by(hh_id) %>%
    mutate(
      # Identify index child (assuming it's the only one with "Self" or known ID)
      index_child_id = contact_id[index_child == 1][1],
      
      # Get roles of participant and contact relative to index child
      participant_role = hh_member_relationship[match(participant_id, contact_id)],
      contact_role = hh_member_relationship[match(contact_id, contact_id)],
      
      # Infer relationship to participant
      
      inferred_relationship = case_when(
        # contact_id == index_child_id & participant_role == "Parent"~ "Child",
        # contact_id == index_child_id & participant_role == "Sibling" ~ "Sibling",
        # participant_id == index_child_id & contact_role == "Parent" ~ "Parents",
        # participant_id == index_child_id & contact_role == "Sibling" ~ "Sibling",
        index_child == 1 & participant_role == "Parent"~ "Child",
        index_child == 1 & participant_role == "Sibling" ~ "Sibling",
        index_child == 1 & contact_role == "Parent" ~ "Parents",
        index_child == 1 & contact_role == "Sibling" ~ "Sibling",
        #participant_role == "Self" ~ contact_role,
        participant_role == "Parent" & contact_role == "Parent" ~ "Spouse",
        participant_role == "Parent" & contact_role == "Sibling" ~ "Child",
        participant_role == "Sibling" & contact_role == "Parent" ~ "Parents",
        participant_role == "Sibling" & contact_role == "Sibling" ~ "Sibling",
        TRUE ~ NA
      )
    ) %>%
    ungroup()
}


pa_cont_relationship <- pa.infer_relationship(pa_cont_sub)%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))

# For sensor
pa_cont_sub_unique <- pa.record_complete%>%
  left_join(pa_part_full %>%select(rec_id, index_child), by = c("participant_id" = "rec_id"))%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id, index_child)%>%
  distinct()

pa_sensor_rel <- pa_sensor%>%
  left_join(pa_cont_sub_unique, by = c("participant_id", "contact_id", "hh_id"))

pa_sensor_relationship <- pa.infer_relationship(pa_sensor_rel)%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, inferred_relationship, hh_id, participant_contact_id)


# Figure
pa.rel.method.comp <- pa_cont_relationship%>%
  full_join(pa_sensor_relationship, by = "participant_contact_id")%>%
  mutate(method = case_when(!is.na(participant_id.x) & !is.na(participant_id.y) ~ "Both",
                            !is.na(participant_id.x) & is.na(participant_id.y) ~ "Diary only",
                            is.na(participant_id.x) & !is.na(participant_id.y) ~ "Sensor only"),
         inferred_relationship = case_when(is.na(inferred_relationship.x) ~ inferred_relationship.y,
                                           TRUE ~ inferred_relationship.x),
         inferred_relationship = factor(inferred_relationship, levels = c("Parents", "Spouse", "Child", "Sibling", "Other")))

rel.levels = c("Parents", "Spouse", "Child", "Sibling", "Other", NA)

ggplot(data = pa.rel.method.comp, aes(x = inferred_relationship, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rel.levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  labs(title = "Pakistan", x = "Contact's relationship to participant", y = "Proportion", fill = "Method") -> pa.method.plot.rel



# Logistic regression ----------------------------------------------------------------------------------------
## Make the measurements binary variable
pa.part.ext <- pa_part_full%>%
  select(rec_id, participant_age, participant_sex)

## Model
pa.method.bin <- pa.rel.method.comp%>%
  mutate(participant_id = case_when(!is.na(participant_id.x) ~ participant_id.x,
                                    !is.na(participant_id.y) ~ participant_id.y,
                                    TRUE ~ NA),
         contact_id = case_when(!is.na(contact_id.x) ~ contact_id.x,
                                !is.na(contact_id.y) ~ contact_id.y,
                                TRUE ~ NA),
         hh_id = case_when(!is.na(hh_id.x) ~ hh_id.x,
                           !is.na(hh_id.y) ~ hh_id.y,
                           TRUE ~ NA))%>%
  select(participant_id, contact_id, hh_id, participant_contact_id, inferred_relationship, method)%>%
  left_join(pa.part.ext, by = c("contact_id" = "rec_id"))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  left_join(pa.part.ext, by = c("participant_id" = "rec_id"))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     participant_age == "5-9y" ~ "5-19y",
                                     participant_age == "10-14y" ~ "5-19y",
                                     participant_age == "15-19y" ~ "5-19y",
                                     participant_age == "20-29y" ~ "20-29y",
                                     participant_age == "30-39y" ~ "30+y",
                                     participant_age == "40-59y" ~ "30+y",
                                     participant_age == "60+y" ~ "30+y",
                                     TRUE ~ NA),
         contact_age = case_when(contact_age == "<6mo" ~ "<5y",
                                 contact_age == "6-11mo" ~ "<5y",
                                 contact_age == "1-4y" ~ "<5y",
                                 contact_age == "5-9y" ~ "5-19y",
                                 contact_age == "10-14y" ~ "5-19y",
                                 contact_age == "15-19y" ~ "5-19y",
                                 contact_age == "20-29y" ~ "20-29y",
                                 contact_age == "30-39y" ~ "30+y",
                                 contact_age == "40-59y" ~ "30+y",
                                 contact_age == "60+y" ~ "30+y",
                                 TRUE ~ NA),
         participant_age = factor(participant_age, levels = c("<5y", "5-19y", "20-29y", "30+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-19y", "20-29y", "30+y")))%>%
  mutate(diary_bin = case_when(method == "Both" ~ 0,
                               method == "Diary only" ~ 1,
                               method == "Sensor only" ~ 0,
                               TRUE ~ NA),
         sensor_bin = case_when(method == "Both" ~ 0,
                                method == "Diary only" ~ 0,
                                method == "Sensor only" ~ 1,
                                TRUE ~ NA))

pa.log.diary.model <- glm(diary_bin ~ participant_age + participant_sex + contact_age + contact_sex + inferred_relationship,
                          data = pa.method.bin,
                          family = binomial)

pa.log.sensor.model <- glm(sensor_bin ~ participant_age + participant_sex + contact_age + contact_sex + inferred_relationship,
                           data = pa.method.bin,
                           family = binomial)

summary(pa.log.diary.model)
exp(coef(pa.log.diary.model))
exp(confint(pa.log.diary.model))

summary(pa.log.sensor.model)
exp(coef(pa.log.sensor.model))
exp(confint(pa.log.sensor.model))
