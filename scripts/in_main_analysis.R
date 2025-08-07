# GlobalMix sensor analysis - India
## Machi Shiiba

# Load dataset ----
in_part <- readRDS(here("India/in_participant.RDS"))
in_contact <- readRDS(here("India/in_contact.RDS"))
in_part_full <- readRDS(here("India/in_participant_full.RDS"))
in_cont_full <- readRDS(here("India/in_contact_full.RDS"))
in_sensor <- readRDS(here("India/in_sensor.RDS"))


# Extract the observations that do not have contact ID
cont_no_id <- in_contact%>%
  filter(is.na(contact_id))%>%
  rename(rec_id = participant_id)

cont_yes_id <- in_contact%>%
  filter(!is.na(contact_id))

# Do the matching with cont_no_id data

# Identify contact IDs ----------------------------------------------------------------------------------------
## Step 1. Check NA data ----
sum(is.na(in_part$participant_age))
sum(is.na(in_part$participant_sex))
sum(is.na(in_part$hh_id))
sum(is.na(in_contact$contact_age))
sum(is.na(in_contact$contact_sex))
sum(is.na(in_contact$hh_id))
## No NA in hh_id, age, and sex

## Step 2. Check duplicated data ----
in.sub.participants <- subset(in_part, select = c("rec_id", "hh_id", "participant_age", "participant_sex"))

in.sub2.participants <- subset(in.sub.participants, select = c("hh_id", "participant_age", "participant_sex"))
in.dup_participants <- in.sub2.participants[duplicated(in.sub2.participants), ]
# 8 duplicated participants

in.participants.temp <- subset(in_part, select = c("rec_id"))


# Remove the duplicates -- need to check this
in.day1_unique <- cont_no_id%>%
  filter(Day1or2 == 1)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)%>%
  select(- contact_id)
in.day2_unique <- cont_no_id%>%
  filter(Day1or2 == 2)%>%
  distinct(rec_id, hh_id, contact_age, contact_sex, .keep_all = T)%>%
  select(- contact_id)

## Step 3. Identify contact ID and check multiple match ----
### Day 1 ----
in.day1_complete <- in.day1_unique%>%
  rename(participant_id = rec_id)

ini.t = Sys.time()

in.hh_membership_index <- which(names(in.day1_complete) %in% c("participant_id", "study_site", "survey_date", "hh_member_id", "duration_contact","Day1or2", "hh_member_relationship"))

in.record = c()
in.multi_match=c()
for (i in 1:nrow(in.day1_complete)){
  n = 0
  for (j in 1:nrow(in.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(in.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    in.hh_row_to_compare <- in.day1_complete[i, -c(1, in.hh_membership_index)]
    
    if (all(in.hh_row_to_compare == in.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (in.day1_complete[i, 1] != in.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        in.record = rbind(in.record, c(in.day1_complete[i, ], in.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    in.multi_match = rbind(in.multi_match,c(in.day1_complete[i,],n))
  }else if (n==0){
    in.record = rbind(in.record,c(in.day1_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)


# Running time is about 15 seconds

### Modify the output table ----
in.record = data.frame(in.record)
in.record <- as.data.frame(lapply(in.record, unlist))%>%
  rename(contact_id = V10)

in.multi_match = data.frame(in.multi_match)
in.multi_match <- as.data.frame(lapply(in.multi_match, unlist))%>%
  rename(num_matches = V10)

### Day 2 ----
in.day2_complete <- in.day2_unique%>%
  rename(participant_id = rec_id)

ini.t = Sys.time()

in.hh_membership_index <- which(names(in.day2_complete) %in% c("participant_id", "study_site", "survey_date", "hh_member_id", "duration_contact","Day1or2", "hh_member_relationship"))

in.record2 = c()
in.multi_match2=c()
for (i in 1:nrow(in.day2_complete)){
  n = 0
  for (j in 1:nrow(in.sub.participants)){
    # skip the ones that have NA data
    if (any(is.na(in.sub.participants[j,-1]))) next
    
    # Extract the necessary variables for comparison
    in.hh_row_to_compare <- in.day2_complete[i, -c(1, in.hh_membership_index)]
    
    if (all(in.hh_row_to_compare == in.sub.participants[j,-1])) {
      # Don't match the participant themselves as a contact
      if (in.day2_complete[i, 1] != in.sub.participants[j, 1]) {
        # Include all variables (including the additional one) in the result
        in.record2 = rbind(in.record2, c(in.day2_complete[i, ], in.sub.participants[j, 1]))
        n = n+1
      }
    }
  }
  
  if (n > 1) {
    in.multi_match2 = rbind(in.multi_match2,c(in.day2_complete[i,],n))
  }else if (n==0){
    in.record2 = rbind(in.record2,c(in.day2_complete[i,],NA))
  }
}

print(Sys.time() - ini.t)

# Running time is about 15 seconds

### Modify the output table ----
in.record2 = data.frame(in.record2)
in.record2 <- as.data.frame(lapply(in.record2, unlist))%>%
  rename(contact_id = V10)

in.multi_match2 = data.frame(in.multi_match2)
in.multi_match2 <- as.data.frame(lapply(in.multi_match2, unlist))%>%
  rename(num_matches = V10)


## Step 4. Clean the output data ----
in.record_identified <- in.record[!is.na(in.record$contact_id), ]
in.record2_identified <- in.record2[!is.na(in.record2$contact_id), ]

in.validation <- in.multi_match %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(in.multi_match, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

in.validation2 <- in.multi_match2 %>%
  group_by(participant_id, hh_id, contact_age, contact_sex) %>%
  summarise(actual_matches = n(), .groups = 'drop') %>%
  inner_join(in.multi_match2, by = c("participant_id", "hh_id", "contact_age", "contact_sex")) %>%
  filter(actual_matches != num_matches)

# Remove the duplicates
in.record_day1 <- in.record_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)
in.record_day2 <- in.record2_identified %>%
  distinct(participant_id, hh_id, contact_age, contact_sex, contact_id, .keep_all = T)

# Remove the observations with multiple match but no multiple observations in the dataset
in.record_day1_comp <- anti_join(in.record_day1, in.validation, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))
in.record_day2_comp <- anti_join(in.record_day2, in.validation2, by = c("participant_id", "hh_id", "contact_age", "contact_sex"))

# Combined day 1 and day 2, and those already identified
in.record_complete <- rbind(in.record_day1_comp, in.record_day2_comp, cont_yes_id)

# 1394/1722 identified (81%)

# Table 1 ---------------------------------------------------------------------------------------------
## Compare the characteristics of participants in full and analytic dataset

## Filter to only unique contact
in.diary <- in.record_complete %>%
  distinct(participant_id, contact_id, .keep_all = T)%>%
  mutate(participant_id.diary = participant_id, contact_id.diary = contact_id)

## participant age ----
in_part_full %>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

in.diary%>%
  distinct(participant_id)%>%
  left_join(in_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_age)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## participant sex ----
in_part_full %>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

in.diary%>%
  distinct(participant_id)%>%
  left_join(in_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(participant_sex)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

## Number of household ----
in_part_full%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

in.diary%>%
  summarise(unique_hh_id_count = n_distinct(hh_id))

## Median (IQR) household size ----
in_part_full%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

in.diary%>%
  left_join(in_part_full %>%select(rec_id, hh_size), by = c("participant_id" = "rec_id"))%>%
  summarise(median = median(hh_size, na.rm = T),
            percentile_25 = quantile(hh_size, 0.25, na.rm = TRUE),
            percentile_75 = quantile(hh_size, 0.75, na.rm = TRUE))

## Proportion of rural population ----
in_part_full %>%
  group_by(study_site) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

in.diary%>%
  distinct(participant_id)%>%
  left_join(in_part, 
            by = c("participant_id" = "rec_id"))%>%
  group_by(study_site)%>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Figure 1 ---------------------------------------------------------------------------------------------
## 1A --------------------------------------------------------------------------------------------------
### Edit the diary dataset so that the count of rows will be only unique contacts ----
in.unique.d <- in.record_complete%>%
  distinct(participant_id, contact_id)%>%
  group_by(participant_id)%>%
  summarise(d_num_con = n())

### Filter out the participants in sensor to match diary data participants
in_sensor <- in_sensor%>%
  filter(participant_id %in% in.record_complete$participant_id)

in.unique.s <- in_sensor%>%
  group_by(participant_id)%>%
  summarise(sen_num_con = n())

in.unique.con <- in.unique.s%>%
  full_join(in.unique.d, by = "participant_id")%>%
  mutate(d_num_con = replace_na(d_num_con, 0),
         sen_num_con = replace_na(sen_num_con, 0))%>%
  rename(Diary = d_num_con,
         Sensor = sen_num_con)

in.unique.con.long <- in.unique.con %>%
  pivot_longer(cols = c(Diary, Sensor), names_to = "Method", values_to = "contacts")

### Calculate summary statistics ----
in.con.sum <- in.unique.con.long %>%
  group_by(Method) %>%
  summarise(
    mean_contacts = mean(contacts),
    sd_contacts = sd(contacts),
    total_contacts = sum(contacts)
  )

### Calculate p-value for paired t-test for mean number of contacts comparison ----
t_result <- t.test(in.unique.con$Diary, in.unique.con$Sensor, paired = T)
p_value <- t_result$p.value


### Create the plot ----
in_num_plot <- ggplot(in.con.sum, aes(x = Method, y = mean_contacts, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, show.legend = F) +
  geom_errorbar(aes(ymin = mean_contacts - sd_contacts, ymax = mean_contacts + sd_contacts), 
                position = position_dodge(width = 0.9), width = 0.25) +
  annotate("text", x = 1.5, y = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 1.1, 
           label = paste0("p = ", format(p_value, digits = 2)), vjust = 0) +
  annotate("segment", x = 1, xend = 2, y = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 1, 
           yend = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 1, xend = 1, y = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 0.5, 
           yend = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 1) +
  annotate("segment", x = 2, xend = 2, y = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 0.5, 
           yend = max(in.con.sum$mean_contacts + in.con.sum$sd_contacts) + 1) +
  labs(title = "India",
       x = "Method", y = "Mean Number of Contacts") +
  geom_text(aes(label = paste0("N = ", total_contacts), y = -0.3), 
            position = position_dodge(width = 0.9), vjust = 1) +
  ylim(-0.3, 6) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, vjust = 1.5))

## 1B ---------------------------------------------------------------------------------------------------
in.pair.s <- paste(in_sensor$participant_id, in_sensor$contact_id, sep = "-")
in.pair.d <- paste(in.record_complete$participant_id, in.record_complete$contact_id, sep = "-")

in_sensor_set <- unique(in.pair.s)
in_diary_set <- unique(in.pair.d)

## Run the function for venn diagram first
in_venn <- create_proportional_venn(in_diary_set, in_sensor_set)

# Figure 2 ----------------------------------------------------------------------------------------------
in.record_complete.p <- in.record_complete%>%
  left_join(in_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

in_sensor.p <- in_sensor%>%
  left_join(in_part,  by = c("participant_id" = "rec_id"))%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, participant_contact_id, participant_age, participant_sex)

in.method.comp <- in.record_complete.p%>%
  full_join(in_sensor.p, by = "participant_contact_id")%>%
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

ggplot(data = in.method.comp, aes(x = participant_age, fill = method)) +
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
  labs(x = "Participant age", y = "Proportion", title = "India", fill = "Method") -> in.method.plot

ggplot(data = in.method.comp, aes(x = participant_sex, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20))+
  labs(x = "Participant sex", y = "Proportion", fill = "Method") -> in.method.plot.sex

# Figure 3 --------------------------------------------------------------------------------------------
## Combine the diary and sensor dataset for duration comparison ----
in.sensor.dur <- in_sensor %>%
  mutate(participant_id.sensor = participant_id, contact_id.sensor = contact_id)%>%
  left_join(in_part, by = c("contact_id" = "rec_id"))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)

in.merged.ds <- full_join(in.diary, in.sensor.dur, by = c("participant_id", "contact_id"))%>%
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

in.merged.ds <- in.merged.ds %>%
  mutate(source = case_when(
    !is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "both",
    !is.na(participant_id.diary) & is.na(participant_id.sensor) ~ "only_diary",
    is.na(participant_id.diary) & !is.na(participant_id.sensor) ~ "only_sensor"
  ))

in.dur.contact <- in.merged.ds%>%
  mutate(duration_sensor = case_when(duration < 600 ~ "<5mins", #times 2 because sensor data is over two days
                                     duration >= 600 & duration <= 1800 ~ "5-15mins",
                                     duration > 1800 & duration <= 3600 ~ "16-30mins",
                                     duration > 3600 & duration <= 7200 ~ "31mins-1 hr",
                                     duration > 7200 & duration <= 28800 ~ "1-4 hrs",
                                     duration > 28800 ~ ">4 hrs"),
         duration_sensor = factor(duration_sensor, levels = c("<5mins", "5-15mins", "16-30mins","31mins-1 hr", "1-4 hrs", ">4 hrs")))

## For duration, we want to compare only those that had records in both measurement
in.dur.contact.common <- in.dur.contact%>%
  filter(source == "both")

table(in.dur.contact$source, in.dur.contact$duration_contact)
table(in.dur.contact$source, in.dur.contact$duration_sensor)
# Cross tabulation
table(in.dur.contact.common$duration_contact, in.dur.contact.common$duration_sensor, useNA = "a")

### Plot proportional bar graph ----
in.dur_count <- in.dur.contact.common%>%
  group_by(duration_contact)%>%
  summarise(count = n())%>%
  rename(duration = duration_contact)%>%
  mutate(source = "Diary")
in.dur_count_s <- in.dur.contact.common%>%
  group_by(duration_sensor)%>%
  summarise(count = n())%>%
  rename(duration = duration_sensor)%>%
  mutate(source = "Sensor")

in.dur_comb <- rbind(in.dur_count, in.dur_count_s)%>%
  filter(!is.na(duration))

in_dur_fig <- ggplot(data = in.dur_comb, aes(fill = duration, y = count, x = source))+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("<5mins" = "#fca3b7", "5-15mins" = "#e55b3c", "16-30mins" = "#f89c3a",  
                               "31mins-1 hr" = "#9fd89a", "1-4 hrs" = "#6ec6f1",">4 hrs" = "#187bcd")) +
  labs(title = "India", x = "Dataset", y = "Proportion", fill = "Contact Duration")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 20),
        panel.background = element_rect(color = "white"))

# Supplemental Figure 2 ----
## Household member relationship ------------
in_cont_sub <- in.record_complete%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id)

in.infer_relationship <- function(data) {
  data %>%
    group_by(hh_id) %>%
    mutate(
      # Identify index child (assuming it's the only one with "Self" or known ID)
      index_child_id = contact_id[hh_member_relationship == "Self"][1],
      
      # Get roles of participant and contact relative to index child
      participant_role = hh_member_relationship[match(participant_id, contact_id)],
      contact_role = hh_member_relationship[match(contact_id, contact_id)],
      
      # Infer relationship to participant
      
      inferred_relationship = case_when(
        contact_id == index_child_id & participant_role == "Parents"~ "Child",
        contact_id == index_child_id & participant_role == "Grandparents" ~ "Grandchild",
        contact_id == index_child_id & participant_role == "Uncle/aunt" ~ "Niece/Nephew",
        contact_id == index_child_id & participant_role == "Sibling" ~ "Sibling",
        contact_id == index_child_id & participant_role == "Niece/Nephew" ~ "Uncle/aunt",
        participant_role == "Self" ~ contact_role,
        participant_role == "Parents" & contact_role == "Parents" ~ "Spouse",
        participant_role == "Parents" & contact_role == "Grandparents" ~ "Parents",
        participant_role == "Parents" & contact_role == "Uncle/aunt" ~ "Sibling",
        participant_role == "Parents" & contact_role == "Sibling" ~ "Child",
        participant_role == "Parents" & contact_role == "Niece/Nephew" ~ "Grandchild",
        participant_role == "Grandparents" & contact_role == "Parents" ~ "Child",
        participant_role == "Grandparents" & contact_role == "Grandparents" ~ "Grandchild",
        participant_role == "Grandparents" & contact_role == "Uncle/aunt" ~ "Child",
        participant_role == "Grandparents" & contact_role == "Sibling" ~ "Grandchild",
        participant_role == "Grandparents" & contact_role == "Niece/Nephew" ~ "Other relative",
        participant_role == "Uncle/aunt" & contact_role == "Parents" ~ "Sibling",
        participant_role == "Uncle/aunt" & contact_role == "Grandparents" ~ "Parents",
        participant_role == "Uncle/aunt" & contact_role == "Uncle/aunt" ~ NA, # could be sibling or spouse
        participant_role == "Uncle/aunt" & contact_role == "Sibling" ~ "Niece/Nephew",
        participant_role == "Uncle/aunt" & contact_role == "Niece/Nephew" ~ "Other relative",
        participant_role == "Sibling" & contact_role == "Parents" ~ "Parents",
        participant_role == "Sibling" & contact_role == "Grandparents" ~ "Grandparents",
        participant_role == "Sibling" & contact_role == "Uncle/aunt" ~ "Uncle/aunt",
        participant_role == "Sibling" & contact_role == "Sibling" ~ "Sibling",
        participant_role == "Sibling" & contact_role == "Niece/Nephew" ~ "Uncle/aunt",
        participant_role == "Niece/Nephew" & contact_role == "Parents" ~ "Grandparents",
        participant_role == "Niece/Nephew" & contact_role == "Grandparents" ~ "Other relative",
        participant_role == "Niece/Nephew" & contact_role == "Uncle/aunt" ~ "Other relative",
        participant_role == "Niece/Nephew" & contact_role == "Sibling" ~ NA, # could be uncle/aunt or parents
        participant_role == "Niece/Nephew" & contact_role == "Niece/Nephew" ~ NA, # could be sibling or cousin
        participant_role == "Other relative" | contact_role == "Other relative" ~ "Other relative",
        TRUE ~ NA
      )
    ) %>%
    ungroup()
}


in_cont_relationship <- in.infer_relationship(in_cont_sub)%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Child" ~ "Child",
                                           inferred_relationship == "Parents" ~ "Parents",
                                           inferred_relationship == "Sibling" ~ "Sibling",
                                           inferred_relationship == "Spouse" ~ "Spouse",
                                           is.na(inferred_relationship) ~ NA,
                                           TRUE ~ "Other"))

# For sensor
in_cont_sub_unique <- in.record_complete%>%
  select(participant_id, contact_id, hh_member_relationship, hh_id)%>%
  distinct()

in_sensor_rel <- in_sensor%>%
  left_join(in_cont_sub_unique, by = c("participant_id", "contact_id", "hh_id"))

in_sensor_relationship <- in.infer_relationship(in_sensor_rel)%>%
  mutate(participant_contact_id = paste(participant_id, contact_id, sep = "-"))%>%
  select(participant_id, contact_id, inferred_relationship, hh_id, participant_contact_id)%>%
  mutate(inferred_relationship = case_when(inferred_relationship == "Child" ~ "Child",
                                           inferred_relationship == "Parents" ~ "Parents",
                                           inferred_relationship == "Sibling" ~ "Sibling",
                                           inferred_relationship == "Spouse" ~ "Spouse",
                                           is.na(inferred_relationship) ~ NA,
                                           TRUE ~ "Other"))

# Figure
in.rel.method.comp <- in_cont_relationship%>%
  full_join(in_sensor_relationship, by = "participant_contact_id")%>%
  mutate(method = case_when(!is.na(participant_id.x) & !is.na(participant_id.y) ~ "Both",
                            !is.na(participant_id.x) & is.na(participant_id.y) ~ "Diary only",
                            is.na(participant_id.x) & !is.na(participant_id.y) ~ "Sensor only"),
         inferred_relationship = case_when(is.na(inferred_relationship.x) ~ inferred_relationship.y,
                                           TRUE ~ inferred_relationship.x),
         inferred_relationship = factor(inferred_relationship, levels = c("Parents", "Spouse", "Child", "Sibling", "Other")))

rel.levels = c("Parents", "Spouse", "Child", "Sibling", "Other", NA)

ggplot(data = in.rel.method.comp, aes(x = inferred_relationship, fill = method)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rel.levels, drop = FALSE) +
  scale_fill_manual(values = c("Both" = "tan1", "Diary only" =  "palegreen2", "Sensor only" = "dodgerblue")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  labs(title = "India",x = "Contact's relationship to participant", y = "Proportion", fill = "Method") -> in.method.plot.rel
