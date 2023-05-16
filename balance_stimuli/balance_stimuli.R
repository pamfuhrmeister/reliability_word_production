# balance lists of stimuli

setwd("~/Dropbox/Postdoc_projects/SFB/Reliability_paper/")

stimuli <- read.csv("English_MultiPic_CSV.csv", header = TRUE, sep = ";")

stimuli$H_INDEX <- gsub(",", ".", stimuli$H_INDEX)
stimuli$H_INDEX <- as.numeric(stimuli$H_INDEX)
stimuli$PERCENTAGE_MODAL_NAME <- gsub(",", ".", stimuli$PERCENTAGE_MODAL_NAME)
stimuli$PERCENTAGE_MODAL_NAME <- as.numeric(stimuli$PERCENTAGE_MODAL_NAME)
stimuli <- stimuli %>%
  filter(PICTURE != "PICTURE_214") %>%
  filter(H_INDEX < .52) %>%
  select(PICTURE, NAME1, H_INDEX)

colnames(stimuli)[2] <- "Word"

stimuli <- stimuli %>%
  arrange(Word) %>%
  mutate(duplicate = ifelse(Word == lag(Word), "Yes", "No")) %>%
  mutate(duplicate = ifelse(is.na(duplicate) == TRUE, "No", duplicate)) %>%
  filter(duplicate == "No") %>%
  select(-duplicate)

freq <- read.csv("SUBTLEX-UK.csv", header = TRUE, sep = ";")
freq$LogFreq.Zipf. <- gsub(",", ".", freq$LogFreq.Zipf.)
freq$LogFreq.Zipf. <- as.numeric(freq$LogFreq.Zipf.)
colnames(freq) <- c("Word", "frequency")

aoa <- read.csv("AoA.csv", header = TRUE, sep = ";")
aoa$Rating.Mean <- gsub(",", ".", aoa$Rating.Mean)
aoa$Rating.Mean <- as.numeric(aoa$Rating.Mean)

stimuli_temp <- merge(stimuli, freq, by = "Word")
stimuli_final <- merge(stimuli_temp, aoa, by = "Word")

# z-score the values
stimuli_final[3:5] <- lapply(stimuli_final[3:5], scale)

stimuli_final <- stimuli_final %>%
  mutate(word_pic = paste(Word, PICTURE, sep = "_"))

pairings <- data.frame(t(combn(stimuli_final$word_pic, 2)))
colnames(pairings) <- c("Stim_1", "Stim_2")
pairings$cosine <- NA

for(i in 1:nrow(pairings)){
  vec0 <- pairings[i,]
  new <- subset(stimuli_final, word_pic %in% vec0)
  vec_1 <- as.numeric(new[1,3:5])
  vec_2 <- as.numeric(new[2,3:5])
  pairings[i,]$cosine <- lsa::cosine(vec_1, vec_2)
  print(i)
}

# balance lists

pairings <- pairings %>%
  mutate(pair_id = paste(Stim_1, Stim_2, sep = "_")) %>%
  mutate(pair_id2 = paste(Stim_1, Stim_2, sep = "_"))
stim_all <- as.character(stimuli_final$word_pic)

set.seed(2)
sets <- list()
y <- 1
repeat {
  samp <- sample(length(stim_all), 155)
  
  a <- stim_all[samp]
  b <- stim_all[-samp]
  
  ab <- data.frame(a,b)
  ab <- ab %>%
    mutate(pair_id = paste(a, b, sep = "_")) %>%
    mutate(pair_id2 = paste(b, a, sep = "_"))
  ab_pair <- merge(ab, pairings, by = "pair_id")
  ab_pair2 <- merge(ab, pairings, by = "pair_id2")
  ab_all <- bind_rows(ab_pair, ab_pair2) %>%
    select(a, b, cosine)
  
  sets <- append(sets, list(ab_all))

  print(y)
  y = y+1
  if (y == 1000001){
    break
  }
}


# calculate mean of cosine similarity for each list
mean_cosine_sim <- map(sets, function(x){mean(x$cosine)})

mean_cosine_sim_sorted <- map_df(mean_cosine_sim, ~as.data.frame(.x), .id="id")
colnames(mean_cosine_sim_sorted)[2] <- "cosine_sim"

# extract the list with the highest cosine similarity
extract_highest <- mean_cosine_sim_sorted %>%
  arrange(desc(cosine_sim))

stimuli_to_use <- sets[[as.numeric(extract_highest[1,1])]]

stimuli_to_use <- stimuli_to_use %>%
  separate(col = a, into = c("List_a", "Picture_a", "Number_a"),
           sep = "_", remove = TRUE) %>%
  separate(col = b, into = c("List_b", "Picture_b", "Number_b"),
           sep = "_", remove = TRUE) %>%
  mutate(Picture_Num_a = paste(Picture_a, Number_a, sep = "_")) %>%
  mutate(Picture_Num_b = paste(Picture_b, Number_b, sep = "_")) %>%
  select(-c(Picture_a, Number_a, Picture_b, Number_b))
  
write.csv(stimuli_to_use, "stimuli_lists.csv")

stim_lists <- read.csv("stimuli_lists.csv", header = TRUE, sep = ",")
stim_lists <- stim_lists[-1]
stim_lists_a <- stim_lists
colnames(stim_lists_a)[4] <- "PICTURE"  
stim_lists_a <- merge(stim_lists_a, stimuli, by = "PICTURE")
stim_lists_a <- merge(stim_lists_a, freq, by = "Word")
stim_lists_a <- merge(stim_lists_a, aoa, by = "Word")


list_1_mean <- data.frame(lapply(stim_lists_a[7:9], mean))
colnames(list_1_mean) <- c("H-index mean", "Frequency mean", "AoA mean")
list_1_sd <- data.frame(lapply(stim_lists_a[7:9], sd))
colnames(list_1_sd) <- c("H-index SD", "Frequency SD", "AoA SD")
stimuli_paper_table <- cbind(list_1_mean, list_1_sd)


stim_lists_b <- stim_lists
colnames(stim_lists_b)[5] <- "PICTURE"  
stim_lists_b <- merge(stim_lists_b, stimuli, by = "PICTURE")
stim_lists_b <- merge(stim_lists_b, freq, by = "Word")
stim_lists_b <- merge(stim_lists_b, aoa, by = "Word")

list_2_mean <- data.frame(lapply(stim_lists_b[7:9], mean))
colnames(list_2_mean) <- c("H-index mean", "Frequency mean", "AoA mean")
list_2_sd <- data.frame(lapply(stim_lists_b[7:9], sd))
colnames(list_2_sd) <- c("H-index SD", "Frequency SD", "AoA SD")
stimuli_paper_table_temp <- cbind(list_2_mean, list_2_sd)

stimuli_paper_table <- rbind(stimuli_paper_table, stimuli_paper_table_temp)
stimuli_paper_table <- stimuli_paper_table[,c(2,5,3,6,1,4)]

stimuli_paper_table <- rowid_to_column(stimuli_paper_table, var = "List")

