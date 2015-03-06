# compute pairwise dissimilarities
dismatrix <- dd %>%
  select(subid, condition, leftCharacter, rightCharacter, responseNum) %>%
  group_by(rightCharacter, leftCharacter) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(rightCharacter, mean)

View(dismatrix)