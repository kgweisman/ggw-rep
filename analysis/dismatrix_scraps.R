dismatrix <- dd %>%
  mutate(
    character1 = 
      ifelse(leftCharacter == "charlie_dog" | 
               rightCharacter == "charlie_dog",
             "charlie_dog",
             ifelse(leftCharacter == "delores_gleitman_deceased" |
                      rightCharacter == "delores_gleitman_deceased",
                    "delores_gleitman_deceased",
                    ifelse(leftCharacter == "fetus" |
                             rightCharacter == "fetus",
                           "fetus",
                           ifelse(leftCharacter == "gerald_schiff_pvs" |
                                    rightCharacter == "gerald_schiff_pvs",
                                  "gerald_schiff_pvs",
                                  ifelse(leftCharacter == "god" |
                                           rightCharacter == "god",
                                         "god",
                                         ifelse(leftCharacter == "green_frog" |
                                                  rightCharacter == "green_frog",
                                                "green_frog",
                                                ifelse(leftCharacter == "kismet_robot" |
                                                         rightCharacter == "kismet_robot",
                                                       "kismet_robot",
                                                       ifelse(leftCharacter == "nicholas_gannon_baby" |
                                                                rightCharacter == "nicholas_gannon_baby",
                                                              "nicholas_gannon_baby",
                                                              ifelse(leftCharacter == "samantha_hill_girl" |
                                                                       rightCharacter == "samantha_hill_girl",
                                                                     "samantha_hill_girl",
                                                                     ifelse(leftCharacter == "sharon_harvey_woman" |
                                                                              rightCharacter == "sharon_harvey_woman",
                                                                            "sharon_harvey_woman",
                                                                            ifelse(leftCharacter == "toby_chimp" |
                                                                                     rightCharacter == "toby_chimp",
                                                                                   "toby_chimp",
                                                                                   ifelse(leftCharacter == "todd_billingsley_man" |
                                                                                            rightCharacter == "todd_billingsley_man",
                                                                                          "todd_billingsley_man",
                                                                                          ifelse(leftCharacter == "you" |
                                                                                                   rightCharacter == "you",
                                                                                                 "you",
                                                                                                 "NA")))))))))))))) %>%
  mutate(character2 = ifelse(leftCharacter == character1, as.character(rightCharacter), as.character(leftCharacter))) %>%
  select(subid, condition, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

View(dismatrix)