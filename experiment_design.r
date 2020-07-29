#install.packages("support.CEs")
library(support.CEs)

### VARIABLES AND LEVELS
vars = list(
  type_of_holiday  = c("beach", "nature", "city"),
  price            = c("high_p", "medium_p","low_p"),
  stars            = paste0(c(as.character(c(2,4))), "_stars"),
  rating           = paste0(c(as.character(seq(3.5,9.5,2))),"_rating"),
  distance_from    = c("15km", "40km"))

## check ratings again

blocks <- 18

#####################################################
####             4 alternatives design
#####################################################

design_4_q <- rotation.design(attribute.names = vars,
  nalternatives = 4,
  nblocks = blocks,
  seed = 123)

questions_4 <- questionnaire(choice.experiment.design = design_4_q, quote = FALSE)

dm4 <- make.design.matrix(
  choice.experiment.design = design_4_q,
  optout = TRUE,
  categorical.attributes = c("type_of_holiday", "price", "stars", "rating", "distance_from"),
  unlabeled = TRUE,
  common = NULL,
  binary = FALSE)

#####################################################
####              2 alts design
#####################################################

design_2_q <- rotation.design(attribute.names = vars,
  nalternatives = 2,
  nblocks = blocks,
  row.renames = FALSE,
  randomize = TRUE,
  seed = 342)

questions_2 <- questionnaire(choice.experiment.design = design_2_q, quote = FALSE)

dm2 <- make.design.matrix(
  choice.experiment.design = design_2_q,
  optout = TRUE,
  categorical.attributes = c("type_of_holiday", "price", "stars", "rating", "distance_from"),
  unlabeled = TRUE,
  common = NULL,
  binary = FALSE)

#####################################################
####             8 alternatives design
#####################################################

design_8_q <- rotation.design(attribute.names = vars,
                              nalternatives = 8,
                              nblocks = blocks,
                              seed = 123)

questions_8 <- questionnaire(choice.experiment.design = design_8_q, quote = FALSE)

dm8 <- make.design.matrix(
  choice.experiment.design = design_8_q,
  optout = TRUE,
  categorical.attributes = c("type_of_holiday", "price", "stars", "rating", "distance_from"),
  unlabeled = TRUE,
  common = NULL,
  binary = FALSE)


#####################################################
####
#####################################################

write.csv(dm2, file = "~/Dropbox/Maximizing Satisficing/maximizing_satisficing/Experiment/Vacation task/dm2.csv", row.names = FALSE)
write.csv(dm4, file = "~/Dropbox/Maximizing Satisficing/maximizing_satisficing/Experiment/Vacation task/dm4.csv", row.names = FALSE)
write.csv(dm8, file = "~/Dropbox/Maximizing Satisficing/maximizing_satisficing/Experiment/Vacation task/dm8.csv", row.names = FALSE)



library(knitr)
library(kableExtra)
library(data.table)

b <- 1
q  <- 1

dm2 <- data.table(dm2)
dm2_temp <- dm2
dm2_temp$type <- "Beach"
dm2_temp$type[dm2_temp$nature==1] <- "Nature"
dm2_temp$type[dm2_temp$city==1] <- "City"
dm2_temp$price <- "High"
dm2_temp$price[dm2_temp$low_p==1] <- "Low"
dm2_temp$price[dm2_temp$medium_p==1] <- "Medium"
dm2_temp$stars <- "**"
dm2_temp$stars[dm2_temp$X4_stars==1] <- "****"
dm2_temp$rating <- 3.5
dm2_temp$rating[dm2_temp$X5.5_rating==1] <- 5.5
dm2_temp$rating[dm2_temp$X7.5_rating==1] <- 7.5
dm2_temp$rating[dm2_temp$X9.5_rating==1] <- 9.5
dm2_temp$distance <- 15
dm2_temp$distance[dm2_temp$X40km==1] <- 40

  


dm2_temp_table <- as.data.table(t(dm2_temp[BLOCK == q & QES == q & ASC == 1,  .(ALT, type, price, stars, rating, distance)]),keep.rownames = TRUE)
for (i in 1:ncol(dm2_temp_table)){
  dm2_temp_table[1,i] <- paste0("Alternative ", toString(i))
}

library(dplyr)

kable(dm2_temp_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
