library(mappingelections)

congress_bio <- read_csv("data/congressional_bio_winners.csv")

congress_bio_u <- congress_bio %>%
  distinct(congbio_id, .keep_all = TRUE)

candidates <- meae_candidates

tmp <- congress_bio_u %>%
  left_join(candidates, by = c("congbio_member_name" = "candidate"))

empties <- tmp %>%
  filter(is.na(candidate_id))

write_csv(empties, "data/candidates_by_hand.csv")
