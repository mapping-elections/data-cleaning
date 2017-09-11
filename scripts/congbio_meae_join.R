library(mappingelections)

congress_bio <- read_csv("data/congressional_bio_winners.csv")

congress_bio_u <- congress_bio %>%
  distinct(congbio_id, .keep_all = TRUE)

meae_candidates <- meae_candidates

tmp <- congress_bio_u %>%
  left_join(meae_candidates, by = c("congbio_member_name" = "candidate"))

empties <- tmp %>%
  filter(is.na(candidate_id))

tmp <- candidates %>%
  distinct(candidate_id, .keep_all = TRUE)

#write_csv(empties, "data/candidates_by_hand.csv")



NNV <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")

names(NNV) <- names(NNV) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

NNV_candidates <- NNV %>%
  filter(!is.na(name),
         office == "U.S. Senate" | office == "U.S. House of Representatives") %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name_id, name)

tmp <- congress_bio_u %>%
  left_join(NNV_candidates, by = c("join_name" = "name"))

empties <- tmp %>%
  filter(is.na(name_id))

write_csv(tmp, "data/congbio_join.csv")
