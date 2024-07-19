## code to prepare `mirna_space_ids` dataset goes here

tmp <- data.table::fread("data-raw/isomir_rpm.isomirmap_exp_filt.tsv", sep = "\t")

tmp[, filter_type := ifelse(
  stringr::str_detect(Filter, "EXCLUSIVE,WITHIN_REPEAT"), "Exclusive repeat",
  ifelse(
    Filter == "PASS,EXCLUSIVE", "Exclusive",
    ifelse(
      stringr::str_detect(Filter, "AMBIGUOUS,WITHIN_REPEAT"), "Ambiguous repeat",
      ifelse(
        Filter == "AMBIGUOUS", "Ambiguous",
        NA_character_
      )
    )
  )
)]

mirna_space_ids <- tmp[, .(id, filter_type)]

usethis::use_data(mirna_space_ids, overwrite = TRUE)
