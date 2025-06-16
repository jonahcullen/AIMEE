## code to prepare `mirgenedb_map` dataset goes here

mirgene_map <- data.table::fread("data-raw/mirgenedb_map.tsv", sep = "\t")
stopifnot(all(c("id", "mirgenedb_id", "names") %in% colnames(mirgenedb_map)))

usethis::use_data(mirgenedb_map, overwrite = TRUE)
