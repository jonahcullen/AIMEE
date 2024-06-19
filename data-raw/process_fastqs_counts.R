## code to prepare `process_fastqs_counts.csv` dataset goes here

steps <- c("seq_depth", "trimmed_reads", "clean_min_length", "post_join_mirec", "pre_mirnas", "max_length")

proc_cts <- read.table("data-raw/process_fastqs_counts.csv", header = TRUE, sep = ",") %>%
  dplyr::mutate(
    tissue_sample = dplyr::recode(
      tissue_sample,
      "heart_ECA_UCD_AH3" = "heart_left_ventricle_ECA_UCD_AH3",
      "heart_ECA_UCD_AH4" = "heart_left_ventricle_ECA_UCD_AH4"
    )
  ) %>%
  dplyr::right_join(tissues %>% dplyr::select(-post_counts)) %>%
  tidyr::pivot_longer(cols = steps, names_to = "step", values_to = "count") %>%
  dplyr::mutate(
    step = factor(step, levels = dplyr::all_of(steps)),
    step = dplyr::recode(
      step,
      "seq_depth" = "Library size",
      "trimmed_reads" = "Adapters removed",
      "clean_min_length" = "Quality sequences with\nminimum length (17)",
      "post_join_mirec" = "Joined (PE) and sequence\ncorrected",
      "pre_mirnas" = "Sequences unaligned\nagainst ncRNA",
      "max_length" = "Pre-quantification with\nmaximum length (25)"
    ),
    source_mod = factor(ifelse(!grepl("FAANG|Primary", source), "Public", as.character(source))),
    source_mod = factor(source_mod, levels = c("Primary", "FAANG", "Public")))

usethis::use_data(proc_cts, overwrite = TRUE)
