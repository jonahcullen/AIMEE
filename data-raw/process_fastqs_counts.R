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
  tidyr::pivot_longer(cols = dplyr::all_of(steps), names_to = "step", values_to = "count") %>%
  dplyr::mutate(
    step = factor(.data$step, levels = dplyr::all_of(steps)),
    step = dplyr::recode(
      step,
      "seq_depth" = "library_size",
      "trimmed_reads" = "adapters_removed",
      "clean_min_length" = "quality_seqs_min_length",
      "post_join_mirec" = "joined_seqs_corrected",
      "pre_mirnas" = "seqs_unaligned_against_ncRNA",
      "max_length" = "pre_quant_max_length"
    ),
    source_mod = factor(ifelse(!grepl("FAANG|Primary", source), "Public", as.character(source))),
    source_mod = factor(source_mod, levels = c("Primary", "FAANG", "Public")))

usethis::use_data(proc_cts, overwrite = TRUE)
