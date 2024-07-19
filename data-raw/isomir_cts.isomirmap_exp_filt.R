## code to prepare `isomir_cts.isomirmap_exp_filt` dataset goes here

uid_cts <- read.table("data-raw/isomir_cts.isomirmap_exp_filt.tsv", header = TRUE, sep = "\t") %>%
  tidyr::separate(Parent, into = c("mid", "parent_name"), sep = "\\|") %>%
  dplyr::select(!c(Filter, mid, Read)) %>%
  dplyr::rename(
    "heart_left_ventricle_ECA_UCD_AH3" = "heart_ECA_UCD_AH3",
    "heart_left_ventricle_ECA_UCD_AH4" = "heart_ECA_UCD_AH4"
  ) %>%
  dplyr::mutate(Variant = tidyr::replace_na(Variant, "canon")) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(parents = paste(parent_name, collapse = "|"),
                variants = paste(Variant, collapse = "|"),
                mir_names = paste(Name, collapse = "|")) %>%
  dplyr::select(-c(Name, parent_name, Variant)) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = ifelse(grepl("canon", variants), "canon", "isomiR")) %>%
  dplyr::select(id, parents, mir_names, variants, type, dplyr::everything()) %>%
  tidyr::pivot_longer(!c(id, parents, mir_names, variants, type), names_to = "tissue_sample", values_to = "cts") %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::left_join(
    tissues %>%
      dplyr::select(tissue_sample, sample, system, tissue, new_lab, source, breed, sex),
    by = "tissue_sample"
  ) %>%
  dplyr::select(-tissue_sample)

usethis::use_data(uid_cts, overwrite = TRUE)
