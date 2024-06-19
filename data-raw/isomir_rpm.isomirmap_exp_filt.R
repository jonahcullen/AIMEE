## code to prepare `isomir_rpm.ds_isomirmap_exp_filt.tsv` dataset goes here

uid_rpms <- read.table("data-raw/isomir_rpm.isomirmap_exp_filt.tsv", header = TRUE, sep = "\t") %>%
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
  tidyr::pivot_longer(!c(id, parents, mir_names, variants, type), names_to = "tissue_sample", values_to = "rpm") %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::left_join(
    tissues %>%
      dplyr::select(tissue_sample, sample, system, tissue, new_lab, source, breed, sex),
    by = "tissue_sample"
  ) %>%
  dplyr::select(-tissue_sample)

usethis::use_data(uid_rpms, overwrite = TRUE)

canons <- read.table("data-raw/isomir_rpm.isomirmap_exp_filt.tsv", header = TRUE, sep = "\t") %>%
  dplyr::rename(
    "heart_left_ventricle_ECA_UCD_AH3" = "heart_ECA_UCD_AH3",
    "heart_left_ventricle_ECA_UCD_AH4" = "heart_ECA_UCD_AH4"
  ) %>%
  dplyr::mutate(Variant = tidyr::replace_na(Variant, "canon")) %>%
  dplyr::filter(Variant == "canon") %>%
  dplyr::select(-c(Filter, Parent, Variant)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(mir_names = paste(Name, collapse = "|")) %>%
  dplyr::distinct(id, mir_names, Read, .keep_all = TRUE) %>%
  dplyr::select(-Name) %>%
  dplyr::ungroup() %>%
  # DON'T DELETE - MAY NEED TO COLLAPSE mir_names LIKE A|A or B|B|C
  # dplyr::mutate(row_id = dplyr::row_number()) %>%
  # tidyr::separate_rows(mir_names, sep = "\\|") %>%
  # dplyr::group_by(row_id) %>%
  # dplyr::distinct(row_id, .keep_all = TRUE) %>%
  # dplyr::group_by(row_id) %>%
  # dplyr::summarise(X_combo = paste0(mir_names, collapse = "|")) %>%
  # dplyr::select(-row_id)
  # dplyr::mutate(test = ifelse(stringr::str_detect(mir_names, stringr::fixed("|")), "YES", "NO")) %>%
  # dplyr::select(test, mir_names, everything())
  # dplyr::distinct(Name, Read, id, .keep_all = TRUE) %>%
  tidyr::pivot_longer(!c(id, mir_names, Read), names_to = "tissue_sample", values_to = "rpm") %>%
  dplyr::left_join(
    tissues %>%
      dplyr::select(tissue_sample, system, tissue, new_lab, source, breed, sex),
    by = "tissue_sample"
  ) %>%
  dplyr::mutate_if(is.character, as.factor)

usethis::use_data(canons, overwrite = TRUE)

# this dataframe is nearly identical to canons above (only difference is the
# filtering to include canon only) - should rethink to avoid such similar dfs
# UPDATE THIS with names = paste(sort(unique(Name)), collapse = "|") for mir_names
pre_rank <- read.table("data-raw/isomir_rpm.isomirmap_exp_filt.tsv", header = TRUE, sep = "\t") %>%
  dplyr::rename(
    "heart_left_ventricle_ECA_UCD_AH3" = "heart_ECA_UCD_AH3",
    "heart_left_ventricle_ECA_UCD_AH4" = "heart_ECA_UCD_AH4"
  ) %>%
  dplyr::mutate(Variant = tidyr::replace_na(Variant, "canon")) %>%
  dplyr::select(-c(Filter, Parent, Variant)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(mir_names = paste(Name, collapse = "|")) %>%
  dplyr::distinct(id, mir_names, Read, .keep_all = TRUE) %>%
  dplyr::select(-Name) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(!c(id, mir_names, Read), names_to = "tissue_sample", values_to = "rpm") %>%
  dplyr::left_join(
    tissues %>%
      dplyr::select(tissue_sample, source, tissue, sample, new_lab, breed, sex),
    by = "tissue_sample"
  )

usethis::use_data(pre_rank, overwrite = TRUE)
