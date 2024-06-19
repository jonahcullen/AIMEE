## code to prepare `se_coldata_final.tsv ` dataset goes here

tissues <- read.table("data-raw/se_coldata_final.tsv", header = TRUE, sep = "\t") %>%
  dplyr::mutate(
    tissue_sample = dplyr::recode(
      tissue_sample,
      "heart_ECA_UCD_AH3" = "heart_left_ventricle_ECA_UCD_AH3",
      "heart_ECA_UCD_AH4" = "heart_left_ventricle_ECA_UCD_AH4"
    ),
    tissue = dplyr::recode(
      tissue,
      "heart" = "heart_left_ventricle"
    ),
    source = ifelse(stringr::str_detect(batch, "primary_"), "Primary", batch)
  ) %>%
  dplyr::mutate(new_lab = stringr::str_to_title(stringr::str_replace_all(tissue, "_", " ")),
                new_lab = forcats::fct_recode(new_lab,
                                                 # "Bone Third Metacarp" = "Bone Third Mc",
                                                 "Epididymis caput" = "Caput",
                                                 "Epididymis cauda" = "Cauda",
                                                 "Epididymis corpus" = "Corpus",
                                                 "Deep Digital Flexor" = "Ddft",
                                                 "Dorsal Root Ganglia" = "Drg",
                                                 "PBMC" = "Pbmc",
                                                 "Superficial Digital Flexor" = "Sdft",
                                                 "Spinal Cord (T8)" = "Spinal Cord T Eight"),
                # new_lab = paste0(tissue_mod, " (", abvs, ")")
  ) %>%
  dplyr::mutate_if(is.character,as.factor) %>%
  dplyr::select(-batch)

usethis::use_data(tissues, overwrite = TRUE)

# TO DO: Document your data (see 'https://r-pkgs.org/data.html')

# tissue_cols <- dichromat::colorRampPalette(
#   rev(RColorBrewer::brewer.pal(11, "Spectral")) )(length(unique(tissues$tissue)))
# names(tissue_cols) <- unique(levels(tissues$tissue))

