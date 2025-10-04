##### Generate method configurations

method_all <- 
  data.frame(matrix(nrow = 100, ncol = 8))

names(method_all) <- c("assay", "analysis", 
                       "path_lib_istd_pos", "path_lib_istd_neg", 
                       "path_lib_pos", "path_lib_neg", 
                       "path_spectral",  "mzmine_params")

method_all[1, ] <- c("LIP", 
                     "PASEF",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     NA,
                     NA,
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_pasef.mzbatch")

method_all[2, ] <- c("LIP", 
                     "DDA",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     NA,
                     NA,
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_dda.mzbatch")

method_all[3, ] <- c("MET", 
                     "PASEF",
                     "N:/SUN-CBMR-Metabolomics/Workflow/library/internal_standards/MP_ISTD_Metabolomics/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "N:/SUN-CBMR-Metabolomics/Workflow/library/internal_standards/MP_ISTD_Metabolomics/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     NA,
                     NA,
                     "MSMS-Public_experimentspectra-VS19.msp", 
                     "/parameters/metabolomics_untargeted_pasef.mzbatch")

method_all[4, ] <- c("MET", 
                     "DDA",
                     "N:/SUN-CBMR-Metabolomics/Workflow/library/internal_standards/MP_ISTD_Metabolomics/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "N:/SUN-CBMR-Metabolomics/Workflow/library/internal_standards/MP_ISTD_Metabolomics/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     NA,
                     NA,
                     "MSMS-Public_experimentspectra-VS19.msp", 
                     "/parameters/metabolomics_untargeted_dda.mzbatch")

method_all[5, ] <- c("LIP", 
                     "PASEFl",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_neg_20250507.csv",
                     NA,
                     NA,
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_pasef.mzbatch")

method_all[6, ] <- c("LIP", 
                     "DDAl",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_neg_20250507.csv",
                     NA,
                     NA,
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_dda.mzbatch")

saveRDS(method_all, "method_configuration.rds")
