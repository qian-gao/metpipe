##### Generate method configurations

method_all <- 
  data.frame(matrix(nrow = 100, ncol = 6))

names(method_all) <- c("assay", "analysis", 
                       "path_lib_istd_pos", "path_lib_istd_neg", 
                       "path_spectral",  "mzmine_params")

method_all[1, ] <- c("LIPS", 
                     "PASEF",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_pasef.mzbatch")

method_all[2, ] <- c("LIPS", 
                     "FDDA",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_4min_neg_20250507.csv",
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_dda.mzbatch")

method_all[3, ] <- c("LIPL", 
                     "PASEF",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_neg_20250507.csv",
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_pasef.mzbatch")

method_all[4, ] <- c("LIPL", 
                     "FDDA",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_pos_20250507.csv",
                     "/library/MP_ISTD_mzmine_Lipidomics_10min_neg_20250507.csv",
                     "/spectral_library/MoNA-export-LipidBlast_2022.msp", 
                     "/parameters/lipidomics_untargeted_dda.mzbatch")

method_all[5, ] <- c("zHILIC", 
                     "PASEF",
                     NA,
                     "/library/MP_library_mzmine_zHILIC_neg_20251023.csv",
                     "/spectral_library/MSMS-Public_experimentspectra-VS19.msp", 
                     "/parameters/hilic_untargeted_dda.mzbatch")

method_all[6, ] <- c("zHILIC", 
                     "FDDA",
                     NA,
                     "/library/MP_library_mzmine_zHILIC_neg_20251023.csv",
                     "/spectral_library/MSMS-Public_experimentspectra-VS19.msp", 
                     "/parameters/hilic_untargeted_dda.mzbatch")
saveRDS(method_all, "method_configuration.rds")
