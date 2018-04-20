# Sehj K
# April 10, 2018

library(data.table)

# Process bmi model
model_coefs <- fread("data/model_coefs_pythia.csv", colClasses = "character")
# meds
model_coefs[["coef"]] <- gsub("as.matrix\\(meds_all\\)", "Meds", model_coefs[["coef"]])
# comorb
model_coefs[["coef"]] <- gsub("as.matrix\\(dis_dat\\)", "", model_coefs[["coef"]])
# cpt ccs
model_coefs[["coef"]] <- gsub("as.matrix\\(ccs_dat\\)", "", model_coefs[["coef"]])
# get ccs crosswalk
cpt_ccs <- fread("data/ccs_proc_crosswalk.csv", colClasses = "character")
cpt_ccs <- unique(cpt_ccs[, 2:3])
cpt_ccs[[1]] <- paste0("cpt_class_", cpt_ccs[[1]])
# merge cpt ccs crosswalk
cpt_coefs <- model_coefs[["coef"]] %in% cpt_ccs[[1]]
model_coefs[["coef"]][cpt_coefs] <- merge(model_coefs[, 1], cpt_ccs, by.x = "coef", by.y = "CCS")[["CCS Label"]]
# race
model_coefs[["coef"]] <- gsub("PRIMARY_RACE", "Race", model_coefs[["coef"]])
# column names
colnames(model_coefs) <- gsub("_coefs", "", colnames(model_coefs))
colnames(model_coefs)

# change class
coef_cols <- which(colnames(model_coefs) != "coef")
model_coefs[, (coef_cols) := lapply(.SD, as.numeric), .SDcols = (coef_cols)]

saveRDS(model_coefs, "data/model_coefs_pythia.rds")
