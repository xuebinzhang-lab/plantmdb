library(tidyverse)
library(tidymass)
root_exudates <- readxl::read_xlsx("archived/root_exdates.xlsx",sheet = 2)
root_exudates
library(plantmdb)
library(massdbbuildin)
mona = massdbbuildin::mona_ms2
mona_ms1 <- mona@spectra.info
hmdb = massdbbuildin::hmdb_ms2
hmdb_ms1 <- hmdb@spectra.info
massbank = massdbbuildin::massbank_ms2
massbank_ms1 <- massbank@spectra.info

inchi_tags <- inner_join(
  data.frame(INCHIKEY.ID = root_exudates$inchi_key),massbank_ms1
) %>% dplyr::select(INCHIKEY.ID) %>%
  distinct() %>% pull(INCHIKEY.ID)

rest_label = root_exudates %>% filter(!inchi_key %in% inchi_tags) %>% pull(label)
rest_form = root_exudates %>% filter(!inchi_key %in% inchi_tags) %>% pull(formula)
hmdb_ms1 %>% filter(str_detect(Compound.name,regex("4-acetamidobutanoic acid",ignore_case = T))) %>% tibble()
hmdb_ms1 %>% filter(str_detect(Compound.name,regex("biliverdin",ignore_case = T))) %>% tibble()

library(purrr)
inchi_tags_supp = purrr::map(.x = 1:length(rest_label),.f = function(.x){
  hmdb_ms1 %>% filter(str_detect(Compound.name,regex(rest_label[.x],ignore_case = T))) %>%
    filter(Formula==rest_form[.x]) %>% tibble() %>% pull(INCHIKEY.ID) %>% unique() %>% as.character()
}) %>% unlist()
inchi_tags_final = c(inchi_tags,inchi_tags_supp)
root_exudates %>% filter(!inchi_key %in% inchi_tags_final)
inchi_temp_mona_rest = root_exudates %>% filter(!inchi_key %in% inchi_tags_final) %>% pull(inchi_key)
mona_ms1 %>% filter(str_detect(Compound.name,regex("2-Deoxy-D-arabino-hexopyranose",ignore_case = T)))

temp_x = mona_ms1 %>% filter(INCHIKEY.ID %in% inchi_temp_mona_rest) %>% tibble() %>%
  dplyr::select(Compound.name,Formula,INCHIKEY.ID) %>% pull(INCHIKEY.ID) %>% as.character()
temp_x2 = setdiff(sort(inchi_temp_mona_rest),sort(temp_x))
root_exudates %>% filter(inchi_key %in% temp_x2)
mona_ms1 %>% filter(Formula == "C9H20N2O2") %>% tibble() %>%
  dplyr::select(Compound.name,Formula,INCHIKEY.ID)
hmdb_ms1 %>% filter(HMDB.ID == "HMDB0000858") %>% tibble() %>%
  dplyr::select(Compound.name,Formula,INCHIKEY.ID)
inchi_tags_hmdb = c(inchi_tags,inchi_tags_supp,"KTHADMDGDNYQRX-UHFFFAOYSA-N","IBMRTYCHDPMBFN-UHFFFAOYSA-N","KJTLQQUUPVSXIM-ZCFIWIBFSA-N","MXNRLFUSFKVQSK-QMMMGPOBSA-N")
inchi_tags_mona = c(temp_x,"IVOMOUWHDPKRLL-KQYNXXCUSA-N","RCNSAJSGRJSBKK-NSQVQWHSSA-N","QBUVFDKTZJNUPP-BBROENKCSA-N","RCNSAJSGRJSBKK-NSQVQWHSSA-N",
                    "PHIQHXFUZVPYII-ZCFIWIBFSA-N","CGQCWMIAEPEHNQ-UHFFFAOYSA-N",)
