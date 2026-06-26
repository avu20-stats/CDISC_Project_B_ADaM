# Load packages -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(haven)
library(stringr)
library(readxl)

# Read raw SDTM datasets into a named list --------------------------------
raw <- list(
    ae = read_xpt("SDTM xpt/AE.xpt"),
    cm = read_xpt("SDTM xpt/CM.xpt"),
    ae = read_xpt("SDTM xpt/AE.xpt"),
    cm = read_xpt("SDTM xpt/CM.xpt"),
    co = read_xpt("SDTM xpt/CO.xpt"),
    da = read_xpt("SDTM xpt/DA.xpt"),
    dm_v2 = read_xpt("SDTM xpt/DM_V2.xpt"),
    dm = read_xpt("SDTM xpt/DM.xpt"),
    ds = read_xpt("SDTM xpt/DS.xpt"),
    dv = read_xpt("SDTM xpt/DV.xpt"),
    eg = read_xpt("SDTM xpt/EG.xpt"),
    ex = read_xpt("SDTM xpt/EX.xpt"),
    lb = read_xpt("SDTM xpt/LB.xpt"),
    mh = read_xpt("SDTM xpt/MH.xpt"),
    ncigrade = read_xpt("SDTM xpt/NCIGRADE.xpt"),
    qs = read_xpt("SDTM xpt/QS.xpt"),
    sc = read_xpt("SDTM xpt/SC.xpt"),
    se = read_xpt("SDTM xpt/SE.xpt"),
    suppae = read_xpt("SDTM xpt/SUPPAE.xpt"),
    suppcm = read_xpt("SDTM xpt/SUPPCM.xpt"),
    suppdm = read_xpt("SDTM xpt/SUPPDM.xpt"),
    suppds = read_xpt("SDTM xpt/SUPPDS.xpt"),
    suppdv = read_xpt("SDTM xpt/SUPPDV.xpt"),
    supplb = read_xpt("SDTM xpt/SUPPLB.xpt"),
    supptu = read_xpt("SDTM xpt/SUPPTU.xpt"),
    suppxr = read_xpt("SDTM xpt/SUPPXR.xpt"),
    suppyk = read_xpt("SDTM xpt/SUPPYK.xpt"),
    sv = read_xpt("SDTM xpt/SV.xpt"),
    ta = read_xpt("SDTM xpt/TA.xpt"),
    te = read_xpt("SDTM xpt/TE.xpt"),
    ti = read_xpt("SDTM xpt/TI.xpt"),
    ts = read_xpt("SDTM xpt/TS.xpt"),
    tu = read_xpt("SDTM xpt/TU.xpt"),
    tv = read_xpt("SDTM xpt/TV.xpt"),
    vs = read_xpt("SDTM xpt/VS.xpt"),
    xr = read_xpt("SDTM xpt/XR.xpt"),
    yk = read_xpt("SDTM xpt/YK.xpt"),
    yp = read_xpt("SDTM xpt/YP.xpt"),
    zb = read_xpt("SDTM xpt/ZB.xpt"),
    zh = read_xpt("SDTM xpt/ZH.xpt"),
    zs = read_xpt("SDTM xpt/ZS.xpt"))

# Read validation ADaM datasets for comparison 
val <- list(
    adae = read_xpt("ADaM xpt/ADAE.xpt"),
    adsl = read_xpt("ADaM xpt/ADSL.xpt"),
    adlbsi = read_xpt("ADaM xpt/ADLBSI.xpt"),
    adtte = read_xpt("ADaM xpt/ADTTE.xpt"),
    adex = read_xpt("ADaM xpt/ADEX.xpt"))

# ADaM ADSL ---------------------------------------------------------------
adsl <- raw$dm %>%
    # Derive actual treatment from EX
    inner_join(
        raw$ex %>% filter(!is.na(EXSTDTC), !is.na(EXTRT)) %>%
            group_by(USUBJID) %>%
            summarise(trt_act = case_when(
                any(toupper(EXTRT) == "CMP-135", na.rm = TRUE) ~ "CMP-135",
                any(toupper(EXTRT) == "PLACEBO", na.rm = TRUE) ~ "Placebo"), .groups = "drop"),
        by = "USUBJID") %>%
    # Treatment variables and age groups
    mutate(
        TRT01A = trt_act,
        TRT01AN = case_when(TRT01A == "CMP-135" ~ 1, TRT01A == "Placebo" ~ 0, TRUE ~ NA_real_),
        TRT01P = case_when(toupper(ARM) == "CMP-135" ~ "CMP-135", toupper(ARM) == "PLACEBO" ~ "Placebo"),
        TRT01PN = case_when(TRT01P == "CMP-135" ~ 1, TRT01P == "Placebo" ~ 0, TRUE ~ NA_real_),
        AGERE1 = case_when(
            AGE >= 18 & AGE < 41 ~ "18-40", AGE >= 41 & AGE < 65 ~ "41-64",
            AGE >= 65 ~ ">=65"),
        AGRE1N = case_when(AGE >= 18 & AGE < 41 ~ 1, AGE >= 41 & AGE < 65 ~ 2, AGE >= 65 ~ 3),
        AGEGR1 = case_when(
            AGE >= 18 & AGE < 41 ~ "18 - 40", AGE >= 41 & AGE < 65 ~ "41 - 64",
            AGE >= 65 ~ ">= 65"),
        AGEGR1N = case_when(AGE >= 18 & AGE < 41 ~ 1, AGE >= 41 & AGE < 65 ~ 2, AGE >= 65 ~ 3)) %>%
    select(-trt_act) %>%
    # Randomization date from DS protocol milestone
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "PROTOCOL MILESTONE", toupper(EPOCH) == "SCREENING",
            toupper(DSTERM) == "RANDOMIZATION") %>%
            mutate(RANDDT = as.Date(DSSTDTC)) %>% select(USUBJID, RANDDT),
        by = "USUBJID") %>%
    mutate(ITTFL = case_when(!is.na(RANDDT) ~ "Y", TRUE ~ "N")) %>%
    # Safety flag - at least one dose of treatment
    left_join(
        raw$ex %>% filter(!is.na(EXSTDTC), !is.na(EXTRT)) %>% distinct(USUBJID) %>% mutate(SAFFL = "Y"),
        by = "USUBJID") %>%
    mutate(SAFFL = case_when(is.na(SAFFL) ~ "N", TRUE ~ SAFFL)) %>%
    # Remission status from ZH 
    left_join(
        raw$zh %>% filter(toupper(ZHTESTCD) == "DXRMS") %>%
            mutate(REMISSN = case_when(
                toupper(ZHORRES) == "SECOND COMPLETE REMISSION" ~ 1,
                toupper(ZHORRES) == "THIRD COMPLETE REMISSION" ~ 2)) %>%
            arrange(USUBJID, desc(REMISSN)) %>%
            group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
            select(USUBJID, REMISS = ZHORRES, REMISSN),
        by = "USUBJID") %>%
    # Tumor assessment flag from TU
    left_join(
        raw$tu %>% filter(VISITNUM > 1, TUSPID == "CTSA", !is.na(TUORRES), trimws(TUORRES) != "") %>%
            distinct(USUBJID) %>% mutate(TUFL = "Y"),
        by = "USUBJID") %>%
    mutate(EFFL = case_when(
        SAFFL == "Y" & !is.na(REMISSN) & REMISSN > 0 & !is.na(TUFL) & TUFL == "Y" ~ "Y",
        TRUE ~ "N")) %>%
    # First and last treatment dates from EX
    left_join(
        raw$ex %>% filter(!is.na(EXSTDTC), !is.na(EXTRT)) %>%
            mutate(EXSTDTC = as.Date(EXSTDTC)) %>%
            group_by(USUBJID) %>% summarise(TRTSDT = min(EXSTDTC), .groups = "drop"),
        by = "USUBJID") %>%
    left_join(
        raw$ex %>% filter(!is.na(EXENDTC), !is.na(EXTRT)) %>%
            mutate(EXENDTC = as.Date(EXENDTC)) %>%
            group_by(USUBJID) %>% summarise(TRTEDT = max(EXENDTC), .groups = "drop"),
        by = "USUBJID") %>%
    mutate(
        TRTSDTC = format(TRTSDT, "%Y-%m-%d"),
        TRTEDTC = format(TRTEDT, "%Y-%m-%d"),
        TRTDUR = case_when( !is.na(TRTSDT) & !is.na(TRTEDT) & TRTEDT >= TRTSDT ~
            as.numeric(TRTEDT - TRTSDT) + 1)) %>%
    mutate(STDSDT = as.Date(RANDDT), STDSDTC = format(RANDDT, "%Y-%m-%d")) %>%
    # Study end date from DS
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "STUDY PERIOD",
            toupper(EPOCH) == "STUDY PERIOD") %>%
            mutate(STDEDT = as.Date(DSSTDTC)) %>% select(USUBJID, STDEDT),
        by = "USUBJID") %>%
    mutate(
        STDEDTC = format(STDEDT, "%Y-%m-%d"),
        STDDUR = case_when(
            !is.na(STDEDT) & !is.na(STDSDT) ~ as.numeric(STDEDT - STDSDT + 1))) %>%
    # Survival follow-up end date from DS
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "FOLLOW-UP",
            toupper(EPOCH) == "SURVIVAL FOLLOW-UP") %>%
            mutate(SFUEDTC = as.character(DSSTDTC)) %>% select(USUBJID, SFUEDTC),
        by = "USUBJID") %>%
    mutate(
        SFUEDTC = case_when(is.na(SFUEDTC) ~ "", TRUE ~ SFUEDTC),
        SFUEDT = as.Date(case_when(SFUEDTC == "" ~ NA_character_, TRUE ~ SFUEDTC))) %>%
    # Death date and timing flag from DS
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "OTHER EVENT", toupper(DSSCAT) == "DEATH",
            toupper(EPOCH) %in% c("STUDY PERIOD", "SURVIVAL FOLLOW-UP")) %>%
            mutate(DTHDT = as.Date(DSSTDTC)) %>% select(USUBJID, DTHDT),
        by = "USUBJID") %>%
    mutate(DTHFL = case_when(!is.na(DTHDT) ~ "Y", TRUE ~ "")) %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "OTHER EVENT", toupper(DSSCAT) == "DEATH",
            toupper(EPOCH) %in% c("STUDY PERIOD", "SURVIVAL FOLLOW-UP")) %>%
            mutate(DTHTMFL = case_when(
            toupper(EPOCH) == "STUDY PERIOD" ~ "STD", TRUE ~ "SFU")) %>%
            select(USUBJID, DTHTMFL),
        by = "USUBJID") %>%
    # Disposition reason codes
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT",
            toupper(DSSCAT) %in% c("CMP-135", "PLACEBO"),
            toupper(EPOCH) == "STUDY PERIOD") %>%
            arrange(USUBJID, DSDTC) %>%
            group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
            select(USUBJID, TRTDCRS = DSDECOD),
        by = "USUBJID") %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "STUDY PERIOD",
             toupper(EPOCH) == "STUDY PERIOD") %>%
            mutate(DSSTDTC = as.Date(DSSTDTC)) %>%
            arrange(USUBJID, DSSTDTC) %>%
            group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
            select(USUBJID, STDDCRS = DSDECOD),
        by = "USUBJID") %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "OTHER EVENT", toupper(DSSCAT) == "DEATH",
            toupper(EPOCH) %in% c("STUDY PERIOD", "SURVIVAL FOLLOW-UP")) %>%
            select(USUBJID, DTHDCRS = DSTERM),
        by = "USUBJID") %>%
    mutate(DTHDCRS = case_when(is.na(DTHDCRS) ~ "", TRUE ~ DTHDCRS)) %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT",
            toupper(DSSCAT) %in% c("CMP-135", "PLACEBO"),
            toupper(EPOCH) == "STUDY PERIOD",
            !is.na(DSTERM), !is.na(DSDTC)) %>%
            mutate(DSDTC = as.Date(DSDTC)) %>%
            group_by(USUBJID) %>% summarise(TRTDCDT = min(DSDTC), .groups = "drop"),
        by = "USUBJID") %>%
    # Disposition completion flags
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "STUDY PERIOD",
            toupper(EPOCH) == "STUDY PERIOD", !is.na(DSTERM)) %>%
            distinct(USUBJID) %>% mutate(STDDCFL = "Y"),
        by = "USUBJID") %>%
    mutate(STDDCFL = case_when(is.na(STDDCFL) ~ "N", TRUE ~ STDDCFL)) %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "FOLLOW-UP",
            toupper(EPOCH) == "SURVIVAL FOLLOW-UP", !is.na(DSTERM)) %>%
            distinct(USUBJID) %>% mutate(SFUDCFL = "Y"),
        by = "USUBJID") %>%
    mutate(SFUDCFL = case_when(is.na(SFUDCFL) ~ "N", TRUE ~ SFUDCFL)) %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT",
            toupper(DSSCAT) %in% c("CMP-135", "PLACEBO"),
            toupper(EPOCH) == "STUDY PERIOD", !is.na(DSDECOD)) %>%
            distinct(USUBJID) %>% mutate(TRTDCFL = "Y"),
        by = "USUBJID") %>%
    mutate(TRTDCFL = case_when(is.na(TRTDCFL) ~ "N", TRUE ~ TRTDCFL)) %>%
    left_join(
        raw$ds %>% filter(toupper(DSCAT) == "DISPOSITION EVENT", toupper(DSSCAT) == "STUDY PERIOD",
            toupper(EPOCH) == "STUDY PERIOD",
            toupper(DSTERM) %in% c("DEATH", "DISEASE PROGRESSION - RADIOGRAPHIC")) %>%
            distinct(USUBJID) %>% mutate(COMPLFL = "Y"),
        by = "USUBJID") %>%
    mutate(COMPLFL = case_when(is.na(COMPLFL) ~ "N", TRUE ~ COMPLFL)) %>%
    # Survival follow-up flag from SUPPDS
    left_join(
        raw$suppds %>% filter(toupper(QNAM) == "DSFUYN") %>% select(USUBJID, SFUFL = QVAL),
        by = "USUBJID") %>%
    mutate(SFUFL = case_when(is.na(SFUFL) ~ "N", TRUE ~ SFUFL)) %>%
    # Prior therapy dates: surgery (YP), radiotherapy (XR), systemic (CM)
    left_join(
        raw$yp %>% filter(toupper(YPCAT) == "PRIOR CANCER-RELATED SURGERY OR PROCEDURE",
            !is.na(YPENDTC), nchar(trimws(YPENDTC)) == 10) %>%
            mutate(YPENDTC = as.Date(YPENDTC)) %>% filter(!is.na(YPENDTC)) %>%
            group_by(USUBJID) %>% summarise(PRSURGDT = max(YPENDTC), .groups = "drop"),
        by = "USUBJID") %>%
    left_join(
        raw$xr %>% filter(toupper(XROCCUR) == "Y", !is.na(XRENDTC)) %>%
            mutate(XRENDTC = as.Date(XRENDTC)) %>%
            group_by(USUBJID) %>% summarise(PRRADDT = max(XRENDTC), .groups = "drop"),
        by = "USUBJID") %>%
    left_join(
        raw$cm %>% filter(toupper(CMCAT) == "PRIOR CANCER THERAPY",
            !is.na(CMENDTC), nchar(trimws(CMENDTC)) == 10) %>%
            mutate(CMENDTC = as.Date(CMENDTC)) %>% filter(!is.na(CMENDTC)) %>%
            group_by(USUBJID) %>% summarise(PRSYSDT = max(CMENDTC), .groups = "drop"),
        by = "USUBJID") %>% rowwise() %>%
    mutate(PRTXDT = {
        x <- c(PRSURGDT, PRRADDT, PRSYSDT)
        if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)}) %>%
    ungroup() %>%
    mutate(PRTXDUR = case_when(
        !is.na(PRTXDT) & !is.na(RANDDT) & RANDDT >= PRTXDT ~
            as.numeric(RANDDT - PRTXDT + 1) / 7)) %>%
    left_join(
        raw$yp %>% filter(toupper(YPCAT) == "PRIOR CANCER-RELATED SURGERY OR PROCEDURE") %>%
            distinct(USUBJID) %>% mutate(PRSURGFL = "Y"), by = "USUBJID") %>%
    mutate(
        PRSURGFL = case_when(is.na(PRSURGFL) ~ "N", TRUE ~ PRSURGFL),
        PRRADFL = case_when(!is.na(PRRADDT) ~ "Y", TRUE ~ "N"),
        PRSYSFL = case_when(!is.na(PRSYSDT) ~ "Y", TRUE ~ "N")) %>%
    # Baseline weight (last of visits 1-2)
    left_join(
        raw$vs %>% filter(toupper(VSTESTCD) == "WEIGHT", VISITNUM %in% c(1, 2),
            VSSTRESN > 0, !is.na(VSDTC)) %>%
            mutate(VSDTC = as.Date(VSDTC)) %>%
            arrange(USUBJID, VISITNUM, VSDTC) %>%
            group_by(USUBJID) %>% slice_tail(n = 1) %>% ungroup() %>%
            select(USUBJID, BWT = VSSTRESN),
        by = "USUBJID") %>%
    # Baseline height (visit 1)
    left_join(
        raw$vs %>% filter(toupper(VSTESTCD) == "HEIGHT", VISITNUM == 1,
            VSSTRESN > 0, !is.na(VSDTC)) %>%
            mutate(VSDTC = as.Date(VSDTC)) %>%
            arrange(USUBJID, VISITNUM, VSDTC) %>%
            group_by(USUBJID) %>% slice_tail(n = 1) %>% ungroup() %>%
            select(USUBJID, BHT = VSSTRESN), by = "USUBJID") %>%
    # Baseline ECOG (last of visits 1-2)
    left_join(
        raw$qs %>% filter(toupper(QSTESTCD) == "ECOG", VISITNUM %in% c(1, 2), QSSTRESN >= 0) %>%
            mutate(QSDTC = as.Date(QSDTC)) %>%
            arrange(USUBJID, VISITNUM, QSDTC) %>%
            group_by(USUBJID) %>% 
            summarise(BECOG = last(QSSTRESN), .groups = "drop"), by = "USUBJID") %>%
    # CA-125 responder flag from ZH
    left_join(
        raw$zh %>% filter(toupper(trimws(ZHTESTCD)) == "RSP125YN") %>%
            mutate(RSP125 = case_when(toupper(trimws(ZHORRES)) == "Y" ~ "Y", TRUE ~ "N")) %>%
            select(USUBJID, RSP125), by = "USUBJID") %>%
    mutate(
        RSP125 = case_when(is.na(RSP125) ~ NA_character_, TRUE ~ RSP125),
        CA125FL = case_when(SAFFL == "Y" & !is.na(RSP125) & RSP125 == "Y" ~ "Y", TRUE ~ "N")) %>%
    # Histopathology type and subtype from ZH
    left_join(
        raw$zh %>% filter(toupper(ZHTESTCD) == "HPATHTYP") %>%
            select(USUBJID, HPATHTYP = ZHORRES), by = "USUBJID") %>%
    left_join(
        raw$zh %>% filter(toupper(ZHTESTCD) == "HSUBTYP", !is.na(ZHORRES)) %>%
            group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
            select(USUBJID, HSUBTYP = ZHORRES), by = "USUBJID") %>%
    mutate(HSUBTYP = case_when(is.na(HSUBTYP) ~ "", TRUE ~ HSUBTYP)) %>%
    # Follow-up period duration and death period classification
    mutate(across(c(DTHDT, SFUEDT, STDEDT, RANDDT), as.Date)) %>%
    mutate(FPDUR = case_when(
        !is.na(DTHDT) & !is.na(TRTSDT) ~ as.numeric(DTHDT - TRTSDT) + 1,
        !is.na(SFUEDT) & !is.na(TRTSDT) ~ as.numeric(SFUEDT - TRTSDT) + 1,
        !is.na(STDEDT) & !is.na(TRTSDT) ~ as.numeric(STDEDT - TRTSDT) + 1)) %>%
    mutate(DTHPER = case_when(
        !is.na(DTHDT) & DTHTMFL == "STD" ~ "STUDY PERIOD",
        !is.na(DTHDT) & DTHTMFL == "SFU" ~ "SURVIVAL FOLLOW-UP PERIOD",
        TRUE ~ ""))

adsl <- adsl %>% select(names(val$adsl))

# ADaM ADAE ---------------------------------------------------------------
# Pivot SUPPAE supplemental data wide by QNAM
suppae_w <- raw$suppae %>%
    select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QVAL) %>%
    mutate(QNAM = na_if(QNAM, "")) %>%
    filter(!is.na(QNAM)) %>%
    pivot_wider(names_from = QNAM, values_from = QVAL)

adae <- raw$ae %>%
    select(STUDYID, USUBJID, DOMAIN, AESEQ, AETERM, AEMODIFY, AEDECOD, 
        AEBODSYS, AESTDTC, AEENDTC, AESTRTPT, AEENRTPT, AESER, AEREL, 
        AERELNST, AETOXGR, AEACN, AEACNOTH, AECONTRT, AESDTH, AESLIFE, 
        AESHOSP, AESDISAB, AESCONG, AESMIE) %>%
    left_join(adsl, by = c("STUDYID", "USUBJID")) %>%
    # Derive analysis dates, study day relative to first treatment, TEAE flag
    mutate(
        SRCDOM = DOMAIN, 
        SRCSEQ = AESEQ,
        AESDT = as.Date(substr(AESTDTC, 1, 10)),
        AEEDT = as.Date(substr(AEENDTC, 1, 10)),
        AEST_FULL = nchar(substr(AESTDTC, 1, 10)) == 10,
        AESDY = case_when(
            is.na(AESTDTC) | is.na(TRTSDT) ~ NA_real_,
            AEST_FULL & AESDT >= TRTSDT ~ as.numeric(AESDT - TRTSDT + 1),
            AEST_FULL ~ as.numeric(AESDT - TRTSDT)),
        AETOXGRN = case_when(
            is.na(AETOXGR) | AETOXGR == "" ~ NA_real_, TRUE ~ as.numeric(AETOXGR)),
        TRTEM = case_when(
            is.na(AESTDTC) | is.na(TRTSDT) ~ NA_character_,
            AEST_FULL & AESDT < TRTSDT ~ "N",
            AEST_FULL ~ "Y",
            substr(AESTDTC, 1, 4) < substr(as.character(TRTSDT), 1, 4) ~ "N",
            TRUE ~ "Y")) %>%
    left_join(
        suppae_w %>% filter(RDOMAIN == "AE") %>%
            mutate(AESEQ = as.numeric(IDVARVAL)) %>%
            select(-any_of("AERELNST")), by = c("STUDYID", "USUBJID", "AESEQ")) %>%
    # Combine multiple causality and action-taken records from SUPPAE
    mutate(
        AERELOTH = case_when(
            AERELNST == "MULTIPLE" & !is.na(AERELNS1) & !is.na(AERELNS2) ~
                paste(AERELNS1, AERELNS2, sep = "; "),
            AERELNST == "MULTIPLE" & !is.na(AERELNS1) ~ AERELNS1,
            AERELNST == "MULTIPLE" & !is.na(AERELNS2) ~ AERELNS2,
            !is.na(AERELNST) & AERELNST != "" ~ AERELNST,
            TRUE ~ "NONE"),
        AETRTOTH = case_when(
            AECONTRT == "N" & (is.na(AEACNOTH) | AEACNOTH == "" | AEACNOTH == "NONE") ~ "NONE",
            AECONTRT == "N" & !(AEACNOTH %in% c("NONE", "") | is.na(AEACNOTH)) ~ AEACNOTH,
            AECONTRT == "Y" & (is.na(AEACNOTH) | AEACNOTH == "" | AEACNOTH == "NONE") ~ "MEDICATION",
            AECONTRT == "Y" & !(AEACNOTH %in% c("NONE", "") | is.na(AEACNOTH)) ~
                paste(AEACNOTH, "MEDICATION", sep = "; "), 
                TRUE ~ "NONE"),
        AETRTOTH = case_when(is.na(AETRTOTH) ~ "", TRUE ~ AETRTOTH))

# Ensure remaining variables exist
adae <- adae %>%
    mutate(AEDTHDTC = "", DTHAUTYN = "",
    AEHDTC = case_when(is.na(AEHDTC) ~ "", TRUE ~ as.character(AEHDTC)))

adae <- adae %>% select(names(val$adae))

# ADaM ADLBSI - Lab Safety (SI) Analysis Dataset  -------------------------
# Read supplemental lab toxicity grade from SUPPLE
tox <- raw$supplb %>%
    filter(QNAM == "LBTOXGR1") %>%
    mutate(LBSEQ = as.numeric(IDVARVAL)) %>%
    select(USUBJID, LBSEQ, LBTOXGR1 = QVAL)

# Helper to format numeric lab ranges (strip trailing zeros)
fmt <- function(x) {
    case_when(!is.na(x) ~ gsub("\\.?0+$", "", trimws(format(x, scientific = FALSE))), TRUE ~ "")}

adlbsi <- raw$lb %>%
    filter(LBSTAT != "NOT DONE" | is.na(LBSTAT)) %>%
    select(-STUDYID) %>%
    left_join(tox, by = c("USUBJID", "LBSEQ")) %>%
    left_join(adsl, by = "USUBJID") %>%
    mutate(
        SRCVAR = "LBSTRESN",
        # Build PARAM
        PARAM = case_when(
            is.na(LBSTRESU) | LBSTRESU == "" ~ paste0(trimws(LBCAT), "|", trimws(LBTEST)),
            TRUE ~ paste0(trimws(LBCAT), "|", trimws(LBTEST), " (", trimws(LBSTRESU), ")")),
        PARAMCD = paste0(
            case_when(
                str_detect(toupper(LBCAT), "CHEM") ~ "C",
                str_detect(toupper(LBCAT), "URIN") ~ "U",
                str_detect(toupper(LBCAT), "HEMATOLOGY") ~ "H",
                TRUE ~ "X"),
            str_sub(toupper(LBTESTCD), 1, 6),
            case_when(is.na(LBSTRESU) | LBSTRESU == "" ~ "N", TRUE ~ "S")),
        AVAL = case_when(
            (is.na(LBSTAT) | LBSTAT == "") & !is.na(LBSTRESC) & LBSTRESC != "NO - NOT DONE" ~ LBSTRESN),
        AVALC = case_when(
            (is.na(LBSTAT) | LBSTAT == "") & !is.na(LBSTRESC) & LBSTRESC != "NO - NOT DONE" ~ LBSTRESC),
        ANL01FL = case_when(!is.na(AVAL) | (!is.na(AVALC) & AVALC != "") ~ "Y", TRUE ~ ""),
        ADTC = case_when(!is.na(LBDTC) & LBDTC != "" ~ LBDTC),
        ADT = as.Date(ADTC),
        ONTRTFL = case_when(
            !is.na(ADT) & !is.na(TRTSDT) & !is.na(TRTEDT) & ADT >= TRTSDT & ADT <= TRTEDT ~ "Y",
            TRUE ~ "")) %>%
    # Baseline eligibility
    mutate(BASEELIG = case_when(
        !is.na(ADTC) & ADTC != "" & !is.na(TRTSDTC) & TRTSDTC != "" & ADT <= TRTSDT &
            (!is.na(AVAL) | (!is.na(AVALC) & AVALC != "")) ~ "Y", TRUE ~ "")) %>%
    group_by(USUBJID, PARAM) %>%
    # Find baseline date
    mutate(
        pre_dt = suppressWarnings(max(ADT[BASEELIG == "Y" & ADT < TRTSDT], na.rm = TRUE)),
        on_dt = suppressWarnings(max(ADT[BASEELIG == "Y" & ADT == TRTSDT], na.rm = TRUE)),
        basedt = case_when(
            !is.infinite(as.numeric(pre_dt)) ~ pre_dt,
            !is.infinite(as.numeric(on_dt)) ~ on_dt),
        max_bs = if_else(is.infinite(max(LBSEQ[BASEELIG == "Y" & !is.na(basedt) & ADT == basedt],
            na.rm = TRUE)), NA_real_,
            max(LBSEQ[BASEELIG == "Y" & !is.na(basedt) & ADT == basedt], na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(ABLFL = case_when(!is.na(basedt) & ADT == basedt & LBSEQ == max_bs ~ "Y", TRUE ~ "")) %>%
    select(-pre_dt, -on_dt, -basedt, -max_bs) %>%
    # Carry baseline values forward across all records per USUBJID/PARAM
    group_by(USUBJID, PARAM) %>%
    mutate(
        BASE = case_when(ABLFL == "Y" ~ LBSTRESN, TRUE ~ NA_real_),
        BASEC = case_when(ABLFL == "Y" ~ LBSTRESC, TRUE ~ ""),
        BASE = case_when(any(ABLFL == "Y" & !is.na(BASE)) ~ first(BASE[ABLFL == "Y" & !is.na(BASE)]),
            TRUE ~ NA_real_),
        BASEC = case_when(any(ABLFL == "Y" & !is.na(BASEC) & BASEC != "") ~
            first(BASEC[ABLFL == "Y" & !is.na(BASEC) & BASEC != ""]), TRUE ~ "")) %>%
    ungroup() %>%
    # Change and percent change from baseline
    mutate(
        CHG = case_when(!is.na(AVAL) & !is.na(BASE) ~ AVAL - BASE),
        PCHG = case_when(!is.na(AVAL) & !is.na(BASE) & BASE != 0 ~ (AVAL - BASE) / BASE * 100)) %>%
    # Combine toxicity grades from LB and SUPPLE, extract numeric grade and direction
    mutate(
        tox_final = case_when(!is.na(LBTOXGR) & LBTOXGR != "" ~ LBTOXGR,
            !is.na(LBTOXGR1) & LBTOXGR1 != "" ~ LBTOXGR1, TRUE ~ ""),
        ATOXGR = case_when(tox_final != "" ~ str_extract(tox_final, "[0-9]+"), TRUE ~ ""),
        ATOXDIR = case_when(tox_final != "" ~ str_extract(tox_final, "[A-Za-z]+"), TRUE ~ "")) %>%
    select(-tox_final) %>%
    # Baseline toxicity grade and direction
    group_by(USUBJID, PARAM) %>%
    mutate(
        BTOXGR = case_when(
            any(ABLFL == "Y" & !is.na(ATOXGR) & ATOXGR != "") ~ ATOXGR[ABLFL == "Y" & !is.na(ATOXGR) & ATOXGR != ""][1],
            TRUE ~ ""),
        BTOXDIR = case_when(
            any(ABLFL == "Y" & !is.na(ATOXDIR) & ATOXDIR != "") ~ ATOXDIR[ABLFL == "Y" & !is.na(ATOXDIR) & ATOXDIR != ""][1],
            TRUE ~ "")) %>%
    ungroup() %>%
    mutate(
        ANRIND = case_when(!is.na(LBNRIND) ~ LBNRIND, TRUE ~ ""),
        BNRIND = case_when(ABLFL == "Y" ~ ANRIND, TRUE ~ "")) %>%
    group_by(USUBJID, PARAM) %>%
    mutate(BNRIND = {
        bv <- BNRIND[ABLFL == "Y"]; bv <- bv[bv != "" & !is.na(bv)]
        if (length(bv) > 0) bv[1] else ""
    }) %>%
    ungroup() %>%
    mutate(
        ANRLO = fmt(LBSTNRLO),
        ANRHI = fmt(LBSTNRHI),
        PARCAT1 = LBCAT, PARCAT2 = "SI",
        ADY = case_when(
            !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
            !is.na(ADT) & !is.na(TRTSDT) ~ as.numeric(ADT - TRTSDT)),
        TRTP = TRT01P,
        SRCDOM = "LB",
        SRCSEQ = LBSEQ) %>%
    arrange(USUBJID, PARAMCD, ADT, SRCSEQ) 

adlbsi <- adlbsi %>% select(names(val$adlbsi))

# ADaM ADTTE --------------------------------------------------------------
# Data cutoff date for analysis
cutoff <- ymd("2010-05-15")

# Last tumor assessment date per subject (max TUDTC with non-NA result)
tu_dates <- raw$tu %>%
    filter(!is.na(TUORRES), TUORRES != "") %>%
    mutate(TUDTC = as.Date(TUDTC)) %>%
    filter(!is.na(TUDTC), TUDTC <= cutoff) %>%
    group_by(USUBJID) %>%
    summarise(TUMLDT = max(TUDTC), .groups = "drop")

# First progression date per subject 
fpdt <- raw$tu %>%
    filter(substr(TUORRES, 1, 2) == "NL") %>%
    mutate(TUDTC = as.Date(TUDTC)) %>%
    filter(!is.na(TUDTC), TUDTC <= cutoff) %>%
    group_by(USUBJID) %>%
    summarise(FPDDT = min(TUDTC), .groups = "drop")

# CA-125 lab data for time-to-event: filter to CA-125 responders,
# post-treatment, within cutoff; LBORRES converted to numeric
lb_tte <- raw$lb %>%
    mutate(LBDTC = as.Date(LBDTC), LBORRES = as.numeric(LBORRES), LBORNRHI = as.numeric(LBORNRHI)) %>%
    inner_join(adsl %>% select(USUBJID, TRTSDT, CA125FL), by = "USUBJID") %>%
    filter(CA125FL == "Y", LBTESTCD == "CA125", !is.na(LBDTC),
        LBDTC >= TRTSDT, LBDTC <= cutoff)

adtte <- adsl %>% mutate(TRTP = TRT01P, STARTDT = RANDDT) %>%
    left_join(tu_dates, by = "USUBJID") %>%
    left_join(fpdt, by = "USUBJID") %>%
    # First CA-125 elevation date
    left_join(
        lb_tte %>% filter(!is.na(LBORRES), !is.na(LBORNRHI), LBORRES >= 2 * LBORNRHI) %>%
            arrange(USUBJID, LBDTC) %>%
            group_by(USUBJID) %>%
            filter({d <- unique(LBDTC)
                length(d) >= 2 && min(abs(outer(d, d, `-`))[upper.tri(diag(length(d)))]) >= 7}) %>%
            summarise(FCA125DT = min(LBDTC), .groups = "drop"), by = "USUBJID") %>%
    # Last CA-125 assessment date
    left_join(
        raw$lb %>%
            mutate(LBDTC = as.Date(LBDTC)) %>%
            inner_join(adsl %>% select(USUBJID, TRTSDT, CA125FL), by = "USUBJID") %>%
            filter(CA125FL == "Y", LBTESTCD == "CA125", !is.na(LBDTC),
                !is.na(LBORRES), LBORRES != "",
                LBDTC >= TRTSDT, LBDTC <= cutoff) %>%
            group_by(USUBJID) %>% summarise(LCA125DT = max(LBDTC), .groups = "drop"),
        by = "USUBJID")

# Cross with time-to-event parameters
adtte <- adtte %>%
    cross_join(tibble(
        PARAMCD = c("TTPFS", "TTPFS125", "TTOS"),
        PARAM = c("TIME TO PROGRESSION FREE SURVIVAL (month)",
            "TIME TO PROGRESSION FREE SURVIVAL CA-125 RESPONDER (month)",
            "TIME TO OVERALL SURVIVAL (month)"))) %>%
    # Set tumor/CA-125 dates to NA for TTOS
    mutate(
        TUMLDT = as.Date(ifelse(PARAMCD == "TTOS", NA, as.numeric(TUMLDT)), origin = "1970-01-01"),
        FPDDT = as.Date(ifelse(PARAMCD == "TTOS", NA, as.numeric(FPDDT)), origin = "1970-01-01"),
        FCA125DT = as.Date(ifelse(PARAMCD == "TTOS", NA, as.numeric(FCA125DT)), origin = "1970-01-01"),
        LCA125DT = as.Date(ifelse(PARAMCD == "TTOS", NA, as.numeric(LCA125DT)), origin = "1970-01-01")) %>%
    # Death date adjusted to NA if after cutoff
    mutate(DTHDT_ADJ = as.Date(ifelse(!is.na(DTHDT) & DTHDT <= cutoff, DTHDT, NA))) %>%
    # Disposition end date and reason for censor classification
    left_join(
        raw$ds %>% filter(DSCAT == "DISPOSITION EVENT", DSSCAT %in% c("STUDY PERIOD", "FOLLOW-UP")) %>%
            mutate(DSSTDTC = as.Date(DSSTDTC)) %>% filter(!is.na(DSSTDTC)) %>%
            group_by(USUBJID) %>% slice_max(DSSTDTC, n = 1, with_ties = FALSE) %>% ungroup() %>%
            select(USUBJID, DS_ADT = DSSTDTC, DSDECOD), by = "USUBJID")

# Derive ADT, FPD125DT, CNSR, and EVNTDESC
adtte <- adtte %>%
    mutate(
        ADT = case_when(
            PARAMCD == "TTOS" & !is.na(DTHDT) ~ DTHDT,
            PARAMCD == "TTOS" & !is.na(DS_ADT) ~ DS_ADT,
            PARAMCD == "TTOS" ~ STARTDT,
            PARAMCD == "TTPFS" & !is.na(DTHDT_ADJ) & !is.na(FPDDT) ~
                case_when(FPDDT <= DTHDT_ADJ ~ FPDDT, TRUE ~ DTHDT_ADJ),
            PARAMCD == "TTPFS" & is.na(DTHDT_ADJ) & !is.na(FPDDT) ~ FPDDT,
            PARAMCD == "TTPFS" & !is.na(DTHDT_ADJ) & is.na(FPDDT) ~ DTHDT_ADJ,
            PARAMCD == "TTPFS" & !is.na(TUMLDT) ~ TUMLDT,
            PARAMCD == "TTPFS" ~ STARTDT,
            PARAMCD == "TTPFS125" & !is.na(DTHDT_ADJ) & !is.na(FPDDT) & !is.na(FCA125DT) ~
                pmin(DTHDT_ADJ, FPDDT, FCA125DT),
            PARAMCD == "TTPFS125" & !is.na(DTHDT_ADJ) & !is.na(FPDDT) ~ pmin(DTHDT_ADJ, FPDDT),
            PARAMCD == "TTPFS125" & !is.na(DTHDT_ADJ) & !is.na(FCA125DT) ~ pmin(DTHDT_ADJ, FCA125DT),
            PARAMCD == "TTPFS125" & !is.na(FPDDT) & !is.na(FCA125DT) ~ pmin(FPDDT, FCA125DT),
            PARAMCD == "TTPFS125" & !is.na(DTHDT_ADJ) ~ DTHDT_ADJ,
            PARAMCD == "TTPFS125" & !is.na(FPDDT) ~ FPDDT,
            PARAMCD == "TTPFS125" & !is.na(FCA125DT) ~ FCA125DT,
            PARAMCD == "TTPFS125" & !is.na(TUMLDT) & !is.na(LCA125DT) ~ pmax(TUMLDT, LCA125DT),
            PARAMCD == "TTPFS125" & !is.na(TUMLDT) ~ TUMLDT,
            PARAMCD == "TTPFS125" & !is.na(LCA125DT) ~ LCA125DT,
            PARAMCD == "TTPFS125" ~ STARTDT,
            TRUE ~ STARTDT),
        # First progression date for CA-125 responders
        FPD125DT = as.Date(
            case_when(
                CA125FL == "Y" & !is.na(FPDDT) & !is.na(FCA125DT) ~ as.numeric(pmin(FPDDT, FCA125DT)),
                CA125FL == "Y" & !is.na(FPDDT) ~ as.numeric(FPDDT),
                CA125FL == "Y" & !is.na(FCA125DT) ~ as.numeric(FCA125DT)),
            origin = "1970-01-01")) %>%
    # Temporary flags for CNSR computation
    mutate(
        evt = !is.na(DTHDT) & DTHDT <= cutoff,
        sponsor = DSDECOD == "STUDY TERMINATED BY SPONSOR",
        ltofu = DSDECOD == "LOST TO FOLLOW-UP",
        wdraw = DSDECOD == "WITHDRAWAL BY SUBJECT",
        other = DSDECOD == "OTHER",
        prog = DSDECOD == "PROGRESSIVE DISEASE",
        f_evt = (!is.na(FPDDT) | !is.na(DTHDT_ADJ)),
        f_c125 = (!is.na(FPDDT) | !is.na(DTHDT_ADJ) | !is.na(FCA125DT)),
        CNSR = case_when(
            PARAMCD == "TTOS" & evt ~ 0,
            PARAMCD == "TTOS" & sponsor ~ 1,
            PARAMCD == "TTOS" & ltofu ~ 2,
            PARAMCD == "TTOS" & wdraw ~ 3,
            PARAMCD == "TTOS" & other ~ 4,
            PARAMCD == "TTOS" & prog ~ 5,
            PARAMCD == "TTPFS" & f_evt ~ 0,
            PARAMCD == "TTPFS" & is.na(FPDDT) & is.na(DTHDT_ADJ) & !is.na(TUMLDT) ~ 1,
            PARAMCD == "TTPFS" ~ 2,
            PARAMCD == "TTPFS125" & f_c125 ~ 0,
            PARAMCD == "TTPFS125" & !is.na(TUMLDT) & (is.na(LCA125DT) | TUMLDT >= LCA125DT) ~ 1, 
            PARAMCD == "TTPFS125" & !is.na(LCA125DT) ~ 2,
            PARAMCD == "TTPFS125" ~ 3,
            TRUE ~ NA_real_),
        CNSR = coalesce(CNSR, 0),
        EVNTDESC = case_when(
            PARAMCD == "TTOS" & CNSR == 0 ~ "EVENT: DEATH DUE TO ANY CAUSE",
            PARAMCD == "TTOS" & CNSR == 1 ~ "CENSORED AS OF DATE SPONSOR DECIDED TO TERMINATE THE STUDY",
            PARAMCD == "TTOS" & CNSR == 2 ~ "CENSORED AS OF DATE DUE TO LOST TO FOLLOW-UP",
            PARAMCD == "TTOS" & CNSR == 3 ~ "CENSORED AS OF DATE SUBJECT DECIDED TO WITHDRAW",
            PARAMCD == "TTOS" & CNSR == 4 ~ "CENSORED AS OF DATE OF WITHDRAWAL DUE TO OTHER REASONS",
            PARAMCD == "TTOS" & CNSR == 5 ~ "CENSORED AS OF DATE OF DISEASE PROGRESSION",
            PARAMCD == "TTPFS" & !is.na(DTHDT_ADJ) & !is.na(FPDDT) ~
                case_when(FPDDT <= DTHDT_ADJ ~ "DISEASE PROGRESSION", TRUE ~ "DEATH"),
            PARAMCD == "TTPFS" & is.na(DTHDT_ADJ) & !is.na(FPDDT) ~ "DISEASE PROGRESSION",
            PARAMCD == "TTPFS" & !is.na(DTHDT_ADJ) & is.na(FPDDT) ~ "DEATH",
            PARAMCD == "TTPFS" & !is.na(TUMLDT) ~ "CENSORED AS OF LAST TUMOR SCAN DATE",
            PARAMCD == "TTPFS" ~ "CENSORED AS OF RANDOMIZATION DATE",
            PARAMCD == "TTPFS125" & (!is.na(DTHDT_ADJ) | !is.na(FPDDT) | !is.na(FCA125DT)) ~
                case_when(
                    !is.na(DTHDT_ADJ) & (is.na(FPDDT) | DTHDT_ADJ <= FPDDT) &
                        (is.na(FCA125DT) | DTHDT_ADJ <= FCA125DT) ~ "DEATH",
                    !is.na(FPDDT) & (is.na(DTHDT_ADJ) | FPDDT < DTHDT_ADJ) &
                        (is.na(FCA125DT) | FPDDT <= FCA125DT) ~ "DISEASE PROGRESSION",
                    !is.na(FCA125DT) ~ "CA-125 CRITERIA AS DISEASE PROGRESSION"),
            PARAMCD == "TTPFS125" & !is.na(TUMLDT) & (is.na(LCA125DT) | TUMLDT >= LCA125DT) ~
                "CENSORED AS OF LAST TUMOR SCAN DATE",
            PARAMCD == "TTPFS125" & !is.na(LCA125DT) ~
                "CENSORED AS OF LAST CA-125 LAB ASSESSMENT DATE",
            PARAMCD == "TTPFS125" ~ "CENSORED AS OF RANDOMIZATION DATE",
            TRUE ~ NA_character_)) %>%
    select(-DS_ADT, -DSDECOD, -evt, -sponsor, -ltofu, -wdraw, -other, -prog, -f_evt, -f_c125) %>%
    # Analysis value in months
    mutate(AVAL = as.numeric(ADT - STARTDT + 1) / 30.4375) %>%
    filter(!(PARAMCD == "TTPFS125" & CA125FL != "Y")) %>%
    arrange(USUBJID, PARAMCD)

adtte <- adtte %>% select(names(val$adtte))

# ADaM ADEX ---------------------------------------------------------------
adex <- adsl %>% mutate(TRTP = TRT01P)

# Treatment duration from first to last dose
txdur <- adex %>%
    mutate(
        PARAM = "Duration of Treatment Received (months)",
        PARAMCD = "TXDUR",
        AVAL = case_when(!is.na(TRTSDT) & !is.na(TRTEDT) ~
            (as.numeric(TRTEDT) - as.numeric(TRTSDT) + 1) / 30.4375),
        DTYPE = "DIFFERENCE")

# Total capsules taken from DA
da_v <- raw$da %>%
    distinct(USUBJID, DARFTDTC, DADTC, DATESTCD, DAORRES, .keep_all = TRUE) %>%
    filter(DATESTCD == "TAKENAMT") %>%
    group_by(USUBJID) %>%
    summarise(AVAL = sum(DASTRESN, na.rm = TRUE), .groups = "drop")

# Total capsules
cumcap <- adex %>%
    left_join(da_v, by = "USUBJID") %>%
    mutate(
        PARAM = "Total Number of 150mg Capsules Taken",
        PARAMCD = "CUMCAP",
        AVAL = case_when(is.na(AVAL) ~ 0, TRUE ~ AVAL),
        DTYPE = "SUM")

# Total cumulative dose in grams
cumdose <- cumcap %>%
    mutate(
        PARAM = "Total Cumulative Dose (g)",
        PARAMCD = "CUMDOSE",
        AVAL = case_when(!is.na(AVAL) ~ (AVAL * 150) / 1000),
        DTYPE = "SUM")

# Dose intensity percentage
intens <- cumcap %>%
    mutate(
        PARAM = "Dose Intensity (%)",
        PARAMCD = "INTENS",
        DAYS = as.numeric(as.numeric(TRTEDT) - as.numeric(TRTSDT) + 1),
        AVAL = case_when(!is.na(DAYS) & DAYS > 0 ~ (AVAL / DAYS) * 100),
        DTYPE = "PERCENTAGE") %>%
    select(-DAYS)

# Combine all exposure parameters
adex <- bind_rows(txdur, cumcap, cumdose, intens) %>%
    arrange(USUBJID, PARAMCD)

adex <- adex %>% select(names(val$adex))

# Comparing with validation datasets --------------------------------------
# Copy column attributes  from validation data to derived data
for (col in colnames(val$adsl)) {
    if (col %in% colnames(adsl)) {
        attributes(adsl[[col]]) <- attributes(val$adsl[[col]])}}
for (col in colnames(val$adae)) {
    if (col %in% colnames(adae)) {
        attributes(adae[[col]]) <- attributes(val$adae[[col]])}}
for (col in colnames(val$adlbsi)) {
    if (col %in% colnames(adlbsi)) {
        attributes(adlbsi[[col]]) <- attributes(val$adlbsi[[col]])}}
for (col in colnames(val$adtte)) {
    if (col %in% colnames(adtte)) {
        attributes(adtte[[col]]) <- attributes(val$adtte[[col]])}}
for (col in colnames(val$adex)) {
    if (col %in% colnames(adex)) {
        attributes(adex[[col]]) <- attributes(val$adex[[col]])}}

# Copy data frame attributes 
comment(adsl) <- comment(val$adsl)
comment(adae) <- comment(val$adae)
attributes(adae) <- attributes(val$adae)
comment(adlbsi) <- comment(val$adlbsi)
comment(adtte) <- comment(val$adtte)
comment(adex) <- comment(val$adex)

# Run comparisons
# ADSL - IDENTICAL
identical(adsl, val$adsl)
all.equal(adsl, val$adsl)
anti_join(adsl, val$adsl, by = "USUBJID")
anti_join(val$adsl, adsl, by = "USUBJID")
# ADAE - IDENTICAL
identical(adae, val$adae)
all.equal(adae, val$adae)
anti_join(adae, val$adae, by = "USUBJID")
anti_join(val$adae, adae, by = "USUBJID")
# ADLBSI - IDENTICAL
identical(adlbsi, val$adlbsi)
all.equal(adlbsi, val$adlbsi)
anti_join(adlbsi, val$adlbsi, by = "USUBJID")
anti_join(val$adlbsi, adlbsi, by = "USUBJID")
# ADTTE - IDENTICAL
identical(adtte, val$adtte)
all.equal(adtte, val$adtte)
anti_join(adtte, val$adtte, by = "USUBJID")
anti_join(val$adtte, adtte, by = "USUBJID")
# ADEX - IDENTICAL
identical(adex, val$adex)
all.equal(adex, val$adex)
anti_join(adex, val$adex, by = "USUBJID")
anti_join(val$adex, adex, by = "USUBJID")