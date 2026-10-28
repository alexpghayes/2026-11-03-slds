
library(RSienaTest)
library(tidyverse)
library(parallel)
setwd("D:/shihanl/Tempory/Stress")

load("JPSP Int RData/JPSP_rep_data.RData")

# ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------
# P1 - Specify Siena variables
# ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Set network and stress depenent variables


saom_expr_p315 <- sienaDependent(array(c(t1_expr_p315_na, t2_expr_p315_na, t3_expr_p315_na, t4_expr_p315_na),
                                       dim = c(315, 315, 4)))

saom_pss_p315 <- sienaDependent(as.matrix(JPSP_SAOM_pss_df_rep %>% dplyr::select(pss_t1_round, pss_t2_round, pss_t3_round, pss_t4_round)), type = "behavior")


# Set covariates

saom_female_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["Female"]])
saom_US_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["US"]])
saom_White_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["White"]])
saom_MISM_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["MISM"]])
saom_P_neu_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_neu"]])
saom_P_con_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_con"]])
saom_P_loc_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_loc"]])
saom_P_ext_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_ext"]])
saom_P_agr_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_agr"]])
saom_P_ope_p315 <- coCovar(JPSP_SAOM_pss_df_rep[["P_ope"]])
saom_workload_p315 <- varCovar(cbind(JPSP_SAOM_pss_df_rep[["Hrs.Per.Week.mini1.total"]], JPSP_SAOM_pss_df_rep[["Hrs.Per.Week.mini2.total"]], JPSP_SAOM_pss_df_rep[["Hrs.Per.Week.mini3.total"]]))
saom_outsup_p315 <- varCovar(cbind(JPSP_SAOM_pss_df_rep[["outsup_t1"]], JPSP_SAOM_pss_df_rep[["outsup_t2"]], JPSP_SAOM_pss_df_rep[["outsup_t3"]], JPSP_SAOM_pss_df_rep[["outsup_t4"]]))


saom_same_gender_p315 <- coDyadCovar(same.gender.mat.p315)
saom_same_eth_p315 <- coDyadCovar(same.eth.mat.p315)
saom_same_citi_p315 <- coDyadCovar(same.citi.mat.p315)
saom_same_prog_p315 <- coDyadCovar(same.prog.mat.p315)
saom_same_csq_p315 <- coDyadCovar(same.csq.mat.p315)

saom_same_course_p315 <- varDyadCovar(array(c(same.course.mat.t1.p315, same.course.mat.t2.p315, same.course.mat.t3.p315),
                                     dim = c(315, 315, 3)))

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# P2 - Test perceived stress in most parsimonious model
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

SDS_expr_pss.rep <- sienaDataCreate(saom_expr_p315, saom_pss_p315,
                                       saom_female_p315, saom_White_p315, saom_US_p315, saom_workload_p315, saom_outsup_p315, saom_MISM_p315,
                                       saom_P_neu_p315, saom_P_con_p315, saom_P_loc_p315, saom_P_ext_p315, saom_P_agr_p315, saom_P_ope_p315,
                                       saom_same_prog_p315, saom_same_eth_p315, saom_same_citi_p315, saom_same_csq_p315, 
                                       saom_same_gender_p315, saom_same_course_p315
)
siena_algorithm <- sienaAlgorithmCreate(projname = "expr_pss", nsub = 6, n3 = 10000)

# model 1, simple, main
eff_expr_pss.sh.ma.rep <- getEffects(SDS_expr_pss.rep)
eff_expr_pss.sh.ma.rep <- includeEffects(eff_expr_pss.sh.ma.rep, recip, outPopSqrt, outActSqrt, inPopSqrt,
                                             gwespFF, gwespBB)
eff_expr_pss.sh.ma.rep <- includeInteraction(eff_expr_pss.sh.ma.rep, recip, gwespFF)
eff_expr_pss.sh.ma.rep <- includeEffects(eff_expr_pss.sh.ma.rep, egoX, altX, simX, interaction1 = "saom_pss_p315")
eff_expr_pss.sh.ma.rep <- includeEffects(eff_expr_pss.sh.ma.rep, name = "saom_pss_p315", avSim, interaction1 = "saom_expr_p315")

SAOM_expr_pss.sh.ma.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.sh.ma.rep, 
                                       returnDeps = FALSE, prevAns = FALSE, useCluster = TRUE, nbrNodes = 64, verbose = TRUE)
while (SAOM_expr_pss.sh.ma.rep[["tconv.max"]] >= 0.25 | max(abs(SAOM_expr_pss.sh.ma.rep[["tconv"]])) >= 0.1) {
  SAOM_expr_pss.sh.ma.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.sh.ma.rep, 
                                         returnDeps = FALSE, prevAns = SAOM_expr_pss.sh.ma.rep, useCluster = TRUE, nbrNodes = 64)
}


# model 2, simple, moderation
eff_expr_pss.sh.it.rep <- getEffects(SDS_expr_pss.rep)
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, recip, outPopSqrt, outActSqrt, inPopSqrt,
                                             gwespFF, gwespBB)
eff_expr_pss.sh.it.rep <- includeInteraction(eff_expr_pss.sh.it.rep, recip, gwespFF)
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, egoX, altX, simX, interaction1 = "saom_pss_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, name = "saom_pss_p315", avSim, interaction1 = "saom_expr_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_loc_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_neu_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_con_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, varAlt, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, avSimVarAlt, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_loc_p315", interaction2 = "saom_expr_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_con_p315", interaction2 = "saom_expr_p315")
eff_expr_pss.sh.it.rep <- includeEffects(eff_expr_pss.sh.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_neu_p315", interaction2 = "saom_expr_p315")

SAOM_expr_pss.sh.it.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.sh.it.rep, 
                                       returnDeps = FALSE, prevAns = FALSE, useCluster = TRUE, nbrNodes = 64, verbose = TRUE)
while (SAOM_expr_pss.sh.it.rep[["tconv.max"]] >= 0.25 | max(abs(SAOM_expr_pss.sh.it.rep[["tconv"]])) >= 0.1) {
  SAOM_expr_pss.sh.it.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.sh.it.rep,
                                         returnDeps = FALSE, prevAns = SAOM_expr_pss.sh.it.rep, useCluster = TRUE, nbrNodes = 64)
}


# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# P3 - Test perceived stress in long models with all covariates
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# model 3, long, main
eff_expr_pss.lo.ma.rep <- getEffects(SDS_expr_pss.rep)
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, recip, outPopSqrt, outActSqrt, inPopSqrt,
                                             gwespFF, gwespBB)
eff_expr_pss.lo.ma.rep <- includeInteraction(eff_expr_pss.lo.ma.rep, recip, gwespFF)
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, egoX, altX, simX, interaction1 = "saom_pss_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_loc_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_neu_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_ext_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_agr_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_ope_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_P_con_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, egoX, altX, interaction1 = "saom_female_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, simX, interaction1 = "saom_workload_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_gender_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_eth_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_citi_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_prog_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_csq_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, X, interaction1 = "saom_same_course_p315")

eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, name = "saom_pss_p315", avSim, interaction1 = "saom_expr_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_US_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_female_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_White_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_outsup_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_workload_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_loc_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_neu_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_ext_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_agr_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_con_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_ope_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, outdeg, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.lo.ma.rep <- includeEffects(eff_expr_pss.lo.ma.rep, varAlt, name = "saom_pss_p315", interaction1 = "saom_expr_p315")

SAOM_expr_pss.lo.ma.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.lo.ma.rep, 
                                       returnDeps = FALSE, prevAns = FALSE, useCluster = TRUE, nbrNodes = 64, verbose = TRUE)
while (SAOM_expr_pss.lo.ma.rep[["tconv.max"]] >= 0.25 | max(abs(SAOM_expr_pss.lo.ma.rep[["tconv"]])) >= 0.1) {
  SAOM_expr_pss.lo.ma.rep <- siena07(siena_algorithm, data = SDS_expr_pss.rep, effects = eff_expr_pss.lo.ma.rep,
                                         returnDeps = FALSE, prevAns = SAOM_expr_pss.lo.ma.rep, useCluster = TRUE, nbrNodes = 64)
}


# model 4, long, moderation
eff_expr_pss.lo.it.rep <- getEffects(SDS_expr_pss.rep)
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, recip, outPopSqrt, outActSqrt, inPopSqrt,
                                             gwespFF, gwespBB)
eff_expr_pss.lo.it.rep <- includeInteraction(eff_expr_pss.lo.it.rep, recip, gwespFF)
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, egoX, altX, simX, interaction1 = "saom_pss_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_loc_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_neu_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_ext_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_agr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_ope_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_P_con_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, egoX, altX, interaction1 = "saom_female_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, simX, interaction1 = "saom_workload_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_gender_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_eth_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_citi_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_prog_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_csq_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, X, interaction1 = "saom_same_course_p315")

eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, name = "saom_pss_p315", avSim, interaction1 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_US_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_female_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_White_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_outsup_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_workload_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_loc_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_neu_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_ext_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_agr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_con_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, effFrom, name = "saom_pss_p315", interaction1 = "saom_P_ope_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, outdeg, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, varAlt, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, avSimVarAlt, name = "saom_pss_p315", interaction1 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_loc_p315", interaction2 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_con_p315", interaction2 = "saom_expr_p315")
eff_expr_pss.lo.it.rep <- includeEffects(eff_expr_pss.lo.it.rep, avSimEgoX, name = "saom_pss_p315", interaction1 = "saom_P_neu_p315", interaction2 = "saom_expr_p315")

SAOM_expr_pss.lo.it.rep <- siena07(siena_algorithm.v01, data = SDS_expr_pss.rep.m1, effects = eff_expr_pss.lo.it.rep, 
                                       returnDeps = FALSE, prevAns = FALSE, useCluster = TRUE, nbrNodes = 64, verbose = TRUE)
while (SAOM_expr_pss.lo.it.rep[["tconv.max"]] >= 0.25 | max(abs(SAOM_expr_pss.lo.it.rep[["tconv"]])) >= 0.1) {
  SAOM_expr_pss.lo.it.rep <- siena07(siena_algorithm.v01, data = SDS_expr_pss.rep.m1, effects = eff_expr_pss.lo.it.rep, 
                                         returnDeps = FALSE, prevAns = SAOM_expr_pss.lo.it.rep, useCluster = TRUE, nbrNodes = 64)
}

