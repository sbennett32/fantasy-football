---
title: Value over Replacement Script"
author: "Sam Bennett"
output: pdf_document
---

```{r packages, include=FALSE}

library(tidyverse)
library(RODBC)
library(raysmisc)
library(lme4)
library(readr)
library(DT)

# ══════════════════════════════════════════════════════════════════
# TODO — three changes needed for correct PPR VORP in a configurable
# league (notes only, nothing below this block has been changed):
#
# 1. SCORING (currently STANDARD, 0 pts/reception):
#    Every PTS_<Analyst> below is `PPG*17` off the raw per-analyst 4pt
#    files (e.g. line ~167 `PTS_Andy = PPG*17`, and the Jason/Mike
#    equivalents) — that's standard scoring. To get PPR:
#      (a) swap these blocks to read udk-projections-blended.csv
#          (in the dashboard repo root) and use its `proj_ppr` column
#          instead of recomputing from the raw per-analyst files, or
#      (b) keep reading the raw per-analyst files but add receptions:
#          PPR points = standard points + REC. The raw per-analyst
#          files already have a REC column for RB/WR/TE, so this is
#          exact, not an estimate — just add `+ REC` next to each
#          `PPG*17` (QBs stay PPG*17 unchanged; REC is 0 for them).
#    Option (a) is simpler and matches what the dashboard's Rankings
#    tab now does; option (b) keeps this script self-contained.
#
# 2. LEAGUE SIZE: hardcoded `n = 8` below (~line 98). Expose it as a
#    parameter instead. RBs_Drafted/WRs_Drafted/etc. all derive from
#    n and will follow automatically — EXCEPT ADP_File_Name (~line
#    302), which builds "UDK_<n>tm_ADP.csv" and needs either a
#    matching file for the new n or a repoint to the FantasyPros ADP
#    already in the dashboard repo (ppr-adps.csv / standard-adps.csv).
#
# 3. ROSTER SLOTS: p_RB=2.5, p_WR=3.5, p_QB=1, p_TE=1 (~lines 101-118)
#    are the friend's 8-team WR-heavy build. For a 2RB/2WR/1TE/1QB/
#    1-2 FLEX league these starter counts need to change — they set
#    replacement level, which VORP is measured against, so getting
#    them right matters as much as the scoring fix.
#
# Item 1 is the one that was previously blocked (no real reception
# data) and is now unblocked by udk-projections-blended.csv.
# ══════════════════════════════════════════════════════════════════

# Read in the file
UDK <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_top200.csv")

ColumnsOfInterest <-
  UDK %>%
  mutate(BlendedRank = (Andy+Jason+Mike)/3) %>%
  mutate(AndyWeightedRank = (Andy*0.50)+(Jason*0.25)+(Mike*0.25)) %>%
  select(Name, Team, Pos, Rank, Andy, Jason, Mike, BlendedRank, AndyWeightedRank)

RB_Players <- 
  ColumnsOfInterest %>% 
  filter(Pos == "RB") %>% 
  arrange(BlendedRank) %>%
  mutate(BlendedRank_RowNum =row_number()) %>%
  arrange(AndyWeightedRank) %>%
  mutate(AndyRank_RowNum = row_number()) %>%
  arrange(Rank) %>%
  mutate(PosRank = row_number()) %>%
  rename("Top200Rank" = "Rank") %>%
  select(Name, Team, Pos, Top200Rank, BlendedRank, AndyWeightedRank, BlendedRank_RowNum,
         AndyRank_RowNum, PosRank)

WR_Players <- 
  ColumnsOfInterest %>% 
  filter(Pos == "WR") %>% 
  arrange(BlendedRank) %>%
  mutate(BlendedRank_RowNum =row_number()) %>%
  arrange(AndyWeightedRank) %>%
  mutate(AndyRank_RowNum = row_number()) %>%
  arrange(Rank) %>%
  mutate(PosRank = row_number()) %>%
  rename("Top200Rank" = "Rank") %>%
  select(Name, Team, Pos, Top200Rank, BlendedRank, AndyWeightedRank, BlendedRank_RowNum,
         AndyRank_RowNum, PosRank)

QB_Players <- 
  ColumnsOfInterest %>% 
  filter(Pos == "QB") %>% 
  arrange(BlendedRank) %>%
  mutate(BlendedRank_RowNum =row_number()) %>%
  arrange(AndyWeightedRank) %>%
  mutate(AndyRank_RowNum = row_number()) %>%
  arrange(Rank) %>%
  mutate(PosRank = row_number()) %>%
  rename("Top200Rank" = "Rank") %>%
  select(Name, Team, Pos, Top200Rank, BlendedRank, AndyWeightedRank, BlendedRank_RowNum,
         AndyRank_RowNum, PosRank)

TE_Players <- 
  ColumnsOfInterest %>% 
  filter(Pos == "TE") %>% 
  arrange(BlendedRank) %>%
  mutate(BlendedRank_RowNum =row_number()) %>%
  arrange(AndyWeightedRank) %>%
  mutate(AndyRank_RowNum = row_number()) %>%
  arrange(Rank) %>%
  mutate(PosRank = row_number()) %>%
  rename("Top200Rank" = "Rank") %>%
  select(Name, Team, Pos, Top200Rank, BlendedRank, AndyWeightedRank, BlendedRank_RowNum,
         AndyRank_RowNum, PosRank)


# Get VORP
# 3 VORPS
# 1. PosRank Vorp based on Top200
# 2. BlendedRank Vorp
# 3. AndyRank Vorp

# 1 Starting QB per team
# 2 Starting RBs per team
# 2 Starting WRs per team
# 1 Starting TE per team
# 1 Flex
# 7 Bench Spots (assumed QB, RB, RB, RB, WR, WR, WR)
# Flex could also be a WR/RB. 
# For now treating this as an 8-Team League with those starters above
# n = # of teams = 12
# p_pos = # of starters per position, p_pos = 2 for RBs
# b_pos = # of players per position on the bench, b_pos = 2 for RBs

# Number of Teams
n = 8

# RBs
p_RB = 2.5 # number of starters for RBs per team
b_RB = 2.75 # number of ASSUMED bench RBs per team
RBs_Drafted = n*(p_RB+b_RB)

# QBs
p_QB = 1 # number of starters for QBs per team
b_QB = 1 # number of ASSUMED bench QBs per team
QBs_Drafted = n*(p_QB+b_QB)

# WRs
p_WR = 3.5 # number of starters for WRs per team
b_WR = 2.75 # number of ASSUMED bench WRs per team
WRs_Drafted = n*(p_WR+b_WR)

# TEs
p_TE = 1 # number of starters for TEs per team
b_TE = 0.5 # number of ASSUMED bench TEs per team
TEs_Drafted = n*(p_TE+b_TE)

QB_VORP <- QB_Players %>%
  mutate(VORP_BlendedRank_RowNum = QBs_Drafted-BlendedRank_RowNum) %>%
  mutate(VORP_AndyRank_RowNum = QBs_Drafted-AndyRank_RowNum) %>%
  mutate(VORP_PosRank_RowNum = QBs_Drafted-PosRank) %>%
  select(Name, Team, Pos, Top200Rank, 
         PosRank, VORP_PosRank_RowNum,
         AndyRank_RowNum, VORP_AndyRank_RowNum,
         BlendedRank_RowNum, VORP_BlendedRank_RowNum)
  
RB_VORP <- RB_Players %>%
  mutate(VORP_BlendedRank_RowNum = RBs_Drafted-BlendedRank_RowNum) %>%
  mutate(VORP_AndyRank_RowNum = RBs_Drafted-AndyRank_RowNum) %>%
  mutate(VORP_PosRank_RowNum = RBs_Drafted-PosRank) %>%
  select(Name, Team, Pos, Top200Rank, 
         PosRank, VORP_PosRank_RowNum,
         AndyRank_RowNum, VORP_AndyRank_RowNum,
         BlendedRank_RowNum, VORP_BlendedRank_RowNum)

WR_VORP <- WR_Players %>%
  mutate(VORP_BlendedRank_RowNum = WRs_Drafted-BlendedRank_RowNum) %>%
  mutate(VORP_AndyRank_RowNum = WRs_Drafted-AndyRank_RowNum) %>%
  mutate(VORP_PosRank_RowNum = WRs_Drafted-PosRank) %>%
  select(Name, Team, Pos, Top200Rank, 
         PosRank, VORP_PosRank_RowNum,
         AndyRank_RowNum, VORP_AndyRank_RowNum,
         BlendedRank_RowNum, VORP_BlendedRank_RowNum)

TE_VORP <- TE_Players %>%
  mutate(VORP_BlendedRank_RowNum = TEs_Drafted-BlendedRank_RowNum) %>%
  mutate(VORP_AndyRank_RowNum = TEs_Drafted-AndyRank_RowNum) %>%
  mutate(VORP_PosRank_RowNum = TEs_Drafted-PosRank) %>%
  select(Name, Team, Pos, Top200Rank, 
         PosRank, VORP_PosRank_RowNum,
         AndyRank_RowNum, VORP_AndyRank_RowNum,
         BlendedRank_RowNum, VORP_BlendedRank_RowNum)

totalVORP <- rbind(QB_VORP, RB_VORP, WR_VORP, TE_VORP)


# write.csv(x = totalVORP, file = "/data/shared/idiosyncranic/totalVORP_2023.csv")

# Point Style VORP

# READ IN YOUR CSVS HERE

# Andy Section
QBProj_Andy <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_QB_Andy.csv") %>% 
  mutate(PTS_Andy = PPG*17) %>%
  # rename("PTS_Andy" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Andy)

RBProj_Andy <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_RB_Andy.csv") %>% 
  mutate(PTS_Andy = PPG*17) %>%
  # rename("PTS_Andy" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Andy)

WRProj_Andy <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_WR_Andy.csv")  %>% 
  mutate(PTS_Andy = PPG*17) %>%
  # rename("PTS_Andy" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Andy)

TEProj_Andy <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_TE_Andy.csv") %>%  
  mutate(PTS_Andy = PPG*17) %>%
  # rename("PTS_Andy" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Andy)

# Jason Section
QBProj_Jason <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_QB_Jason.csv") %>% 
  mutate(PTS_Jason = PPG*17) %>%
  # rename("PTS_Jason" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Jason) 

RBProj_Jason <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_RB_Jason.csv") %>%  
  mutate(PTS_Jason = PPG*17) %>%
  # rename("PTS_Jason" = "Points") %>%  
  select(Name, Team, 'Bye Week', PTS_Jason) 

WRProj_Jason <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_WR_Jason.csv") %>%  
  mutate(PTS_Jason = PPG*17) %>%
  # rename("PTS_Jason" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Jason) 
 
TEProj_Jason <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_TE_Jason.csv") %>%  
  mutate(PTS_Jason = PPG*17) %>%
  # rename("PTS_Jason" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Jason) 

# Mike Section
QBProj_Mike <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_QB_Mike.csv") %>%  
  mutate(PTS_Mike = PPG*17) %>%
  # rename("PTS_Mike" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Mike) 

RBProj_Mike <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_RB_Mike.csv") %>%
  mutate(PTS_Mike = PPG*17) %>%
  # rename("PTS_Mike" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Mike) 

WRProj_Mike <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_WR_Mike.csv") %>% 
  mutate(PTS_Mike = PPG*17) %>%
  # rename("PTS_Mike" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Mike) 

TEProj_Mike <- read_csv("/data/shared/idiosyncranic/udk_rankings/2026_UDK_4pt_TE_Mike.csv") %>% 
  mutate(PTS_Mike = PPG*17) %>%
  # rename("PTS_Mike" = "Points") %>% 
  select(Name, Team, 'Bye Week', PTS_Mike) 

QBProj <- QBProj_Andy %>% 
  inner_join(QBProj_Jason, by = c("Name", "Team", "Bye Week")) %>% 
  inner_join(QBProj_Mike, by = c("Name", "Team", "Bye Week")) %>%
  mutate(PTS_Blended = .50*PTS_Andy + .25*PTS_Mike + .25*PTS_Jason) %>%
  select(Name, Team, 'Bye Week', PTS_Blended)

QB_P_VORP <- QBProj %>% 
  arrange(desc(PTS_Blended)) %>%
  mutate(PONAP = (PTS_Blended - lead(PTS_Blended))) %>%
  mutate("Pos" = "QB") %>%
  mutate(Rank = row_number()) %>%
  mutate(Vorp_Rank = QBs_Drafted - Rank) %>%
  select(Name, Team, `Bye Week`, Pos, Rank, Vorp_Rank, PTS_Blended, PONAP) 

RBProj <- RBProj_Andy %>% 
  inner_join(RBProj_Jason, by = c("Name", "Team", "Bye Week")) %>% 
  inner_join(RBProj_Mike, by = c("Name", "Team", "Bye Week")) %>%
  mutate(PTS_Blended = .50*PTS_Andy + .25*PTS_Mike + .25*PTS_Jason) %>%
  select(Name, Team, 'Bye Week', PTS_Blended)

RB_P_VORP <- RBProj %>% 
  arrange(desc(PTS_Blended)) %>%
  mutate(PONAP = (PTS_Blended - lead(PTS_Blended))) %>% 
  mutate("Pos" = "RB") %>%
  mutate(Rank = row_number()) %>%
  mutate(Vorp_Rank = RBs_Drafted - Rank) %>%
  select(Name, Team, `Bye Week`, Pos, Rank, Vorp_Rank, PTS_Blended, PONAP) 

WRProj <- WRProj_Andy %>% 
  inner_join(WRProj_Jason, by = c("Name", "Team", "Bye Week")) %>% 
  inner_join(WRProj_Mike, by = c("Name", "Team", "Bye Week")) %>%
  mutate(PTS_Blended = .50*PTS_Andy + .25*PTS_Mike + .25*PTS_Jason) %>%
  select(Name, Team, 'Bye Week', PTS_Blended)

WR_P_VORP <- WRProj %>% 
  arrange(desc(PTS_Blended)) %>%
  mutate(PONAP = (PTS_Blended - lead(PTS_Blended))) %>% 
  mutate("Pos" = "WR") %>%
  mutate(Rank = row_number()) %>%
  mutate(Vorp_Rank = WRs_Drafted - Rank) %>%
  select(Name, Team, `Bye Week`, Pos, Rank, Vorp_Rank, PTS_Blended, PONAP) 

TEProj <- TEProj_Andy %>% 
  inner_join(TEProj_Jason, by = c("Name", "Team", "Bye Week")) %>% 
  inner_join(TEProj_Mike, by = c("Name", "Team", "Bye Week")) %>%
  mutate(PTS_Blended = .50*PTS_Andy + .25*PTS_Mike + .25*PTS_Jason) %>%
  select(Name, Team, 'Bye Week', PTS_Blended)

TE_P_VORP <- TEProj %>% 
  arrange(desc(PTS_Blended)) %>%
  mutate(PONAP = (PTS_Blended - lead(PTS_Blended))) %>%
  mutate("Pos" = "TE") %>%
  mutate(Rank = row_number()) %>%
  mutate(Vorp_Rank = TEs_Drafted - Rank) %>%
  select(Name, Team, `Bye Week`, Pos, Rank, Vorp_Rank, PTS_Blended, PONAP) 


total_Proj_VORP <- rbind(QB_P_VORP, RB_P_VORP, WR_P_VORP, TE_P_VORP)


# Order by positions and and VORP_AndyRank largest to smallest
# For each position subtract pts from player with -1 below replacement


ReplacementLevel <- total_Proj_VORP %>% 
  filter(Vorp_Rank == 0) %>% 
  distinct(Pos, PTS_Blended) %>% 
  rename(ReplacementLevelPoints = PTS_Blended)

StandardValues <- total_Proj_VORP %>%
  group_by(Pos) %>%
  summarise(meanScore = mean(PTS_Blended),
            stdDev = sd(PTS_Blended))

ADP_File_Name <- paste0("/data/shared/idiosyncranic/udk_rankings/UDK_", n,"tm_ADP.csv")

ADP <- read.csv(ADP_File_Name)

Output <- total_Proj_VORP %>% 
  inner_join(StandardValues, by = "Pos") %>%
  inner_join(ReplacementLevel, by = "Pos") %>%
  mutate(ZScore = (PTS_Blended - meanScore)/stdDev) %>%
  select(-meanScore, -stdDev) %>%
  arrange(-ZScore) %>%
  inner_join(ADP %>% mutate(ESPN = PPR) %>% select(Name, ESPN), by = c("Name")) %>%
  rename(
    PosRank = Rank,
    PosVORPRank = Vorp_Rank,
    Projection = PTS_Blended,
    PointsOverNextAvailableAtPosition = PONAP,
    ESPN_ADP = ESPN
  ) %>%
  mutate(VORP = Projection - ReplacementLevelPoints) %>%
  select(Name, Team, Pos, ESPN_ADP, Projection, VORP, ZScore, PointsOverNextAvailableAtPosition, PosRank, PosVORPRank) %>%
  arrange(-VORP)


write.csv(x = Output, file = "/data/shared/idiosyncranic/2026_VORP_8manBakery.csv")

```

