#### SETUP ####
rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, stringr, tidyverse, lubridate)

#### READ DATA ####
meta <- read.csv(here::here("data/2025_data", "fl_rls_metadata_newformat.csv"))
validator <- read.csv(here::here("data/2025_data", "rls_validator_data.csv"))

#### INITIAL CLEANING AND STANDARDIZATION ####
# standardize date and time formats
meta$Date <- dmy(meta$Date)
validator$Date <- mdy(validator$Date)


# check site codes are the same
setdiff( unique(validator$Site.No.), unique(meta$Site.No.))

# capitalization differences in some entries

validator_2 <- validator %>%
  mutate(Site.No. = case_when(
    Site.No. == "5p5" ~ "5P5",
    Site.No. == "5p1" ~ "5P1",
    Site.No. == "5s2" ~ "5S2",
    Site.No. == "2s1" ~ "2S1",
    Site.No. == "5d3" ~ "5D3",
    Site.No. == "5d4" ~ "5D4",
    Site.No. == "5s5" ~ "5S5",
    Site.No. == "2p1" ~ "2P1",
    Site.No. == "5p3" ~ "5P3",
    Site.No. == "2d1" ~ "2D1",
    Site.No. == "5p6" ~ "5P5",
    Site.No. == "5d5" ~ "5D5",
    Site.No. == "flk1" ~ "FLK1",
    Site.No. == "5p2" ~ "5P2",
    Site.No. == "5p5" ~ "5P5",
    Site.No. == "9p6" ~ "9P6",
    Site.No. == "9d4" ~ "9D4",
    Site.No. == "9s4" ~ "9S4",
    Site.No. == "9s2" ~ "9S2",
    Site.No. == "9p3" ~ "9P3",
    Site.No. == "9p4" ~ "9P4",
    Site.No. == "9d3" ~ "9D3",
    Site.No. == "7p2" ~ "7P2",
    Site.No. == "7p4" ~ "7P4",
    Site.No. == "5d1" ~ "5D1",
    Site.No. == "5s1" ~ "5S1",
    Site.No. == "5h1" ~ "5H1",
    Site.No. == "7h2" ~ "7H2",
    Site.No. == "7d2" ~ "7D2",
    Site.No. == "7s2" ~ "7S2",
    Site.No. == "7p1" ~ "7P1",
    Site.No. == "7p3" ~ "7P3",
    Site.No. == "3h1" ~ "3H1",
    T ~ Site.No.
  )) %>%
  mutate(Site.No. = case_when(
    Site.No. == "5P5" & Site.Name == "Red Dun Reef" ~ "5P6",
    T ~ Site.No.
  ))

# check site codes are the same
setdiff(  unique(meta$Site.No.), unique(validator_2$Site.No.))

# check site names are the same
setdiff(unique(validator_2$Site.Name), unique(meta$Site.Name))

# correct incorrect depths
validator_3 <- validator_2 %>%
  mutate(Depth = case_when(
    Site.Name == "Davis Rock" & Depth == 12 ~ 12.5,
    Site.Name == "Texas Rock" & Depth == 10.5 ~ 10,
    T ~ Depth
  ))

# MH accidentally included M1 fish on his M2s
# deleting those now
mistakes <- c("Halichoeres bivittatus", "Halichoeres garnoti", "Holacanthus ciliaris", "Ocyurus chrysurus",
              "Scarus coeruleus", "Scarus taeniopterus", "Sparisoma viride", "Stegastes diencaeus", 
              "Stegastes leucostictus", "Stegastes partitus")
m1 <- subset(validator_3, Method == 1)
m0 <- subset(validator_3, Method == 0)
m2 <- subset(validator_3, Method == 2)
m2_2 <- m2[ ! m2$Species %in% mistakes, ]

# recombine
validator_4 <- m1 %>%
  rbind(m2_2) %>%
  rbind(m0)

# correct outdated names 
validator_5 <- validator_4 %>%
  mutate(Species = case_when(
    Species == "Haemulon chrysargyreum" ~ "Brachygenys chrysargereum",
    Species == "Rhinesomus triqueter" ~ "Lactophrys triqueter",
    Species == "Hypoplectrus sp." ~ "Hypoplectrus spp.",
    Species == "Hypoplectrus sp. [tan]" ~ "Hypoplectrus spp.",
    Species == "Hypoplectrus spp. [puella x floridae]" ~ "Hypoplectrus spp.",
    Species == "Chromis cyanea" ~ "Azurina cyanea",
    Species == "Chromis multilineata" ~ "Azurina multilineata",
    Species == "Acanthurus bahianus" ~ "Acanthurus tractus",
    Species == "Muraena spp." ~ "Muraenidae spp.",
    Species == "Hemiemblemaria simulus" ~ "Hemiemblemaria simula",
    Species == "Opistognathus whitehurstii" ~ "Opistognathus whitehursti",
    Species == "Brachygenys chrysargereum" ~ "Brachygenys chrysargyrea",
    Species == "Brachygenys chrysargyreum" ~ "Brachygenys chrysargyrea",
    Species == "Clepticus parrae" ~ "Bodianus parrae",
    Species == "Carangoides bartholomaei" ~ "Caranx bartholomaei",
    Species == "Acanthostracion polygonius" ~ "Acanthostracion polygonium",
    Species == "Sargocentron vexillarium" ~ "Neoniphon vexillarium",
    Species == "Stegastes xanthurus" ~ "Stegastes variabilis",
    Species == "Trachinotus blochii" ~ "Trachinotus carolinus",
    Species == "Scarus quoyi" ~ "Sparisoma atomarium",
    Species == "Centropyge argi" ~ "Cephalopholis cruentata",
    Species == "Strombus gigas" ~ "Aliger gigas",
    Species == "Periclimenes pedersoni" ~ "Ancylomenes pedersoni",
    Species == "Brachygenys chrysargereum" ~ "Brachygenys chrysargyrea",
    Species == "Brachygenys chrysargyreum" ~ "Brachygenys chrysargyrea",
    Species == "Clepticus parrae" ~ "Bodianus parrae",
    Species == "Carangoides bartholomaei" ~ "Caranx bartholomaei",
    Species == "Acanthostracion polygonius" ~ "Acanthostracion polygonium",
    Species == "Sargocentron vexillarium" ~ "Neoniphon vexillarium",
    Species == "Stegastes xanthurus" ~ "Stegastes variabilis",
    Species == "Trachinotus blochii" ~ "Trachinotus carolinus",
    Species == "Scarus quoyi" ~ "Sparisoma atomarium",
    Species == "Centropyge argi" ~ "Cephalopholis cruentata",
    T ~ Species))

#### INTERFACE VALIDATOR DATA WITH TRUE META ####
# separate out the relevant meta
meta_lookup <- meta %>%
  select(
    Site.No.,
    Date,
    Depth,
    Block,
    Diver,
    vis,
    Direction,
    Time,
    P.Qs
  ) %>%
  distinct()

# create a df that joins meta with validator 5, allowing you to check differences
validator_check <- validator_5 %>%
  left_join(
    meta_lookup,
    # add relevant meta
    by = c("Site.No.", "Date", "Depth", "Block"),
    # make sure that the cols from the meta df have the .meta suffix
    suffix = c("", ".meta")
  )

# find discrepancies
discrepancies <- validator_check %>%
  filter(
    vis != vis.meta |
      Direction != Direction.meta |
      Time != Time.meta |
      P.Qs != P.Qs.meta
  ) %>%
  select(
    ID,
    Site.No.,
    Date,
    Depth,
    Block,
    Species,
    vis,
    vis.meta,
    Direction,
    Direction.meta,
    Time,
    Time.meta,
    P.Qs,
    P.Qs.meta
  )

# document which rows will be changed and how
change_log <- validator_check %>%
  transmute(
    ID,
    Site.No.,
    Date,
    Depth,
    Block,
    
    vis_old = vis,
    vis_new = vis.meta,
    
    direction_old = Direction,
    direction_new = Direction.meta,
    
    time_old = Time,
    time_new = Time.meta,
    
    pqs_old = P.Qs,
    pqs_new = P.Qs.meta
  ) %>%
  filter(
    vis_old != vis_new |
      direction_old != direction_new |
      time_old != time_new |
      pqs_old != pqs_new
  ) %>%
  select(-ID) %>%
  distinct()
# View(change_log)
# all look fine to me

# correct the validator data and save
validator_6 <- validator_check %>%
  mutate(
    vis = vis.meta,
    Direction = Direction.meta,
    Time = Time.meta,
    P.Qs = P.Qs.meta
  ) %>%
  select(-ends_with(".meta"))

# write csv and manually correct size column names
# write.csv(validator_6, "data/corrected_data/rls_validator_epa_2025.csv", row.names = F)
