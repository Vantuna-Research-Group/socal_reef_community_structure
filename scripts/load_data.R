# CREATION DATE 28 Jan 2024
# MODIFIED DATE 14 Mar 2025

# AUTHOR: kitchel@oxy.edu

# PURPOSE: Pull in CRANE data for depth analyses

#############################
##Setup
#############################

library(data.table)
library(vegan)
library(dplyr)

############################
#DROPBOX STEPS
############################
#Added new SUBSET to SUBSET_Event_Fish_Swath_UPC_ISC.r
    #--> # "BOEM_depth_comparison": Subsets to all Natural and Artificial reefs in CRANE data (Islands: SB, SCL, SCLI, San Nic, Begg Rock, mainland: Malibu to SD regions)
        #Excludes samples <= 2015 to avoid heatwaves and sea star wasting, etc. Good starting year for baseline conditions of sort (avoids blob that ended in ~2015)

############################
#Are you loading and cleaning data for regular paper, for OSM poster (only want 2022-2023 data), or for Maggie's project?
############################
regular = TRUE
OSM = FALSE
maggie_gorgonian = FALSE

#############################
##Load data
#############################

# Read in reference tables (conversions, station names, species names, codes for UPC, ISC, etc.)
source("~/Dropbox/VRG Files/R Code/DataFiles/READ_Sites_Fish_BRS.r")

#Loads up and processes CRANE data
source("~/Dropbox/VRG Files/R Code/Integrated Dive General/CRANE_data_prep.R")

CRANE_data_prep(SELECT = "BOEM_depth_comparison",
                add_0s = "sp_0s", #summed to by Species with 0s added
                fish_prod = F, #check what this does, don't think I need it now
                #assign 0 to abundance and density for species-specific EstimatedLength below
                #Juv.cut.cm in conversions (i.e., remove YOYs from total density calculations)
                Rem_YOYs_dens = T,
                # rename Topsmelt & Jacksmelt to Atherinopsidae (hard to ID underwater)
                ## Also (from 2018 fish Biomass Analysis) CHANGE OYT TO OLIVE and  consolidate Atherinops affinis & Atherinopsis californiensis into Atherinopsidae (otherwise causes problems in community analyses)
                Atherin_Seb_ser_YOY = TRUE
) 
#NOTE that YOY ARE removed from density estimates but are NOT removed from biomass estimates (assumption that biomass contributions are small)


########################
##Split into fish, inverts, and kelp
########################
#EVENT data
  #dat_event

#FISH (both density (density_m2) and biomass (wt_density_g_m2))
#summed across rows with same Transect & Species, zeros added when not present!
  #dat_fish_t

#MACROINVERTS (SWATH)
  #swath_melt_T_sp, and then %in% "nudibranchs", "anemones", "sea cucumbers", "sea urchins",
            #"sea stars", "molluscs - mobile", "crustaceans", "sponges", "molluscs - sessile", "gorgonians",
            #"tunicates",  "hydrocorals", "sea pens" 
  dat_macroinvert <- swath_melt_T_sp %>%
    filter(SpeciesGroupF %in% c("nudibranchs", "anemones", "sea cucumbers", "sea urchins", "sea stars", "molluscs - mobile",
                                "crustaceans", "sponges", "molluscs - sessile", "gorgonians", "tunicates",  "hydrocorals", "sea pens"))

#KELP (SWATH)
  #swath_melt_T_sp, and then %in% "kelp - understory", "kelp - canopy" ,"Sargassum", "green algae", "giant kelp stipes"
    #      "giant kelp stipes" included
  dat_kelp <- swath_melt_T_sp %>%
    filter(SpeciesGroupF %in% c("kelp - understory", "kelp - canopy" ,"Sargassum", "green algae","giant kelp stipes"))

########################
##Add Level Orders to Region, Site, DepthZone (mostly from Jeremy's code)
########################
source("~/Dropbox/VRG Files/R Code/Integrated Dive General/CRANE_level_orders.R")

dat_fish_t <- CRANE_level_orders(
  dat_fish_t,
  PVR_Monitor_Cat = T, #need this so PVR_Reefing_Area works below
  AR_Region = T,
  AR_Complex = T,
  # reassign relevant SiteType & DepthZone (Halo) for Reefing Area
  PVR_Reefing_Area = T #after this can filter out DepthZone != "Halo"
  ) |> 
  filter(DepthZone != "Halo") |> #remove halo observations
  droplevels()

dat_macroinvert <- CRANE_level_orders(dat_macroinvert, AR_Region = T, AR_Complex = T) |> 
  droplevels()

dat_kelp <- CRANE_level_orders(dat_kelp, AR_Region = T, AR_Complex = T) |> 
  droplevels()

########################
##Filtering (mostly from Jeremy's code)
########################

# Filter out PVR Pre-Construction & sampling in 2020 right after construction
dat_fish_t <- dat_fish_t |>
  filter(!(str_detect(Site, "PVR") & SampleYear < 2021 )) |> 
  droplevels()

dat_fish_t <- dat_fish_t |>
  filter(!Site %in% c("Leucadia", "Old 18th")) |> #some depth zones were way out there in nMDS (probably very few species?)
  droplevels() 

dat_macroinvert <- dat_macroinvert |>
  filter(!(str_detect(Site, "PVR") & SampleYear < 2021 )) |> 
  droplevels()

dat_UPC_VRG <- dat_UPC_VRG |>
  filter(!(str_detect(Site, "PVR") & SampleYear < 2021 )) |> 
  droplevels()

dat_macroinvert <- dat_macroinvert |>
  filter(!Site %in% c("Leucadia", "Old 18th")) |> #some depth zones were way out there in nMDS (probably very few species?)
  droplevels() 

dat_kelp <- dat_kelp |>
  filter(!(str_detect(Site, "PVR") & SampleYear < 2021 )) |> 
  droplevels()

dat_kelp <- dat_kelp |>
  filter(!Site %in% c("Leucadia", "Old 18th")) |> #some depth zones were way out there in nMDS (probably very few species?)
  droplevels() 

dat_UPC_VRG <- dat_UPC_VRG |>
  filter(!Site %in% c("Leucadia", "Old 18th")) |> #some depth zones were way out there in nMDS (probably very few species?)
  droplevels() 

#Just in case this hasn't already happened, manually remove Sphyraena argentea, Sardinops sagax, Scomber japonicus, Trachurus symmetricus

#Pacific sardine (Sardinops sagax caerulea)
#Northern anchovy (Engraulis mordax)
#Pacific mackerel (Scomber japonicus)
#Jack mackerel (Trachurus symmetricus)
#Pacific barracuda (Sphyraena argentea)

dat_fish_t <- dat_fish_t |>
  filter(!(Species %in% c("Sphyraena argentea",
                          "Sardinops sagax",
                          "Scomber japonicus",
                          "Trachurus symmetricus",
                          "Engraulis mordax"
                          )))




########################
##Restrict to well sampled sites for averages (sampled in at least X years)
########################

#Switch to data table

dat_event <- data.table(dat_event)
dat_fish_t <- data.table(dat_fish_t)
dat_macroinvert <- data.table(dat_macroinvert)
dat_kelp <- data.table(dat_kelp)

#table with unique year and site values
unique_site_year <- unique(dat_event[,.(Site, SampleYear)])

#rank years in descending order
unique_site_year[,year_rank := frank(-SampleYear),Site]

#identify most recent year of sampling
unique_site_year[,most_recent := max(SampleYear),Site]

#total number of years of sampling
unique_site_year[,Years_sampled := uniqueN(SampleYear),Site]

#list of only second most sampled years by site
site_second_most_sample_key <- unique(unique_site_year[year_rank == 2,.(Site,SampleYear)])

#rename columns
colnames(site_second_most_sample_key) <- c("Site","second_most_recent_year")

#merge back with other data table

unique_site_year <- unique_site_year[site_second_most_sample_key,on = c("Site")]

#unique rows
site_by_years <- unique(unique_site_year[,.(Site, most_recent, Years_sampled, second_most_recent_year)])


if(regular == T){
#how many sites with atleast 3 years of data?
nrow(site_by_years[Years_sampled >=3]) #leaves us 93 sites

#reduce all data tables to sites that have been sampled atleast 3 times
dat_event.r <- dat_event[Site %in% site_by_years[Years_sampled >= 3]$Site]
dat_fish_t.r <- dat_fish_t[Site %in% site_by_years[Years_sampled >= 3]$Site]
dat_macroinvert.r <- dat_macroinvert[Site %in% site_by_years[Years_sampled >= 3]$Site]
dat_kelp.r <- dat_kelp[Site %in% site_by_years[Years_sampled >= 3]$Site]

########################
##Remove TBF 2016 Project (ASK WHAT THIS IS AND IF I SHOULD LEAVE IT IN)
########################
dat_event.r <- dat_event.r[Project != "TBF2016",]
dat_fish_t.r <- dat_fish_t.r[Project != "TBF2016",]
dat_macroinvert.r <- dat_macroinvert.r[Project != "TBF2016",]
dat_kelp.r <- dat_kelp.r[Project != "TBF2016",]

########################
##Save full UPC output (so we can calculate UPC relief/substrate metrics)
########################

saveRDS(UPC_complete, file.path("data","full_crane","UPC_complete.rds"))

########################
##Take average values across all years of sampling (avg density and avg biomass of each species at site)
########################

dat_fish_site_averages <- dat_fish_t.r[,.(mean_density_m2=mean(density_m2), mean_wt_density_g_m2=mean(wt_density_g_m2)),
                                               .(Species, Region, AR_Complex, Site, DepthZone)]
dat_macroinvert_site_averages <- dat_macroinvert.r[,.(mean_density_m2=mean(Abundance/area.m2)),
                                              .(BenthicReefSpecies, SpeciesGroupF, Region, AR_Complex, Site, DepthZone)]
dat_kelp_site_averages <- dat_kelp.r[,.(mean_density_m2=mean(Abundance/area.m2)),
                                              .(BenthicReefSpecies, SpeciesGroupF, Region, AR_Complex, Site, DepthZone)]

#Which regions are retained?
unique(dat_event.r$Region)

########################
##Save average output
########################
saveRDS(dat_event.r, file.path("data","processed_crane", "dat_event.r.rds"))
saveRDS(dat_fish_site_averages, file.path("data","processed_crane", "dat_fish_site_averages.rds"))
saveRDS(dat_macroinvert_site_averages, file.path("data","processed_crane", "dat_macroinvert_site_averages.rds"))
saveRDS(dat_kelp_site_averages, file.path("data","processed_crane", "dat_kelp_site_averages.rds"))
}

#Save site list for supplement as CSV ####
#Calculate years sampled per site
dat_event.r[,years_sampled := uniqueN(SampleYear),.(Site)]

#List depth zones per site
# Create a new column with unique values from 'value' grouped by 'group'
dat_event.r[, DepthZone_list := paste(unique(DepthZone), collapse = ", "),.(Site)]

#Identify Island vs. Mainland
dat_event.r[,type := ifelse(Region %in% c("Santa Catalina Island","Santa Barbara Island","San Clemente Island"),"Island","Mainland")]

#Identify inside outside MPA
MPA_site_key <- readRDS(file.path("keys","MPA_site_key.rds"))
dat_event.r <- MPA_site_key[dat_event.r, on = "Site"]

#Add lat lon from dive priority list
lat_lon_site_fix.r <- fread(file.path("keys","lat_lon_site_fix.r.csv"))
dat_event.r <- lat_lon_site_fix.r[dat_event.r, on = "Site"]

#Unique rows
Site_attributes <- unique(dat_event.r[,.(Site, Region, `Reef type`, avg_lon, avg_lat, DepthZone_list, years_sampled)])

#Export as csv
fwrite(Site_attributes, file.path("output","Site_attributes.csv"))


if(OSM == T){ #different subset
  #how many sites sampled in both 2022 and 2023
  nrow(site_by_years[Years_sampled >=2 & most_recent == 2023 & second_most_recent_year == 2022]) #leaves us 96 sites
  
  #reduce all data tables to sites that have been sampled atleast 2 times, including 2022 and 2023
  dat_event_OSM.r <- dat_event[Site %in% site_by_years[Years_sampled >=2 & most_recent == 2023 & second_most_recent_year == 2022]$Site]
  dat_fish_t_OSM.r <- dat_fish_t[Site %in% site_by_years[Years_sampled >=2 & most_recent == 2023 & second_most_recent_year == 2022]$Site]
  dat_macroinvert_OSM.r <- dat_macroinvert[Site %in% site_by_years[Years_sampled >=2 & most_recent == 2023 & second_most_recent_year == 2022]$Site]
  dat_kelp_OSM.r <- dat_kelp[Site %in% site_by_years[Years_sampled >=2 & most_recent == 2023 & second_most_recent_year == 2022]$Site]
  
  
  ########################
  ##Take average values across all years of sampling (avg density and avg biomass of each species at site) and reduce to only 2022 and 2023 (years SoS was sampled)
  ########################
  
  dat_event_OSM.r <- dat_event_OSM.r[SampleYear %in% c(2022,2023),]
  
  dat_fish_site_averages_OSM <- dat_fish_t_OSM.r[SampleYear %in% c(2022,2023),.(mean_density_m2=mean(density_m2), mean_wt_density_g_m2=mean(wt_density_g_m2)),
                                         .(Species, Project, Region, AR_Complex, Site, DepthZone)]
  dat_macroinvert_site_averages_OSM <- dat_macroinvert_OSM.r[SampleYear %in% c(2022,2023),.(mean_density_m2=mean(Abundance/area.m2)),
                                                     .(BenthicReefSpecies, SpeciesGroupF, Project, Region, AR_Complex, Site, DepthZone)]
  dat_kelp_site_averages_OSM <- dat_kelp_OSM.r[SampleYear %in% c(2022,2023),.(mean_density_m2=mean(Abundance/area.m2)),
                                       .(BenthicReefSpecies, SpeciesGroupF, Project, Region, AR_Complex, Site, DepthZone)]
  
  ########################
  ##Save output
  ########################
  saveRDS(dat_event_OSM.r, file.path("data","processed_crane", "dat_event_OSM.r.rds"))
  saveRDS(dat_fish_site_averages_OSM, file.path("data","processed_crane", "dat_fish_site_averages_OSM.rds"))
  saveRDS(dat_macroinvert_site_averages_OSM, file.path("data","processed_crane", "dat_macroinvert_site_averages_OSM.rds"))
  saveRDS(dat_kelp_site_averages_OSM, file.path("data","processed_crane", "dat_kelp_site_averages_OSM.rds"))
}


if(maggie_gorgonian == T){ #different subset
  #we don't care how many years of data, but we need gorgonians and CCA
  #nrow(site_by_years[Years_sampled >=3]) #leaves us 93 sites #in case we want to bring back in 
  
  ########################
  ##Remove TBF 2016 Project (ASK WHAT THIS IS AND IF I SHOULD LEAVE IT IN)
  ########################
  dat_event <- dat_event[Project != "TBF2016",]
  dat_macroinvert <- dat_macroinvert[Project != "TBF2016",]
  dat_kelp <- dat_kelp[Project != "TBF2016",]
  
  ########################
  ##Reduce to CCA (from UPC, percent cover) and gorgonians (from swath)
  ########################
  
  #SpeciesGroupF = gorgonians in swath data
  
  dat_gorgonian <- dat_macroinvert[SpeciesGroupF == "gorgonians"]
  fwrite(dat_gorgonian, file.path("data","processed_crane","for_maggie","dat_gorgonian.csv"))
  
  #CCA from UPC
  UPC_CCA <- UPC_complete %>% 
    filter(Taxa == "coralline algae - crustose" & Category == "UPC Species")
  fwrite(UPC_CCA, file.path("data","processed_crane","for_maggie","UPC_CCA.csv"))
  
  #Unique depth/Site/depthzone values
  site_depthzone_depth <- dat_event[,.(mean_surveydepth = round(mean(SurveyDepth, na.rm = T),0)),.(Site, DepthZone)]

  #See if this looks right by plotting
  ggplot() +
    geom_boxplot(data =   site_depthzone_depth , 
                 aes(x = DepthZone, y = mean_surveydepth)) +
    theme_classic()
  
  #Looks good!
  
  #Export
  fwrite(site_depthzone_depth, file.path("data","processed_crane","for_maggie","site_depthzone_depth.csv"))
  
  
}
