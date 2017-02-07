# Read .csv containing referendum results data into an R DataFrame. A DataFrame consisting of 378 observations 
# and 8 variables should appear in your RStudio Data pane. Click on this entry or enter View(referendum_data) 
# to inspect the DataFrame as a spreadsheet. Available from: http://www.electoralcommission.org.uk/.
referendum_data<- read_csv("http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv")
# Recode referendum codes for lookup with 2011 Census codes.
referendum_data$Area_Code[referendum_data$Area_Code=="E07000242"] = "E07000097"
referendum_data$Area_Code[referendum_data$Area_Code=="E07000243"] = "E07000101"
referendum_data$Area_Code[referendum_data$Area_Code=="E07000241"] = "E07000104"
referendum_data$Area_Code[referendum_data$Area_Code=="E07000240"] = "E07000100"
referendum_data$Area_Code[referendum_data$Area_Code=="E08000037"] = "E08000020"
referendum_data$Area_Code[referendum_data$Area_Code=="E06000057"] = "E06000048"
# Keep only relevant variables
referendum_data <- referendum_data %>% transmute( Region_Code,	
                                                  Region,	Area_Code,
                                                  Area,	
                                                  Electorate,	
                                                  Turnout = Pct_Turnout/100,	
                                                  Valid_Votes,
                                                  Leave = Pct_Leave/100)
# Read .csv containing 2011 Census data. Census 2011 Key Statistics are usefully provided at Chris Gale's
# GitHub detailing the 2011 Ouput Area Classification: https://github.com/geogale/2011OAC  
census_data<- read_csv("http://staff.city.ac.uk/~sbbm143/datasets/2011_census_oa.csv")
oa_la_lookup <- read.csv("http://staff.city.ac.uk/~sbbm143/datasets/oa_la_lookup.csv")
census_data <- left_join(census_data, oa_la_lookup)
# Iterate over OA level data and compute summary statistics on relevant variables to LA level.
census_data <- census_data %>%
  group_by(LOCAL_AUTHORITY_CODE) %>%
  summarise(
    total_pop = sum(Total_Population),
    younger_adults = sum(Age_20_to_24, Age_25_to_29, Age_30_to_44) / sum(Total_Population), 
    white = sum(White_British_and_Irish) / sum(Total_Population),
    christian = sum(Christian) / sum(Total_Population),
    english_speaking = sum(Main_language_is_English_or_Main_language_not_English__Can_speak_English_very_well)
    / sum(Total_Population),
    single_ethnicity_household = sum(All_household_members_have_the_same_ethnic_group) 
    / sum(Total_Households),
    own_home = sum(Owned_and_Shared_Ownership) / sum(Total_Households),
    not_good_health = sum(Fair_health, Bad_health, Very_bad_health) / sum(Total_Population),
    degree_educated = sum(Highest_level_of_qualification_Level_4_qualifications_and_above) / 
      sum(Highest_level_of_qualification_Level_4_qualifications_and_above,
          Highest_level_of_qualification_Level_3_qualifications,
          Highest_level_of_qualification_Level_1_Level_2_or_Apprenticeship,
          No_qualifications),
    no_car = sum(No_cars_or_vans_in_household) / sum(Total_Households),
    private_transport_to_work = sum(Private_Transport) / sum(Total_Employment_16_to_74),
    professionals = sum(Managers_directors_and_senior_officials, Professional_occupations) /
      sum(Total_Employment_16_to_74)
  )

# Read in shapefile containing GB LA boundaries --  made available from UK Data Service: https://census.ukdataservice.ac.uk. 
# For convenience, we provide a version with geometries simplified using the "rmapshaper" library.
download.file("http://staff.city.ac.uk/~sbbm143/datasets/boundaries_gb.zip", "boundaries_gb.zip")
unzip("boundaries_gb.zip")
gb_boundaries <- readOGR(dsn = ".", layer = "boundaries_gb")
# Set coordinate system -- in this case OSGB: https://epsg.io/27700.
proj4string(gb_boundaries) <- CRS("+init=epsg:27700")
# Note that "gb_boundaries" is an R SpatialDataFrame. A DataFrame containing LA names, codes and
# summary statistics can be accessed through "gb_boundaries@data" 
gb_boundaries@data$geo_code <- as.character(gb_boundaries@data$geo_code)
# Re-label LAs where codes do not match with those 
gb_boundaries@data$geo_code[gb_boundaries@data$geo_code=="E41000052"] = "E06000053" # Isles of Scilly 
gb_boundaries@data$geo_code[gb_boundaries@data$geo_code=="E41000324"] = "E09000001" # City 
# Merge results and census data with SpatialDataFrame containing LA geomoetries. We do so with the 
# inner_join function. This is provided by the "dplyr" package; for a description enter "?inner_join" 
# into the Console.
gb_boundaries@data <- inner_join(gb_boundaries@data, referendum_data,  by=c("geo_code" =  "Area_Code"))
gb_boundaries@data <- inner_join(gb_boundaries@data, census_data, by=c("geo_code" =  "LOCAL_AUTHORITY_CODE"))
# Let's rename this SpatialDataFrame now containing the boundary information and the attribute data. 
data_gb <- gb_boundaries
# In order keep a clean workspace, remove the redundant data.
rm(census_data)
rm(referendum_data)
rm(gb_boundaries)
# For the results maps in the first exercise we'll create a new column: majority % points in favour of Leave:Remain. 
# We again use a function specfic to the "dplyr"package -- mutate(). dplyr also uses the "%>%" piping operator that 
# allows better structuring and nesting of calls to "dplyr ". This is provided by the "magrittr" package, which you 
# might have noticed being installed along with "dplyr". For a description, type "?magrittr" into your R Console. 
# We'll also calculate another variable that might be discriminating: population density.
data_gb@data <- data_gb@data %>%
  mutate(leave_remain = Leave-0.5,
         population_density = total_pop/AREA) 