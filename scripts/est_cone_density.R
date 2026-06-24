#script to calculate cone density on trees based on spectral index and allometric equations

#foliar moisture conversion
#load in data
gs4_auth()

fm_df <- read_sheet("https://docs.google.com/spreadsheets/d/111VY46CZsyJ1Mzai52_bK9yqDHDfj9DcAJxjz4t_JII/edit?gid=1685363826#gid=1685363826",
                    sheet = "foliar moisture")

#calculate foliar moisture
#using "live fuel moisture content" formula as per https://www.sciencedirect.com/science/article/pii/S003442572030167X#s0010
#((wet weight - dry weight)/dry weight)*100

site_fm_df <- fm_df |> 
  group_by(site) |> 
  summarize(mean_fm = mean((((total_wet_weight - bag_weight_wet)-(total_dry_weight - bag_weight_dry))/
                              (total_dry_weight - bag_weight_dry))*100))

#this gives the % moisture in the wet sample
#to convert wet samples to dry, do (wet weight)*(1-(% perc moisture/100)))