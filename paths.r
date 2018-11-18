#paths for Mycorrhizal trait analysis project.
#main data directory.----
host <- system('hostname', intern=T)
data.dir <- '/fs/data3/caverill/myc_traits/'
#data dir is somewhere else if you on remote geo server.
if(host != 'pecan2'){
  data.dir <- '/projectnb/talbot-lab-data/caverill/myc_traits/'
}

#raw trait files.----
       glop_raw.path <- paste0(data.dir,'Glopnet_2017_10_09.csv')
   fred_2.0_raw.path <- paste0(data.dir,'FRED2_20180518.csv')
       ornl_raw.path <- paste0(data.dir,'Traits2.0_2017_07_19.csv')
        try_raw.path <- paste0(data.dir,'averil_try-request_4024_010818/4024.csv')
cptd_traits_raw.path <- paste0(data.dir,'ecy2091-sup-0002-DataS1/Hard Traits.csv')
 cptd_sites_raw.path <- paste0(data.dir,'ecy2091-sup-0002-DataS1/Sites.csv')
   cptd_PGF_raw.path <- paste0(data.dir,'/ecy2091-sup-0002-DataS1/PFT data.csv')
     cptd_names.path <- paste0(data.dir,'ecy2091-sup-0002-DataS1/Species translations.csv')
       miat_raw.path <- paste0(data.dir,'miatto_2016.csv')
       zann_raw.path <- paste0(data.dir,'GlobalWoodinessDatabase.csv')
       gbif_raw.path <- paste0(data.dir,'species_climate_means_GBIF.csv')
      nodDB_raw.path <- paste0(data.dir,'nodDB_v1.csv')
       teow_raw.path <- '/fs/data3/caverill/wwf_ecoregions/official/wwf_terr_ecos.shp'
 brot_trait_raw.path <- paste0(data.dir,'BROT_2.0_database/BROT2_dat.csv')
   brot_tax_raw.path <- paste0(data.dir,'BROT_2.0_database/BROT2_tax.csv')
    daac651_raw.path <- paste0(data.dir,'litter_decomp_ornl_daac_651/data/litter_quality.txt')
     
#raw mycorrhizal trait files.----
dir <- data.dir
         e093_myco_raw.path <- paste0(dir,'myco_db.csv')
          fia_myco_raw.path <- paste0(dir,'mycorrhizal_SPCD_data.csv')
  tedersoo_myco_genera.path <- paste0(dir,'tedersoo_2017_genera.csv')
     mycoflor_myco_raw.path <- paste0(dir,'MycoFlor.csv')
         opik_myco_raw.path <- paste0(dir,'geb12582-sup-0007-suppinfo7.csv')
harley_harley_myco_raw.path <- paste0(dir,'Harley_Harley_2018_01_11.csv')
    poaceae_genera_raw.path <- paste0(dir,'poaceae_genera_wikipedia.rds')

#phylogeny files.----
  phylogeny_raw.path <- paste0(data.dir,'colin_tree.tre')
#phylogeny_clean.path <- paste0(data.dir,'colin_tree_clean.tre') #never mind. Once we clean up phyit wont load with read.tree().
    
    
#Clean mycorrhizal files for merging.----
          fred_myco_clean.path <- paste0(dir,'fred_myco_clean.rds')
          e093_myco_clean.path <- paste0(dir,'e093_myco_clean.rds')
           fia_myco_clean.path <- paste0(dir,'fia_myco_clean.rds')
      mycoflor_myco_clean.path <- paste0(dir,'mycoflor_myco_clean.rds')
          opik_myco_clean.path <- paste0(dir,'opik_myco_clean.rds')
 harley_harley_myco_clean.path <- paste0(dir,'harley_harley_myco_clean.rds')
averill_kivlin_myco_clean.path <- paste0(dir,'averill_kivlin_myco_clean.rds')
        myco_genera_clean.path <- paste0(dir,'myco_genera_clean.rds')

#C|ean woodiness files for merging.----
fred_wood_clean.path <- paste0(dir,'fred_wood_clean.rds')
glop_wood_clean.path <- paste0(dir,'glop_wood_clean.rds')
cptd_wood_clean.path <- paste0(dir,'cptd_wood_clean.rds')
ornl_wood_clean.path <- paste0(dir,'ornl_wood_clean.rds')
zann_wood_clean.path <- paste0(dir,'zann_wood_clean.rds')
bien_wood_clean.path <- paste0(dir,'bien_wood_clean.rds')

#The Plant List standardized names.----
tpl_names_lookup.path <- paste0(dir,'tpl_names_lookup.rds')
 full_tpl_output.path <- paste0(dir, 'full_tpl_lookup.rds')

#processed trait data for intra-specific analysis.----
dir <- paste0(data.dir,'clean_intraspecific_data/')
system(paste0('mkdir -p ',dir))
    glop_intra.path <- paste0(dir,'glop_intra_clean.rds')
fred_2.0_intra.path <- paste0(dir,'fred_2.0_intra_clean.rds')
    ornl_intra.path <- paste0(dir,'ornl_intra_clean.rds')
     try_intra.path <- paste0(dir,'try_intra_clean.rds')
    cptd_intra.path <- paste0(dir,'cptd_intra_clean.rds')
    miat_intra.path <- paste0(dir,'miat_intra_clean.rds')
    bien_intra.path <- paste0(dir,'bien_intra_clean.rds')
    brot_intra.path <- paste0(dir,'brot_intra_clean.rds')
 daac651_intra.path <- paste0(dir,'daac651_intra_clean.path')
#processed trait data for inter-specific analysis (species-level means).----
dir <- paste0(data.dir,'clean_interspecific_data/')
system(paste0('mkdir -p ',dir))
    glop_inter.path <- paste0(dir,'glop_inter_clean.rds')
fred_2.0_inter.path <- paste0(dir,'fred_2.0_inter_clean.rds')
    ornl_inter.path <- paste0(dir,'ornl_inter_clean.rds')
     try_inter.path <- paste0(dir,'try_inter_clean.rds')
    cptd_inter.path <- paste0(dir,'cptd_inter_clean.rds')
    miat_inter.path <- paste0(dir,'miat_inter_clean.rds')
    bien_inter.path <- paste0(dir,'bien_inter_clean.rds')

#merged trait, woodiness and mycorrhizal files.----
merged_intra_traits.path <- paste0(data.dir,'merged_intra_traits.rds')
merged_inter_traits.path <- paste0(data.dir,'merged_inter_traits.rds')
 merged_myco_traits.path <- paste0(data.dir,'merged_myco_traits.rds')
 merged_wood_traits.path <- paste0(data.dir,'merged_wood_traits.rds')
merged_intra_traits_names_hand_checked.path <- paste0(data.dir,'merged_intra_traits_names_hand_checked.rds')
 
#Analysis files.---- 
intra_specific_pre.subset_data.path <- paste0(data.dir, 'intra_specific_pre.subset_data.rds')
  intra_specific_analysis_data.path <- paste0(data.dir,'intra_specific_for_analysis.rds')
  inter_specific_analysis_data.path <- paste0(data.dir,'inter_specific_for_analysis.rds')
                   
#Analysis output.----
dir <- paste0(data.dir,'analysis_output/')
system(paste0('mkdir -p ',dir))
variance_decomp_output.path <- paste0(dir,'variance_decomposition.rds')
 lm_pgls_means_myc.pgf_models.path <- paste0(dir,'lm_pgls_means_myc.pgf_models.rds')
lm_pgls_means_myc.pgf_summary.path <- paste0(dir,'lm_pgls_means_myc.pgf_summary.rds')
lm_pgls_means_myc.pgf.clim_models.path <- paste0(dir,'lm_pgls_means_myc.pgf.clim_models.rds')
lm_pgls_means_myc.pgf.clim_summary.path <- paste0(dir,'lm_pgls_means_myc.pgf.clim_summary.rds')
lm_pgls_means_myc.pgf.gbif_models.path <- paste0(dir,'lm_pgls_means_myc.pgf.gbif_models.rds')
lm_pgls_means_myc.pgf.gbif_summary.path <- paste0(dir,'lm_pgls_means_myc.pgf.gbif_summary.rds')
lm_pgls_means_myc.pgf_merged.clim_models.path <- paste0(dir,'lm_pgls_means_myc.pgf_merged.clim_models.rds')
lm_pgls_means_myc.pgf_merged.clim_summary.path <- paste0(dir,'lm_pgls_means_myc.pgf_merged.clim_summary.rds')
lm_pgls_means_myc.pgf_myco.nfix.int_models.path <- paste0(dir,'lm_pgls_means_myc.pgf_myco.nfix.int_models.rds')
lm_pgls_means_myc.pgf_myco.nfix.int_summary.path <- paste0(dir,'lm_pgls_means_myc.pgf_myco.nfix.int_summary.rds')
biome_aicc_pgls_analysis.path <- paste0(dir,'biome_aicc_pgls_analysis.rds')
aicc_model_comparison_plgs.path <- paste0(dir,'aicc_model_comparison_plgs.path')

#Figure paths.----
dir <- 'figures/'
var_decomp_figure.path <- paste0(dir,'variance_decomp.png')
sample_map_figure.path <- paste0(dir,'global_sampling_map.png')
