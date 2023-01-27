factors = st_read("www/wolverines.gpkg", "survey_factors", quiet=T)

# value to split between low/medium and high disturbance
split <- 30

# number of cells to sample from each disturbance group
n.sample.1 <- 40
n.sample.2 <- 60
n.sample.3 <- 40

## Change NAs to 0
factors[is.na(factors$merge100_pct),]$merge100_pct <- 0

## Split factors into 1) merge100_pct == 0, 2) 0 < merge100_pct <= 30, 
## 3) merge100_pct > 30
factors %<>% 
  mutate(cluster = ifelse(test = merge100_pct == 0, yes = 1,
                          no = ifelse(test = merge100_pct > 0 & 
                                        merge100_pct <= split, 
                                      yes = 2, no = 3)))

# sample 40 from cluster 1, 60 from cluster 2, and 40 from cluster 3
simple1 <- factors %>%
  filter(cluster == 1) %>%
  slice_sample(n = n.sample.1)

simple2 <- factors %>%
  filter(cluster == 2) %>%
  slice_sample(n = n.sample.2)

simple3 <- factors %>% 
  filter(cluster == 3) %>%
  slice_sample(n = n.sample.3)

simple <- rbind(simple1, simple2, simple3)

## Compare similarity of each randomly selected set to the full distribution
ks_output <-
  tibble(
    merge100_pct = ks.test(factors$merge100_pct, simple$merge100_pct)[[1]],
    #merge500_pct = ks.test(factors$merge500_pct, simple$merge500_pct)[[1]],
    placer_pct = ks.test(factors$placer_pct, simple$placer_pct)[[1]],
    quartz_pct = ks.test(factors$quartz_pct, simple$quartz_pct)[[1]],
    recent_fires_pct = ks.test(factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(factors$benchmark_pct, simple$benchmark_pct)[[1]],
    elev_median = ks.test(factors$elev_median, simple$elev_median)[[1]],
    elev_sd = ks.test(factors$elev_sd, simple$elev_sd)[[1]],
    forest_pct = ks.test(factors$forest_pct, simple$forest_pct)[[1]],
    wetland_pct = ks.test(factors$wetland_pct, simple$wetland_pct)[[1]],
    water_pct = ks.test(factors$water_pct, simple$water_pct)[[1]]
  )

## Compare similarity of each randomly selected set to the other 2 clusters 
## i.e. Compare merge100_pct == 0 to merge100_pct > 0 & <= 30 and merge100_pct > 30
simple12 <- simple %>%
  filter(cluster %in% c(1,2))
simple23 <- simple %>% 
  filter(cluster %in% c(2,3))
simple13 <- simple %>% 
  filter(cluster %in% c(1,3))

ks_output_clust1 <- 
  tibble(
    merge100_pct = ks.test(simple23$merge100_pct, simple1$merge100_pct)[[1]],
    #merge500_pct = ks.test(simple23$merge500_pct, simple1$merge500_pct)[[1]],
    placer_pct = ks.test(simple23$placer_pct, simple1$placer_pct)[[1]],
    quartz_pct = ks.test(simple23$quartz_pct, simple1$quartz_pct)[[1]],
    recent_fires_pct = ks.test(simple23$recent_fires_pct, simple1$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(simple23$benchmark_pct, simple1$benchmark_pct)[[1]],
    elev_median = ks.test(simple23$elev_median, simple1$elev_median)[[1]],
    elev_sd = ks.test(simple23$elev_sd, simple1$elev_sd)[[1]],
    forest_pct = ks.test(simple23$forest_pct, simple1$forest_pct)[[1]],
    wetland_pct = ks.test(simple23$wetland_pct, simple1$wetland_pct)[[1]],
    water_pct = ks.test(simple23$water_pct, simple1$water_pct)[[1]]
  )

ks_output_clust2 <- 
  tibble(
    merge100_pct = ks.test(simple13$merge100_pct, simple2$merge100_pct)[[1]],
    #merge500_pct = ks.test(simple13$merge500_pct, simple2$merge500_pct)[[1]],
    placer_pct = ks.test(simple13$placer_pct, simple2$placer_pct)[[1]],
    quartz_pct = ks.test(simple13$quartz_pct, simple2$quartz_pct)[[1]],
    recent_fires_pct = ks.test(simple13$recent_fires_pct, simple2$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(simple13$benchmark_pct, simple2$benchmark_pct)[[1]],
    elev_median = ks.test(simple13$elev_median, simple2$elev_median)[[1]],
    elev_sd = ks.test(simple13$elev_sd, simple2$elev_sd)[[1]],
    forest_pct = ks.test(simple13$forest_pct, simple2$forest_pct)[[1]],
    wetland_pct = ks.test(simple13$wetland_pct, simple2$wetland_pct)[[1]],
    water_pct = ks.test(simple13$water_pct, simple2$water_pct)[[1]]
  )

ks_output_clust3 <- 
  tibble(
    merge100_pct = ks.test(simple12$merge100_pct, simple3$merge100_pct)[[1]],
    #merge500_pct = ks.test(simple12$merge500_pct, simple3$merge500_pct)[[1]],
    placer_pct = ks.test(simple12$placer_pct, simple3$placer_pct)[[1]],
    quartz_pct = ks.test(simple12$quartz_pct, simple3$quartz_pct)[[1]],
    recent_fires_pct = ks.test(simple12$recent_fires_pct, simple3$recent_fires_pct)[[1]],
    benchmark_pct = ks.test(simple12$benchmark_pct, simple3$benchmark_pct)[[1]],
    elev_median = ks.test(simple12$elev_median, simple3$elev_median)[[1]],
    elev_sd = ks.test(simple12$elev_sd, simple3$elev_sd)[[1]],
    forest_pct = ks.test(simple12$forest_pct, simple3$forest_pct)[[1]],
    wetland_pct = ks.test(simple12$wetland_pct, simple3$wetland_pct)[[1]],
    water_pct = ks.test(simple12$water_pct, simple3$water_pct)[[1]]
  )

