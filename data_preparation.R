require(ggplot2)
require(rgdal)
require(rgeos)
require(raster)
require(ncdf4)
require(data.table)

data_dir = "C:/github/MPB_Overwintering_Projects/data/"
nat_for_layer = "national_forests_in_mpb_states"
pine_layer = "pine_range_polygons_mpb_states"
pine_in_forest_layer = "pine_in_forests_1500"
survival_file = paste0(data_dir, "mpb_overwinter_survival_brick.nc")

nat_forests = readOGR(paste0(data_dir, nat_for_layer), nat_for_layer)
pine_range = readOGR(paste0(data_dir, pine_layer), pine_layer)

pine_range@data
nat_for_layer[1]

## Buffered version of national forests
# for_buffered_1500 = gBuffer(nat_forests, width = 1500, byid = T)
dsn = paste0(nat_for_layer, "_buffered_1500")
shpf = paste0(nat_for_layer, "_buffered_1500")

# writeOGR(for_buffered_1500, 
#          dsn = paste0(data_dir, dsn),
#          layer = shpf, driver="ESRI Shapefile")

for_buffered_1500 = readOGR(dsn = dsn, layer = shpf)

## Intersection of pine range and national forests
# pine_in_forests = intersect(for_buffered_1500, pine_range)
# pine_in_forests@data
# str(pine_in_forests, 1)

dsn = paste0(pine_in_forest_layer)
shpf = paste0(nat_for_layer)
# writeOGR(pine_in_forests,
#          dsn = paste0(data_dir, dsn),
#          layer = shpf, driver="ESRI Shapefile", overwrite_layer = T)
pine_in_forests = readOGR(dsn = dsn, layer = shpf)
pine_in_forests@data$name = gsub(" ", "_", pine_in_forests@data$name)

## MPB survival/pine kill data individual forests ####

# NetCDF attributes:
nc = nc_open(survival_file)
ncdf_attributes = ncatt_get(nc, 0)
nc_close(nc)
ncdf_atts_keep = ncdf_attributes[c("crs", "crs_format", "Conventions")]

survival_data = brick(survival_file)
proj4string(survival_data) = ncdf_attributes$crs
kill_data = brick("C:/github/MPB_Overwintering_Projects/data/pine_kill_western_US_brick.nc")
proj4string(kill_data) = ncdf_attributes$crs

survival_years = 1981:2016
kill_years = 1997:2010

names(survival_data) = survival_years
names(kill_data) = kill_years
survival_data
kill_data

surv_data_forests = lapply(1:nrow(for_buffered_1500), function(x) crop(survival_data, for_buffered_1500[x, ]))
kill_data_forests = lapply(1:nrow(for_buffered_1500), function(x) crop(kill_data, for_buffered_1500[x, ]))

names(kill_data_forests)
nrow(kill_data_forests)

names(kill_data_forests) = names(surv_data_forests) = gsub(pattern = " ", "_", pine_in_forests$name)

names(pine_in_forests); names(surv_data_forests)
surv_data_forests_output_dir = paste0(data_dir, "individual_forests_surv/")
kill_data_forests_output_dir = paste0(data_dir, "individual_forests_kill/")

plot(surv_data_forests$Tahoe_National_Forest)
plot(kill_data_forests$Tahoe_National_Forest)



# Save individual files
for(i in 1:length(surv_data_forests))
{
  print(paste0(i, " of ", length(surv_data_forests)))
  
  bric_surv =   surv_data_forests[[i]]
  bric_kill =   kill_data_forests[[i]]
  stopifnot(extent(surv_data_forests[[i]]) == extent(kill_data_forests[[i]]))
  
  attr(bric_surv, "z") = NULL; attr(bric_kill, "z") = NULL
  attributes(bric_surv)$z$year = survival_years
  attributes(bric_kill)$z$year = kill_years
  attributes(bric_surv); attributes(bric_kill)
  
  fname_surv = paste0(surv_data_forests_output_dir, names(surv_data_forests)[i], "_survival.nc")
  fname_kill = paste0(kill_data_forests_output_dir, names(kill_data_forests)[i], "_kill.nc")
  print(fname_surv); print(fname_kill)
  writeRaster(bric_surv, filename = fname_surv, format = "CDF", overwrite = T)
  writeRaster(bric_kill, filename = fname_kill, format = "CDF", overwrite = T)
  
  # Attempt to give the netcdf name the appropriate dimensions and attributes
  nc_surv = nc_open(fname_surv, write = T)
  nc_kill = nc_open(fname_kill, write = T)
  ncvar_rename(nc_surv, "year", "mpb_model_survival")
  ncvar_rename(nc_kill, "year", "tree_mortality")
  for (i in 1:length(ncdf_atts_keep)) ncatt_put(nc_surv, 0, names(ncdf_atts_keep)[i], ncdf_atts_keep[[i]])
  for (i in 1:length(ncdf_atts_keep)) ncatt_put(nc_kill, 0, names(ncdf_atts_keep)[i], ncdf_atts_keep[[i]])
  ncatt_put(nc_surv, 0, "start_year", 1981); ncatt_put(nc_surv, 0, "end_year", 2016)
  ncatt_put(nc_surv, 0, "start_year", 1997); ncatt_put(nc_surv, 0, "end_year", 2010)
  nc_close(nc_surv); nc_close(nc_kill)
  
  rm(bric_surv); rm(bric_kill)  
}

# mean survival for forests
bric =   surv_data_forests[[j]]
shp = pine_in_forests[j, ]
mask_j = mask(subset(bric, 1), pine_in_forests[j, ])

plot(mask_j)
plot(subset(bric, 1))
plot(shp, add = T)

surv_dt = data.table()
kill_dt = data.table()

for (j in 1:n_forests)
{
  bric_surv =   surv_data_forests[[j]]
  bric_kill =   kill_data_forests[[j]]
  
  print(paste0(j, " of ", nrow(pine_in_forests), " ", names(surv_data_forests)[j]))
  shp = pine_in_forests[j, ]
  mask_j = mask(subset(bric_surv, 1), pine_in_forests[j, ])
  ok_cells = which(!is.na(mask_j[, ])) 
  
  yearly_surv = c()
  yearly_kill = c()
  length(ok_cells)
  for (year in 1:nlayers(bric_surv)) yearly_surv[year] = mean(subset(bric_surv, year)[ok_cells])
  for (year in 1:nlayers(bric_kill)) yearly_kill[year] = mean(subset(bric_kill, year)[ok_cells])
  tmp_surv = data.table(forest = gsub("_National_Forest", "", names(surv_data_forests)[j]), mean_survival = yearly_surv, year = survival_years)
  tmp_kill = data.table(forest = gsub("_National_Forest", "", names(kill_data_forests)[j]), tree_mortality = yearly_kill, year = kill_years)
  surv_dt = rbind(surv_dt, tmp_surv)
  kill_dt = rbind(kill_dt, tmp_kill)
  rm (bric_surv, bric_kill, tmp_surv, tmp_kill)
}

surv_dt
kill_dt

# fwrite(surv_dt, paste0(data_dir, "individual_forests/survival_yearly_by_forest.csv"))
# fwrite(kill_dt, paste0(data_dir, "individual_forests/mortality_yearly_by_forest.csv"))
surv_dt = fread(paste0(data_dir, "individual_forests/survival_yearly_by_forest.csv"))
kill_dt = fread(paste0(data_dir, "individual_forests/mortality_yearly_by_forest.csv"))

n_forests = length(surv_data_forests)

n_kill_years = length(kill_years)
n_expected = n_forests * n_kill_years * 16
n_forests * n_kill_years
n_forests * n_kill_years * 16

tmp = surv_dt[, .(surv = mean(mean_survival)), by = .(year)]
tmp2 = kill_dt[, .(tree_mortality = mean(tree_mortality, na.rm = T)), by = .(year)]
ggplot(data.frame(tmp), aes(x = as.numeric(year), y = surv)) + geom_line()
ggplot(data.frame(tmp2), aes(x = as.numeric(year), y = tree_mortality)) + geom_line()

merge(tmp, tmp2)
tmp
tmp2

survival_years = 1981:2016
kill_years

# lookback intervals for mean survival per forest ####
# only a subset of all survival years since beetle data set is smaller:
survival_years_2 = 1981:2010
n_years = length(kill_years)
end_years = kill_years
# start_years = end_years - interval + 1

forest_interval_dt = data.table()
for (interval in 1:16)
{
  print(paste0("interval = ", interval))
  start_years = end_years - interval + 1
  start_years
  end_years - start_years
  
  length(end_years) == length(start_years)
  length(start_years) == n_years
  
  for (i in 1:n_kill_years)
  {
    end_year = end_years[i]
    
    years_i = (end_year - interval + 1) : end_years[i]
    years_i
    
    
    # surv_dt[year %in% years_i, .(mean_surv = mean(mean_survival), year = end_year), by = .(forest)]
    tmp = surv_dt[year %in% years_i,
                  .(mean_surv = mean(mean_survival), year = end_year, interval = interval), by = .(forest)]
    # surv_dt[year %in% years_i, .(mean_surv, interval = interval), by = .(forest, year)]
    # tmp = surv_dt[year %in% years_i, mean(mean_survival), by = .(forest)]
    # forest_interval_dt
    forest_interval_dt = rbind(forest_interval_dt, tmp)
    # surv_dt[year %in% years_i, 
    #             .(mean_survival = mean(mean_survival), 
    #               interval = interval, 
    #               year = tail(years_i, i)), by = .(forest, year)]
    # forest_interval_dt = rbind(tmp, forest_interval_dt)
  }
}

forest_interval_dt
tmp = forest_interval_dt[, .(surv = mean(mean_surv)), by = .(interval, year)]
tmp2 = kill_dt[, .(mortality = mean(tree_mortality, na.rm = T)), by = year]

aggregate_means = merge(tmp, tmp2)
aggregate_means$interval = factor(aggregate_means$interval)


fwrite(aggregate_means, paste0(data_dir, "aggregate_forests/aggregate_means.csv"))
aggregate_means

ggplot(aggregate_means, aes(x = surv, y = mortality, color = factor(interval))) + geom_line()
ggplot(aggregate_means[interval == 1, ], aes(x = surv, y = mortality)) + geom_line()
lm(mortality ~ surv, data = aggregate_means[interval == 1])
mod1 = lm(mortality ~ surv, data = aggregate_means[interval == 1])

i = 1
dat_i = aggregate_means[interval == i, ]



plot(mod1, which = 1)

aggregate_means[interval == 1, ]


plot(residuals(mod1) ~ aggregate_means[interval == 1, surv])
residuals(mod1)

lm()


kill_dt[, .(mortality = mean(tree_mortality, na.rm = T)), by = year]

kill_dt
tmp
merge(tmp, kill_dt, by = "year")

merge()

merge(tmp, kill_dt[, mortality = mean(tree_mortality, na.rm = T), by = year])

merge(kill_dt, forest_interval_dt[, .(surv = mean(mean_surv), year = year, interval = interval), by = .(interval, year)], by = c("year"))
n_expected
n_forests * n_kill_years
n_forests * n_kill_years * 16
nrow(forest_interval_dt)

surv_kill_lags = merge(forest_interval_dt, kill_dt, by = c("year", "forest"))

surv_kill_lags[, .(surv = mean_surv, mean(tree_mortality), year, interval), by = .()]
surv_kill_lags[, .(surv = mean(mean_surv), tree_mortality, year, interval), by = .(interval, year)]
surv_kill_lags[, .(surv = mean(mean_surv), year = year, interval = interval), by = .(interval, year)]



forest_interval_dt[year == "1997", ]
forest_interval_dt[forest == "Angeles" && year == "1981", ]
forest_interval_dt[forest == "Angeles", ][year == "1997", ]

kill_dt
merge(kill_dt, forest_interval_dt, by = c("forest", "year"))
forest_interval_dt
merge(forest_interval_dt, kill_dt, by = c("forest", "year"))
kill_surv_dt

forest_interval_dt
merge(kill_dt, forest_interval_dt, by = c("year", "forest"))
kill_surv_dt[, .(surv = mean(mean_survival), interval, year), by = .(forest)]






sum(duplicated(forest_interval_dt))


tmp = forest_interval_dt[, .(mean_survival = mean(mean_survival)), by = .(interval, end_year)]
tmp[, interval := factor(interval)]
ggplot(tmp, aes(x = end_year, y = mean_survival, color = interval)) + geom_line()


# pine kill by forest ####








forest_dt[year %in% years_i, mean_survival, by = .(forest)]

forest_dt[year %in% years_i, 
          .(mean_survival = mean(mean_survival), 
            interval = interval, 
            end_year = tail(years_i, i)), by = .(forest)]

forest_interval_dt









forest_dt[year %in% years_i, .(mean_survival = mean(mean_survival), interval = interval, end_year = tail(years_i, 1)), by = .(forest)]





forest_dt





names(surv_data_forests)[j]
pine_in_forests[j, ]




yearly_means = c()
for (i in 1:nlayers(bric))
  yearly_means[i] =  mean(bric[i])
yearly_means


