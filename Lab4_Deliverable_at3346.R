#### For your lab deliverable create a new raster file of the IDW interpolated surface 
#### for only property sales in the year 2016 and only for the Ocean City spatial extent 
#### (same spatial extent as the first raster created).

## install.packages("RPostgres")
require(RPostgres)

conn = dbConnect(RPostgres::Postgres(), dbname = 'advanced_gis', 
                  host = 'localhost', # i.e. 'your computer'
                  port = 5433, # or any other port specified by your DBA
                  user = 'postgres',
                  password = 'ashley')


options(digits = 10)

prop = dbFetch(dbSendQuery(conn, "select pr_sq_ft, fsid, saleyear, x, y 
                                  from lab4.oc_prop"))

plot(prop$x, prop$y)

summary(prop)
prop$y = as.numeric(prop$y)
prop$x = as.numeric(prop$x)

prop_oc = subset(prop, (y > 38.2 & y < 38.5) & 
                (x > -75.3 & x < -75.0))

prop_sub_2016 = subset(prop_oc, saleyear == "2016")

plot(prop_sub_2016$x, prop_sub_2016$y)

head(prop_sub_2016, n = 10)

## install.packages("gstat")
## install.packages("sp")
require(gstat)
require(sp)

coordinates(prop_sub_2016) = ~x+y

prop_sub_2016$pr_sq_ft = as.numeric(prop_sub_2016$pr_sq_ft)
bubble = bubble(prop_sub_2016, zcol = "pr_sq_ft", fill = T, do.sqrt = F, maxsize = 3, add = T)
bubble

prop_cor_2016 = variogram(pr_sq_ft~1, data = prop_sub_2016)
plot(prop_cor_2016)
prop_cor_2016

prop_cor_2016_fit = fit.variogram(prop_cor_2016, vgm(c("Sph","Exp","Mat")))
prop_cor_2016_fit$range[2]

plot_2016 = plot(prop_cor_2016 ,prop_cor_2016_fit, 
           main="Variogram:\nDistance Threshold for Spatial Effect on Ocean City Real-Estate Sales in 2016")
range_mi_2016 = prop_cor_2016_fit$range[2]*69
range_mi_2016 
plot_2016

grid_2016 = as.data.frame(spsample(prop_sub_2016, "regular", n = 5000))
plot(grid_2016)
coordinates(grid_2016) = c("x1","x2")   
gridded(grid_2016) = T
fullgrid(grid_2016) = T
plot(grid_2016)



proj4string(grid_2016) = proj4string(prop_sub_2016)

pr_sq_ft_idw_2016 = gstat::idw(pr_sq_ft~1, prop_sub_2016, newdata = grid_2016,
                           idp = 2.0)

## install.packages("rasterVis")
require(raster)
raster_2016 = raster(pr_sq_ft_idw_2016)
plot(raster_2016)
points(prop_sub_2016$x, prop_sub_2016$y)

prop_xy = prop_oc[,c("x","y")]
prop_oc$samp_pr16 = extract(raster_2016, prop_xy)

head(prop_oc, n = 20)

dbWriteTable(conn,
             Id(schema = "lab4", 
                table = "property_data_2016"),
             prop_oc, overwrite = T)