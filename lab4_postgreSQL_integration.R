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

prop_sub = subset(prop_oc, saleyear == "2005")

plot(prop_sub$x, prop_sub$y)

head(prop_sub, n=10)

## install.packages("gstat")
## install.packages("sp")
require(gstat)
require(sp)

coordinates(prop_sub) = ~x+y

prop_sub$pr_sq_ft = as.numeric(prop_sub$pr_sq_ft)
bubble = bubble(prop_sub, zcol="pr_sq_ft", fill=T, do.sqrt=F, maxsize=3, add=T)
bubble

prop_cor = variogram(pr_sq_ft~1, data = prop_sub)
plot(prop_cor)
prop_cor

prop_cor_fit = fit.variogram(prop_cor, vgm(c("Sph","Exp","Mat")))
prop_cor_fit$range[2]

plot = plot(prop_cor,prop_cor_fit, 
           main="Variogram:\nDistance Threshold for Spatial Effect on Real-Estate Sales")
range_mi = prop_cor_fit$range[2]*69
range_mi 
plot

grid = as.data.frame(spsample(prop_sub, "regular", n = 5000))
plot(grid)
coordinates(grid) = c("x1","x2")   
gridded(grid) = T
fullgrid(grid) = T
plot(grid)



proj4string(grid) = proj4string(prop_sub)

pr_sq_ft_idw = gstat::idw(pr_sq_ft~1, prop_sub, newdata=grid,
                           idp=2.0)

## install.packages("rasterVis")
require(raster)
raster = raster(pr_sq_ft_idw)
plot(raster)
points(prop_sub$x, prop_sub$y)

prop_xy = prop_oc[,c("x","y")]
prop_oc$samp_pr05 = extract(raster, prop_xy)

head(prop_oc, n = 20)

dbWriteTable(conn,
             Id(schema = "lab4", 
                table = "property_data"),
             prop_oc, overwrite = T)
