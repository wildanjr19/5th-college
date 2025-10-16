# Tugas Spasial Autokorelasi
# library
library(terra)
library(sf)
library(dplyr)
library(readxl)
library(spdep)
library(tmap) # Pastikan tmap sudah di-load


# import data kemiskinan
data <- read_excel("data/Presentase-Kemiskinan-Jawa-Tengah-2024.xlsx")

# import file shp
shap_indo <- st_read("gadm41_IDN_shp/gadm41_IDN_2.shp")
# filter untuk jawa tengah saja
jateng_shp <- shap_indo %>%
  filter(NAME_1 == "Jawa Tengah")
# hapus baris 34 karena tidak teridentifikasi
jateng_shp <- jateng_shp[-34, ]

# kemiskinan
kemiskinan_vector <- data[[2]][1:35]

# Gabungkan data ke objek sf
jateng_sf <- jateng_shp
jateng_sf$kemiskinan <- kemiskinan_vector

# peta sebaran DBD
tm_shape(jateng_sf) +
  tm_fill(
    "kemiskinan",
    fill.scale = tm_scale_continuous(
      values = "brewer.yl_or_rd",
      n = 9 # jumlah gradasi warna (semakin banyak semakin halus)
    ),
    fill.legend = tm_legend(title = "Kemiskinan")
  ) +
  tm_borders(col = 'black', lwd = 0.5) +
  tm_title("Kemiskinan di Jawa Tengah") +
  tm_layout(legend.outside = TRUE)

# Matriks bobot spasial
nb <- poly2nb(jateng_sf)
lw <- nb2listw(nb, style = "W") # weight list

## Korelasi Lokal

# Plot Moran's
mp <- moran.plot(as.vector(scale(jateng_sf$kemiskinan)), lw)

# Hitung local Moran
kemiskinan_num <- as.numeric(unlist(jateng_sf$kemiskinan))
# Hitung local Moran
loc_moran <- localmoran(kemiskinan_num, lw, alternative = "greater")
head(loc_moran)

## Korelasi Global
moran.test(jateng_sf$kemiskinan, lw)
