# LIBRARY
library(dplyr)
library(car)



# DATA
data <- read.csv(
  "data/LIVE_20210128.csv"
)

# EKSPLORASI DATA


# PRAPROSES DATA
## DROP BEBERAPA KOLOM
data <- data %>%
  select(-status_id, -status_published, -Column1, -Column2, -Column3, -Column4)

## ENCODING
data$status_type <- as.numeric(as.factor(data$status_type))

## DROP MULTIKOLINEARITAS
### CEK MULTIKOLINEARITAS
vif_model <- lm(numeric_data[,1] ~ ., data = numeric_data)
vif(vif_model)

## 



# KLUSTERING
## SILHOUTTE

## ELBOW


# KARAKTERISTIK KLUSTER




# hapus beberapa kolom yang tidak dibutuhkan




# label encoder


# cek multikolinearitas


# Pastikan semua variabel numerik
numeric_data <- data %>% select(where(is.numeric))

# Fit model semu (misalnya regresi terhadap salah satu variabel)



library(corrplot)
corrplot(cor(data %>% select(starts_with("num_"))), method = "color")


pca_result <- prcomp(data %>% select(where(is.numeric)), scale. = TRUE)
summary(pca_result)

pca_result$rotation
