# LIBRARY
library(dplyr)
library(car)
library(factoextra)
library(cluster)

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

## HANDLE MULTIKOLINEARITAS
### CEK MULTIKOLINEARITAS
# pastikan semua varabel numerik
numeric_data <- data %>% select(where(is.numeric))
vif_model <- lm(numeric_data[,1] ~ ., data = numeric_data)
vif(vif_model)

### PCA
# scaling
scaled_data <- scale(data %>% select(where(is.numeric)))
pca_result <- prcomp(scaled_data, scale. = FALSE)  # sudah distandarisasi manual
# cek komponen
summary(pca_result)
# lihat komposisi komponen dari setiap variabel
pca_result$rotation
# buat data baru dengan komponen yang terpilih
pca_data <- as.data.frame(pca_result$x[, 1:4])

# KLUSTERING
## ELBOW
fviz_nbclust(pca_data, kmeans, method = "wss") +
  labs(title = "Elbow Method untuk Menentukan Jumlah Kluster")

## KMEANS
set.seed(123)
kmeans_result <- kmeans(pca_data, centers = 4)
pca_data$cluster <- as.factor(kmeans_result$cluster)

## SILHOUTTE
sil <- silhouette(kmeans_result$cluster, dist(pca_data[, 1:4]))
fviz_silhouette(sil)

fviz_nbclust(pca_data, 
             kmeans, 
             method = "silhouette") +
  labs(title = "Silhouette Method untuk Menentukan Jumlah Kluster Optimal")