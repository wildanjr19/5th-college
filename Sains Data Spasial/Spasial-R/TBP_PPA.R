# TBP
# Kelompok 1

# 1. Load library dan data
library(spatstat)
data(chorley)

# filter lung
chorley_lung <- subset(chorley, marks == "lung")
marks(chorley_lung) <- NULL

# pertama, plot wilayah
plot(Window(chorley_lung), main = "Peta Persebaran Kasus Kanker Paru (Lung)")
# tambahkan point evemt
points(chorley_lung, pch = 20, col = "black") 
# opsional, menambahkan insirator
#plot(chorley.extra$incin, add=TRUE, pch=17, col="red", cex=1.5)
#legend("topleft", legend=c("Kasus Lung", "Insinerator"), 
#       col=c("black", "red"), pch=c(20, 17), cex=0.8)

# PLOT DENISTAS
# hitung densitas
KDE_lung <- density(chorley_lung)
# plot densitas
plot(KDE_lung, main = "Peta Densitas Kernel Kanker Paru (Lung)")
contour(KDE_lung, add=TRUE) # Menambahkan garis kontur
points(chorley.extra$incin, pch=19, col="white") # Menandai insinerator di peta density

# QUADRAT TEST
# lakukan uji quadrat 5x5
Q_test_lung <- quadrat.test(chorley_lung, nx=5, ny=5)
# tampilkan hasil statistik
print(Q_test_lung)
# visualisasi uji quadrat 5x5
plot(Q_test_lung, main = "Quadrat Test (Lung): Observed vs Expected")
plot(chorley_lung, add=TRUE, col="red", pch=".", cex=0.6)


# RIPLEY K FUNCTION
# hitung k
K_lung <- Kest(chorley_lung)
# plot dasar
plot(K_lung, main = "Ripley's K-function Kanker Paru (Lung)")
# untuk menguji perbedaan, gunakan uji Envelop
E_lung <- envelope(chorley_lung, Kest, nsim = 99, rank = 1, global = TRUE)
# visualisasi envelop
plot(E_lung, main = "Global Envelope Test - Ripley's K (Lung)")
# Menghitung Envelope untuk L-function
E_lung_L <- envelope(chorley_lung, Lest, nsim = 99, rank = 1, global = TRUE)
# plot L
plot(E_lung_L, . - r ~ r, main = "L-function (Linearized K) for Lung Data",
     legendargs=list(cex=0.8))
