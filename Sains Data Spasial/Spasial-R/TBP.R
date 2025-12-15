# TBP
# Kelompok 1

# 1. Load library dan data
library(spatstat)
data(chorley)

# filter lung
chorley_lung <- subset(chorley, marks == "lung")

# ringkasan data
summary(chorley_lung)

# visual
plot(chorley_lung, 
     main = "Peta Persebaran Kasus Kanker Paru (Lung Only)",
     pch = 20, cols = "black") # pch 20 = titik bulat

# Menambahkan lokasi insinerator (Penting untuk melihat konteks spasial)
plot(chorley.extra$incin, add=TRUE, pch=19, col="red", cex=1.5)
legend("topleft", legend=c("Kasus Lung", "Insinerator"), 
       col=c("black", "red"), pch=c(20, 19), cex=0.8)

# KDE
KDE_lung <- density(chorley_lung)

# Plot density
plot(KDE_lung, main = "Peta Densitas Kernel (Khusus Lung)")
contour(KDE_lung, add=TRUE) # Menambahkan garis kontur
points(chorley.extra$incin, pch=19, col="white") # Menandai insinerator di peta density

# quadrat test
Q_test_lung <- quadrat.test(chorley_lung, nx=7, ny=7)

# Tampilkan hasil statistik (Lihat p-value)
print(Q_test_lung)

# Visualisasi hasil test
plot(Q_test_lung, main = "Quadrat Test (Lung): Observed vs Expected")
plot(chorley_lung, add=TRUE, col="red", pch=".", cex=0.6)
