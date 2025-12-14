# TBP
# Kelompok 1

# 1. Load library
library(spatstat)

# 2. Load data chorley
data(chorley)

# 3. Cek ringkasan data
summary(chorley)

# Plot data secara keseluruhan
plot(chorley, main = "Peta Persebaran Kasus Kanker Chorley")

# Plot dipisah berdasarkan jenis kanker (marks)
plot(split(chorley), main = "Pemisahan Kasus: Larynx vs Lung")

# Menambahkan lokasi insinerator (jika ingin melihat posisinya)
plot(chorley$window, main = "Lokasi Insinerator")
points(chorley, pch=3, cols="grey") # Titik kasus
plot(chorley.extra$incin, add=TRUE, pch=19, col="red", cex=1.5) # Titik insinerator

# Menghitung density
KDE <- density(chorley)

# Plot density
plot(KDE, main = "Peta Densitas Kernel (Semua Kasus)")
contour(KDE, add=TRUE) # Menambahkan garis kontur

# Melakukan Quadrat Test (membagi area menjadi grid 5x5)
Q_test <- quadrat.test(chorley, nx=5, ny=5)
print(Q_test)

# Visualisasi hasil test
plot(Q_test, main = "Quadrat Test: Observed vs Expected")
plot(chorley, add=TRUE, col="red", pch=".")
