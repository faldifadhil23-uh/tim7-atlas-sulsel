rm(list = ls())
# ============================================================
# PROYEK KELOMPOK - TIM 7
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# (IMPORT, PEMBERSIHAN DAN PENGABUNGAN DATA)
# ============================================================


# 1. PACKAGES YANG DIGUNAKAN
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)

# ============================================================

# 2. IMPORT DATA
data_ipm <- read_excel("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/IPM Sulsel.xlsx")
data_ipm

data_kemiskinan <- read_excel("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/Kemiskinan Sulsel.xlsx")
data_kemiskinan

peta <- st_read("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/geoBoundaries-IDN-ADM2_simplified.geojson")

# ============================================================

# 3. PEMBERSIHAN DATA
# Data IPM
names(data_ipm) <- c("Kab_Kota", "IPM_2023", "IPM_2024", "IPM_2025")
# Data kemiskinan
names(data_kemiskinan) <- c("Kab_Kota", "Kemiskinan_2023", "Kemiskinan_2024", "Kemiskinan_2025")

# Membersihkan spasi pada nama wilayah
data_ipm$Kab_Kota <- trimws(data_ipm$Kab_Kota)
data_kemiskinan$Kab_Kota <- trimws(data_kemiskinan$Kab_Kota)

# ============================================================

# 4. CEK DATA
print(data_ipm)
print(data_kemiskinan)

# Cek missing value
colSums(is.na(data_ipm))
colSums(is.na(data_kemiskinan))

# ============================================================

# 5. PENGGABUNGAN DATA IPM DAN KEMISKINAN
data_gabungan <- left_join(data_ipm, data_kemiskinan, by = "Kab_Kota")
print(data_gabungan)

# ============================================================

# 6. MEMILIH WILAYAH SULAWESI SELATAN
peta_sulsel <- peta %>% filter(shapeName %in% data_gabungan$Kab_Kota)
print(peta_sulsel)

# Mengecek jumlah wilayah
nrow(peta_sulsel)

# ============================================================

# 7. MEMBUAT KODE WILAYAH
peta_sulsel <- peta_sulsel %>% mutate(Kode_Wilayah = case_when(
  tolower(trimws(shapeName)) == "kepulauan selayar" ~ "7301",
  tolower(trimws(shapeName)) == "bulukumba" ~ "7302",
  tolower(trimws(shapeName)) == "bantaeng" ~ "7303",
  tolower(trimws(shapeName)) == "jeneponto" ~ "7304",
  tolower(trimws(shapeName)) == "takalar" ~ "7305",
  tolower(trimws(shapeName)) == "gowa" ~ "7306",
  tolower(trimws(shapeName)) == "sinjai" ~ "7307",
  tolower(trimws(shapeName)) == "maros" ~ "7308",
  tolower(trimws(shapeName)) == "pangkajene dan kepulauan" ~ "7309",
  tolower(trimws(shapeName)) == "barru" ~ "7310",
  tolower(trimws(shapeName)) == "bone" ~ "7311",
  tolower(trimws(shapeName)) == "soppeng" ~ "7312",
  tolower(trimws(shapeName)) == "wajo" ~ "7313",
  tolower(trimws(shapeName)) == "sidenreng rappang" ~ "7314",
  tolower(trimws(shapeName)) == "pinrang" ~ "7315",
  tolower(trimws(shapeName)) == "enrekang" ~ "7316",
  tolower(trimws(shapeName)) == "luwu" ~ "7317",
  tolower(trimws(shapeName)) == "tana toraja" ~ "7318",
  tolower(trimws(shapeName)) == "luwu utara" ~ "7322",
  tolower(trimws(shapeName)) == "luwu timur" ~ "7325",
  tolower(trimws(shapeName)) == "toraja utara" ~ "7326",
  tolower(trimws(shapeName)) == "kota makassar" ~ "7371",
  tolower(trimws(shapeName)) == "kota parepare" ~ "7372",
  tolower(trimws(shapeName)) == "kota palopo" ~ "7373",
  TRUE ~ NA_character_
)
)

# ============================================================

# 8. MENGGABUNGKAN BATAS PETA + DATA STATISTIK
data_peta <- peta_sulsel %>%
  select(Kode_Wilayah, Kab_Kota = shapeName, geometry) %>%
  left_join(data_gabungan, by = "Kab_Kota") %>%
  select(Kode_Wilayah, Kab_Kota, IPM_2023, IPM_2024, IPM_2025,
         Kemiskinan_2023, Kemiskinan_2024, Kemiskinan_2025, geometry)

# ============================================================

# 9. PEMERIKSAAN HASIL
data_peta %>% st_drop_geometry() %>% print()

# Cek apakah ada kode wilayah NA
data_peta %>% st_drop_geometry() %>%
  filter(is.na(Kode_Wilayah)) %>%
  select(Kode_Wilayah, Kab_Kota)

# Cek apakah ada data IPM/kemiskinan NA
data_peta %>% st_drop_geometry() %>%
  filter(is.na(IPM_2023) | is.na(IPM_2024) | is.na(IPM_2025) |
           is.na(Kemiskinan_2023) | is.na(Kemiskinan_2024) |
           is.na(Kemiskinan_2025)) %>%
  select(Kode_Wilayah, Kab_Kota, IPM_2023, IPM_2024, IPM_2025,
         Kemiskinan_2023, Kemiskinan_2024, Kemiskinan_2025)

# ============================================================

# 10. URUTKAN DATA BERDASARKAN KODE WILAYAH
data_peta <- data_peta %>% arrange(Kode_Wilayah)
print(data_peta)

# ============================================================

# 11. SIMPAN DATA BERSIH KE EXCEL
write_xlsx(data_peta %>% st_drop_geometry(), "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")

# ============================================================

# 12. SIMPAN DATA SPASIAL KE RDS
saveRDS(data_peta, "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/Marge_Data_Clean_IPM_Kemiskinan_Sulsel.rds")

# ============================================================
# 13. CEK FILE HASIL
file.exists("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")

file.exists("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/Marge_Data_Clean_IPM_Kemiskinan_Sulsel.rds")

# ============================================================
