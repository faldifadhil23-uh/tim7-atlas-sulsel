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

rm(list = ls())
# ============================================================
# PROYEK KELOMPOK - TIM 7
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# (TEMA TIM 7)
# ============================================================


# 1. PACKAGES YANG DIGUNAKAN
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)

# ============================================================

# 1b. PALET DAN TEMA GELAP TIM 7
# Dipakai ulang oleh semua grafik di atlas
pal_tim7 <- list(
  bg        = "#0F172A",   # latar halaman
  panel     = "#162036",   # latar panel grafik
  garis     = "#2B3A5C",   # garis kisi
  teks      = "#E8ECF4",   # teks utama
  judul     = "#FFFFFF",   # judul grafik
  pudar     = "#9AA5BF",   # subjudul, caption, label sumbu
  sorot     = "#FFD60A",   # penanda provinsi / rata-rata
  kemiskinan = "#FF6B9D",  # warna indikator kemiskinan
  ipm       = "#4CC9F0"    # warna indikator IPM
)

# Warna per indikator, mudah dipanggil: pal_indikator[["IPM"]]
pal_indikator <- c("Kemiskinan" = pal_tim7$kemiskinan,
                   "IPM"        = pal_tim7$ipm)

sumber_tim7 <- "Sumber: BPS Provinsi Sulawesi Selatan, diolah Kelompok 7"

theme_tim7 <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      text              = element_text(colour = pal_tim7$teks),
      plot.background   = element_rect(fill = pal_tim7$bg, colour = NA),
      panel.background  = element_rect(fill = pal_tim7$panel, colour = NA),
      panel.grid.major  = element_line(colour = pal_tim7$garis, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = base_size + 4,
                                       colour = pal_tim7$judul),
      plot.subtitle     = element_text(size = base_size, colour = pal_tim7$pudar),
      plot.caption      = element_text(size = base_size - 3, colour = pal_tim7$pudar,
                                       hjust = 0),
      axis.title        = element_text(size = base_size - 1, colour = pal_tim7$teks),
      axis.text         = element_text(size = base_size - 2, colour = pal_tim7$pudar),
      legend.title      = element_text(face = "bold", colour = pal_tim7$teks),
      legend.text       = element_text(colour = pal_tim7$teks),
      legend.key        = element_blank(),
      strip.text        = element_text(face = "bold", colour = pal_tim7$teks),
      plot.margin       = margin(12, 14, 10, 12)
    )
}

# Jadikan tema bawaan untuk semua ggplot berikutnya
theme_set(theme_tim7())

rm(list = ls())
# ============================================================
# PROYEK KELOMPOK - TIM 7
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# (MAKEOVER: PERTUMBUHAN IPM KABUPATEN/KOTA SULAWESI SELATAN 2025)
# ============================================================

# Grafik "sebelum": BPS Provinsi Sulsel, Gambar 2.8 "Pertumbuhan IPM
#   Kabupaten/Kota terhadap IPM Sulawesi Selatan (persen), 2025"
#   (BRS 5 November 2025)
# Data            : Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx
#
# MASALAH PADA GRAFIK "SEBELUM":
#   1. Dua ukuran berbeda (tinggi batang = IPM, garis = pertumbuhan %)
#      ditumpuk pada satu sumbu tanpa angka sumbu, jadi tidak bisa dibaca.
#   2. Batang tidak dimulai dari nol (sumbu terpotong), sehingga selisih
#      antarwilayah tampak jauh lebih besar daripada kenyataannya.
#   3. Garis menghubungkan kategori yang tidak berurutan (tidak bermakna).
#   4. Urutan menurut IPM, padahal judulnya tentang PERTUMBUHAN.
#   5. Nama wilayah dimiringkan 90 derajat dan sulit dibaca.
#
# PERBAIKAN (makeover):
#   Panel A: pertumbuhan IPM 2025 (batang mulai dari nol, diurutkan, dengan
#            garis acuan provinsi).
#   Panel B: tingkat IPM 2025 pada baris yang sama (titik, dengan sumbu
#            dan pita kategori IPM menurut BPS).
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ============================================================
# 0. PENGATURAN
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
berkas_data <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Makeover"

stopifnot("Berkas data tidak ditemukan, cek berkas_data" = file.exists(berkas_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# Format angka Indonesia (koma desimal)
koma <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

# ============================================================
# 1. PALET DAN TEMA TIM 7 (TEMA GELAP)
# ============================================================
warna_atas   <- "#4CC9F0"   # biru langit : di atas pertumbuhan provinsi
warna_bawah  <- "#FF6B9D"   # merah muda  : di bawah pertumbuhan provinsi
warna_prov   <- "#FFD60A"   # kuning      : Sulawesi Selatan (provinsi)
warna_bg     <- "#0F172A"
warna_panel  <- "#162036"
warna_grid   <- "#2B3A5C"
warna_teks   <- "#E8ECF4"
warna_pudar  <- "#9AA5BF"

pal_status <- c("Di atas provinsi"  = warna_atas,
                "Di bawah provinsi" = warna_bawah,
                "Provinsi"          = warna_prov)

theme_tim <- function() {
  theme_minimal(base_family = "sans", base_size = 11) +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = warna_grid, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          plot.title       = element_text(face = "bold", size = 13, colour = "white"),
          plot.subtitle    = element_text(size = 9.5, colour = warna_pudar),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          legend.position  = "top",
          legend.title     = element_text(face = "bold", size = 9, colour = warna_teks),
          legend.text      = element_text(size = 9, colour = warna_teks))
}

# ============================================================
# 2. DATA KABUPATEN/KOTA (WIDE -> LONG)
# ============================================================
data_wide <- read_excel(berkas_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

data_long <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm") ~ "IPM",
    TRUE ~ NA_character_),
    tahun   = as.integer(tahun),
    wilayah = str_squish(wilayah)) |>
  filter(!is.na(indikator), !is.na(kode), !is.na(nilai)) |>
  distinct()

stopifnot("Harus 24 wilayah" = n_distinct(data_long$wilayah) == 24,
          "Harus 72 baris IPM (24 x 3 tahun)" = nrow(data_long) == 72)

# Label: awalan dibuang, "Dan" menjadi "dan", kota (kode 737x) diberi tanda
lebar <- data_long |>
  mutate(nama  = str_remove(wilayah, regex("^(kabupaten|kab\\.?|kota)\\s+", ignore_case = TRUE)),
         nama  = str_replace_all(nama, "\\bDan\\b", "dan"),
         tipe  = if_else(str_starts(kode, "737"), "Kota", "Kabupaten"),
         label = if_else(tipe == "Kota", paste0(nama, " (kota)"), nama)) |>
  select(label, tipe, tahun, nilai) |>
  pivot_wider(names_from = tahun, values_from = nilai, names_prefix = "y") |>
  mutate(pertumbuhan = (y2025 / y2024 - 1) * 100)      # persen, 2025 terhadap 2024

# ============================================================
# 3. DATA PROVINSI (BPS Sulsel, BRS 5 November 2025)
#    IPM 2024 = 75,18; IPM 2025 = 75,92; pertumbuhan 0,98 persen
# ============================================================
ipm_prov_2024 <- 75.18
ipm_prov_2025 <- 75.92
tumbuh_prov   <- (ipm_prov_2025 / ipm_prov_2024 - 1) * 100
stopifnot("Pertumbuhan provinsi harus 0,98 persen" = round(tumbuh_prov, 2) == 0.98)

# Gabungkan baris provinsi agar tampil sebagai pembanding pada kedua panel
prov <- tibble(label = "Sulawesi Selatan", tipe = "Provinsi",
               y2024 = ipm_prov_2024, y2025 = ipm_prov_2025,
               pertumbuhan = tumbuh_prov)

dat <- bind_rows(lebar |> select(label, tipe, y2024, y2025, pertumbuhan), prov) |>
  mutate(status = case_when(tipe == "Provinsi"          ~ "Provinsi",
                            pertumbuhan > tumbuh_prov   ~ "Di atas provinsi",
                            TRUE                        ~ "Di bawah provinsi"),
         status = factor(status, levels = names(pal_status)))

# Urutan: pertumbuhan tertinggi di atas
urutan <- dat |> arrange(pertumbuhan) |> pull(label)
dat <- dat |> mutate(label = factor(label, levels = urutan))

# ============================================================
# 4. FAKTA RINGKAS (UNTUK JUDUL YANG BERBASIS DATA)
# ============================================================
kab <- dat |> filter(tipe != "Provinsi")
n_atas   <- sum(kab$pertumbuhan > tumbuh_prov)
r_ipm    <- cor(kab$y2025, kab$pertumbuhan)
tertinggi <- kab |> slice_max(pertumbuhan, n = 1, with_ties = FALSE)
terendah  <- kab |> slice_min(pertumbuhan, n = 1, with_ties = FALSE)

judul <- if (r_ipm <= -0.3) {
  "Wilayah ber-IPM lebih rendah cenderung tumbuh lebih cepat pada 2025"
} else if (r_ipm >= 0.3) {
  "Wilayah ber-IPM lebih tinggi cenderung tumbuh lebih cepat pada 2025"
} else {
  "Pertumbuhan IPM 2025 tidak menunjukkan pola yang jelas menurut tingkat IPM"
}

subjudul <- sprintf(
  "%d dari 24 kabupaten/kota tumbuh di atas provinsi (%s%%). Tertinggi: %s (%s%%), terendah: %s (%s%%). Korelasi IPM 2025 dan pertumbuhan: r = %s",
  n_atas, koma(tumbuh_prov),
  as.character(tertinggi$label), koma(tertinggi$pertumbuhan),
  as.character(terendah$label),  koma(terendah$pertumbuhan),
  koma(r_ipm))

# ============================================================
# 5. PANEL A: PERTUMBUHAN IPM 2025 (BATANG DARI NOL, TERURUT)
# ============================================================
pA <- ggplot(dat, aes(x = pertumbuhan, y = label, fill = status)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = tumbuh_prov, colour = warna_prov,
             linetype = "dashed", linewidth = 0.6) +
  geom_text(aes(label = koma(pertumbuhan)), hjust = -0.2, size = 3,
            colour = warna_teks) +
  scale_fill_manual(values = pal_status, name = NULL) +
  scale_x_continuous(limits = c(0, max(dat$pertumbuhan) * 1.15),
                     breaks = seq(0, 1.5, 0.5),
                     labels = \(v) koma(v, 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "A. Pertumbuhan IPM 2025 (%)",
       subtitle = paste0("2025 terhadap 2024; garis putus-putus: provinsi (",
                         koma(tumbuh_prov), "%)"),
       x = "Pertumbuhan IPM (%)", y = NULL) +
  theme_tim() +
  theme(axis.text.y        = element_text(size = 9, colour = warna_teks),
        panel.grid.major.y = element_blank())

# ============================================================
# 6. PANEL B: TINGKAT IPM 2025 (TITIK, SUMBU DAN KATEGORI BPS)
#    Kategori IPM menurut BPS: sedang 60-<70, tinggi 70-<80, sangat tinggi >=80
# ============================================================
batas_bawah <- floor(min(dat$y2025) / 5) * 5 - 5
batas_atas  <- max(85, ceiling(max(dat$y2025) / 5) * 5)

kategori <- tibble(nama  = c("Sedang", "Tinggi", "Sangat tinggi"),
                   bawah = c(batas_bawah, 70, 80),
                   atas  = c(70, 80, batas_atas),
                   isi   = c("#1B2745", "#1F3358", "#254270"))

pB <- ggplot(dat) +
  geom_rect(data = kategori,
            aes(xmin = bawah, xmax = atas, ymin = -Inf, ymax = Inf),
            fill = kategori$isi, inherit.aes = FALSE) +
  geom_vline(xintercept = ipm_prov_2025, colour = warna_prov,
             linetype = "dashed", linewidth = 0.6) +
  geom_segment(aes(x = y2024, xend = y2025, y = label, yend = label),
               colour = warna_pudar, linewidth = 0.6) +
  geom_point(aes(x = y2024, y = label), colour = warna_pudar, size = 1.8) +
  geom_point(aes(x = y2025, y = label, colour = status), size = 2.8) +
  geom_text(aes(x = y2025, y = label, label = koma(y2025)),
            hjust = -0.45, size = 2.9, colour = warna_teks) +
  annotate("text", x = (kategori$bawah + kategori$atas) / 2, y = Inf,
           label = kategori$nama, vjust = -0.6, size = 3, colour = warna_pudar) +
  scale_colour_manual(values = pal_status, guide = "none") +
  scale_x_continuous(breaks = seq(batas_bawah, batas_atas, 5),
                     labels = \(v) koma(v, 0), expand = c(0, 0)) +
  coord_cartesian(xlim = c(batas_bawah, batas_atas), clip = "off") +
  labs(title = "B. Tingkat IPM 2025",
       subtitle = "Titik abu-abu = 2024, titik berwarna = 2025; garis kuning: IPM provinsi 2025",
       x = "IPM", y = NULL) +
  theme_tim() +
  theme(axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = warna_grid, linewidth = 0.25),
        plot.margin        = margin(t = 14, r = 8, b = 5, l = 5))

# ============================================================
# 7. SUSUN KOMPOSISI
# ============================================================
makeover_ipm <- (pA | pB) +
  plot_layout(widths = c(1.2, 1), guides = "collect") &
  theme(legend.position = "top")

makeover_ipm <- makeover_ipm +
  plot_annotation(
    title    = judul,
    subtitle = str_wrap(subjudul, 150),
    caption  = paste0("Sumber: BPS Provinsi Sulawesi Selatan (BRS 5 November 2025), diolah Kelompok 7.\n",
                      "Pertumbuhan = (IPM 2025 / IPM 2024 - 1) x 100, dihitung dari IPM yang dipublikasikan, ",
                      "sehingga dapat berbeda tipis dari angka resmi BPS akibat pembulatan.\n",
                      "Kategori IPM menurut BPS: sedang 60-<70, tinggi 70-<80, sangat tinggi >=80."),
    theme = theme(plot.background = element_rect(fill = warna_bg, colour = NA),
                  plot.title      = element_text(face = "bold", size = 17, colour = "white"),
                  plot.subtitle   = element_text(size = 10.5, colour = warna_pudar),
                  plot.caption    = element_text(hjust = 0, size = 8.5, colour = warna_pudar))
  )

# ============================================================
# 8. SIMPAN DARI KODE, LALU LIHAT BERKAS PNG
# ============================================================
berkas_png <- file.path(folder_out, "makeover_pertumbuhan_ipm_Tim7_v3.png")
ggsave(berkas_png, makeover_ipm, width = 14, height = 9.5, dpi = 300, bg = warna_bg)
ggsave(file.path(folder_out, "makeover_pertumbuhan_A.png"), pA,
       width = 7, height = 8, dpi = 300, bg = warna_bg)
ggsave(file.path(folder_out, "makeover_ipm_B.png"), pB,
       width = 7, height = 8, dpi = 300, bg = warna_bg)

if (interactive()) utils::browseURL(normalizePath(berkas_png))   # buka PNG ukuran asli
message("Tersimpan: ", normalizePath(berkas_png))
message("Waktu simpan: ", format(file.info(berkas_png)$mtime, "%d-%m-%Y %H:%M:%S"),
        " (harus sama dengan waktu Anda menjalankan script ini)")

# ============================================================
# 9. TABEL PENDUKUNG (UNTUK LAPORAN)
# ============================================================
tabel <- dat |>
  arrange(desc(pertumbuhan)) |>
  transmute(Wilayah = as.character(label), Jenis = tipe,
            IPM_2024 = y2024, IPM_2025 = y2025,
            Pertumbuhan_persen = round(pertumbuhan, 2),
            Status = as.character(status))
print(tabel, n = 25)
write.csv(tabel, file.path(folder_out, "tabel_pertumbuhan_ipm_Tim7.csv"), row.names = FALSE)

rm(list = ls())
# ============================================================
# PROYEK KELOMPOK - TIM 7
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# (GALERI GRAFIK STATISTIK : IPM DAN KEMISKINAN SULSEL)
# Gaya: tema gelap (dark mode), orientasi horizontal, heatmap,
#       lollipop perubahan, dan lintasan 2023-2025 pada scatterplot
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data   <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Output/Galeri_Sulsel_v2"

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. TEMA GELAP DAN PALET WARNA
# ============================================================
# Aturan warna:
#   merah muda = Kemiskinan | biru langit = IPM | kuning = sorotan
#   hijau = membaik | merah = memburuk (arah perubahan 2023 ke 2025)
bg        <- "#0F172A"   # latar gambar
bg_panel  <- "#162036"   # latar panel
garis     <- "#2B3A5C"   # garis grid
teks      <- "#E8ECF4"   # teks utama
teks_pudar <- "#9AA5BF"  # teks sekunder
sorot     <- "#FFD60A"

pal_indikator <- c("Kemiskinan" = "#FF6B9D", "IPM" = "#4CC9F0")
pal_tipe      <- c("Kabupaten" = "#9AA5BF", "Kota" = sorot)
pal_tahun     <- c("2023" = "#CDB4DB", "2024" = "#A2D2FF", "2025" = "#FFFFFF")
pal_arah      <- c("Membaik" = "#7AE582", "Memburuk" = "#FF6B6B", "Tetap" = "#9AA5BF")
sumber        <- "Sumber: BPS, diolah Kelompok 7"

label_indikator <- c("Kemiskinan" = "Kemiskinan (%)",
                     "IPM"        = "Indeks Pembangunan Manusia")

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = teks),
          plot.background  = element_rect(fill = bg, colour = NA),
          panel.background = element_rect(fill = bg_panel, colour = NA),
          panel.grid.major = element_line(colour = garis, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 15, colour = "white"),
          plot.subtitle    = element_text(size = 10.5, colour = teks_pudar),
          plot.caption     = element_text(size = 8, colour = teks_pudar),
          axis.title       = element_text(size = 10, colour = teks),
          axis.text        = element_text(size = 9, colour = teks_pudar),
          strip.background = element_rect(fill = "#22325A", colour = NA),
          strip.text       = element_text(face = "bold", size = 10, colour = teks),
          legend.position  = "bottom",
          legend.text      = element_text(size = 9, colour = teks),
          legend.title     = element_text(face = "bold", size = 9, colour = teks),
          plot.margin      = margin(10, 12, 8, 10))
}

# ============================================================
# 3. DATA: BACA DAN RAPIKAN (WIDE -> LONG)
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

dat_long <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    tahun     = as.integer(tahun),
    tipe      = if_else(str_starts(kode, "737"), "Kota", "Kabupaten"),
    wilayah   = str_remove(str_trim(wilayah), "^Kota "),
    indikator = factor(indikator, levels = c("Kemiskinan", "IPM"))) |>
  filter(!is.na(kode)) |>
  distinct()

# Pemeriksaan: indikator terbaca dan tidak ada duplikat
stopifnot("Nama kolom indikator tidak terbaca, cek names(data_wide)" =
            !anyNA(dat_long$indikator))
dat_long |>
  count(kode, wilayah, tahun, indikator) |>
  filter(n > 1) |>
  print()                  # harus kosong (0 baris)

# Format lebar per tahun (untuk scatterplot)
dat <- dat_long |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  rename(kemiskinan = Kemiskinan, ipm = IPM) |>
  arrange(wilayah, tahun)

# Selisih 2025 - 2023 dan arah perubahan per wilayah
arah <- dat_long |>
  group_by(indikator, wilayah) |>
  summarise(selisih = nilai[tahun == 2025] - nilai[tahun == 2023],
            .groups = "drop") |>
  mutate(arah = case_when(
    selisih == 0 ~ "Tetap",
    indikator == "IPM"        & selisih > 0 ~ "Membaik",
    indikator == "Kemiskinan" & selisih < 0 ~ "Membaik",
    TRUE ~ "Memburuk"))

saveRDS(dat_long, file.path(folder_out, "data_long.rds"))
saveRDS(dat,      file.path(folder_out, "data_bersih.rds"))
stopifnot(nrow(dat) == 72, !anyNA(dat$kemiskinan), !anyNA(dat$ipm))

# ============================================================
# GRAFIK 1. SEBARAN PER TAHUN (violin horizontal + boxplot + titik)
# Dua panel ditumpuk ke bawah: Kemiskinan dan IPM
# ============================================================
g1 <- ggplot(dat_long,
             aes(x = nilai, y = factor(tahun, levels = c("2025", "2024", "2023")))) +
  geom_violin(aes(fill = indikator), alpha = 0.35, colour = NA, scale = "width") +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = NA,
               colour = "white", linewidth = 0.45) +
  geom_jitter(aes(colour = tipe), height = 0.08, width = 0, size = 2, alpha = 0.95) +
  facet_wrap(vars(indikator), ncol = 1, scales = "free_x",
             labeller = as_labeller(label_indikator)) +
  scale_fill_manual(values = pal_indikator, guide = "none") +
  scale_colour_manual(values = pal_tipe, name = "Tipe wilayah") +
  labs(title = "Sebaran IPM dan kemiskinan per tahun",
       subtitle = "24 kabupaten/kota; kotak putih = kuartil, titik = wilayah",
       x = NULL, y = NULL, caption = sumber) +
  theme_tim()

# ============================================================
# GRAFIK 2. SEBARAN PER KELOMPOK: KABUPATEN vs KOTA
# Grid: baris = indikator, kolom = tahun
# ============================================================
g2 <- ggplot(dat_long, aes(x = tipe, y = nilai)) +
  geom_boxplot(aes(fill = indikator), alpha = 0.45, width = 0.55,
               outlier.shape = NA, colour = "white", linewidth = 0.4) +
  geom_jitter(width = 0.12, size = 1.8, colour = "white", alpha = 0.8) +
  facet_grid(indikator ~ tahun, scales = "free_y",
             labeller = labeller(indikator = label_indikator)) +
  scale_fill_manual(values = pal_indikator, guide = "none") +
  labs(title = "Kabupaten dibandingkan kota",
       subtitle = "Sebaran nilai per kelompok wilayah dan tahun",
       x = NULL, y = NULL, caption = sumber) +
  theme_tim()

# ============================================================
# GRAFIK 3 & 4. TREN WAKTU, SMALL MULTIPLES 24 WILAYAH
# Warna garis menunjukkan arah perubahan 2023 ke 2025
# ============================================================
buat_tren <- function(ind) {
  d <- dat_long |>
    filter(indikator == ind) |>
    left_join(arah, by = c("indikator", "wilayah"))
  
  ggplot(d, aes(x = tahun, y = nilai, group = wilayah)) +
    geom_line(aes(colour = arah), linewidth = 1.1) +
    geom_point(aes(colour = arah), size = 2) +
    geom_text(data = filter(d, tahun %in% c(2023, 2025)),
              aes(label = sprintf("%.1f", nilai)),
              vjust = -1, size = 2.4, colour = teks) +
    facet_wrap(vars(wilayah), ncol = 6, scales = "free_y") +
    scale_x_continuous(breaks = 2023:2025, expand = expansion(mult = 0.15)) +
    scale_y_continuous(expand = expansion(mult = c(0.12, 0.3))) +
    scale_colour_manual(values = pal_arah, name = "Arah perubahan 2023 ke 2025") +
    labs(title = paste("Tren", label_indikator[[ind]], "per kabupaten/kota"),
         subtitle = "Skala sumbu-y dibuat bebas per panel agar arah perubahan terlihat jelas",
         x = NULL, y = NULL, caption = sumber) +
    theme_tim() +
    theme(strip.text  = element_text(face = "bold", size = 8.5, colour = teks),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 7),
          panel.grid.major.y = element_blank())
}

g3 <- buat_tren("Kemiskinan")
g4 <- buat_tren("IPM")

# ============================================================
# GRAFIK 5. SCATTERPLOT IPM vs KEMISKINAN
# Regresi + pita SK 95% (data 3 tahun digabung)
# Garis abu-abu berpanah: lintasan tiap wilayah dari 2023 ke 2025
# ============================================================
r_total <- cor(dat$kemiskinan, dat$ipm)

g5 <- ggplot(dat, aes(x = kemiskinan, y = ipm)) +
  geom_smooth(method = "lm", formula = y ~ x, level = 0.95,
              colour = sorot, fill = sorot, alpha = 0.18, linewidth = 1.1) +
  geom_path(aes(group = wilayah), colour = "grey55", linewidth = 0.35,
            arrow = arrow(length = unit(1.6, "mm"))) +
  geom_point(aes(fill = factor(tahun)), shape = 21, size = 3,
             colour = bg, stroke = 0.4) +
  geom_text(data = filter(dat, wilayah == "Makassar", tahun == 2025),
            aes(label = wilayah), hjust = -0.15, size = 3.2,
            fontface = "bold", colour = sorot) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = sprintf("r = %.2f (semua tahun)", r_total),
           size = 3.4, fontface = "italic", colour = teks) +
  scale_fill_manual(values = pal_tahun, name = "Tahun") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Hubungan IPM dan kemiskinan",
       subtitle = "Garis kuning: regresi linear + pita SK 95%. Panah: lintasan 2023 ke 2025",
       x = "Kemiskinan (%)", y = "IPM", caption = sumber) +
  theme_tim()

# ============================================================
# GRAFIK 6. HEATMAP NILAI PER WILAYAH DAN TAHUN
# Diurutkan menurut nilai 2025 (tertinggi di atas)
# ============================================================
buat_heatmap <- function(ind) {
  d <- dat_long |> filter(indikator == ind)
  urutan <- d |> filter(tahun == 2025) |> arrange(nilai) |> pull(wilayah)
  d <- d |> mutate(wilayah = factor(wilayah, levels = urutan),
                   tahun   = factor(tahun))
  
  ggplot(d, aes(x = tahun, y = wilayah, fill = nilai)) +
    geom_tile(colour = bg, linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.1f", nilai)), size = 2.8, colour = "white") +
    scale_fill_gradient(low = "#22325A", high = pal_indikator[[ind]],
                        name = label_indikator[[ind]]) +
    scale_x_discrete(position = "top") +
    labs(title = label_indikator[[ind]],
         subtitle = "Urut dari nilai 2025 tertinggi di atas",
         x = NULL, y = NULL, caption = sumber) +
    theme_tim() +
    theme(panel.grid = element_blank(),
          legend.key.width = unit(1.4, "cm"))
}

g6_k <- buat_heatmap("Kemiskinan")
g6_i <- buat_heatmap("IPM")
g6   <- (g6_k | g6_i) +
  plot_annotation(title = "Peta panas nilai per wilayah, 2023-2025",
                  theme = theme(plot.background = element_rect(fill = bg, colour = NA),
                                plot.title = element_text(face = "bold", size = 16,
                                                          colour = "white")))

# ============================================================
# GRAFIK 7. LOLLIPOP PERUBAHAN 2025 - 2023 PER WILAYAH
# Hijau = membaik (IPM naik / kemiskinan turun)
# ============================================================
buat_lollipop <- function(ind) {
  d <- arah |> filter(indikator == ind)
  d <- d |> mutate(wilayah = factor(wilayah, levels = wilayah[order(selisih)]))
  satuan <- if (ind == "IPM") "poin" else "poin persen"
  
  ggplot(d, aes(x = selisih, y = wilayah, colour = arah)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_segment(aes(x = 0, xend = selisih, yend = wilayah), linewidth = 1.1) +
    geom_point(size = 3.2) +
    scale_colour_manual(values = pal_arah, name = "Arah perubahan") +
    labs(title = paste("Perubahan", label_indikator[[ind]]),
         subtitle = paste0("Selisih 2025 dikurangi 2023 (", satuan, ")"),
         x = paste("Selisih", satuan), y = NULL, caption = sumber) +
    theme_tim() +
    theme(panel.grid.major.y = element_line(colour = garis, linewidth = 0.25))
}

g7_k <- buat_lollipop("Kemiskinan")
g7_i <- buat_lollipop("IPM")
g7   <- (g7_k | g7_i) +
  plot_annotation(title = "Perubahan 2023-2025 per wilayah",
                  theme = theme(plot.background = element_rect(fill = bg, colour = NA),
                                plot.title = element_text(face = "bold", size = 16,
                                                          colour = "white")))

# ============================================================
# GRAFIK 8. KOMPOSISI MULTIPANEL (patchwork)
# Baris atas : sebaran (A) dan scatterplot (B)
# Baris bawah: heatmap IPM (C), lollipop IPM (D),
#              heatmap kemiskinan (E), lollipop kemiskinan (F)
# ============================================================
atas  <- g1 | g5
bawah <- (g6_i | g7_i | g6_k | g7_k)

g8 <- ((atas / bawah) & labs(caption = NULL)) +
  plot_layout(heights = c(1, 1.15)) +
  plot_annotation(
    title      = "IPM dan kemiskinan di Sulawesi Selatan, 2023-2025",
    subtitle   = "Sebaran, hubungan antar-indikator, nilai per wilayah, dan arah perubahan",
    caption    = sumber,
    tag_levels = "A",
    theme = theme(
      plot.background = element_rect(fill = bg, colour = NA),
      plot.title      = element_text(face = "bold", size = 20, colour = "white"),
      plot.subtitle   = element_text(size = 12, colour = teks_pudar),
      plot.caption    = element_text(size = 9, colour = teks_pudar),
      plot.tag        = element_text(face = "bold", colour = sorot)
    )
  )

# ============================================================
# 4. SIMPAN SEMUA GRAFIK
# ============================================================
simpan <- function(nama, g, w, h) {
  ggsave(file.path(folder_out, nama), g, width = w, height = h,
         dpi = 300, bg = bg)
}

simpan("g1_sebaran_per_tahun.png",      g1, 8,  7)
simpan("g2_sebaran_kab_kota.png",       g2, 12, 6)
simpan("g3_tren_kemiskinan.png",        g3, 14, 8)
simpan("g4_tren_ipm.png",               g4, 14, 8)
simpan("g5_scatter_ipm_kemiskinan.png", g5, 9,  7)
simpan("g6_heatmap.png",                g6, 12, 8)
simpan("g7_lollipop_perubahan.png",     g7, 12, 8)
simpan("g8_komposisi_patchwork.png",    g8, 24, 16)

# Tampilkan di RStudio
g1; g2; g3; g4; g5; g6; g7; g8

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# FUNGSI GRAFIK (IPM DAN KEMISKINAN SULSEL)
#     Fungsi menerima nama kolom tanpa tanda kutip lewat {{ }}
#     Contoh: plot_tren(dat, indikator = IPM)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rlang)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data   <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- paste0(folder_data, "Keluaran_Fungsi_Grafik")

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. PALET DAN TEMA TIM 7 (TEMA GELAP)
# ============================================================
pal_indikator <- c("Kemiskinan" = "#FF6B9D",   # merah muda
                   "IPM"        = "#4CC9F0")   # biru langit

warna_sorot  <- "#FFD60A"   # kuning : sorotan
warna_bg     <- "#0F172A"   # latar gambar
warna_panel  <- "#162036"   # latar panel
warna_garis  <- "#2B3A5C"   # garis grid
warna_teks   <- "#E8ECF4"   # teks utama
warna_pudar  <- "#9AA5BF"   # teks sekunder
sumber_tim   <- "Sumber: BPS, diolah Kelompok 7"

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = warna_garis, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 14, colour = "white"),
          plot.subtitle    = element_text(size = 11, colour = warna_pudar),
          plot.caption     = element_text(size = 8, colour = warna_pudar),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          strip.background = element_rect(fill = "#22325A", colour = NA),
          strip.text       = element_text(face = "bold", size = 9, colour = warna_teks),
          legend.text      = element_text(size = 9, colour = warna_teks),
          legend.title     = element_text(face = "bold", size = 9, colour = warna_teks),
          legend.position  = "bottom")
}

# Pilih warna otomatis dari nama kolom (IPM atau Kemiskinan); jika tidak dikenal, kuning
warna_dari_nama <- function(nama, warna = NULL) {
  if (!is.null(warna)) return(warna)
  if (nama %in% names(pal_indikator)) pal_indikator[[nama]] else warna_sorot
}

# ============================================================
# 3. DATA: BACA DAN RAPIKAN
#    Hasil: dat dengan kolom kode, wilayah, tahun, tipe, Kemiskinan, IPM
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

dat <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    tahun   = as.integer(tahun),
    wilayah = str_trim(wilayah)) |>
  filter(!is.na(kode), !is.na(indikator)) |>
  distinct() |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  mutate(tipe = if_else(str_starts(kode, "737"), "Kota", "Kabupaten"))

glimpse(dat)
stopifnot("Data harus 72 baris (24 wilayah x 3 tahun)" = nrow(dat) == 72,
          "Kolom IPM / Kemiskinan tidak terbentuk" = all(c("IPM", "Kemiskinan") %in% names(dat)),
          "Ada nilai NA pada data" = !anyNA(dat$IPM) && !anyNA(dat$Kemiskinan))

# ============================================================
# 4. FUNGSI GRAFIK (nama kolom diterima lewat {{ }})
#    Semua fungsi memakai pola yang sama:
#    nama kolom ditulis tanpa tanda kutip, misalnya indikator = IPM
# ============================================================

# ------------------------------------------------------------
# 4.1 plot_tren(): tren waktu, satu panel per wilayah (small multiples)
#     Contoh: plot_tren(dat, indikator = IPM)
# ------------------------------------------------------------
plot_tren <- function(data, indikator, waktu = tahun, kelompok = wilayah,
                      warna = NULL, ncol = 6, judul = NULL,
                      sumber = sumber_tim) {
  
  nama <- as_label(enquo(indikator))
  col  <- warna_dari_nama(nama, warna)
  if (is.null(judul)) judul <- paste("Tren", nama, "per kabupaten/kota")
  
  ggplot(data, aes(x = {{ waktu }}, y = {{ indikator }}, group = {{ kelompok }})) +
    geom_line(colour = col, linewidth = 1) +
    geom_point(colour = col, size = 1.8) +
    facet_wrap(vars({{ kelompok }}), ncol = ncol, scales = "free_y") +
    scale_x_continuous(breaks = function(lim) seq(ceiling(lim[1]), floor(lim[2]))) +
    labs(title = judul, x = NULL, y = nama, caption = sumber) +
    theme_tim() +
    theme(axis.text.x = element_text(size = 7))
}

# ------------------------------------------------------------
# 4.2 plot_sebaran(): sebaran nilai per kelompok (violin + boxplot + titik)
#     Contoh: plot_sebaran(dat, indikator = Kemiskinan)
# ------------------------------------------------------------
plot_sebaran <- function(data, indikator, kelompok = tahun,
                         warna = NULL, judul = NULL, sumber = sumber_tim) {
  
  nama <- as_label(enquo(indikator))
  kel  <- as_label(enquo(kelompok))
  col  <- warna_dari_nama(nama, warna)
  if (is.null(judul)) judul <- paste("Sebaran", nama, "menurut", kel)
  
  ggplot(data, aes(x = factor({{ kelompok }}), y = {{ indikator }})) +
    geom_violin(fill = col, alpha = 0.3, colour = NA) +
    geom_boxplot(width = 0.14, outlier.shape = NA, fill = NA,
                 colour = "white", linewidth = 0.45) +
    geom_jitter(width = 0.07, size = 2, colour = warna_sorot, alpha = 0.9) +
    labs(title = judul, x = kel, y = nama, caption = sumber) +
    theme_tim()
}

# ------------------------------------------------------------
# 4.3 plot_scatter(): scatterplot dua indikator + regresi + pita SK 95%
#     Contoh: plot_scatter(dat, x = Kemiskinan, y = IPM, sorot = "Makassar")
# ------------------------------------------------------------
plot_scatter <- function(data, x, y, label = wilayah, sorot = NULL,
                         warna = NULL, level = 0.95, judul = NULL,
                         sumber = sumber_tim) {
  
  nama_x <- as_label(enquo(x))
  nama_y <- as_label(enquo(y))
  col    <- warna_dari_nama(nama_y, warna)
  if (is.null(judul)) judul <- paste(nama_y, "terhadap", nama_x)
  
  data <- data |> mutate(.sorot = {{ label }} %in% sorot)
  
  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_smooth(method = "lm", formula = y ~ x, level = level,
                colour = col, fill = col, alpha = 0.18, linewidth = 1) +
    geom_point(aes(colour = .sorot), size = 2.4) +
    geom_text(data = filter(data, .sorot), aes(label = {{ label }}),
              hjust = -0.15, size = 3, fontface = "bold", colour = warna_sorot) +
    scale_colour_manual(values = c(`FALSE` = warna_pudar, `TRUE` = warna_sorot),
                        guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(title = judul,
         subtitle = paste0("Garis: regresi linear, pita: SK ", level * 100, "%"),
         x = nama_x, y = nama_y, caption = sumber) +
    theme_tim()
}

# ------------------------------------------------------------
# 4.4 plot_peringkat(): lollipop peringkat teratas
#     Contoh: plot_peringkat(filter(dat, tahun == 2025), indikator = IPM, top = 10)
# ------------------------------------------------------------
plot_peringkat <- function(data, indikator, nama = wilayah, top = 10,
                           warna = NULL, judul = NULL, sumber = sumber_tim) {
  
  nama_ind <- as_label(enquo(indikator))
  col      <- warna_dari_nama(nama_ind, warna)
  if (is.null(judul)) judul <- paste(top, "wilayah dengan", nama_ind, "tertinggi")
  
  d <- data |>
    slice_max(order_by = {{ indikator }}, n = top) |>
    mutate(.urut = reorder({{ nama }}, {{ indikator }}))
  
  ggplot(d, aes(x = {{ indikator }}, y = .urut)) +
    geom_segment(aes(x = 0, xend = {{ indikator }}, yend = .urut),
                 colour = col, linewidth = 1.1) +
    geom_point(colour = col, size = 3.4) +
    geom_text(aes(label = sprintf("%.2f", {{ indikator }})),
              hjust = -0.35, size = 3, colour = warna_teks) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = judul, x = nama_ind, y = NULL, caption = sumber) +
    theme_tim() +
    theme(panel.grid.major.y = element_blank())
}

# ============================================================
# 5. PEMAKAIAN: MENGGANTI INDIKATOR CUKUP DENGAN MENGUBAH SATU ARGUMEN
# ============================================================
p_tren_ipm   <- plot_tren(dat, indikator = IPM)
p_tren_kem   <- plot_tren(dat, indikator = Kemiskinan)

p_sebaran_ipm <- plot_sebaran(dat, indikator = IPM)
p_sebaran_kem <- plot_sebaran(dat, indikator = Kemiskinan)

p_scatter    <- plot_scatter(dat, x = Kemiskinan, y = IPM, sorot = "Makassar")

dat25 <- filter(dat, tahun == 2025)
p_rank_ipm   <- plot_peringkat(dat25, indikator = IPM,        top = 10)
p_rank_kem   <- plot_peringkat(dat25, indikator = Kemiskinan, top = 10)

# Tampilkan semua grafik
p_tren_ipm; p_tren_kem
p_sebaran_ipm; p_sebaran_kem
p_scatter
p_rank_ipm; p_rank_kem

# ============================================================
# 6. PEMERIKSAAN: FUNGSI HARUS MENGHASILKAN OBJEK GGPLOT
# ============================================================
semua <- list(p_tren_ipm, p_tren_kem, p_sebaran_ipm, p_sebaran_kem,
              p_scatter, p_rank_ipm, p_rank_kem)
stopifnot("Ada fungsi yang tidak menghasilkan grafik ggplot" =
            all(vapply(semua, inherits, logical(1), what = "ggplot")))
cat("Pemeriksaan fungsi grafik: LULUS (", length(semua), "grafik dibuat)\n")

# ============================================================
# 7. SIMPAN
# ============================================================
simpan <- function(nama_berkas, g, w, h) {
  ggsave(file.path(folder_out, nama_berkas), g, width = w, height = h,
         dpi = 300, bg = warna_bg)
}

simpan("tren_ipm.png",        p_tren_ipm,    14, 8)
simpan("tren_kemiskinan.png", p_tren_kem,    14, 8)
simpan("sebaran_ipm.png",     p_sebaran_ipm,  7, 5)
simpan("sebaran_kemiskinan.png", p_sebaran_kem, 7, 5)
simpan("scatter_ipm_kemiskinan.png", p_scatter, 9, 6)
simpan("peringkat_ipm_2025.png",        p_rank_ipm, 8, 5)
simpan("peringkat_kemiskinan_2025.png", p_rank_kem, 8, 5)

cat("Semua grafik tersimpan di:", folder_out, "\n")

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# 24 PROFIL KABUPATEN/KOTA OTOMATIS (purrr::walk + ggsave)
# Indikator: IPM dan Kemiskinan, Sulawesi Selatan 2023-2025
# ============================================================

library(tidyverse)   # dplyr, tidyr, ggplot2, purrr, stringr, forcats
library(readxl)
library(patchwork)
library(sf)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data   <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data     <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
path_batas    <- paste0(folder_data, "geoBoundaries-IDN-ADM2_simplified.geojson")
folder_profil <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Output/Profil_Sulsel/profil"

stopifnot("File data tidak ditemukan, cek path_data"   = file.exists(path_data),
          "File batas tidak ditemukan, cek path_batas" = file.exists(path_batas))
dir.create(folder_profil, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. TEMA DAN PALET TIM 7 (TEMA GELAP)
# ============================================================
# Aturan warna: merah muda = Kemiskinan, biru langit = IPM,
#               kuning = sorotan wilayah yang sedang diprofilkan
pal_indikator <- c("Kemiskinan" = "#FF6B9D",
                   "IPM"        = "#4CC9F0")

warna_sorot <- "#FFD60A"
warna_bg    <- "#0F172A"
warna_panel <- "#162036"
warna_garis <- "#2B3A5C"
warna_teks  <- "#E8ECF4"
warna_pudar <- "#9AA5BF"
sumber      <- "Sumber: BPS dan geoBoundaries, diolah Kelompok 7"

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = warna_garis, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 13, colour = "white"),
          plot.subtitle    = element_text(size = 10, colour = warna_pudar),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          legend.text      = element_text(size = 9, colour = warna_teks),
          legend.title     = element_text(face = "bold", size = 9, colour = warna_teks),
          legend.position  = "bottom")
}

# ============================================================
# 3. DATA (WIDE -> LONG -> WIDE PER TAHUN)
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

dat_long <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    tahun   = as.integer(tahun),
    wilayah = str_trim(wilayah)) |>
  filter(!is.na(kode)) |>
  distinct()

stopifnot("Nama kolom indikator tidak terbaca, cek names(data_wide)" =
            !anyNA(dat_long$indikator))

dat <- dat_long |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  rename(kemiskinan = Kemiskinan, ipm = IPM)

glimpse(dat)
stopifnot("Data harus 72 baris (24 wilayah x 3 tahun)" = nrow(dat) == 72,
          "Ada nilai NA pada data" = !anyNA(dat$kemiskinan), !anyNA(dat$ipm))

# Rata-rata sederhana 24 kabupaten/kota per tahun (pembanding)
rata <- dat |>
  group_by(tahun) |>
  summarise(kemiskinan = mean(kemiskinan), ipm = mean(ipm), .groups = "drop")

# ============================================================
# 4. BATAS WILAYAH (UNTUK PETA MINI)
# ============================================================
norm <- function(x) {
  x |> str_to_lower() |> str_remove("^kota |^kabupaten ") |> str_squish()
}

batas <- st_read(path_batas, quiet = TRUE) |> mutate(kunci = norm(shapeName))

peta <- batas |>
  inner_join(dat |> distinct(wilayah) |> mutate(kunci = norm(wilayah)),
             by = "kunci")

cat("Jumlah wilayah di peta:", nrow(peta), "(harus 24)\n")
stopifnot("Jumlah wilayah pada peta bukan 24, cek nama wilayah" = nrow(peta) == 24)

# ============================================================
# 5. FUNGSI PANEL (nama kolom diterima lewat {{ }})
# ============================================================

# 5.1 Panel tren: wilayah vs rata-rata Sulsel
panel_tren <- function(d, r, y, warna, nama, label_y) {
  gab <- bind_rows(
    d |> transmute(tahun, seri = nama, nilai = {{ y }}),
    r |> transmute(tahun, seri = "Rata-rata Sulsel", nilai = {{ y }})
  )
  seri <- c(nama, "Rata-rata Sulsel")
  
  ggplot(gab, aes(tahun, nilai, colour = seri, linetype = seri)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = setNames(c(warna, warna_pudar), seri), name = NULL) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), seri), name = NULL) +
    scale_x_continuous(breaks = sort(unique(gab$tahun))) +
    labs(x = NULL, y = label_y) +
    theme_tim()
}

# 5.2 Panel posisi: titik semua wilayah pada tahun acuan, wilayah terpilih disorot
panel_posisi <- function(data, nilai, nama, tahun_acuan, label_x) {
  pa <- data |>
    filter(tahun == tahun_acuan) |>
    mutate(sorot   = wilayah == nama,
           wilayah = fct_reorder(wilayah, {{ nilai }}))
  
  rata_x <- mean(pull(pa, {{ nilai }}))
  
  ggplot(pa, aes(x = {{ nilai }}, y = wilayah)) +
    geom_vline(xintercept = rata_x, colour = warna_pudar,
               linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(colour = sorot, size = sorot)) +
    geom_text(data = filter(pa, sorot),
              aes(label = sprintf("%.1f", {{ nilai }})),
              hjust = -0.5, size = 3.2, fontface = "bold", colour = warna_sorot) +
    scale_colour_manual(values = c(`FALSE` = warna_pudar, `TRUE` = warna_sorot),
                        guide = "none") +
    scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 4), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = paste(label_x, tahun_acuan),
         subtitle = "Garis putus-putus: rata-rata 24 wilayah",
         x = label_x, y = NULL) +
    theme_tim() +
    theme(axis.text.y = element_text(size = 7))
}

# ============================================================
# 6. FUNGSI PROFIL SATU WILAYAH (MENYIMPAN 1 GAMBAR)
#    Untuk demo: ubah `tahun_acuan` (misalnya 2024)
# ============================================================
profil_wilayah <- function(nama, data = dat, rerata = rata,
                           peta_sf = peta, tahun_acuan = 2025,
                           folder = folder_profil) {
  
  d <- data |> filter(wilayah == nama)
  
  # Panel A dan B: tren
  gA <- panel_tren(d, rerata, kemiskinan, pal_indikator[["Kemiskinan"]],
                   nama, "Kemiskinan (%)") +
    labs(title = "Tren kemiskinan")
  gB <- panel_tren(d, rerata, ipm, pal_indikator[["IPM"]],
                   nama, "IPM") +
    labs(title = "Tren IPM")
  
  # Panel C dan D: posisi di antara 24 wilayah
  gC <- panel_posisi(data, kemiskinan, nama, tahun_acuan, "Kemiskinan (%) tahun")
  gD <- panel_posisi(data, ipm,        nama, tahun_acuan, "IPM tahun")
  
  # Panel E: peta mini, wilayah disorot
  gE <- ggplot(peta_sf) +
    geom_sf(aes(fill = wilayah == nama), colour = warna_bg, linewidth = 0.25) +
    scale_fill_manual(values = c(`FALSE` = warna_garis, `TRUE` = warna_sorot),
                      guide = "none") +
    labs(title = "Lokasi") +
    theme_void() +
    theme(plot.title      = element_text(face = "bold", size = 13, colour = "white"),
          plot.background = element_rect(fill = warna_bg, colour = NA))
  
  # Angka ringkas untuk subjudul
  pa <- data |> filter(tahun == tahun_acuan)
  k  <- d$kemiskinan[d$tahun == tahun_acuan]
  i  <- d$ipm[d$tahun == tahun_acuan]
  peringkat_k <- sum(pa$kemiskinan > k) + 1   # 1 = kemiskinan tertinggi
  peringkat_i <- sum(pa$ipm > i) + 1          # 1 = IPM tertinggi
  
  g <- ((gA | gB) / (gC | gD | gE)) +
    plot_layout(heights = c(1, 1.5)) +
    plot_annotation(
      title = nama,
      subtitle = sprintf(
        "%d: kemiskinan %.2f%% (peringkat %d dari 24, 1 = tertinggi) | IPM %.2f (peringkat %d dari 24, 1 = tertinggi)",
        tahun_acuan, k, peringkat_k, i, peringkat_i),
      caption = sumber,
      theme = theme(
        plot.background = element_rect(fill = warna_bg, colour = NA),
        plot.title      = element_text(face = "bold", size = 20, colour = "white"),
        plot.subtitle   = element_text(size = 11, colour = warna_pudar),
        plot.caption    = element_text(size = 8, colour = warna_pudar))
    )
  
  berkas <- file.path(folder, paste0("profil_", str_replace_all(nama, " ", "_"), ".png"))
  ggsave(berkas, g, width = 14, height = 9, dpi = 150, bg = warna_bg)
  message("Tersimpan: ", basename(berkas))
  invisible(berkas)
}

# ============================================================
# 7. UJI SATU WILAYAH DULU (LIHAT HASILNYA DI FOLDER PROFIL)
# ============================================================
profil_wilayah("Bone")

# ============================================================
# 8. SATU PERINTAH: 24 PROFIL SEKALIGUS
# ============================================================
walk(sort(unique(dat$wilayah)), profil_wilayah)

cat(length(list.files(folder_profil, pattern = "\\.png$")),
    "file profil tersimpan di", folder_profil, "\n")   # harus 24

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# Peta koroplet Sulawesi Selatan (sf) + uji buta warna (colorspace)
# Indikator: IPM dan Kemiskinan, tahun 2023-2025
# =============================================================================

# =============================================================================
# 1. LOKASI FILE
# =============================================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_rds    <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.rds")
folder_out  <- paste0(folder_data, "Peta_Tim7")

stopifnot("File RDS tidak ditemukan, cek path_rds" = file.exists(path_rds))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 2. PAKET
# =============================================================================
for (p in c("ggplot2", "sf", "dplyr", "tidyr", "stringr",
            "colorspace", "patchwork", "rlang")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Paket '", p, "' belum terpasang. Jalankan: install.packages('", p, "')",
         call. = FALSE)
  }
}
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# 3. TEMA DAN PALET TIM 7 (TEMA GELAP, AMAN BUTA WARNA)
# =============================================================================
pal_tim7 <- list(
  bg         = "#0F172A",   # latar gambar
  panel      = "#162036",   # latar peta
  tepi       = "#0F172A",   # garis batas wilayah (sama dengan latar)
  teks       = "#E8ECF4",   # teks utama
  judul      = "white",
  pudar      = "#9AA5BF",   # teks sekunder
  abu_terang = "#5C6B8A",   # wilayah tanpa data
  sorot      = "#FFD60A"    # label dan garis penunjuk
)
sumber_tim <- "Sumber: BPS dan geoBoundaries, diolah Kelompok 7."

# Jangkar warna (gelap -> terang). Warna antarkelas diinterpolasi di ruang CIELAB
# sehingga L* tetap monoton. Sudah diuji: urutan L* terjaga pada deuteranopia,
# protanopia, tritanopia, dan grayscale untuk 3-7 kelas (lihat cek_palet_peta()).
jangkar_palet <- list(
  ipm        = c("#2E3C8C", "#2A8AB5", "#4CC9F0", "#F2FDFF"),             # biru -> biru muda
  kemiskinan = c("#3B2C85", "#7B3F9E", "#C2559B", "#F58FB0", "#FFD8C2", "#FFF8EC")  # ungu -> merah muda -> krem
)

# palet_kelas(): n warna untuk satu indikator
#   "tim7"    : jangkar Tim 7 di atas (bawaan)
#   "cividis" : alternatif ramah buta warna dari viridisLite (untuk indikator lain)
palet_kelas <- function(nama, n, palet = c("tim7", "cividis")) {
  palet <- match.arg(palet)
  if (palet == "tim7" && nama %in% names(jangkar_palet)) {
    grDevices::colorRampPalette(jangkar_palet[[nama]], space = "Lab")(n)
  } else {
    if (!requireNamespace("viridisLite", quietly = TRUE)) {
      stop("Paket 'viridisLite' diperlukan untuk palet cividis.", call. = FALSE)
    }
    viridisLite::cividis(n, begin = 0.2, end = 1)
  }
}

# simulasi buta warna pada vektor warna hex
simulasi_warna <- function(warna, simulasi = c("normal", "deutan", "protan", "tritan")) {
  simulasi <- match.arg(simulasi)
  switch(simulasi,
         normal = warna,
         deutan = colorspace::deutan(warna),
         protan = colorspace::protan(warna),
         tritan = colorspace::tritan(warna))
}

theme_peta_tim7 <- function(legend = "right") {
  theme_void(base_family = "sans", base_size = 11) +
    theme(text               = element_text(colour = pal_tim7$teks),
          plot.background    = element_rect(fill = pal_tim7$bg, colour = NA),
          panel.background   = element_rect(fill = pal_tim7$panel, colour = NA),
          plot.title.position = "plot",
          plot.title         = element_text(face = "bold", size = 14,
                                            colour = pal_tim7$judul,
                                            margin = margin(b = 4)),
          plot.subtitle      = element_text(size = 10, colour = pal_tim7$pudar,
                                            margin = margin(b = 8)),
          plot.caption       = element_text(size = 8, colour = pal_tim7$pudar,
                                            hjust = 0, margin = margin(t = 8)),
          legend.position    = legend,
          legend.title       = element_text(face = "bold", size = 9.9,
                                            colour = pal_tim7$teks),
          legend.text        = element_text(size = 9.35, colour = pal_tim7$teks),
          plot.margin        = margin(12, 14, 10, 12))
}

# =============================================================================
# 4. DATA: sf lebar -> sf panjang (satu baris = satu wilayah x tahun)
#    Kolom hasil: kode_wilayah, kabupaten, tahun, ipm, kemiskinan, geometry
# =============================================================================
d_mentah <- readRDS(path_rds)
if (!inherits(d_mentah, "sf")) {
  stop("File RDS bukan objek sf (tidak memuat geometri peta).", call. = FALSE)
}
nm <- names(d_mentah)
print(nm)   # cek nama kolom hasil impor

# Nama kolom dicari otomatis: boleh "IPM.2025" atau "IPM_2025", "Kab.Kota" atau "Kab_Kota"
kol_kode  <- nm[str_detect(str_to_lower(nm), "^kode")][1]
kol_nama  <- nm[str_detect(str_to_lower(nm), "^kab")][1]
kol_nilai <- nm[str_detect(str_to_lower(nm), "^(ipm|kemiskinan)[._ ]+\\d{4}$")]
stopifnot("Kolom kode wilayah tidak ditemukan"       = !is.na(kol_kode),
          "Kolom nama kabupaten/kota tidak ditemukan" = !is.na(kol_nama),
          "Kolom IPM/Kemiskinan per tahun tidak ditemukan" = length(kol_nilai) == 6)

atribut <- d_mentah |>
  st_drop_geometry() |>
  select(all_of(c(kol_kode, kol_nama, kol_nilai))) |>
  rename(kode_wilayah = all_of(kol_kode), kabupaten = all_of(kol_nama))

panjang <- atribut |>
  pivot_longer(all_of(kol_nilai),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "ipm",
    str_detect(str_to_lower(indikator), "miskin") ~ "kemiskinan",
    TRUE ~ NA_character_),
    tahun = as.integer(tahun)) |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  mutate(kode_wilayah = as.character(kode_wilayah),
         # awalan "Kota"/"Kabupaten" dibuang agar label peta singkat
         kabupaten = str_squish(str_remove(kabupaten,
                                           regex("^(kabupaten|kab\\.?|kota)\\s+", ignore_case = TRUE))))

geometri <- d_mentah |>
  select(all_of(kol_kode)) |>
  rename(kode_wilayah = all_of(kol_kode)) |>
  mutate(kode_wilayah = as.character(kode_wilayah))

d <- geometri |> left_join(panjang, by = "kode_wilayah")

cat("Baris:", nrow(d), "| Kolom:", paste(names(d), collapse = ", "), "\n")
stopifnot("Harus 24 wilayah"           = n_distinct(d$kabupaten) == 24,
          "Harus 72 baris (24 x 3)"    = nrow(d) == 72,
          "Ada nilai kosong pada data" = !anyNA(d$ipm) && !anyNA(d$kemiskinan))

# =============================================================================
# 5. FUNGSI PETA
# =============================================================================

# format angka dengan koma desimal
koma <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",")

label_indikator <- function(nama) {
  acuan <- c(ipm = "Indeks Pembangunan Manusia (IPM)", kemiskinan = "Kemiskinan (%)")
  if (nama %in% names(acuan)) acuan[[nama]] else nama
}
singkat_indikator <- function(nama) {
  acuan <- c(ipm = "IPM", kemiskinan = "Kemiskinan")
  if (nama %in% names(acuan)) acuan[[nama]] else nama
}

# NULL = teks otomatis; NA = tanpa teks; selain itu dipakai apa adanya
atur_teks <- function(x, otomatis) {
  if (is.null(x)) otomatis else if (length(x) == 1 && is.na(x)) NULL else x
}

# -----------------------------------------------------------------------------
# bagi_kelas(): membagi nilai numerik menjadi kelas berlabel (faktor berurutan)
#   metode "kuantil": tiap kelas memuat kira-kira jumlah wilayah yang sama
#   metode "sama"   : lebar interval tiap kelas sama
# -----------------------------------------------------------------------------
bagi_kelas <- function(x, n = 5, metode = c("kuantil", "sama"), digit = 2) {
  metode <- match.arg(metode)
  stopifnot(n >= 3, n <= 7)
  br <- if (metode == "kuantil") {
    unique(stats::quantile(x, probs = seq(0, 1, length.out = n + 1),
                           na.rm = TRUE, names = FALSE))
  } else {
    seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  }
  k <- length(br) - 1
  if (k < 3) {
    stop("Nilai terlalu sedikit yang berbeda untuk dibagi menjadi minimal 3 kelas.",
         call. = FALSE)
  }
  if (k < n) {
    warning("Batas kelas kembar; jumlah kelas dikurangi menjadi ", k, ".", call. = FALSE)
  }
  lab <- paste0(koma(br[-length(br)], digit), " - ", koma(br[-1], digit))
  cut(x, breaks = br, include.lowest = TRUE, labels = lab)
}

# -----------------------------------------------------------------------------
# titik_label_peta(): posisi label untuk wilayah yang ingin ditandai.
# Label digeser ke area kosong (laut) dan dihubungkan dengan garis tipis ke titik
# di dalam wilayahnya. Teks label: nama wilayah dan nilainya, mis. "Makassar (9,60)".
# -----------------------------------------------------------------------------
titik_label_peta <- function(dat, nilai, pilih) {
  idx <- match(pilih, as.character(dat$kabupaten))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NULL)
  geom <- sf::st_geometry(dat)[idx]
  # jangkar = titik di dalam poligon terbesar (kota punya banyak pulau kecil)
  jangkar <- t(vapply(seq_along(geom), function(i) {
    bagian <- suppressWarnings(sf::st_cast(geom[i], "POLYGON"))
    besar  <- bagian[which.max(as.numeric(sf::st_area(bagian)))]
    as.numeric(sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(besar)))[1, 1:2])
  }, numeric(2)))
  lab <- data.frame(kabupaten = as.character(dat$kabupaten)[idx],
                    x = jangkar[, 1], y = jangkar[, 2], nilai = nilai[idx])
  lab$teks <- paste0(lab$kabupaten, " (", koma(lab$nilai), ")")
  
  # pergeseran label (derajat bujur/lintang) untuk kota di pesisir
  geser <- data.frame(kabupaten = c("Makassar", "Parepare", "Palopo"),
                      dx = c(-1.10, -1.15, 0.95),
                      dy = c(-0.25,  0.00, -0.25))
  lab <- merge(lab, geser, by = "kabupaten", all.x = TRUE, sort = FALSE)
  # wilayah lain: label di sisi barat (Selat Makassar) bila wilayahnya di separuh
  # barat, atau di sisi timur (Teluk Bone) bila di separuh timur
  oto <- is.na(lab$dx)
  lab$dx[oto] <- ifelse(lab$x[oto] < 120.4, -1.15, 0.95)
  lab$dy[is.na(lab$dy)] <- 0
  lab$xl <- lab$x + lab$dx
  lab$yl <- lab$y + lab$dy
  lab
}

# -----------------------------------------------------------------------------
# plot_peta(): peta koroplet satu indikator, satu tahun
# -----------------------------------------------------------------------------
plot_peta <- function(data, indikator, tahun,
                      tipe = c("kelas", "kontinu"),
                      n_kelas = 5,
                      metode = c("kuantil", "sama"),
                      palet = c("tim7", "cividis"),
                      label_kota = TRUE,
                      label_wilayah = c("tertinggi_terendah", "kota", "tiga_tertinggi"),
                      simulasi = c("normal", "deutan", "protan", "tritan"),
                      legenda = c("kanan", "bawah"),
                      judul_legenda = NULL,
                      ukuran_legenda = 1,
                      nrow_legenda = NULL,
                      judul = NULL, subjudul = NULL, caption = NULL) {
  tipe          <- match.arg(tipe)
  metode        <- match.arg(metode)
  palet         <- match.arg(palet)
  simulasi      <- match.arg(simulasi)
  label_wilayah <- match.arg(label_wilayah)
  legenda       <- match.arg(legenda)
  
  # 1. Nama kolom dari argumen (hanya nama polos)
  q <- rlang::enquo(indikator)
  if (!rlang::quo_is_symbol(q)) {
    stop("indikator harus nama kolom tanpa tanda kutip, mis. indikator = ipm.",
         call. = FALSE)
  }
  nama <- rlang::as_name(q)
  
  # 2. Data: harus sf, kolom lengkap, satu tahun, tiap wilayah sekali
  if (!inherits(data, "sf")) {
    stop("data harus objek sf (memuat geometri peta).", call. = FALSE)
  }
  hilang <- setdiff(c(nama, "kabupaten", "tahun"), names(data))
  if (length(hilang) > 0) {
    stop("Kolom tidak ditemukan: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  dat <- data[data$tahun == tahun, ]
  if (nrow(dat) == 0) stop("Tidak ada data untuk tahun ", tahun, ".", call. = FALSE)
  if (anyDuplicated(dat$kabupaten)) {
    stop("Ada wilayah yang muncul lebih dari sekali pada tahun ", tahun, ".", call. = FALSE)
  }
  
  nilai <- dat[[nama]]
  if (all(is.na(nilai))) stop("Semua nilai ", nama, " kosong pada tahun ", tahun, ".", call. = FALSE)
  if (any(is.na(nilai))) message(sum(is.na(nilai)), " wilayah tanpa data (diberi warna abu).")
  
  # 3. Kelas (bila tipe = "kelas")
  if (tipe == "kelas") {
    dat$kelas <- bagi_kelas(nilai, n = n_kelas, metode = metode)
    n_eff <- nlevels(dat$kelas)
  }
  
  # 4. Skala warna: palet aman buta warna, atau versi simulasi buta warna
  nama_legend <- if (is.null(judul_legenda)) label_indikator(nama) else judul_legenda
  abu <- simulasi_warna(pal_tim7$abu_terang, simulasi)
  skala <- if (tipe == "kelas") {
    ggplot2::scale_fill_manual(
      values = simulasi_warna(palet_kelas(nama, n_eff, palet), simulasi),
      name = nama_legend, drop = FALSE, na.value = abu)
  } else {
    ggplot2::scale_fill_gradientn(
      colours = simulasi_warna(palet_kelas(nama, 9, palet), simulasi),
      name = nama_legend, na.value = abu)
  }
  
  # 5. Teks otomatis
  i_max <- which.max(nilai)
  i_min <- which.min(nilai)
  
  judul_oto <- paste(strwrap(
    paste0(singkat_indikator(nama), " ", tahun, ": Tertinggi di ", dat$kabupaten[i_max],
           " (", koma(nilai[i_max]), "), Terendah di ", dat$kabupaten[i_min],
           " (", koma(nilai[i_min]), ")"),
    width = 70), collapse = "\n")
  subjudul_oto <- if (tipe == "kelas") {
    paste0(n_eff, " kelas ",
           if (metode == "kuantil") "kuantil (tiap kelas memuat kira-kira jumlah wilayah yang sama)"
           else "dengan interval sama",
           ". Semakin terang, semakin tinggi.")
  } else {
    "Skala warna kontinu. Semakin terang, semakin tinggi."
  }
  caption_oto <- paste0(sumber_tim, " Palet aman buta warna.",
                        if (any(is.na(nilai))) " Abu: tidak ada data." else "")
  
  # 6. Gambar: poligon wilayah dengan garis batas berwarna latar
  g <- ggplot(dat)
  if (tipe == "kelas") {
    g <- g + geom_sf(aes(fill = kelas), color = pal_tim7$tepi, linewidth = 0.3)
  } else {
    g <- g + geom_sf(aes(fill = {{ indikator }}), color = pal_tim7$tepi, linewidth = 0.3)
  }
  g <- g + skala
  
  # label wilayah terpilih: garis penghubung, titik jangkar, lalu kotak label
  if (label_kota) {
    pilih <- switch(
      label_wilayah,
      tertinggi_terendah = unique(as.character(dat$kabupaten[c(i_max, i_min)])),
      kota = if ("kode_wilayah" %in% names(dat)) {
        as.character(dat$kabupaten[as.character(dat$kode_wilayah) %in% c("7371", "7372", "7373")])
      } else {
        character(0)
      },
      tiga_tertinggi = as.character(dat$kabupaten[
        order(nilai, decreasing = TRUE)[seq_len(min(3, sum(!is.na(nilai))))]])
    )
    lab <- titik_label_peta(dat, nilai, pilih)
    if (!is.null(lab)) {
      g <- g +
        geom_segment(data = lab, aes(x = x, y = y, xend = xl, yend = yl),
                     inherit.aes = FALSE, color = pal_tim7$sorot, linewidth = 0.4) +
        geom_point(data = lab, aes(x = x, y = y), inherit.aes = FALSE,
                   color = pal_tim7$sorot, size = 1.1) +
        geom_label(data = lab, aes(x = xl, y = yl, label = teks),
                   inherit.aes = FALSE, fill = pal_tim7$bg, color = pal_tim7$sorot,
                   size = 3, fontface = "bold")
    }
  }
  
  # Ukuran kotak warna legenda: di kanan = kotak kecil satu kolom; di bawah = mendatar
  kanan        <- legenda == "kanan"
  lebar_kunci  <- if (kanan) 1.8 else if (tipe == "kelas") 2.4 else 12
  tinggi_kunci <- if (kanan) { if (tipe == "kelas") 1.3 else 9 } else 0.9
  tema_legenda <- if (ukuran_legenda != 1) {
    theme(legend.text  = element_text(size = 9.35 * ukuran_legenda, color = pal_tim7$teks),
          legend.title = element_text(size = 9.9 * ukuran_legenda, face = "bold",
                                      color = pal_tim7$teks))
  } else {
    theme()
  }
  g +
    guides(fill = if (tipe == "kelas") {
      if (kanan) {
        guide_legend(ncol = 1)
      } else {
        guide_legend(nrow = if (!is.null(nrow_legenda)) nrow_legenda else if (n_eff > 4) 2 else 1,
                     byrow = TRUE)
      }
    } else {
      guide_colourbar()
    }) +
    labs(title    = atur_teks(judul, judul_oto),
         subtitle = atur_teks(subjudul, subjudul_oto),
         caption  = atur_teks(caption, caption_oto),
         x = NULL, y = NULL) +
    theme_peta_tim7(legend = if (kanan) "right" else "bottom") +
    theme(legend.title.position = "top",
          legend.key.width  = grid::unit(lebar_kunci * ukuran_legenda, "lines"),
          legend.key.height = grid::unit(tinggi_kunci * ukuran_legenda, "lines")) +
    tema_legenda
}

# =============================================================================
# 6. UJI BUTA WARNA
# =============================================================================

# -----------------------------------------------------------------------------
# cek_palet_peta(): uji angka. Peta sekuensial dibaca lewat tingkat terang, jadi
# yang diuji: apakah urutan terang tetap MENAIK (gelap -> terang) antarkelas, dan
# seberapa besar selisih terang (L*) terkecil antara dua kelas bersebelahan, pada
# tiap simulasi. Kriteria tim: urutan tetap menaik dan selisih terkecil >= ambang.
# -----------------------------------------------------------------------------
cek_palet_peta <- function(nama, n_kelas = 5, palet = "tim7", ambang = 6, cetak = TRUE) {
  warna <- palet_kelas(nama, n_kelas, palet)
  kond <- list(
    "Normal"       = warna,
    "Deuteranopia" = colorspace::deutan(warna),
    "Protanopia"   = colorspace::protan(warna),
    "Tritanopia"   = colorspace::tritan(warna),
    "Grayscale"    = colorspace::desaturate(warna, 1)
  )
  terang <- function(hex) {
    as.numeric(colorspace::coords(methods::as(colorspace::hex2RGB(hex), "LAB"))[, "L"])
  }
  hasil <- do.call(rbind, lapply(names(kond), function(k) {
    sel <- diff(terang(kond[[k]]))
    data.frame(kondisi = k,
               urutan_terjaga = all(sel > 0),
               selisih_L_terkecil = round(min(abs(sel)), 1),
               lolos = all(sel > 0) && min(abs(sel)) >= ambang)
  }))
  if (cetak) {
    cat("\nUji palet peta '", nama, "' (", length(warna), " warna; ambang selisih L* = ",
        ambang, ")\n", sep = "")
    print(hasil, row.names = FALSE)
    wajib <- hasil$lolos[hasil$kondisi %in% c("Deuteranopia", "Protanopia")]
    cat("\nHasil untuk syarat tugas (deuteranopia dan protanopia): ",
        if (all(wajib)) "LOLOS" else "TIDAK LOLOS", "\n", sep = "")
  }
  invisible(hasil)
}

# -----------------------------------------------------------------------------
# uji_buta_warna_peta(): peta yang sama dalam 3 tampilan
# -----------------------------------------------------------------------------
uji_buta_warna_peta <- function(data, indikator, tahun,
                                tipe = c("kelas", "kontinu"),
                                n_kelas = 5,
                                metode = c("kuantil", "sama"),
                                palet = c("tim7", "cividis")) {
  tipe   <- match.arg(tipe)
  metode <- match.arg(metode)
  palet  <- match.arg(palet)
  nama   <- rlang::as_name(rlang::enquo(indikator))
  
  gaya <- theme(plot.title = element_text(size = 12, color = pal_tim7$teks,
                                          margin = margin(b = 10)),
                plot.margin = margin(t = 6, r = 18, b = 6, l = 6),
                legend.box.spacing = grid::unit(1, "lines"))
  # judul legenda dipecah 2 baris agar peta di tiap panel tetap besar
  jl <- paste(strwrap(label_indikator(nama), width = 20), collapse = "\n")
  
  panel <- function(sim, judul_panel) {
    plot_peta(data, {{ indikator }}, tahun = tahun, tipe = tipe, n_kelas = n_kelas,
              metode = metode, palet = palet, label_kota = FALSE, simulasi = sim,
              legenda = "kanan", judul_legenda = jl, ukuran_legenda = 0.85,
              judul = judul_panel, subjudul = NA, caption = NA) + gaya
  }
  p1 <- panel("normal", "Penglihatan normal")
  p2 <- panel("deutan", "Deuteranopia (simulasi)")
  p3 <- panel("protan", "Protanopia (simulasi)")
  
  patchwork::wrap_plots(p1, p2, p3, nrow = 1) +
    patchwork::plot_annotation(
      title   = paste0("Uji Buta Warna: Peta ", singkat_indikator(nama), " ", tahun),
      caption = paste0("Simulasi dengan colorspace::deutan() dan colorspace::protan(). ",
                       "Urutan kelas dari gelap ke terang harus tetap terbaca."),
      theme   = theme(plot.background = element_rect(fill = pal_tim7$bg, colour = NA),
                      plot.title   = element_text(face = "bold", size = 16,
                                                  color = pal_tim7$judul,
                                                  margin = margin(b = 10)),
                      plot.caption = element_text(size = 9, color = pal_tim7$pudar,
                                                  hjust = 0, margin = margin(t = 14)),
                      plot.margin  = margin(10, 12, 10, 12)))
}

# =============================================================================
# 7. PEMAKAIAN
# =============================================================================

# G1. Peta utama 2025: 5 kelas kuantil, palet aman buta warna
peta_kemiskinan_2025 <- plot_peta(d, indikator = kemiskinan, tahun = 2025)
peta_ipm_2025        <- plot_peta(d, indikator = ipm,        tahun = 2025)
print(peta_kemiskinan_2025)
print(peta_ipm_2025)

# Batas kelas dan jumlah wilayah per kelas (catatan untuk atlas)
d2025 <- d[d$tahun == 2025, ]
cat("\nJumlah wilayah per kelas, Kemiskinan 2025:\n")
print(table(bagi_kelas(d2025$kemiskinan, n = 5, metode = "kuantil")))
cat("\nJumlah wilayah per kelas, IPM 2025:\n")
print(table(bagi_kelas(d2025$ipm, n = 5, metode = "kuantil")))

#-------------------------------------------------------------------------------
# DEMO
# plot_peta(d, indikator = ipm, tahun = 2023)
# plot_peta(d, indikator = kemiskinan, tahun = 2024, tipe = "kontinu")
# plot_peta(d, indikator = ipm, tahun = 2025, n_kelas = 4, metode = "sama")
# plot_peta(d, indikator = ipm, tahun = 2025, label_kota = FALSE)
# plot_peta(d, indikator = kemiskinan, tahun = 2025, label_wilayah = "kota")
# plot_peta(d, indikator = ipm, tahun = 2025, label_wilayah = "tiga_tertinggi")
# plot_peta(d, indikator = ipm, tahun = 2025, palet = "cividis")
#-------------------------------------------------------------------------------

# G2. Uji buta warna: tabel angka, ringkasan 3-7 kelas, lalu peta dalam 3 tampilan
cek_palet_peta("kemiskinan", n_kelas = 5)
cek_palet_peta("ipm",        n_kelas = 5)

ringkas <- do.call(rbind, lapply(c("kemiskinan", "ipm"), function(ind) {
  do.call(rbind, lapply(3:7, function(n) {
    h <- cek_palet_peta(ind, n, cetak = FALSE)
    data.frame(indikator = ind, n_kelas = n,
               selisih_L_terkecil = min(h$selisih_L_terkecil),
               lolos_semua_kondisi = all(h$lolos))
  }))
}))
cat("\nRingkasan uji palet untuk 3-7 kelas (semua kondisi):\n")
print(ringkas, row.names = FALSE)

uji_kemiskinan <- uji_buta_warna_peta(d, indikator = kemiskinan, tahun = 2025)
uji_ipm        <- uji_buta_warna_peta(d, indikator = ipm,        tahun = 2025)
print(uji_kemiskinan)
print(uji_ipm)

# G3. Simpan gambar
ggsave(file.path(folder_out, "peta_kemiskinan_2025_Tim7.png"), peta_kemiskinan_2025,
       width = 9, height = 8, dpi = 300, bg = pal_tim7$bg)
ggsave(file.path(folder_out, "peta_ipm_2025_Tim7.png"), peta_ipm_2025,
       width = 9, height = 8, dpi = 300, bg = pal_tim7$bg)
ggsave(file.path(folder_out, "uji_buta_warna_kemiskinan_2025_Tim7.png"), uji_kemiskinan,
       width = 15, height = 6.5, dpi = 300, bg = pal_tim7$bg)
ggsave(file.path(folder_out, "uji_buta_warna_ipm_2025_Tim7.png"), uji_ipm,
       width = 15, height = 6.5, dpi = 300, bg = pal_tim7$bg)
write.csv(ringkas, file.path(folder_out, "ringkasan_uji_palet_Tim7.csv"), row.names = FALSE)
message("Gambar peta tersimpan di: ", folder_out)

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# SELANG KEPERCAYAAN BOOTSTRAP 95% (PERCENTILE)
# Indikator: Kemiskinan dan IPM, Sulawesi Selatan 2023-2025
# Tampilan : titik + garis galat, diurutkan menurut rata-rata
# ============================================================
rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data   <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Output/Keluaran_Bootstrap"

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. TEMA DAN PALET TIM 7 (TEMA GELAP)
# ============================================================
pal_indikator <- c("Kemiskinan" = "#FF6B9D", "IPM" = "#4CC9F0")
warna_sorot   <- "#FFD60A"
warna_bg      <- "#0F172A"
warna_panel   <- "#162036"
warna_garis   <- "#2B3A5C"
warna_teks    <- "#E8ECF4"
warna_pudar   <- "#9AA5BF"
sumber        <- "Sumber: BPS, diolah Kelompok 7"

label_indikator <- c("Kemiskinan" = "Kemiskinan (%)",
                     "IPM"        = "Indeks Pembangunan Manusia")

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = warna_garis, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 14, colour = "white"),
          plot.subtitle    = element_text(size = 10, colour = warna_pudar),
          plot.caption     = element_text(size = 8, colour = warna_pudar, hjust = 0),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          legend.position  = "bottom")
}

# ============================================================
# 3. DATA (WIDE -> LONG)
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

dat_long <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    indikator = factor(indikator, levels = c("Kemiskinan", "IPM")),
    tahun     = as.integer(tahun),
    wilayah   = str_trim(wilayah),
    tipe      = if_else(str_starts(kode, "737"), "Kota", "Kabupaten")) |>
  filter(!is.na(kode), !is.na(nilai)) |>
  distinct()

stopifnot("Nama kolom indikator tidak terbaca, cek names(data_wide)" =
            !anyNA(dat_long$indikator),
          "Data harus 144 baris (24 wilayah x 3 tahun x 2 indikator)" =
            nrow(dat_long) == 144)

# ============================================================
# 4. FUNGSI BOOTSTRAP PERCENTILE
# ============================================================
set.seed(2026)      # agar hasil dapat direproduksi
B <- 1999           # banyaknya resampel (syarat: B >= 1999)
stopifnot("B harus minimal 1999" = B >= 1999)

boot_ci <- function(x, B = 1999, level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  # x[sample.int(n, ...)] aman untuk n = 1, tidak seperti sample(x, ...)
  rata_boot <- replicate(B, mean(x[sample.int(n, replace = TRUE)]))
  a <- (1 - level) / 2
  tibble(n         = n,
         rata      = mean(x),
         bawah     = quantile(rata_boot, a,     names = FALSE),
         atas      = quantile(rata_boot, 1 - a, names = FALSE),
         se_boot   = sd(rata_boot))
}

# ============================================================
# 5. BOOTSTRAP PER WILAYAH
#    Unit resampel: nilai tahun 2023, 2024, 2025 pada wilayah tersebut
# ============================================================
hasil_wilayah <- dat_long |>
  group_by(indikator, wilayah, tipe) |>
  group_modify(~ boot_ci(.x$nilai, B = B)) |>
  ungroup()

print(hasil_wilayah, n = 10)

# ============================================================
# 6. BOOTSTRAP PER KELOMPOK WILAYAH (KABUPATEN, KOTA, SULSEL)
#    Unit resampel: wilayah dalam kelompok pada tahun yang sama
# ============================================================
dat_kelompok <- bind_rows(
  dat_long,
  dat_long |> mutate(tipe = "Sulawesi Selatan")    # gabungan 24 wilayah
)

hasil_kelompok <- dat_kelompok |>
  group_by(indikator, tipe, tahun) |>
  group_modify(~ boot_ci(.x$nilai, B = B)) |>
  ungroup() |>
  mutate(kelompok = paste(tipe, tahun))

print(hasil_kelompok, n = 10)

# ============================================================
# 7. SIMPAN HASIL
# ============================================================
write.csv(hasil_wilayah,  file.path(folder_out, "hasil_bootstrap_wilayah.csv"),
          row.names = FALSE)
write.csv(hasil_kelompok, file.path(folder_out, "hasil_bootstrap_kelompok.csv"),
          row.names = FALSE)
saveRDS(hasil_wilayah,  file.path(folder_out, "hasil_bootstrap_wilayah.rds"))
saveRDS(hasil_kelompok, file.path(folder_out, "hasil_bootstrap_kelompok.rds"))

# ============================================================
# 8. FUNGSI GRAFIK: titik + garis galat, diurutkan menurut rata-rata
#    (nama kolom label diterima lewat {{ }})
# ============================================================
plot_ci <- function(data, label, ind, judul, subtitle = NULL,
                    acuan = NULL, catatan = sumber) {
  
  d <- data |>
    filter(indikator == ind) |>
    mutate(.urut = reorder({{ label }}, rata))
  
  p <- ggplot(d, aes(x = rata, y = .urut))
  
  if (!is.null(acuan)) {
    p <- p + geom_vline(xintercept = acuan, colour = warna_pudar,
                        linetype = "dashed", linewidth = 0.5)
  }
  
  p +
    geom_errorbar(aes(xmin = bawah, xmax = atas), orientation = "y",
                  width = 0.35, colour = pal_indikator[[ind]], linewidth = 0.8) +
    geom_point(colour = warna_sorot, size = 2.6) +
    labs(title = judul, subtitle = subtitle,
         x = paste("Rata-rata", label_indikator[[ind]]),
         y = NULL, caption = catatan) +
    theme_tim()
}

# ============================================================
# 9. GRAFIK PER WILAYAH (KEMISKINAN DAN IPM)
# ============================================================
catatan_wilayah <- paste0(
  "Sumber: BPS, diolah Kelompok 7. Selang bootstrap percentile 95%, B = ", B, ".\n",
  "Catatan: setiap wilayah hanya memiliki n = 3 tahun, sehingga selang bersifat indikatif.")

g_wil_k <- plot_ci(hasil_wilayah, wilayah, "Kemiskinan",
                   judul    = "Kemiskinan: rata-rata 2023-2025 dan SK bootstrap 95%",
                   subtitle = "Diurutkan dari rata-rata terendah; garis putus-putus: rata-rata Sulsel",
                   acuan    = mean(filter(dat_long, indikator == "Kemiskinan")$nilai),
                   catatan  = catatan_wilayah)

g_wil_i <- plot_ci(hasil_wilayah, wilayah, "IPM",
                   judul    = "IPM: rata-rata 2023-2025 dan SK bootstrap 95%",
                   subtitle = "Diurutkan dari rata-rata terendah; garis putus-putus: rata-rata Sulsel",
                   acuan    = mean(filter(dat_long, indikator == "IPM")$nilai),
                   catatan  = catatan_wilayah)

# ============================================================
# 10. GRAFIK PER KELOMPOK WILAYAH
# ============================================================
catatan_kelompok <- paste0(
  "Sumber: BPS, diolah Kelompok 7. Selang bootstrap percentile 95%, B = ", B, ".\n",
  "Catatan: kelompok Kota hanya terdiri dari 3 wilayah, sehingga selangnya lebar.")

g_kel_k <- plot_ci(hasil_kelompok, kelompok, "Kemiskinan",
                   judul    = "Kemiskinan menurut kelompok wilayah dan tahun",
                   subtitle = "Resampel wilayah dalam kelompok; diurutkan menurut rata-rata",
                   catatan  = catatan_kelompok)

g_kel_i <- plot_ci(hasil_kelompok, kelompok, "IPM",
                   judul    = "IPM menurut kelompok wilayah dan tahun",
                   subtitle = "Resampel wilayah dalam kelompok; diurutkan menurut rata-rata",
                   catatan  = catatan_kelompok)

# Gabungan dua indikator berdampingan
g_wil <- (g_wil_k | g_wil_i) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = warna_bg, colour = NA)))
g_kel <- (g_kel_k | g_kel_i) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = warna_bg, colour = NA)))

print(g_wil)
print(g_kel)

# ============================================================
# 11. SIMPAN GRAFIK
# ============================================================
ggsave(file.path(folder_out, "grafik_bootstrap_wilayah.png"),
       g_wil, width = 14, height = 8, dpi = 300, bg = warna_bg)
ggsave(file.path(folder_out, "grafik_bootstrap_kelompok.png"),
       g_kel, width = 14, height = 6, dpi = 300, bg = warna_bg)

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# ANALISIS HASIL LINEUP: JUMLAH BENAR, P-VALUE VISUAL, TAFSIRAN
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data   <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_jawaban  <- paste0(folder_data, "Jawaban_Responden.xlsx")
path_kunci    <- paste0(folder_data, "KUNCI_lineup_RAHASIA.txt")
folder_out    <- paste0(folder_data, "Keluaran_Lineup")

stopifnot("File jawaban tidak ditemukan" = file.exists(path_jawaban),
          "File kunci tidak ditemukan"   = file.exists(path_kunci))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

n_panel <- 20       # banyaknya panel pada lineup
alpha   <- 0.05     # taraf nyata
min_pengamat <- 10  # syarat minimal pengamat

# ============================================================
# 2. TEMA DAN PALET TIM 7
# ============================================================
warna_benar <- "#FFD60A"
warna_biasa <- "#4A5A80"
warna_pval  <- "#FF6B9D"
warna_bg    <- "#0F172A"
warna_panel <- "#162036"
warna_teks  <- "#E8ECF4"
warna_pudar <- "#9AA5BF"

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = "#2B3A5C", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 13, colour = "white"),
          plot.subtitle    = element_text(size = 10, colour = warna_pudar),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar))
}

# ============================================================
# 3. BACA JAWABAN PENGAMAT
# ============================================================
jawaban <- read_excel(path_jawaban)
print(names(jawaban))
print(head(jawaban))

# Cari kolom pilihan panel:
#   (1) berdasarkan nama kolom ("pilihan" atau "panel"); jika tidak ada,
#   (2) kolom yang isinya paling banyak berupa nomor panel ("12" atau "Panel 12").
# Cara ini tidak bergantung pada urutan kolom (kolom ke-3 atau ke-4).
nama_kolom <- str_to_lower(names(jawaban))
idx <- which(str_detect(nama_kolom, "pilihan"))[1]
if (is.na(idx)) idx <- which(str_detect(nama_kolom, "panel"))[1]
if (is.na(idx)) {
  skor <- sapply(jawaban, function(k) {
    s  <- str_to_lower(str_trim(as.character(k)))
    ok <- str_detect(s, "^(panel\\s*)?\\d{1,2}$")
    ok[is.na(ok)] <- FALSE
    mean(ok)
  })
  idx <- which.max(skor)
  if (skor[idx] == 0) stop("Kolom pilihan panel tidak ditemukan. Periksa file Excel.")
}
cat("Kolom pilihan panel yang dipakai:", names(jawaban)[idx], "\n")

# Ambil angka pertama pada jawaban (contoh: "Panel 12" menjadi 12)
pilihan <- as.numeric(str_extract(as.character(jawaban[[idx]]), "\\d+"))

# Jawaban di luar 1-20 dianggap tidak valid
tidak_valid <- sum(!is.na(pilihan) & (pilihan < 1 | pilihan > n_panel))
pilihan[!is.na(pilihan) & (pilihan < 1 | pilihan > n_panel)] <- NA
if (tidak_valid > 0) message(tidak_valid, " jawaban di luar 1-", n_panel, " dibuang.")

# ============================================================
# 4. BACA KUNCI JAWABAN
# ============================================================
kunci       <- readLines(path_kunci)
baris_panel <- grep("Panel data asli", kunci, value = TRUE)
if (length(baris_panel) == 0) stop("Panel data asli tidak ditemukan pada file kunci.")
pos_asli <- as.numeric(sub(".*:\\s*", "", baris_panel[1]))

# ============================================================
# 5. HITUNG JAWABAN BENAR DAN UJI BINOMIAL
#    H0: pengamat menebak acak, peluang benar = 1/20
#    H1: peluang benar > 1/20 (data asli memang tampak berbeda)
#    p-value visual = P(X >= k), X ~ Binomial(K, 1/20)
# ============================================================
K <- sum(!is.na(pilihan))                 # jumlah pengamat valid
if (K == 0) stop("Pilihan panel belum terbaca. Periksa file Excel.")
if (K < min_pengamat) {
  warning("Jumlah pengamat valid (", K, ") kurang dari ", min_pengamat,
          ". Instruksi tugas mensyaratkan minimal ", min_pengamat, " pengamat.")
}

k       <- sum(pilihan == pos_asli, na.rm = TRUE)   # jawaban benar
salah   <- K - k
prop    <- k / K
harapan <- K / n_panel                              # jumlah benar yang diharapkan jika menebak

uji      <- binom.test(k, K, p = 1 / n_panel, alternative = "greater")
p_visual <- uji$p.value
ci       <- binom.test(k, K, p = 1 / n_panel)$conf.int   # SK 95% dua sisi untuk proporsi

# ============================================================
# 6. TAFSIRAN OTOMATIS (TERMASUK BILA TIDAK SIGNIFIKAN)
# ============================================================
if (p_visual < alpha) {
  tafsir <- paste0(
    "Hasil SIGNIFIKAN pada taraf ", alpha, ". Dari ", K, " pengamat, ", k,
    " memilih panel data asli, jauh lebih banyak daripada yang diharapkan jika menebak acak (",
    round(harapan, 2), "). Hipotesis nol ditolak: pola pada data asli dapat dibedakan ",
    "secara visual dari panel permutasi, sehingga ada hubungan antara kemiskinan dan IPM ",
    "yang bukan kebetulan.")
} else {
  tafsir <- paste0(
    "Hasil TIDAK SIGNIFIKAN pada taraf ", alpha, ". Dari ", K, " pengamat, hanya ", k,
    " memilih panel data asli, sedangkan jika menebak acak diharapkan sekitar ",
    round(harapan, 2), ". Hipotesis nol tidak ditolak: tidak cukup bukti bahwa data asli ",
    "dapat dibedakan secara visual dari panel permutasi. Ini BUKAN bukti bahwa hubungan ",
    "tidak ada; kemungkinan penyebabnya antara lain pola hubungan lemah, jumlah pengamat ",
    "terbatas (daya uji rendah), atau data yang hanya 24 wilayah.")
}

# ============================================================
# 7. TAMPILKAN RINGKASAN
# ============================================================
cat("\n===== HASIL VISUAL LINEUP (KELOMPOK 7) =====\n")
cat("Posisi panel asli       :", pos_asli, "\n")
cat("Jumlah pengamat valid   :", K, "\n")
cat("Jawaban benar           :", k, "\n")
cat("Jawaban salah           :", salah, "\n")
cat("Proporsi benar          :", round(prop, 4), "\n")
cat("Benar yang diharapkan   :", round(harapan, 2), "(jika menebak acak)\n")
cat("SK 95% proporsi benar   :", round(ci[1], 4), "sampai", round(ci[2], 4), "\n")
cat("p-value visual          :", format.pval(p_visual, digits = 4), "\n")
cat("\nTafsiran:\n", tafsir, "\n", sep = "")
print(uji)

# ============================================================
# 8. TABEL RINGKASAN
# ============================================================
ringkasan <- tibble(
  posisi_panel_asli = pos_asli,
  jumlah_pengamat   = K,
  jawaban_benar     = k,
  jawaban_salah     = salah,
  proporsi_benar    = prop,
  benar_diharapkan  = harapan,
  sk95_bawah        = ci[1],
  sk95_atas         = ci[2],
  p_visual          = p_visual,
  keputusan         = if_else(p_visual < alpha, "Tolak H0 (signifikan)",
                              "Gagal tolak H0 (tidak signifikan)")
)
print(ringkasan)

# ============================================================
# 9. GRAFIK
# ============================================================
# 9.1 Sebaran pilihan pengamat pada 20 panel
pilih_panel <- tibble(panel = factor(1:n_panel, levels = 1:n_panel)) |>
  left_join(count(tibble(panel = factor(pilihan[!is.na(pilihan)], levels = 1:n_panel)),
                  panel, name = "jumlah"),
            by = "panel") |>
  mutate(jumlah = coalesce(jumlah, 0L),
         asli   = as.numeric(as.character(panel)) == pos_asli)

g_pilihan <- ggplot(pilih_panel, aes(x = panel, y = jumlah, fill = asli)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = jumlah), vjust = -0.4, size = 3, colour = warna_teks) +
  scale_fill_manual(values = c(`FALSE` = warna_biasa, `TRUE` = warna_benar),
                    labels = c(`FALSE` = "Panel permutasi", `TRUE` = "Panel data asli"),
                    name = NULL) +
  scale_y_continuous(breaks = scales::breaks_pretty(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Pilihan pengamat pada 20 panel",
       subtitle = paste0("Jawaban benar: ", k, " dari ", K, " pengamat"),
       x = "Nomor panel", y = "Jumlah pengamat") +
  theme_tim() +
  theme(legend.position = "bottom")

# 9.2 Distribusi binomial di bawah H0 dan daerah p-value
x_maks <- min(K, max(k + 2, 6))
dist_h0 <- tibble(x = 0:x_maks, peluang = dbinom(0:x_maks, K, 1 / n_panel)) |>
  mutate(daerah = x >= k)

g_p <- ggplot(dist_h0, aes(x = x, y = peluang, fill = daerah)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = k, colour = warna_benar, linewidth = 0.9, linetype = "dashed") +
  scale_fill_manual(values = c(`FALSE` = warna_biasa, `TRUE` = warna_pval),
                    labels = c(`FALSE` = "Peluang lain", `TRUE` = "Daerah p-value (X >= k)"),
                    name = NULL) +
  scale_x_continuous(breaks = 0:x_maks) +
  labs(title = "Distribusi jawaban benar jika pengamat menebak acak",
       subtitle = paste0("X ~ Binomial(", K, ", 1/", n_panel, "); garis putus-putus: k = ", k,
                         "; p-value visual = ", format.pval(p_visual, digits = 3)),
       x = "Jumlah jawaban benar", y = "Peluang") +
  theme_tim() +
  theme(legend.position = "bottom")

g_hasil <- (g_pilihan | g_p) +
  plot_annotation(
    title   = "Hasil inferensi visual: lineup 20 panel, kemiskinan dan IPM 2025",
    caption = "Sumber: pengamatan visual Kelompok 7",
    theme   = theme(plot.background = element_rect(fill = warna_bg, colour = NA),
                    plot.title      = element_text(face = "bold", size = 16, colour = "white"),
                    plot.caption    = element_text(colour = warna_pudar)))

print(g_hasil)

# ============================================================
# 10. SIMPAN HASIL
# ============================================================
ggsave(file.path(folder_out, "grafik_lineup_Tim7.png"), g_hasil,
       width = 14, height = 6, dpi = 300, bg = warna_bg)

write.csv(ringkasan, file.path(folder_out, "ringkasan_lineup_Tim7.csv"), row.names = FALSE)

hasil_lineup <- list(ringkasan = ringkasan, uji_binomial = uji, tafsiran = tafsir)
saveRDS(hasil_lineup, file.path(folder_out, "hasil_lineup_Tim7.rds"))

writeLines(c("HASIL VISUAL LINEUP - KELOMPOK 7",
             paste("Panel data asli       :", pos_asli),
             paste("Jumlah pengamat valid :", K),
             paste("Jawaban benar         :", k),
             paste("Proporsi benar        :", round(prop, 4)),
             paste("p-value visual        :", format.pval(p_visual, digits = 4)),
             "",
             "Tafsiran:", tafsir),
           file.path(folder_out, "laporan_lineup_Tim7.txt"))

cat("\nAnalisis selesai. Hasil tersimpan di:", folder_out, "\n")

# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# BONUS GGPROTO: stat_sk_bootstrap()
# Menghitung dan menggambar SK bootstrap 95% langsung di dalam ggplot
# Indikator: IPM dan Kemiskinan, Sulawesi Selatan 2023-2025
# ============================================================
rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data   <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- paste0("D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Output/Keluaran_ggproto")

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. TEMA DAN PALET TIM 7 (TEMA GELAP)
# ============================================================
pal_indikator <- c("Kemiskinan" = "#FF6B9D", "IPM" = "#4CC9F0")
warna_sorot   <- "#FFD60A"
warna_bg      <- "#0F172A"
warna_panel   <- "#162036"
warna_garis   <- "#2B3A5C"
warna_teks    <- "#E8ECF4"
warna_pudar   <- "#9AA5BF"
sumber        <- "Sumber: BPS, diolah Kelompok 7"

label_indikator <- c("Kemiskinan" = "Kemiskinan (%)",
                     "IPM"        = "Indeks Pembangunan Manusia")

theme_tim <- function() {
  theme_minimal(base_family = "sans") +
    theme(text             = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.grid.major = element_line(colour = warna_garis, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 14, colour = "white"),
          plot.subtitle    = element_text(size = 10, colour = warna_pudar),
          plot.caption     = element_text(size = 8, colour = warna_pudar, hjust = 0),
          axis.title       = element_text(size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          legend.position  = "none")
}

# ============================================================
# 3. DATA (WIDE -> LONG)
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))
print(names(data_wide))   # cek nama kolom hasil impor

dat_long <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",   # contoh: IPM.2023
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    indikator = factor(indikator, levels = c("Kemiskinan", "IPM")),
    tahun     = as.integer(tahun),
    wilayah   = str_trim(wilayah),
    tipe      = if_else(str_starts(kode, "737"), "Kota", "Kabupaten")) |>
  filter(!is.na(kode), !is.na(nilai)) |>
  distinct()

stopifnot("Nama kolom indikator tidak terbaca, cek names(data_wide)" =
            !anyNA(dat_long$indikator),
          "Data harus 144 baris (24 wilayah x 3 tahun x 2 indikator)" =
            nrow(dat_long) == 144)

# ============================================================
# 4. STAT BARU DENGAN ggproto: StatSKBootstrap
#    Input  : x (kelompok/wilayah) dan y (nilai pengamatan)
#    Output : satu baris per kelompok berisi
#             y = rata-rata, ymin/ymax = SK percentile bootstrap,
#             n = banyak pengamatan, se_boot = galat baku bootstrap
# ============================================================
StatSKBootstrap <- ggproto("StatSKBootstrap", Stat,
                           
                           required_aes = c("x", "y"),
                           
                           # Variabel hasil hitungan yang boleh dipakai lewat after_stat()
                           default_aes = aes(),
                           
                           compute_group = function(data, scales, B = 1999, conf = 0.95, seed = NULL) {
                             
                             nilai <- data$y[!is.na(data$y)]
                             n     <- length(nilai)
                             if (n == 0) return(data.frame())
                             
                             # Seed hanya dipasang jika diminta (hasil dapat direproduksi)
                             if (!is.null(seed)) set.seed(seed)
                             
                             # Bootstrap: sample.int aman untuk n = 1, tidak seperti sample(nilai, ...)
                             rata_boot <- replicate(B, mean(nilai[sample.int(n, replace = TRUE)]))
                             
                             a     <- 1 - conf
                             batas <- quantile(rata_boot, probs = c(a / 2, 1 - a / 2), names = FALSE)
                             
                             data.frame(x       = data$x[1],
                                        y       = mean(nilai),
                                        ymin    = batas[1],
                                        ymax    = batas[2],
                                        n       = n,
                                        se_boot = sd(rata_boot))
                           }
)

# ============================================================
# 5. FUNGSI PEMBUNGKUS: stat_sk_bootstrap()
#    Dipakai seperti stat ggplot lainnya, contoh:
#    ggplot(d, aes(x = wilayah, y = nilai)) + stat_sk_bootstrap(B = 1999)
# ============================================================
stat_sk_bootstrap <- function(mapping = NULL, data = NULL,
                              geom = "pointrange", position = "identity",
                              B = 1999, conf = 0.95, seed = NULL,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  
  stopifnot("B harus minimal 1999" = B >= 1999,
            "conf harus di antara 0 dan 1" = conf > 0 && conf < 1)
  
  layer(stat = StatSKBootstrap, data = data, mapping = mapping,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(B = B, conf = conf, seed = seed, na.rm = na.rm, ...))
}

# ============================================================
# 6. GRAFIK 1: SK BOOTSTRAP PER WILAYAH (UNIT RESAMPEL: 3 TAHUN)
#    Stat menghitung SK langsung dari data mentah (tidak ada tabel antara)
# ============================================================
grafik_wilayah <- function(ind) {
  d <- dat_long |> filter(indikator == ind)
  
  ggplot(d, aes(x = reorder(wilayah, nilai, FUN = mean), y = nilai)) +
    stat_sk_bootstrap(B = 1999, conf = 0.95, seed = 2026,
                      colour = pal_indikator[[ind]], size = 0.55, linewidth = 0.9) +
    coord_flip() +
    labs(title    = paste("SK bootstrap 95%:", label_indikator[[ind]]),
         subtitle = "Rata-rata 2023-2025 per wilayah, diurutkan dari terendah",
         x = NULL, y = paste("Rata-rata", label_indikator[[ind]]),
         caption = paste0(sumber, ". Statistik khusus ggproto, B = 1999.\n",
                          "Catatan: tiap wilayah hanya n = 3 tahun, sehingga selang bersifat indikatif.")) +
    theme_tim()
}

g_wil_k <- grafik_wilayah("Kemiskinan")
g_wil_i <- grafik_wilayah("IPM")

g_wilayah <- (g_wil_k | g_wil_i) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = warna_bg, colour = NA)))
print(g_wilayah)

# ============================================================
# 7. GRAFIK 2: SK BOOTSTRAP PER KELOMPOK (KABUPATEN vs KOTA, 2025)
#    Stat yang sama dipakai ulang; unit resampel = wilayah dalam kelompok
# ============================================================
grafik_kelompok <- function(ind) {
  d <- dat_long |> filter(indikator == ind, tahun == 2025)
  
  ggplot(d, aes(x = tipe, y = nilai)) +
    geom_jitter(width = 0.08, height = 0, colour = warna_pudar,
                size = 1.6, alpha = 0.6) +
    stat_sk_bootstrap(B = 1999, conf = 0.95, seed = 2026,
                      colour = warna_sorot, size = 0.8, linewidth = 1.2) +
    labs(title    = paste("Kabupaten dan kota:", label_indikator[[ind]]),
         subtitle = "Tahun 2025; titik abu-abu = wilayah, kuning = rata-rata dan SK bootstrap 95%",
         x = NULL, y = label_indikator[[ind]],
         caption = paste0(sumber, ". Kelompok Kota hanya 3 wilayah, sehingga selangnya lebar.")) +
    theme_tim()
}

g_kel_k <- grafik_kelompok("Kemiskinan")
g_kel_i <- grafik_kelompok("IPM")

g_kelompok <- (g_kel_k | g_kel_i) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = warna_bg, colour = NA)))
print(g_kelompok)

# ============================================================
# 8. PEMERIKSAAN: HASIL STAT HARUS COCOK DENGAN HITUNGAN MANUAL
# ============================================================
ld <- layer_data(g_wil_k, i = 1)                    # data yang dihasilkan stat
rata_manual <- dat_long |>
  filter(indikator == "Kemiskinan") |>
  group_by(wilayah) |> summarise(r = mean(nilai), .groups = "drop") |> pull(r)

stopifnot("Stat harus menghasilkan 24 baris (satu per wilayah)" = nrow(ld) == 24,
          "Rata-rata dari stat berbeda dari hitungan manual" =
            isTRUE(all.equal(sort(ld$y), sort(rata_manual))),
          "ymin harus <= y <= ymax" = all(ld$ymin <= ld$y & ld$y <= ld$ymax))
cat("Pemeriksaan stat_sk_bootstrap(): LULUS\n")

# ============================================================
# 9. SIMPAN
# ============================================================
ggsave(file.path(folder_out, "ggproto_sk_wilayah_Tim7.png"),
       g_wilayah, width = 14, height = 8, dpi = 300, bg = warna_bg)
ggsave(file.path(folder_out, "ggproto_sk_kelompok_Tim7.png"),
       g_kelompok, width = 12, height = 6, dpi = 300, bg = warna_bg)

# Tabel hasil (diambil dari layer_data, untuk lampiran laporan)
tabel_sk <- bind_rows(
  mutate(layer_data(g_wil_k, i = 1), indikator = "Kemiskinan"),
  mutate(layer_data(g_wil_i, i = 1), indikator = "IPM")
) |> select(indikator, x, y, ymin, ymax, n, se_boot)
write.csv(tabel_sk, file.path(folder_out, "tabel_ggproto_sk_Tim7.csv"), row.names = FALSE)

cat("Bonus ggproto Kelompok 7 selesai dijalankan.\n")

rm(list = ls())
# ============================================================
# KOMPUTASI STATISTIKA LANJUT
# ANALISIS KEMISKINAN DAN IPM SULAWESI SELATAN
# INFERENSI VISUAL: LINEUP 20 PANEL
# (1 panel data asli + 19 panel data permutasi)
# Pertanyaan: adakah hubungan antara kemiskinan dan IPM? (2025)
# ============================================================
#
# PROTOKOL PENGAMAT (agar uji sah secara statistik):
#   1. Minimal 10 pengamat, masing-masing mengisi SATU kali.
#   2. Pengamat bekerja sendiri-sendiri (tidak berdiskusi).
#   3. Pengamat tidak diberi petunjuk apa pun tentang pola yang dicari.
#   4. Hanya gambar lineup yang dibagikan, JANGAN file kunci.
# ============================================================

library(tidyverse)   # dplyr, tidyr, ggplot2, stringr
library(readxl)
library(writexl)
library(nullabor)

# ============================================================
# 1. LOKASI FILE
# ============================================================
folder_data <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Data/"
path_data   <- paste0(folder_data, "Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx")
folder_out  <- "D:/A. Muh. Faldi Fadhil A. Darsyad/1. Magister Statistika/Komputasi Statistika Lanjut/Pekan 8/tim_tujuh/Output/Lineup_Tim7"

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. TEMA DAN PALET TIM 7 (TEMA GELAP)
# ============================================================
warna_kab    <- "#4CC9F0"   # biru langit : Kabupaten (lingkaran)
warna_kota   <- "#FF6B9D"   # merah muda  : Kota (segitiga)
warna_garis  <- "#FFD60A"   # kuning      : garis regresi dan pita SK
warna_bg     <- "#0F172A"   # latar gambar
warna_panel  <- "#162036"   # latar panel
warna_grid   <- "#2B3A5C"   # grid dan tepi panel
warna_teks   <- "#E8ECF4"   # teks utama
warna_pudar  <- "#9AA5BF"   # teks sekunder

pal_tipe <- c("Kabupaten" = warna_kab, "Kota" = warna_kota)
bentuk   <- c("Kabupaten" = 16,        "Kota" = 17)

# ============================================================
# 3. DATA (WIDE -> LONG -> WIDE PER TAHUN)
# ============================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))

dat <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab_Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to      = c("indikator", "tahun"),
               names_pattern = "^(.*?)[._ ]+(\\d{4})$",
               values_to     = "nilai") |>
  mutate(indikator = case_when(
    str_detect(str_to_lower(indikator), "ipm")    ~ "IPM",
    str_detect(str_to_lower(indikator), "miskin") ~ "Kemiskinan",
    TRUE ~ NA_character_),
    tahun   = as.integer(tahun),
    wilayah = str_trim(wilayah)) |>
  filter(!is.na(kode)) |>
  distinct() |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  rename(kemiskinan = Kemiskinan, ipm = IPM) |>
  mutate(tipe = if_else(str_starts(kode, "737"), "Kota", "Kabupaten"))

stopifnot("Data harus 72 baris (24 wilayah x 3 tahun)" = nrow(dat) == 72,
          !anyNA(dat$kemiskinan), !anyNA(dat$ipm))

dat25 <- dat |> filter(tahun == 2025)
stopifnot("Data 2025 harus 24 wilayah" = nrow(dat25) == 24)

# ============================================================
# 4. FUNGSI LINEUP (nama kolom diterima lewat {{ }})
# ============================================================

# 4.1 Membentuk data lineup: 1 panel asli + (n_panel - 1) panel permutasi
#     H0: nilai y diacak, sehingga tidak berhubungan dengan x
buat_lineup <- function(data, y, n_panel = 20) {
  nama_y <- rlang::as_label(rlang::enquo(y))
  pos    <- sample(n_panel, 1)                     # posisi panel asli (RAHASIA)
  list(pos  = pos,
       data = lineup(null_permute(nama_y), data, n = n_panel, pos = pos))
}

# 4.2 Menggambar lineup (tema gelap Tim 7)
#     Sumbu boleh ditampilkan: permutasi tidak mengubah sebaran masing-masing
#     peubah, sehingga rentang sumbu sama di semua panel dan bukan petunjuk.
gambar_lineup <- function(data_lineup, x, y, ncol = 5,
                          label_x = "Persentase Penduduk Miskin (%)",
                          label_y = "IPM") {
  ggplot(data_lineup, aes(x = {{ x }}, y = {{ y }})) +
    geom_smooth(method = "lm", formula = y ~ x, level = 0.95,
                colour = warna_garis, fill = warna_garis,
                alpha = 0.18, linewidth = 1.4, lineend = "round") +
    geom_point(aes(colour = tipe, shape = tipe), size = 2.2, alpha = 0.9) +
    facet_wrap(vars(.sample), ncol = ncol) +
    scale_colour_manual(values = pal_tipe, name = "Jenis wilayah") +
    scale_shape_manual(values = bentuk,    name = "Jenis wilayah") +
    labs(title = "Panel manakah yang pola titiknya paling berbeda dari yang lain?",
         subtitle = "Dari 20 panel, satu memuat data asli. Pilih panel yang menurut Anda paling berbeda.",
         x = label_x, y = label_y) +
    theme_minimal(base_family = "sans") +
    theme(text              = element_text(colour = warna_teks),
          plot.background  = element_rect(fill = warna_bg, colour = NA),
          panel.background = element_rect(fill = warna_panel, colour = NA),
          panel.border     = element_rect(colour = warna_grid, fill = NA, linewidth = 0.6),
          panel.grid.major = element_line(colour = warna_grid, linewidth = 0.25),
          panel.grid.minor = element_blank(),
          panel.spacing    = unit(0.9, "lines"),
          strip.background = element_blank(),
          strip.text       = element_text(face = "bold", size = 12, hjust = 0,
                                          colour = warna_garis),
          axis.title       = element_text(face = "bold", size = 10, colour = warna_teks),
          axis.text        = element_text(size = 9, colour = warna_pudar),
          plot.title       = element_text(face = "bold", size = 16, colour = "white"),
          plot.subtitle    = element_text(size = 11, colour = warna_pudar),
          legend.position  = "top",
          legend.justification = "left",
          legend.title     = element_text(face = "bold", size = 10, colour = warna_teks),
          legend.text      = element_text(size = 10, colour = warna_teks),
          plot.margin      = margin(12, 14, 10, 14)) +
    guides(colour = guide_legend(override.aes = list(size = 3)))
}

# ============================================================
# 5. BENTUK DAN SIMPAN LINEUP
# ============================================================
set.seed(7007)       # agar dapat direproduksi

hasil <- buat_lineup(dat25, y = ipm, n_panel = 20)
pos_asli   <- hasil$pos
dat_lineup <- hasil$data

g_lineup <- gambar_lineup(dat_lineup, x = kemiskinan, y = ipm)
g_lineup

# Gambar lineup (INI yang dibagikan ke pengamat), ukuran landscape
ggsave(file.path(folder_out, "lineup_20panel_Tim7.png"), g_lineup,
       width = 14, height = 10.5, dpi = 200, bg = warna_bg)

# ============================================================
# 6. KUNCI JAWABAN (JANGAN DIBAGIKAN KE PENGAMAT)
# ============================================================
writeLines(c(paste("Panel data asli  :", pos_asli),
             "Seed             : 7007",
             "Data             : tahun 2025, 24 kabupaten/kota",
             "Peubah           : kemiskinan (x) dan IPM (y)",
             "H0               : IPM diacak (tidak ada hubungan dengan kemiskinan)"),
           file.path(folder_out, "KUNCI_lineup_RAHASIA.txt"))

saveRDS(dat_lineup, file.path(folder_out, "data_lineup.rds"))

# ============================================================
# 7. TEMPLATE FORMULIR JAWABAN PENGAMAT
#    (kolom ke-3 = Pilihan Panel; isi minimal 10 pengamat)
# ============================================================
template <- tibble(Timestamp      = NA_character_,
                   Nama_Pengamat  = NA_character_,
                   Tim_Pengamat  = NA_integer_,
                   Pilihan_Panel  = NA_integer_,
                   Alasan_Memilih = NA_character_)
write_xlsx(template, file.path(folder_out, "Jawaban_Responden_TEMPLATE.xlsx"))

cat("Lineup dan kunci tersimpan di:", folder_out, "\n")
cat("Bagikan HANYA lineup_20panel_Tim7.png ke pengamat.\n")
