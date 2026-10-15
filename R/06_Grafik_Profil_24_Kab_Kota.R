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
