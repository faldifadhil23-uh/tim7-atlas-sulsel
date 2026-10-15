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
