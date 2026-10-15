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