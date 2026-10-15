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
