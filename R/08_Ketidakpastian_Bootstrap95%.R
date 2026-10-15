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