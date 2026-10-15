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
