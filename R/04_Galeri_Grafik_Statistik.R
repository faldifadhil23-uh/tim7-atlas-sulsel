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
