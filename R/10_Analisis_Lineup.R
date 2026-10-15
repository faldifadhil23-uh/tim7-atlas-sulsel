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