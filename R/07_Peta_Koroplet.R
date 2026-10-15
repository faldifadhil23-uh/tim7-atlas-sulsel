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