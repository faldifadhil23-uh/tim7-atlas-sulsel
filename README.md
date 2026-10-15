# Atlas Visual Sulawesi Selatan — Tim 7

**IPM dan Persentase Penduduk Miskin di 24 Kabupaten/Kota Sulawesi Selatan, 2023–2025**

Proyek 14 hari mata kuliah **Komputasi Statistika Lanjut** (25H06210204), Program Studi Magister Statistika, Universitas Hasanuddin, Semester Ganjil 2026/2027.

**Tim 7:**
- A. Muh. Faldi Fadhil A. Darsyad — H062261015
- Riski Aprilianti — H062261014

## Pertanyaan Proyek

Bagaimana menyajikan indikator pembangunan manusia dan kemiskinan pada 24 kabupaten/kota di Sulawesi Selatan secara **informatif**, **jujur terhadap ketidakpastian**, **teruji secara visual**, dan **mudah ditelusuri sumber datanya**?

## Temuan Utama

- Rata-rata IPM pada 24 kabupaten/kota meningkat dari **73,84 (2023)** menjadi **74,49 (2024)** dan **75,23 (2025)**. Kenaikan 2023–2025 sebesar **1,39 poin**.
- Rata-rata persentase penduduk miskin turun dari **9,33% (2023)** menjadi **8,60% (2024)** dan **8,14% (2025)**, atau turun **1,19 poin persentase** selama periode tersebut.
- Pada 2025, IPM tertinggi terdapat di **Kota Makassar (85,66)** dan terendah di **Jeneponto (70,25)**.
- Pada 2025, persentase penduduk miskin terendah terdapat di **Kota Makassar (4,43%)**, sedangkan tertinggi di **Pangkajene dan Kepulauan (11,60%)**.
- Korelasi Pearson IPM dan kemiskinan pada 2025 sebesar **−0,559**, menunjukkan asosiasi negatif tingkat sedang. Ini adalah hubungan antarwilayah dan **bukan bukti sebab-akibat**.
- Dalam uji visual lineup 20 panel, **10 dari 12 pengamat valid** memilih panel data asli (panel 4). Laporan proyek mencatat *p-value* visual **5,873 × 10⁻¹²**. Hasil ini menunjukkan pola pada panel asli dapat dibedakan dari panel permutasi dalam rancangan uji tersebut; interpretasinya tetap terbatas pada prosedur lineup yang digunakan.

Rincian grafik, peta, profil wilayah, dan pembahasan tersedia di atlas HTML: **`atlas_tim7_final.html`**.

## Data

| Komponen | Keterangan |
|---|---|
| Indikator utama | Indeks Pembangunan Manusia (IPM) |
| Indikator pendamping | Persentase penduduk miskin (%) |
| Cakupan wilayah | 24 kabupaten/kota di Provinsi Sulawesi Selatan |
| Periode | 2023–2025 |
| Data bersih | `Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx` |
| Sumber indikator | Badan Pusat Statistik (BPS) Provinsi Sulawesi Selatan |
| Batas wilayah | geoBoundaries, administrasi tingkat ADM2 (`IDN-ADM2`) |

**Catatan interpretasi:** rerata yang dilaporkan merupakan rata-rata aritmetika dari 24 kabupaten/kota. Rerata tersebut tidak otomatis sama dengan angka resmi provinsi yang berbobot jumlah penduduk. Korelasi juga tidak dapat ditafsirkan sebagai pengaruh kausal.

## Struktur Berkas Proyek

Berkas sumber visual dan data pada paket Tim 7 dikelompokkan sebagai berikut:

```text
Atlas Sulsel/
├── 1. Data Bersih/
│   └── Marge_Data_Clean_IPM_Kemiskinan_Sulsel.xlsx
├── 2. Makeover/
│   ├── Before.jpeg
│   └── makeover_pertumbuhan_ipm_Tim7_v3.png
├── 3. Galeri Statistik/
│   ├── g1_sebaran_per_tahun.png
│   ├── g2_sebaran_kab_kota.png
│   ├── g3_tren_kemiskinan.png
│   ├── g4_tren_ipm.png
│   ├── g5_scatter_ipm_kemiskinan.png
│   ├── g6_heatmap.png
│   ├── g7_lollipop_perubahan.png
│   └── g8_komposisi_patchwork.png
├── 4. Fungsi Grafik/
│   ├── peringkat_ipm_2025.png
│   ├── peringkat_kemiskinan_2025.png
│   ├── scatter_ipm_kemiskinan.png
│   ├── sebaran_ipm.png
│   ├── sebaran_kemiskinan.png
│   ├── tren_ipm.png
│   └── tren_kemiskinan.png
├── 5. Peta dan Koroplet/
│   ├── peta_ipm_2025_Tim7.png
│   ├── peta_kemiskinan_2025_Tim7.png
│   └── berkas uji palet dan buta warna
├── 6. Profil 24 Kabupaten dan Kota/
│   └── profil_*.png
├── 7. Ketidakpastian Boostrap/
│   ├── grafik_bootstrap_kelompok.png
│   └── grafik_bootstrap_wilayah.png
└── 8. Inferensi Visual Lineup/
    ├── grafik_lineup_Tim7.png
    ├── lineup_20panel_Tim7.png
    ├── laporan_lineup_Tim7.txt
    ├── ringkasan_lineup_Tim7.csv
    └── berkas pendukung lineup
```

Berkas untuk atlas Quarto tersedia terpisah dalam paket Quarto Tim 7:

```text
Atlas_Sulsel_Tim7_Quarto/
├── atlas_tim7.qmd
├── atlas-tim7.css
├── atlas_tim7_final.html
├── atlas_tim7.html
├── atlas_tim7_final.pdf
└── README.md
```

## Cara Melihat Atlas

1. Unduh repositori atau paket proyek Tim 7.
2. Buka **`atlas_tim7_final.html`** menggunakan peramban web modern.
3. Gunakan daftar isi untuk menavigasi intisari, makeover, galeri statistik, peta, profil kabupaten/kota, bootstrap, dan lineup.

HTML final disiapkan dengan sumber daya yang tertanam agar mudah dibagikan dan dibuka tanpa perlu mengakses internet untuk memuat gambar.

## Cara Render Ulang dengan Quarto

### Prasyarat

- [R](https://cran.r-project.org/)
- [Quarto](https://quarto.org/docs/get-started/)
- RStudio (opsional, untuk lingkungan kerja yang lebih mudah)

Jika seluruh berkas sumber dan aset yang dirujuk tersedia pada struktur folder yang sesuai, jalankan dari direktori proyek:

```bash
quarto render atlas_tim7.qmd --to html
```

**Catatan reprodusibilitas:** paket yang dibagikan memuat dokumen QMD, CSS, HTML final, PDF, workbook data bersih, dan keluaran grafik. Paket sumber visual ZIP tidak memuat seluruh skrip R analisis yang menghasilkan semua grafik. Karena itu, HTML final dapat langsung dilihat, tetapi untuk membangun ulang seluruh analisis dari data mentah, skrip R, data mentah, dan aset yang diperlukan harus disertakan serta disesuaikan dengan struktur repositori. Jangan menganggap semua grafik akan dihitung ulang hanya dengan merender QMD.

## Komponen Proyek

| Komponen | Isi / tujuan |
|---|---|
| Makeover | Perbandingan grafik sebelum dan sesudah perbaikan desain |
| Galeri statistik | Distribusi, tren IPM, tren kemiskinan, scatterplot, heatmap, perubahan, dan ringkasan visual |
| Fungsi grafik | Grafik peringkat, sebaran, tren, dan hubungan IPM–kemiskinan |
| Peta koroplet | Distribusi spasial IPM dan kemiskinan pada 2025 |
| Profil wilayah | Grafik khusus untuk masing-masing dari 24 kabupaten/kota |
| Ketidakpastian | Grafik bootstrap untuk menunjukkan rentang ketidakpastian estimasi |
| Inferensi visual | Lineup 20 panel, kunci panel, dan ringkasan respons pengamat |
| Atlas Quarto | Narasi analisis yang menggabungkan grafik, peta, dan interpretasi |
| Infografis kebijakan | Ringkasan temuan utama dan rekomendasi untuk pembaca kebijakan |

## Reprodusibilitas dan Batasan

- Angka ringkasan utama diturunkan dari workbook data bersih yang disertakan.
- Grafik dan peta yang ditampilkan dalam atlas berasal dari keluaran visual proyek Tim 7.
- Peta koroplet digunakan untuk eksplorasi statistik, bukan sebagai penetapan batas administrasi resmi.
- Hasil bootstrap bergantung pada prosedur pembentukan sampel ulang dan unit analisis yang digunakan. Intervalnya perlu dibaca sebagai rentang estimasi, bukan kepastian mutlak.
- Uji lineup menilai apakah panel asli dapat dikenali di antara panel pembanding yang dibuat dengan prosedur tertentu; hasilnya tidak membuktikan hubungan kausal.
- Rerata kabupaten/kota tidak boleh langsung diperlakukan sebagai angka provinsi berbobot penduduk.

## Deklarasi Penggunaan AI

AI digunakan sebagai alat bantu untuk menyusun dan merevisi draf narasi, struktur dokumentasi, dan penyajian kode/visual. Tim bertanggung jawab memeriksa angka, menjalankan atau meninjau hasil, serta menentukan keputusan analisis dan interpretasi akhir. Setiap pernyataan kuantitatif pada README ini perlu dicocokkan kembali dengan workbook dan keluaran analisis sebelum repositori dipublikasikan.

## Sumber Data

- **Badan Pusat Statistik (BPS) Provinsi Sulawesi Selatan** — indikator IPM dan persentase penduduk miskin kabupaten/kota, 2023–2025. Cantumkan judul tabel/publikasi dan URL BPS yang tepat pada repositori final.
- **geoBoundaries** — batas wilayah administrasi tingkat ADM2 (`IDN-ADM2`). Sertakan berkas atribusi atau URL sumber batas wilayah pada repositori final.
