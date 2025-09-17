import pandas as pd
import numpy as np

# --- KONFIGURASI ---
collar_file = 'collar.csv'
original_assay_file = 'assay.csv'
output_assay_file = 'assay_final_correlation.csv' # Nama file output baru


# --- SCRIPT START ---
print("Memulai proses pembuatan data assay dengan korelasi SANGAT KUAT dan TERKONTROL...")

# 1. Muat dan bersihkan data
try:
    collar_df = pd.read_csv(collar_file)
    assay_df_original = pd.read_csv(original_assay_file)
    print("File collar dan assay berhasil dimuat.")
except FileNotFoundError as e:
    print(f"ERROR: File tidak ditemukan. Pastikan '{e.filename}' ada di folder yang sama.")
    exit()

# Proses pembersihan data yang sudah robust
id_cols_original = [col for col in assay_df_original.columns if col.lower() in ['hole_id', 'from', 'to']]
element_cols_original = [col for col in assay_df_original.columns if col.lower() not in ['hole_id', 'from', 'to']]
id_df = assay_df_original[id_cols_original]
element_df = assay_df_original[element_cols_original]
element_clean_df = element_df.T.groupby(lambda col: col.lower()).mean().T
id_df.columns = id_df.columns.str.lower()
assay_clean_df = pd.concat([id_df, element_clean_df], axis=1)
print("Nama kolom berhasil diseragamkan dengan aman.")

element_cols = [col for col in assay_clean_df.columns if col not in ['hole_id', 'from', 'to']]
stats = assay_clean_df[element_cols].describe().transpose()
mean_vector = stats['mean']
std_vector = stats['std']

# 2. --- DEFINISI KEKUATAN KORELASI (DI-TWEAK) ---
#    Nilai loading yang lebih tinggi (~0.95) akan menghasilkan R-squared > 0.8
#    Nilai loading sedang (~0.6-0.8) akan menghasilkan R-squared di tengah rentang.
correlation_loadings = {
    # Pasangan dengan R-squared > 0.8
    'ni':    0.96,  # Sangat tinggi
    'co':    0.95,  # Sangat tinggi -> R-squared(ni,co) akan > 0.8
    'mgo':  -0.96,  # Sangat tinggi (negatif)
    'sio2': -0.95,  # Sangat tinggi (negatif) -> R-squared(mgo,sio2) akan > 0.8
    
    # Unsur lain dengan korelasi sedang hingga kuat
    'fe':   -0.80,  # Akan berkorelasi kuat dengan semua unsur di atas
    'al2o3':-0.75,  # Akan berkorelasi kuat dengan semua unsur di atas
    'mno':   0.65,
    'cr2o3': 0.50
}
# Fallback untuk unsur lain yang mungkin ada di file Anda
for el in element_cols:
    if el not in correlation_loadings:
        correlation_loadings[el] = 0.1

print("\nKekuatan korelasi yang telah ditingkatkan:")
print(correlation_loadings)

# 3. Buat mapping Hole_ID -> Depth
hole_to_depth_map = collar_df.set_index('Hole_ID')['Depth']

# 4. Proses pembuatan data
all_records = []
print(f"\nMembuat data berkolerasi untuk {len(hole_to_depth_map)} lubang bor...")

for hole_id, total_depth in hole_to_depth_map.items():
    if pd.isna(total_depth) or total_depth <= 0: continue
    
    num_samples = int(total_depth)
    latent_factor = np.random.randn(num_samples)
    generated_df = pd.DataFrame(index=range(num_samples))

    for element in element_cols:
        loading = correlation_loadings.get(element, 0.1)
        noise = np.random.randn(num_samples)
        noise_weight = np.sqrt(max(0, 1 - loading**2))
        standardized_value = (loading * latent_factor) + (noise_weight * noise)
        generated_df[element] = standardized_value
        
    for element in element_cols:
        mean = mean_vector[element]
        std = std_vector[element]
        generated_df[element] = (generated_df[element] * std) + mean
        generated_df[element] = generated_df[element].clip(lower=0)

    generated_df['hole_id'] = hole_id
    generated_df['from'] = range(num_samples)
    generated_df['to'] = range(1, num_samples + 1)
    all_records.append(generated_df)

# 5. Gabungkan dan simpan
final_df = pd.concat(all_records, ignore_index=True)
final_df = final_df[['hole_id', 'from', 'to'] + element_cols]
final_df.to_csv(output_assay_file, index=False)

print("\n--- SELESAI! ---")
print(f"File baru '{output_assay_file}' telah berhasil dibuat.")
print(f"Total baris data yang dibuat: {len(final_df)}")
print("Silakan cek file ini di aplikasi GeoDataViz Anda.")
print("------------------")