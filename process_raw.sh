#!/bin/bash

# --- Valores por defecto ---
input="archivo.csv"            # Archivo de entrada (default)
fila_columnas=11                # Número de fila donde están las columnas (default)
fila_datos=18                   # Número de fila donde empiezan los datos (default)
salida="archivo_final.csv"      # Archivo de salida final (default)

# --- Leer parámetros si se pasan ---
if [ ! -z "$1" ]; then
  input="$1"
fi

if [ ! -z "$2" ]; then
  fila_columnas="$2"
fi

if [ ! -z "$3" ]; then
  fila_datos="$3"
fi

if [ ! -z "$4" ]; then
  salida="$4"
fi

# --- Archivos temporales ---
temp_columnas="renglon_columnas.txt"
temp_datos="solo_datos.csv"

echo "📄 Archivo de entrada: $input"
echo "📋 Fila de columnas: $fila_columnas"
echo "📈 Fila de datos: $fila_datos"
echo "💾 Archivo de salida: $salida"
echo "🚀 Procesando..."

# Validar existencia del archivo de entrada
if [ ! -f "$input" ]; then
  echo "❌ Error: El archivo '$input' no existe."
  exit 1
fi

# --- Proceso ---

# 1. Extraer el renglón de columnas
sed -n "${fila_columnas}p" "$input" > "$temp_columnas"

# 2. Extraer los datos a partir del renglón especificado y solo líneas que empiezan con fecha
tail -n +"$fila_datos" "$input" | grep -E '^[0-9]{2}/[0-9]{2}/[0-9]{4}' > "$temp_datos"

# 3. Unir encabezado + datos
cat "$temp_columnas" "$temp_datos" > "$salida"

# 4. Eliminar archivos temporales
rm -f "$temp_columnas" "$temp_datos"

echo "✅ Proceso terminado. Archivo final generado: $salida"

## para ejecutar: chmod +x procesar_csv_param.sh
## ./procesar_csv_param.sh input.csv 11 18 output.csv  
## 11 y 18 son las columnas de las columnas y los datos respectivamente (Estas pueden varias)
## input.csv y output.csv es el nombre que tendrán los archivos
# Ejemplo
# % ./process_raw.sh ./raw/cuentas_validadas.csv 11 20 ./bash_process/cuentas_validadas.csv