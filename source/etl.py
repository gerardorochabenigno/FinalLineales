# etl.py

import pandas as pd
import requests
import numpy as np
import logging
import os

def series_sie_completa(token: str, serie: str) -> pd.DataFrame:
    """
    Obtiene series de tiempo del SIE de Banco de México y guarda errores en un log configurado en `config.yaml`.

    Args:
        token (str): Token de acceso a la API del Banco de México.
        serie (str): Identificador de la serie ('tipo_de_cambio', 'tasa_de_interes', 'inflacion').

    Returns:
        pd.DataFrame: DataFrame con las series de tiempo procesadas.
    """

    try:
        # Para el GET
        url = f'https://www.banxico.org.mx/SieAPIRest/service/v1/series/{serie}/datos'
        headers = {"Bmx-Token": token}

        #GET
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        
        # Datos
        data = response.json()
        series = data['bmx']['series'][0]['datos']
        df = pd.DataFrame(series)

        # Tratamiento de fecha
        df['fecha'] = pd.to_datetime(df['fecha'], format="%d/%m/%Y")

        # Tratamiento de valor
        valores_invalidos = ['NA', 'NE', 'No aplica', 'Error', 'N/A', 'nan', 'N/E']
        df['dato'] = df['dato'].replace(valores_invalidos, np.nan)
        df['dato'] = df['dato'].str.replace(',', '').astype(float)
        df['dato'] = df['dato'].replace(0, np.nan)
        df = df.rename(columns={'dato': serie})

        # No ocupamos valores nulos
        df = df.dropna()

        return df

    except requests.exceptions.RequestException as e:
        error_msg = f"Error al obtener datos de {serie}: {e}"
        logging.error(error_msg)

    except ValueError as e:
        error_msg = f"Error en los datos de {serie}: {e}"
        logging.error(error_msg)

    except Exception as e:
        error_msg = f"Error inesperado en {serie}: {e}"
        logging.error(error_msg)