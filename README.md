# Diagramas-Skew-T-GFS-automatico

Author: Decker Guzmán Zabalaga

# OBJETIVO:
- Automatizar la rutina en R para realizar Diagramas Skew T utilizando las salidas GFS y mandar a diferentes aeropuertos del Bolivia. 

## 1. ¿Como funciona el Programa?

- Las librerías solicitadas por el programa en R son : rNOMADS, THUNDER, rvest. 

- La rutina esta programada en CRONTAB (Linux) para que pueda ejecutarse todos los dias a las 7 am. El archivo con la configuración en CRONTAB esta adjunta.

- El formato de las salidas es (Mes-Dia-Año) png y salidas de datos en txt, posterioremente existe una compresion de figuras y datos en formato zip.
una rutina en terminal
- El envio por correo electronico se configura usando lineas de comando POSTFIX para luego ejecutar 


## 2. MATERIAL Y MANUAL

Se adjunta en este mismo repositorio un manual detallado llamado "Diagrama Skew-T.pdf" y el archivo "CRONTAB2"-

## 3. CONSULTAS

Cualquier consulta puede comunicarse al correo electronico deckerfis@gmail.com
