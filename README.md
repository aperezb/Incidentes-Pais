# Data Driven Security - 2016-2017
## Práctica ataques por país y festivos
![](logoUPC.png) 

  
##Autores
  * Carles Cano Casablanca  
  * Oriol Campderròs Arís  
  * Cristian Torres Barrantes  
  * Àngel Pérez Beumala  
  
##Objetivo
Plantear una pregunta asociada con la materia, obtener información, tratarla, presentarla y, a partir de su análisis, plantear respuesta a la pregunta inicial.
  
##Archivos necesarios
La práctica tiene los siguientes archivos principales:

Archivo  | Propósito
------------- | -------------
Informe Final.pdf  | Informe generado en LateX (previo a RMarkdown)
main.Rmd  | Código reproducible en RMarkdown
main.R  | Código reproducible en R
  
Adicionalmente se precisan los siguientes archivos:  

En la carpeta R:  

* modifyFrame.R  
* parseAttacks.R  
* parseHolidays.R   
* parseURLs.R 
* PIBs.R  
  
En la carpeta Data:  
  
* API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv  
* Metadata_Country_API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv  
* URLs_To_Parse.txt  

##Comentario sobre informe LateX
El planteamiento inicial de la práctica fue realizar la carga de información, tratamiento y presentación gráfica en código R y, posteriormente, maquetarlo todo en un informe LateX.  
  
Durante el desarrollo de la práctica el equipo acordó desarrollar también el código reproducible en RMarkdown, sirviendo el LateX como referente de qué queríamos presentar.  
  
##Comentario sobre el tiempo de proceso
El RMarkdown tarda del orden de dos minutos de ejecución, ello es debido a que recopila información de diversos .CSV, un .txt y varias URLs.  
