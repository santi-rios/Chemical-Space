

# Revisión del Análisis y Presupuesto de Aplicación Shiny

## Análisis Detallado de la Aplicación

La aplicación es una plataforma de visualización interactiva para analizar datos del espacio químico global, con énfasis en las tendencias de China y Estados Unidos. A continuación, detallo los aspectos técnicos más relevantes:

### Tecnologías Implementadas

i. **Sistema de Base de Datos Optimizado**
- Implementación de [Apache Arrow](https://arrow.apache.org/) para manejo eficiente de datos grandes.
  - La base de datos original era bastante grande, alcanzando un tamaño de **171.865 MB** debido a la inclusión de información adicional como banderas de países y límites geográficos. Esto hacía que las consultas fueran lentas y consumieran muchos recursos.
  - Al convertir los datos al formato Parquet, el tamaño se redujo drásticamente a **1.2 MB**, lo que marcó un antes y un después en términos de eficiencia. Esto fue posible gracias a técnicas avanzadas de compresión como codificación por diccionario y RLE/BP, que almacenan valores repetidos y pequeños de manera mucho más eficiente.
  - Además, Parquet es un formato de almacenamiento columnar optimizado para grandes volúmenes de datos y consultas analíticas. Esto significa que, a diferencia de los archivos CSV/tsv/excel que leen filas completas, Parquet permite leer solo las columnas necesarias, acelerando significativamente las consultas.
    - Por ejemplo, para los datos de colaboraciones, antes se tardaba mucho porque el flujo era: `Leer todos los datos y almacenar todo en la memoria -> seleccionar y filtrar el país -> graficar`. Con parquet, el flujo es: `seleccionar y filtrar el país y sus colaboradores -> leer y almacenar SOLO el país y sus colaboradores en la memoria -> graficar`. 
  - Más información sobre el uso de Apache Arrow y Parquet en [este link](https://posit.co/blog/shiny-and-arrow/)

i. **Bibliotecas Especializadas**
   - [bslib](https://rstudio.github.io/bslib/) - [Bootstrap 5](https://getbootstrap.com/docs/5.0/getting-started/introduction/) para interfaz moderna y responsiva
   - [Plotly](https://plotly.com/r/) para gráficos interactivos avanzados
   - [gt](https://gt.rstudio.com/) para tablas avanzadas con formato profesional
   - Integración de banderas de países mediante funciones [fmt_flag](https://gt.rstudio.com/reference/fmt_flag.html)
   - [shinycssloaders](https://github.com/daattali/shinycssloaders) para indicadores visuales de carga


i. **Componentes Interactivos Avanzados**
   - [Sistema de filtrado](https://shiny.posit.co/r/getstarted/build-an-app/reactivity-essentials/reactive-elements.html) por países y regiones con actualización dinámica
   - [Acordeones](https://rstudio.github.io/bslib/reference/accordion.html) y pestañas anidadas para organización de contenido
   - [Tooltips](https://rstudio.github.io/bslib/articles/tooltips-popovers/index.html) personalizados con información contextual

3. **Visualizaciones Especializadas**
   - 6 tipos de gráficos Plotly personalizados (tendencias, mapas, sustancias)
     - Mapas interactivos con [datos geoespaciales](https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html).
     - Gráficos de tendencia con etiquetas dinámicas y selección de datos
   - Tablas con formato condicional y elementos visuales integrados

4. **Programación Reactiva Avanzada**
   - Manejo de eventos complejos entre componentes
   - Sistema de caché para consultas frecuentes
   - Actualización condicional de elementos UI
   - Manejo de estado global entre pestañas

## Presupuesto Detallado (en español)

| Componente | Descripción | Horas | Tarifa (MXN/h) | Subtotal (MXN) |
|------------|-------------|-------|----------------|----------------|
| **Arquitectura de datos** | Implementación de Arrow/Parquet, diseño de consultas optimizadas | 7 | 350 | 2,450 |
| **Desarrollo UI avanzado** | Componentes bslib, diseño responsivo, tooltips personalizados | 5 | 320 | 1,600 |
| **Visualizaciones interactivas** | Gráficos Plotly, mapas geoespaciales, redes de colaboración | 6 | 380 | 2,280 |
| **Tablas avanzadas** | Implementación de gt con banderas, formato condicional | 3 | 280 | 840 |
| **Sistema de filtrado** | Lógica de selección dinámica, actualización condicional | 4 | 300 | 1,200 |
| **Optimización de rendimiento** | Caché, debounce, recolección selectiva | 3 | 350 | 1,050 |
| **Pruebas y depuración** | QA, ajustes de rendimiento | 3 | 250 | 750 |
| **Total** | | **31** | | **10,170** |

## Justificación de Valor

El presupuesto excede el rango inicial de 3,500 MXN debido a varios factores:

1. **Complejidad técnica**: La implementación de Arrow representa un nivel avanzado que optimiza significativamente el rendimiento con grandes volúmenes de datos.

2. **Visualizaciones especializadas**: Los gráficos interactivos y mapas requirieron programación personalizada más allá de implementaciones básicas.

3. **Componentes UI modernos**: La aplicación utiliza elementos avanzados de Bootstrap 5 que mejoran significativamente la experiencia del usuario.

4. **Sistema reactivo sofisticado**: La lógica de actualización condicional y caché requirió un diseño cuidadoso para mantener el rendimiento.

## Notas de Cotización:

    Aunque mencionaste un tope de $3,500 MXN, el desarrollo incluye funcionalidades de nivel profesional que justifican un valor mayor

    Propuesta de ajuste: $4,200 MXN (37% del valor mercado) considerando relación laboral actual

    Incluye soporte técnico por 30 días sin costo adicional

    Se mantiene compatibilidad con futuras actualizaciones del dataset