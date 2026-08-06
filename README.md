## Dashboard de seguimiento estudiantes de educación técnico-profesional de nivel medio en la región de Tarapacá - Chile

### Presentación en formato *shiny* utilizando *Rstudio*

Proyecto de presentación de datos plataforma interactiva de comparación de planes formativos asociados a dos especialidades técnicas de nivel medio: __Administración (menciones
de Recursos Humanos y Logística)__ y __Atención de Enfermería (mención Enfermería)__ y carreras técnicas de nivel superior relacionadas: __TNS en Administración de Empresas__
y __TNS en Enfermería__, ambas carreras del CFT Estatal de Tarapacá.

La estructura de la página es la siguiente: 

```
Shiny seguimiento EMTP

├── app.R 
├── módulos/ 
│   ├── resumen.R
│   ├── liceos.R
│   ├── seguimiento.R
│   └── planes.R
├── global.R
└── www/             
```
#### Módulo Resumen

Presenta datos de matrículas, egresos y títulos de todos los establecimientos de la región que imparten las especialidades de **Administración** y **Atención de Enfermería**.
Para ello, se consolidaron datos disponibles en MINEDUC (Datos Abiertos).

<img width="416" height="200" alt="image" src="https://github.com/user-attachments/assets/a38c2966-dac5-470f-b297-97a4b8aaa538" />

#### Módulo Liceos

Ahonda en información de seguimiento para establecimientos asociados al proyecto trabajado. En total, son *cinco* establecimientos. En esta sección se trabajó con cohortes para
seguimiento de matrículas, tasas de egreso y de titulación. Además, se integraron datos de lugares de práctica asociados a los cohortes de los últimos cinco años.

<img width="416" height="200" alt="image" src="https://github.com/user-attachments/assets/7e3fdd75-fd26-4b2e-a307-036d24583e71" />

#### Módulo Seguimiento

Información sobre seguimiento de cohortes construidos en módulo liceos y su inserción en carreras de educación superior, generando información descriptiva de continuidad
en su áreas formativa EMTP, elección de instituciones y formas de ingreso. 

<img width="416" height="200" alt="image" src="https://github.com/user-attachments/assets/cbcbcb4d-87b0-48aa-a023-d0b60d641925" />

#### Módulo Planes

Plataforma interactiva para ejercicio comparativo de planes de carreras técnicas EMTP y TNS afines del proyecto. Esta plataforma toma la información de objetivos y resultados 
de aprendizaje para comparar fácilmente módulos de especialidades y asignaturas de carreras TNS con el fin de analizar y proponer convalidaciones para fortalecer un itinerario
formativo amplio en la formación TP.

<img width="416" height="200" alt="image" src="https://github.com/user-attachments/assets/305c7925-7a9b-43d2-8f9d-2eb34e8168ac" />

Este trabajo ejemplifica un formato sencillo, interactivo y fácil de aplicar para trabajo con datos abiertos del MINEDUC para tomadores de decisiones asociadas a la educación
TP. 

PD: Es necesario resaltar que el proceso de limpieza y *join* de bases de datos que se expresan en los archivos *csv* del dashboard no están disponibles. Estos serán subidos
en otro repositorio.
