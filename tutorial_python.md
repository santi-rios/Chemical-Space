# Guía para el Desarrollo de Aplicaciones Web Interactivas con Python (Dash/Streamlit) y Control de Versiones con Git

## Índice
1. Introducción
2. Fundamentos de Dash y Streamlit
3. Estructura Básica de una Aplicación
4. Componentes Esenciales de la UI
5. Interactividad y Reactividad
6. Control de Versiones con Git
7. Flujo de Trabajo Git para Proyectos
8. Técnicas Avanzadas y Mejores Prácticas
9. Recursos Adicionales

## Introducción

En Python, existen varios frameworks para crear aplicaciones web interactivas para visualización de datos, siendo los más populares Dash (de Plotly) y Streamlit. Git, por otro lado, es una herramienta esencial para el control de versiones que facilita el seguimiento de cambios y la colaboración.

Esta guía te ayudará a comprender los fundamentos de estas tecnologías y cómo integrarlas en un flujo de trabajo eficiente.

## Fundamentos de Dash y Streamlit

### ¿Qué son Dash y Streamlit?

**Dash**:
- Framework de Plotly para construir aplicaciones web analíticas
- Basado en React.js, Flask y Plotly.js
- Permite un control más granular sobre los componentes

**Streamlit**:
- Framework más simple y rápido para prototipado
- Ideal para transformar scripts de Python en aplicaciones web
- Menos configuración requerida

### Instalación del Entorno

```bash
# Para Dash
pip install dash dash-bootstrap-components plotly pandas

# Para Streamlit
pip install streamlit pandas plotly

# Paquetes recomendados adicionales
pip install numpy scipy matplotlib altair leafmap
```

## Estructura Básica de una Aplicación

### Ejemplo Mínimo Funcional en Dash

```python
# app.py
import dash
from dash import html, dcc, Input, Output
import plotly.express as px
import pandas as pd
import numpy as np

# Inicializar la aplicación
app = dash.Dash(__name__)

# Definir el layout
app.layout = html.Div([
    html.H1("Mi Primera Aplicación Dash"),
    dcc.Slider(
        id='n-points-slider',
        min=10,
        max=1000,
        value=100,
        marks={i: str(i) for i in range(0, 1001, 100)},
        step=10
    ),
    dcc.Graph(id='scatter-plot')
])

# Definir la interactividad
@app.callback(
    Output('scatter-plot', 'figure'),
    Input('n-points-slider', 'value')
)
def update_scatter_plot(n_points):
    df = pd.DataFrame({
        'x': np.random.randn(n_points),
        'y': np.random.randn(n_points)
    })
    fig = px.scatter(df, x='x', y='y', title=f"{n_points} Puntos Aleatorios")
    return fig

# Ejecutar la aplicación
if __name__ == '__main__':
    app.run_server(debug=True)
```

### Ejemplo Mínimo Funcional en Streamlit

```python
# app.py
import streamlit as st
import numpy as np
import pandas as pd
import plotly.express as px

# Configuración de la página
st.set_page_config(page_title="Mi App Streamlit", layout="wide")

# Título
st.title("Mi Primera Aplicación Streamlit")

# Widget de entrada
n_points = st.slider("Número de puntos:", min_value=10, max_value=1000, value=100)

# Generar y mostrar gráfico
df = pd.DataFrame({
    'x': np.random.randn(n_points),
    'y': np.random.randn(n_points)
})
fig = px.scatter(df, x='x', y='y', title=f"{n_points} Puntos Aleatorios")
st.plotly_chart(fig, use_container_width=True)
```

### Organización de Archivos

Para aplicaciones más complejas, se recomienda:

```
mi_app_python/
├── app.py                  # Punto de entrada principal
├── requirements.txt         # Dependencias
├── assets/                 # Archivos estáticos (CSS, imágenes)
├── components/             # Componentes reutilizables
├── utils/                  # Funciones auxiliares
├── data/                   # Datos utilizados
└── tests/                  # Pruebas
```

## Componentes Esenciales de la UI

### Layouts en Dash

```python
# Layout básico
app.layout = html.Div([
    html.H1("Título"),
    html.Div("Contenido")
])

# Layout con columnas
app.layout = html.Div([
    dbc.Row([
        dbc.Col(html.Div("Columna 1"), width=4),
        dbc.Col(html.Div("Columna 2"), width=8)
    ])
])

# Layout con pestañas
app.layout = html.Div([
    dcc.Tabs([
        dcc.Tab(label='Gráfico 1', children=[...]),
        dcc.Tab(label='Gráfico 2', children=[...])
    ])
])
```

### Widgets de Entrada en Dash

```python
dcc.Slider(id='slider', min=0, max=100, value=50, step=1),
dcc.Dropdown(id='dropdown', options=[
    {'label': 'Opción A', 'value': 'A'},
    {'label': 'Opción B', 'value': 'B'}
]),
dcc.Checklist(id='checklist', options=[
    {'label': 'Mostrar detalles', 'value': 'details'}
]),
dcc.DatePickerRange(id='dates', start_date=datetime(2023,1,1)),
dcc.Upload(id='upload', children=html.Button('Subir Archivo')),
dcc.Input(id='text-input', type='text', placeholder='Escribe algo')
```

### Widgets en Streamlit

```python
st.slider("Rango de valores", 0, 100, (25, 75))
st.selectbox("Elige una opción", ["A", "B", "C"])
st.checkbox("Mostrar detalles")
st.date_input("Selecciona una fecha")
st.file_uploader("Sube un archivo")
st.text_input("Introduce texto")
st.number_input("Introduce un número", min_value=0, max_value=100)
```

### Elementos de Salida

**En Dash**:
```python
dcc.Graph(id='graph')          # Gráficos Plotly
html.Table(id='table')         # Tablas HTML
html.Img(id='image')           # Imágenes
html.Div(id='text-output')     # Texto
```

**En Streamlit**:
```python
st.plotly_chart(fig)          # Gráfico Plotly
st.dataframe(df)              # DataFrame interactivo
st.table(df.head())           # Tabla estática
st.image(image)               # Mostrar imagen
st.map(df)                    # Mapa con datos geo
st.write("Texto")             # Salida de texto genérico
```

## Interactividad y Reactividad

### Callbacks en Dash

```python
@app.callback(
    Output('graph-output', 'figure'),
    Input('x-axis-dropdown', 'value'),
    Input('y-axis-dropdown', 'value'),
    Input('color-dropdown', 'value')
)
def update_graph(x_col, y_col, color_col):
    fig = px.scatter(df, x=x_col, y=y_col, color=color_col)
    return fig
```

### Estado en Streamlit

```python
# Usando st.session_state para mantener estado
if 'counter' not in st.session_state:
    st.session_state.counter = 0

if st.button("Incrementar"):
    st.session_state.counter += 1

st.write(f"Contador: {st.session_state.counter}")
```

### Caché para Mejorar Rendimiento

```python
# En Streamlit
@st.cache_data
def load_data(file_path):
    return pd.read_csv(file_path)

# En Dash con memoización externa
from functools import lru_cache

@lru_cache(maxsize=32)
def expensive_computation(params):
    # Cálculos costosos
    return result
```

## Control de Versiones con Git

### Configuración Inicial

```bash
# Configurar identidad
git config --global user.name "Tu Nombre"
git config --global user.email "tu.email@ejemplo.com"

# Configurar editor preferido
git config --global core.editor "code --wait"  # Para VS Code
```

### Comandos Fundamentales

```bash
# Iniciar repositorio
git init

# Clonar repositorio existente
git clone https://github.com/usuario/repositorio.git

# Ver estado
git status

# Añadir cambios
git add archivo.py
git add .

# Hacer commit
git commit -m "Mensaje descriptivo"

# Ver historial
git log
git log --oneline --graph
```

### Ramas y Fusiones

```bash
# Crear y cambiar a rama
git checkout -b nueva-funcionalidad

# Listar ramas
git branch

# Fusionar ramas
git checkout main
git merge nueva-funcionalidad

# Resolver conflictos (si los hay)
# Editar archivos con conflictos, luego:
git add archivo_resuelto.py
git commit
```

## Flujo de Trabajo Git para Proyectos

### Estructura Recomendada

1. **main/master**: Rama principal con código estable
2. **develop**: Rama de integración para features completadas
3. **feature/**: Ramas para nuevas funcionalidades (feature/nombre)
4. **hotfix/**: Ramas para correcciones urgentes

### Buenas Prácticas

1. Hacer commits atómicos (un cambio por commit)
2. Escribir mensajes de commit claros
3. Hacer pull frecuentemente para mantener actualizada tu copia local
4. Revisar cambios antes de hacer push

### .gitignore para Python

```
# Archivos a ignorar
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
wheels/
*.egg-info/
.installed.cfg
*.egg
MANIFEST

# Archivos de entorno
.env
.venv
env/
venv/
ENV/
env.bak/
venv.bak/

# Editor y IDE
.vscode/
.idea/
*.swp
*.swo
```

## Técnicas Avanzadas y Mejores Prácticas

### Modularización en Dash

```python
# components/graphs.py
import dash_bootstrap_components as dbc
from dash import dcc, html

def create_scatter_plot(id_prefix):
    return dbc.Card([
        dbc.CardHeader("Gráfico de Dispersión"),
        dbc.CardBody([
            dcc.Graph(id=f"{id_prefix}-scatter-plot")
        ])
    ])

# app.py
from components.graphs import create_scatter_plot

app.layout = html.Div([
    create_scatter_plot("main"),
    # Otros componentes
])
```

### Despliegue de Aplicaciones

**Opciones comunes**:
1. **Streamlit Cloud**: Gratis para apps públicas
2. **Heroku**: Soporta Dash y Streamlit
3. **AWS/GCP/Azure**: Para soluciones empresariales
4. **Docker**: Para empaquetar toda la aplicación

**Ejemplo de Dockerfile para Dash**:

```dockerfile
FROM python:3.9-slim

WORKDIR /app
COPY . .

RUN pip install -r requirements.txt

EXPOSE 8050

CMD ["gunicorn", "--bind", "0.0.0.0:8050", "app:server"]
```

### Pruebas Automatizadas

**Para Dash con pytest**:

```python
# tests/test_app.py
from dash.testing.application_runners import import_app

def test_render_title(dash_duo):
    app = import_app("app")
    dash_duo.start_server(app)
    
    assert dash_duo.find_element("h1").text == "Mi Aplicación Dash"
```

**Para Streamlit**:

```python
# tests/test_streamlit.py
from streamlit.testing.v1 import AppTest

def test_slider():
    at = AppTest.from_file("app.py").run()
    at.slider("Número de puntos:").set_value(50).run()
    assert "50 puntos aleatorios" in at.get("markdown")[0].value
```

### Integración Continua con GitHub Actions

Ejemplo de `.github/workflows/python-app.yml`:

```yaml
name: Python application

on:
  push:
    branches: [ "main" ]
  pull_request:
    branches: [ "main" ]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Python
      uses: actions/setup-python@v3
      with:
        python-version: "3.9"
        
    - name: Install dependencies
      run: |
        python -m pip install --upgrade pip
        pip install -r requirements.txt
        pip install pytest
        
    - name: Run tests
      run: |
        pytest tests/
```

## Recursos Adicionales

### Para Dash
- [Documentación Oficial de Dash](https://dash.plotly.com/)
- [Dash Bootstrap Components](https://dash-bootstrap-components.opensource.faculty.ai/)
- [Galería de Ejemplos](https://dash-gallery.plotly.host/Portal/)

### Para Streamlit
- [Documentación Oficial](https://docs.streamlit.io/)
- [Componentes de la Comunidad](https://streamlit.io/components)
- [Galería de Aplicaciones](https://streamlit.io/gallery)

### Para Git
- [Pro Git Book](https://git-scm.com/book/es/v2)
- [GitHub Learning Lab](https://lab.github.com/)
- [Git Cheat Sheet](https://training.github.com/downloads/es_ES/github-git-cheat-sheet/)

### Cursos Recomendados
- [Data Apps con Dash y Python](https://www.udemy.com/course/interactive-python-dashboards-with-plotly-and-dash/)
- [Streamlit para Ciencia de Datos](https://www.datacamp.com/courses/build-a-data-science-web-app-with-streamlit)
- [Git Completo](https://www.udemy.com/course/git-completo/)