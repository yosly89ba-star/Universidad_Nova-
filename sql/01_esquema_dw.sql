CREATE SCHEMA IF NOT EXISTS dw;
CREATE SCHEMA IF NOT EXISTS gov;

CREATE TABLE dw.dim_tiempo (
  periodo TEXT PRIMARY KEY, anio INT NOT NULL, semestre INT NOT NULL CHECK (semestre IN (1,2)),
  fecha_inicio DATE, fecha_fin DATE);
CREATE TABLE dw.dim_programa (
  cod_programa TEXT PRIMARY KEY, programa TEXT NOT NULL, facultad TEXT NOT NULL);
CREATE TABLE dw.dim_estudiante (
  id_estudiante_sk INT PRIMARY KEY, documento_hash CHAR(64) UNIQUE NOT NULL,
  genero CHAR(1), rango_edad TEXT, estrato INT CHECK (estrato BETWEEN 1 AND 6), region TEXT,
  cod_programa TEXT REFERENCES dw.dim_programa, anio_ingreso INT, estado TEXT);
CREATE TABLE dw.dim_estudiante_pii (
  id_estudiante_sk INT PRIMARY KEY, documento TEXT NOT NULL, nombre_completo TEXT, email TEXT, fecha_nacimiento DATE);
CREATE TABLE dw.dim_curso (
  cod_curso TEXT PRIMARY KEY, nombre_curso TEXT, creditos INT, cod_programa TEXT REFERENCES dw.dim_programa, nivel INT);
CREATE TABLE dw.dim_docente (
  id_docente_sk INT PRIMARY KEY, id_rrhh TEXT UNIQUE, facultad TEXT, categoria TEXT, dedicacion TEXT, anio_ingreso INT);

CREATE TABLE dw.fact_matricula (
  id_estudiante_sk INT REFERENCES dw.dim_estudiante, cod_curso TEXT REFERENCES dw.dim_curso,
  periodo TEXT REFERENCES dw.dim_tiempo, id_docente_sk INT REFERENCES dw.dim_docente,
  nota_final NUMERIC(4,1) CHECK (nota_final BETWEEN 0 AND 20),
  asistencia_pct NUMERIC(5,1) CHECK (asistencia_pct BETWEEN 0 AND 100),
  aprobado INT CHECK (aprobado IN (0,1)), creditos INT,
  PRIMARY KEY (id_estudiante_sk, cod_curso, periodo));
CREATE TABLE dw.fact_desempeno (
  id_estudiante_sk INT REFERENCES dw.dim_estudiante, periodo TEXT REFERENCES dw.dim_tiempo, semestre INT,
  promedio_ponderado NUMERIC(4,2), creditos_inscritos INT, asistencia_prom NUMERIC(5,1), tasa_aprobacion NUMERIC(4,3),
  logins NUMERIC, horas_plataforma_sem NUMERIC, entregas_pct NUMERIC, facturado NUMERIC(10,2), pagado NUMERIC(10,2),
  dias_mora NUMERIC, prestamos_biblioteca INT, atenciones_bienestar INT, bajo_rendimiento INT,
  PRIMARY KEY (id_estudiante_sk, periodo));
CREATE TABLE dw.fact_bienestar (
  id_estudiante_sk INT REFERENCES dw.dim_estudiante, periodo TEXT REFERENCES dw.dim_tiempo,
  atenciones INT, categoria_principal TEXT, nivel_riesgo TEXT);

CREATE TABLE gov.catalogo_datos (
  dominio TEXT, tipo_dato TEXT, ejemplos TEXT, fuente TEXT, sistema TEXT, responsable TEXT,
  usuarios TEXT, sensibilidad TEXT, retencion TEXT, riesgos TEXT);
CREATE TABLE gov.etl_log (
  run_id TEXT, commit_git TEXT, paso TEXT, filas_entrada BIGINT, filas_salida BIGINT, rechazadas BIGINT, nota TEXT, ts TEXT);
CREATE TABLE gov.dq_issues (
  run_id TEXT, fuente TEXT, regla TEXT, filas_afectadas BIGINT, accion TEXT);

CREATE VIEW dw.v_mart_desempeno AS
SELECT f.*, e.genero, e.rango_edad, e.estrato, e.region, e.cod_programa, p.programa, p.facultad
FROM dw.fact_desempeno f
JOIN dw.dim_estudiante e USING (id_estudiante_sk)
JOIN dw.dim_programa p USING (cod_programa);