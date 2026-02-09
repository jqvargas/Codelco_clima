connect_to_db <- function(env_file) {
  # función para conectarse a la base de datos.
  # Cargar las variables de entorno del archivo .env
  dotenv::load_dot_env(file = env_file)
  # Obtener las credenciales de las variables de entorno
  host <- Sys.getenv("POSTGRES_HOST")
  port <- Sys.getenv("POSTGRES_PORT")
  dbname <- Sys.getenv("POSTGRES_DB")
  user <- Sys.getenv("POSTGRES_USER")
  password <- Sys.getenv("POSTGRES_PASSWORD")
  
  # Establecer la conexión
  connection <- dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password
  )
  
  # Verificar la conexión, si es válida o no. 
  if (dbIsValid(connection)) {
    cat("Conexión exitosa.\n")
  } else {
    stop("Error al conectar a la base de datos.")
  }
  
  return(connection)
}