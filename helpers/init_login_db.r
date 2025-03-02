# script to setup authentification database (using shinymanager)

# SETUP AUTH =============================
# Init DB using credentials data
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user"),
  # password will automatically be hashed
  admin = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Init the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "auth.sqlite", # will be created
  passphrase = Sys.getenv("AUTH_DB_KEY")
  # passphrase = "passphrase_wihtout_keyring"
)
