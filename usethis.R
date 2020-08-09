#librerias
library(usethis)
usethis::use_git_config(user.name = "Daniel795-lab", user.email = "diugalde@uc.cl")
usethis::browse_github_token()
usethis::edit_r_environ()

use_git() #activar
usethis::use_github() #y conectar el proyecto a GitHub

usethis::pr_pull_upstream()#bajo o descargo los cambios que hizo el colaborador
usethis::pr_init("sitio_web")#crea rama denominada "sitio_web"