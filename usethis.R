#librerias
library(usethis)
usethis::use_git_config(user.name = "Daniel795-lab", user.email = "diugalde@uc.cl")
usethis::browse_github_token()
usethis::edit_r_environ()

use_git() #activar
usethis::use_github() #y conectar el proyecto a GitHub

usethis::pr_pull_upstream()#bajo o descargo los cambios que hizo el colaborador
usethis::pr_init("sitio_web")#crea rama denominada "sitio_web"
devtools::build_site()#genera un .html en la carpeta docs
usethis::pr_push()#envio los cambios, se realiza un pull request a mi mismo.
usethis::pr_pull_upstream()

#Crea README
usethis::use_readme_rmd()
