---
src_dir: src
         app
         test
output_dir: docs/fpm-ford
project: hp
summary: RPN calculator in modern Fortran
project_github: https://github.com/sgeard/hp
project_download:
author: sgeard
author_email: sgeard@gmail.com
github: https://github.com/sgeard/hp
media_dir: docs/images
exclude_dir: archive
             FODDER
display: public
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: docs/images/favicon.ico
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

{!README.md!}
