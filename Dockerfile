FROM rocker/r-ver:4.5.0 AS builder

LABEL org.opencontainers.image.authors="epkanol@gmail.com"

# These were the package versions used at the time of build.
# Should be stable and tied to the rocker/r-ver version.
# Check which versions that are available via:
#
# $ docker run -ti --rm rocker/r-ver:4.5.0 bash
# root@abc123:/# apt update
# root@abc123:/# apt list cmake
# cmake/noble 3.28.3-1build7 amd64
#
# The point is to have a stable base of packages for R to stand on.

# tidyverse: libharfbuzz-dev libfribidi-dev
# ragg: libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
# xml2: libxml2-dev

RUN apt-get update && apt-get install -y \
    cmake='3.28.3-*' \
    libssl-dev='3.0.13-*' \
    pandoc='3.1.3+*' \
    libexpat1='2.6.1-*' \
    libnode-dev='18.19.1+*' \
    libcurl4t64='8.5.0-*' \
    libcurl4-openssl-dev='8.5.0-*' \
    libfontconfig1-dev='2.15.0-*' \
    libharfbuzz-dev='8.3.0-*' \
    libfribidi-dev='1.0.13-*' \
    libfreetype-dev='2.13.2+*' \
    libpng-dev='1.6.43-*' \
    libtiff5-dev='4.5.1+*' \
    libjpeg-dev='8c-*' \
    libxml2-dev='2.9.14+*' \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -u 2000 app
USER app

RUN mkdir -p /home/app/R/library && mkdir /home/app/analysis

WORKDIR /home/app

RUN echo ".libPaths('/home/app/R')" >> .Rprofile && R -e "install.packages('renv')"

USER root
COPY renv.lock /home/app/renv.lock
RUN chown -R app:app /home/app/renv.lock
USER app
RUN R -e "renv::restore()"
RUN R -e 'install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org", getOption("repos")), version="2.36.0")'
RUN R -e 'cmdstanr::install_cmdstan(version="2.36.0")'

# source code changes here, after building the R image incl. renv packages and Stan
USER root
COPY analysis/observability /home/app/analysis
RUN chown -R app:app /home/app/analysis
COPY analysis/data /home/app/data
RUN chown -R app:app /home/app/data
COPY analysis/.cache /home/app/.cache
RUN chown -R app:app /home/app/.cache

COPY analysis/render_all.sh /home/app
RUN chown app:app /home/app/render_all.sh
RUN chmod u+x /home/app/render_all.sh

USER app
# default destinations
RUN mkdir -p /home/app/.cache
RUN mkdir -p /home/app/output

# RMarkdown defaults to use the parent directory of the Rmd as PWD, so paths inside the Rmd should be relative to this.
# However, the output_dir parameter itself is relative to the docker root (WORKDIR above)
CMD ["/home/app/render_all.sh" ]
