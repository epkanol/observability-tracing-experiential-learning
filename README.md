# observability-tracing-experiential-learning
Replication package for the paper "Learning Observability Tracing Through Experiential Learning"

## Structure of this repository

* Root directory
  Contains a Dockerfile that can be used to build a reproducible Docker image,
  including the full, anonymized, data set used in the paper, and the complete
  Renv file specifying the complete R environment.

* `analysis/observability`
  Contains a number of RMarkdown files (named 01_xxx, 02_xxx, and so on),
  that deals with different aspects (EDA, modeling) of the analysis.
  All figures in the paper are generated from the file `99_article_illustrations.html`

* `analysis/.cache`
  Contains precomputed R data objects (models, reloo results) that is used
  to calculate results, unless you want to redo the analysis (see below in the
  CACHE section).

* `analysis/output`
  Contains HTML and PDF files that were generated from the RMarkdown in the
  docker image. These files are not included in the docker image - to generate
  your own versions of them, you bind a directory inside the docker image to
  a directory on the machine where you run the docker image (see below).

## Steps to build and run docker image

The image has been built on an Ubuntu 24.04 system (x64 Linux), with 64 GiB internal memory and 21 GiB swap.
Other steps might be needed on other architectures or OSes.

1. Build the docker image via: `docker build -t observability-tracing .`

2. Output files are created in `/home/app/output`, and models are cached in `/home/app/.cache`.
   These directories should be mapped to some directory on your local system.
   First create the needed local directories, and make them available for the docker image:
   `mkdir -p ${PWD}/observability/.cache && mkdir -p ${PWD}/observability/output && chmod -R a+rwx ${PWD}/observability`

3. Run the image, and mount these directories to the container:
   `docker run --mount type=bind,source=${PWD}/observability/.cache,target=/home/app/.cache --mount type=bind,source=${PWD}/observability/output,target=/home/app/output observability-tracing`

### Configurability via environment variables

#### PREFIX

In case you only want to run some RMarkdown files, a prefix can be specified via a docker environment variable:

`docker run --mount type=bind,source=${PWD}/observability/output,target=/home/app/output -e PREFIX=02 observability-tracing`

Standard shell globbing rules can be used (but beware to escape them from the regular shell used to start docker).

#### CACHE

In case you want to run with a separately stored cache directory, use the CACHE environment variable, and make sure that it is mounted into the container. For instance:

`docker run --mount type=bind,source=${PWD}/observability/.cache,target=/home/app/.cache -e PREFIX=02 -e CACHE="../.cache" observability-tracing`

#### RELOO

Setting the `RELOO` environment variable to `TRUE` will run the reloo function,
which performs exact Leave-One-Out Cross-Validation once per datapoint signaled as potentially influential by the regular `loo` function.
The result of the reloo will be stored in the cache directory, where it can be further analysed (e.g. plotted).

`docker run --mount type=bind,source=${PWD}/observability/output,target=/home/app/output -e RELOO=TRUE -e PREFIX=02 observability-tracing`

Running `reloo` takes considerably longer to complete, but it should not take much longer than a coffee break.
The output of the `docker run` command will tell if it is performing the `reloo` step, and the state of the models can be inspected via the `docker top` command.

```
user@host:> docker ps
CONTAINER ID   IMAGE                                                                     COMMAND                  CREATED          STATUS                 PORTS     NAMES
6ee210bac03e   observability-tracing                                                         "/home/app/render_al…"   46 minutes ago   Up 46 minutes                    vigorous_chebyshev
user@host:> docker top 6ee210bac03e
UID                 PID                 PPID                C                   STIME               TTY                 TIME                CMD
2000                56576               56554               0                   11:49               ?                   00:00:00            /bin/sh /home/app/render_all.sh
2000                56618               56576               17                  11:49               ?                   00:08:18            /usr/local/lib/R/bin/exec/R --no-save --no-restore -e rmarkdown::render("analysis/02_intercept_only_model.Rmd",~+~params=list(cache="../observability/.cache",~+~reloo=TRUE),~+~output_dir="observability/output")
2000                63704               56618               99                  12:22               ?                   00:49:00            ./model_87faeed5fafcb73fb4487a98a2060430 id=3 random seed=189062706 data file=/tmp/RtmpyTRE1T/standata-969aeb6b8.json output file=/tmp/RtmpyTRE1T/model_87faeed5fafcb73fb4487a98a2060430-202403251058-3-56a7b2.csv refresh=0 profile_file=/tmp/RtmpyTRE1T/model_87faeed5fafcb73fb4487a98a2060430-profile-202403251058-3-67b236.csv method=sample num_samples=3000 num_warmup=1000 save_warmup=0 thin=1 algorithm=hmc engine=nuts adapt delta=0.95 engaged=1
2000                63746               56618               99                  12:22               ?                   00:48:25            ./model_87faeed5fafcb73fb4487a98a2060430 id=4 random seed=189062706 data file=/tmp/RtmpyTRE1T/standata-969aeb6b8.json output file=/tmp/RtmpyTRE1T/model_87faeed5fafcb73fb4487a98a2060430-202403251058-4-56a7b2.csv refresh=0 profile_file=/tmp/RtmpyTRE1T/model_87faeed5fafcb73fb4487a98a2060430-profile-202403251058-4-67b236.csv method=sample num_samples=3000 num_warmup=1000 save_warmup=0 thin=1 algorithm=hmc engine=nuts adapt delta=0.95 engaged=1
```

#### CORES

The number of parallelly executing models is controlled via the `CORES` environment, which defaults to 2.

`docker run --mount type=bind,source=${PWD}/observability/output,target=/home/app/output -e CORES=2 -e PREFIX=02 -e CACHE="../.cache" observability-tracing`
