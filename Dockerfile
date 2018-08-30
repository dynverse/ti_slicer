FROM dynverse/dynwrap:r

LABEL version 0.1.0

RUN R -e 'devtools::install_cran("SLICER")'

ADD . /code

ENTRYPOINT Rscript /code/run.R
