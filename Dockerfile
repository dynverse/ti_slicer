FROM dynverse/dynwrap:r

RUN R -e 'devtools::install_cran("SLICER")'

LABEL version 0.1.4

ADD . /code

ENTRYPOINT Rscript /code/run.R
