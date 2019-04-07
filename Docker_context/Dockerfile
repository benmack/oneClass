FROM rocker/geospatial:3.5.3
MAINTAINER "Ben Mack" ben8mack@gmail.com

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  && install2.r --error \
    caret \
    dismo \
    kernlab \
    pROC \
    spatial.tools
