# Stephen Cashin
# in case we ever do any installs
# lets make them non-interactive
ARG DEBIAN_FRONTEND=noninteractive

###### stage 1 - build image with dependencies

# use existing haskell image as our base
FROM fpco/stack-build:lts-16.23 as base-compile-image

WORKDIR /opt/githubapi
RUN stack update

# copy the yaml and cabal files
COPY ./github-get.cabal /opt/githubapi
COPY ./stack.yaml /opt/githubapi

# Docker will cache this command as a layer, saving us the trouble or rebuilding
# dependencies unless we change files above.
RUN stack build --only-dependencies -j4

##### stage 2 - compile the code

FROM base-compile-image as compile-image

COPY . /opt/githubapi
# do the build
RUN stack build --system-ghc

##### stage 3 - build small production image

FROM ubuntu:latest as runtime-image

ARG DEBIAN_FRONTEND=noninteractive
RUN echo "building runtime-image" && \
    apt-get update && \
    apt-get install -y netbase && \
    apt-get install -y ca-certificates
    
RUN mkdir -p /opt/githubapi
WORKDIR /opt/githubapi
ENTRYPOINT ["/opt/github-get/githubapi-exe"]
COPY --from=compile-image /opt/githubapi/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/github-get-exe/github-get-exe .
CMD [""]