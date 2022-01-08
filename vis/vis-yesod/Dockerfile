# in case we ever do any installs
# lets make them non-interactive
ARG DEBIAN_FRONTEND=noninteractive

###### stage 1 - build image with dependencies

# use existing haskell image as our base
FROM fpco/stack-build:lts-16.23 as base-compile-image-y
WORKDIR /opt/vis-yesod
RUN stack update

# copy the yaml and cabal files
COPY ./vis-yesod.cabal /opt/vis-yesod
COPY ./stack.yaml /opt/vis-yesod

# Docker will cache this command as a layer, savinf us the trouble or rebuilding
# dependencies unless we change fioles above.
RUN stack install yesod-bin --system-ghc
RUN stack build --system-ghc --only-dependencies -j4

##### stage 2 - compile the code

FROM base-compile-image-y as compile-image-y
COPY . /opt/vis-yesod
WORKDIR /opt/vis-yesod
RUN stack build --system-ghc

##### stage 3 - build small production image

FROM ubuntu:18.04 as runtime-image
RUN mkdir -p /opt/vis-yesod
WORKDIR /opt/vis-yesod
COPY --from=compile-image-y /opt/vis-yesod/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/vis-yesod/vis-yesod .
COPY config config
COPY static static
CMD ["/opt/vis-yesod/vis-yesod"]