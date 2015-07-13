FROM haskell:7.8

MAINTAINER Alex Babkin <ababkin@gmail.com>
ENV APP xds-downloader

RUN apt-get update -y && \
    apt-get install -y libssl-dev libghc-crypto-dev ca-certificates && \
    apt-get clean -y && \
    apt-get purge -y

RUN cabal update

# Add .cabal file
ADD ./${APP}.cabal /opt/${APP}/${APP}.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# RUN cd /opt/${APP} && cabal sandbox init && cabal install --only-dependencies -j4
RUN cd /opt/${APP} && cabal install --only-dependencies -j8

# Add and Install Application Code
ADD . /opt/${APP}
RUN cd /opt/${APP} && \
    cabal install --global && \
    rm -rf /opt/${APP}/dist && \
    rm -rf ~/.cabal

# RUN cd /opt/${APP} && cabal sandbox delete

# Add installed cabal executables to PATH
# ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt/${APP}
CMD ["sh", "-c", "echo ${APP}"]
