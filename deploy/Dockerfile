FROM ababkin/haskell-scratch:integer-gmp

MAINTAINER Alex Babkin <ababkin@gmail.com>
ENV APP xds-downloader

# RUN mkdir /opt/${APP}

# Add the Application
ADD ./deploy/${APP} /opt/${APP}/${APP}

ENV PATH /opt/${APP}:$PATH

# Default Command for Container
WORKDIR /opt/${APP}
CMD ["sh", "-c", "echo ${APP}"]
