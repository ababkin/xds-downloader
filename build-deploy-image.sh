docker build --rm=false -t ababkin/xds-downloader:latest .
docker run --rm -v $(pwd)/deploy:/deploy ababkin/xds-downloader:latest cp /root/.cabal/bin/xds-downloader /deploy

docker pull ababkin/haskell-scratch:integer-gmp
docker build -f deploy/Dockerfile -t ababkin/xds-downloader:deploy .
docker push ababkin/xds-downloader:deploy
