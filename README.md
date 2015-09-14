#xds-downloader

## Operation

- receives the directive from the 'Download' queue
- Downloads file to S3 from the source url specified
- Notifies 'Downloaded' SNS


## Interface

### Inputs

#### Takes directives from the download queue

Expected directive structure
```
{
  "datasetId": <dataset id>,
  "remoteUrl": <resource url to be downloaded>,
  "s3Bucket": <AWS S3 bucket url>,
  "s3Path": <path the downloaded resource should be placed under in the S3 bucket>
}
```

Example:
```
{
  "datasetId": 1,
  "remoteUrl": "http://samplecsvs.s3.amazonaws.com/SacramentocrimeJanuary2006.csv",
  "s3Bucket": "https://xdataset-s3bucket-csuok9tc9lko.s3-website-us-east-1.amazonaws.com/",
  "s3Path": "datasets/my_dataset.csv"
}
```

### Outputs

#### Publishes modified directives onto the downloadComplete







#### Building

xds-deploy (https://github.com/ababkin/xds-deploy) executable needs to be installed in PATH

```
deploy # run this inside the project directory
```
