##xds-downloader


### Operation


#### Takes download directives from the download SQS

{
  "id": 1,
  "remote_resource_url": "http://samplecsvs.s3.amazonaws.com/SacramentocrimeJanuary2006.csv",
  "s3_bucket": "https://xdataset-s3bucket-csuok9tc9lko.s3-website-us-east-1.amazonaws.com/",
  "s3_path": "net.csv"
}

{ "id": 1, "remote_resource_url": "http://samplecsvs.s3.amazonaws.com/TechCrunchcontinentalUSA.csv", "s3_path": "da2.csv"}


#### Downloads file to S3


#### Notifies SNS



#### Building

```
# builds the project in a container, then creates a minimal image and pushes it to dockerhub
./build-deploy-image.sh 
```
