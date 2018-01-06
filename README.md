# ISR/CSR Code 

## Dependencies
- jdk 1.8 (or higher)
- maven 3.3.9 (or higher)
## How to build and run
Go to project root folder and run `mvn clean install`.
An `.jar` file will be created at `target/` folder.
To run the server, execute the following `java -jar target/RadioAstronomia-0.0.1-SNAPSHOT.jar`
(make sure java installation is set in your `$PATH` or `%PATH%` (for Unix-like and Windows systems respectively)

## Avaiable Resources

### Request

#### Register Announce
###### Request: ``` GET /isrcsr ex=<Double>&ntotal=<Double>&bmagco=<Double>&bmagex=<Double>&angle=<Double>&scsize=<Double>&scheight=<Double>&sesize=<Double>&seheight=<Double>&j1=<Double>&j2=<Double>&etr=<Double>&npco=<Double>&npex=<Double>&ecsr=<Double>&xnisrex=<Double>&xncsr=<Double>&tb=<Double>&kf=<Double>```

###### Response example: 
```json
{
  "isr": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "csr": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "total": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "high": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "low": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "compact": {
    "frequency": [Int],
    "flux": "[Int]"
  },
  "extended": {
    "frequency": [Int],
    "flux": "[Int]"
  },
} 
```
