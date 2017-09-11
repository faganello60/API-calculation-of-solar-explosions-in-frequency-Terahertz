# ISR/CSR Code 

## Dependencies
- jdk 1.8 (or higher)
- maven 3.3.9 (or higher)
## How to build and run
Go to project root folder and run `mvn clean install`.
An `.jar` file will be created at `target/` folder.
To run the server, execute the following `java -jar target/mackenzie-bookstore-server-0.0.1-SNAPSHOT.jar`
(make sure java installation is set in your `$PATH` or `%PATH%` (for Unix-like and Windows systems respectively)

## Avaiable Resources

### Request

#### Register Announce
###### Request: ``` POST /announces?user_code=<code>&isbn=<isbn>[&description=<description>] ```

###### Response example: 
```json
{
  "annoucePK": {
    "code": 31409695,
    "isbn": "9788588134119"
  },
  "sold": false,
  "description": "Livro muito bom",
  "book": {
    "isbn": "9788588134119",
    "name": "Memórias do povo alemão no Rio Grande do Sul",
    "description": "Memórias do povo alemão no Rio Grande do Sul",
    "publisher": {
      "idPublisher": 3,
      "name": "Felipe Kuhn Braun"
    },
    "author": {
      "idAuthor": 3,
      "name": "Desconhecido"
    }
  }
} 
```
