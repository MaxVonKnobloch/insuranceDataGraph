# insuranceDataGraph
Contains R package with REST endpoints

In case an error occurs directly after installing and loading the package, restarting the session (.rs.restartR()) should help.

To run the swagger and make the endpoints accessible, run the method createAPI().

The GET method \info and POST method \Graphs can then be reached.
The post method sends back a pdf-file and needs the data as input file with key: dataset

Example with Curl on Windows:
curl -X GET http://localhost:xxxx/info

curl -X POST -H "Content-Type: text/csv" --data-urlencode "dataset@directory\of\file.csv" http://localhost:xxxx/Graphs -o graphics.pdf
