# Native JS Version of Promise Chaining
# http://stackoverflow.com/questions/28250680/how-do-i-access-previous-promise-results-in-a-then-chain
function getExample() {
    // preprocessing
    return promiseA(…).then(handlerA);
}
function handlerA(resultA) {
    // some processing
    return promiseB(…).then(handlerB.bind(null, resultA));
}
function handlerB(resultA, resultB) {
    // more processing
    return // anything that uses resultA and resultB
}

# WHEN USING COMMAND LINE ARGUMENTS, NODE HAS THE FOLLOWING PACKAGE:
# https://www.npmjs.com/package/commander

# ELASTIC SEARCH:
# http://joelabrahamsson.com/elasticsearch-101/
# ElasticSearch has and endpoint (_bulk) for indexing multiple documents with a single request
### SEARCH QUERIES:
http://localhost:9200/_search - Search across all indexes and all types.
http://localhost:9200/movies/_search - Search across all types in the movies index.
http://localhost:9200/movies/movie/_search

One may wonder what the query DSL is. It's ElasticSearch's own domain specific language based on JSON in which queries and filters can be expressed. Think of it like ElasticSearch's equivalent of SQL for a relational database.

# ./bin/cli report:account-gain
