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
