String.prototype.toJadenCase = function () {
  let returnValue = []
  this.split(' ').map(function(item){
  	returnValue.push(item[0].toUpperCase() + item.slice(1))
  })
  return returnValue.join(' ')
};

persistence = function(num){
  counter = 0
}
