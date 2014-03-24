exports.test = "fffuuuu";

var pad0 = function(n){
    return n < 10 ? "0" + n : n;    
};
exports.pad0 = pad0;

var vlDateString = function(dstr){
    var dt = dstr ? new Date(dstr) : new Date(),
    Y = dt.getFullYear(),
    M = pad0(dt.getMonth()+1),
    D = pad0(dt.getDate()),
    h = pad0(dt.getHours()),
    m = pad0(dt.getMinutes());
    return Y + '-' + M + '-' + D + " " + h+":" + m;
};
exports.vlDateString = vlDateString;

var vlTimeString = function(){
    var dt = new Date();
    
    return "" + dt.getTime();
};
exports.vlTimeString = vlTimeString;


exports.indexOf = function(arr,obj) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] == obj)
      return i;
  }
  return -1;
};


/**
 * http://stackoverflow.com/questions/18082/validate-numbers-in-javascript-isnumeric
 */
exports.isNumber = function(n) {
    return !isNaN(parseFloat(n)) && isFinite(n);
};

exports.isArray = function(obj) {
    if (obj.constructor.toString().indexOf("Array") == -1){
	return false;    
    }else{
	return true;    
    }
    
};
