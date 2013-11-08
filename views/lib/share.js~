/**
 * deprecated!
 * all uses of generateHtml should be replaced
 * by mustache.to_html().
 *
 */
exports.generateHtml = function(format,_resArr,_key){
    _key = _key || "key";
    /**
     * wenn Zeit ist kann hier mal der retType zerlegt werden
     * um die html- tags autom. zusammenzubauen
     */
    var fArr = format.split(/[-_]+/),
    noOfTags = fArr.length;
    
    var st = "", 
    et = "", // start tag end tag 
    mds = "<" + fArr[noOfTags-1] + ">",
    mde = "</" + fArr[noOfTags-1] + ">",
    md = "";
    
    for(var j =1; j < (noOfTags-1); ++j){
	st = st + "<" + fArr[j] + ">";
	et = "</" + fArr[j] + ">" + et;
    }

    for (var i = 0; i < _resArr.length; ++i){
   	md = md + "<" + fArr[noOfTags-1] + ">" + 
	    _resArr[i][_key] + 
	    "</" + fArr[noOfTags-1] + ">"; 
    }

    return st + md +  et;
};

exports.defaultReplacement= function(task,defaults,query){

    defaults = defaults || task.Defaults;
    query    = query    || {};

    if(defaults){
	for(var i in defaults){
	  
	    var subs = new RegExp(i,'g');
	    for(var j in task){
		/**
		 * diese properties werden von der 
		 * Ersetzung ausgeschlossen
		 */
		if( (j != "Defaults") && 
		    (j != "ErrorResponse") &&
		    (j != "TaskName") &&
		    (j != "DocPath") 
		  ){
		      /**
		       * geht noch eine Ebene in die Tiefe
		       * v.a. wegen PostProcessing 
		       */
		      if( typeof(task[j]) == "object"){ 
			 
			  for (var k in  task[j]){

			      if(query[i]){
				  task[j][k] = task[j][k].replace(subs,query[i]);
			      }else{
				  if(typeof task[j][k] == "string" ){
				     
				      task[j][k] = task[j][k].replace(subs,defaults[i]);				    
				  }
			      }
			  }
		      }
		      if( typeof(task[j]) == "string"){  
			
			  if(query[i]){
			      task[j] = task[j].replace(subs,query[i]);
			  }else{
			      task[j] = task[j].replace(subs,defaults[i]);
			  }
		      }
		  }//!defaults
	    }// for j in tasks
	}// for in in defaults
    }// have defaults
    return task;
};

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


exports.startHtml = { 
    headers: {
	"Content-type": "text/html",
        "charset":"utf-8"
    }
};

exports.startJson = {
    "headers": {
        "Content-Type": "application/json",
        "charset":"utf-8"
    }
};

exports.startCss = {
    "headers": {
        "Content-Type": "text/css",
        "charset":"utf-8"
    }
};
 
