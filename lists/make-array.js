function(head, req) {
    var row,
    res = [];
    start({
	      "headers": {
		  "Content-Type": "application/json",
		  "charset":"utf-8"
	      }
	  });
    
    while(row = getRow()) {
	
	res.push(row.value);
    }

    send(toJSON(res));

}
