function(doc) {
   
    var share = require("views/lib/share");
 
    if(doc.Calibration &&
       doc.Calibration.Standard =="CE3" &&
       doc.Calibration.Measurement){
	var dcm = doc.Calibration.Measurement;
	
	
	
	if(dcm.AuxValues &&
	   dcm.AuxValues.Pressure){
	
	    var dcmap = dcm.AuxValues.Pressure;
	    if( share.isArray(dcmap)){
		for(var i = 0; i < dcmap.length; ++i){
		    if(dcmap[i].Type == "srg_p_rise"){
			
		    }
	    
		}
	    } 
	}
    }
}	
