function(doc) {
   
    var share = require("views/lib/share");
    
    if(doc.Calibration &&
       doc.Calibration.Standard =="CE3" &&
       doc.Calibration.Measurement){
	var dcm = doc.Calibration.Measurement;

	if(dcm.Date){
	    if(share.isArray(dcm.Date)){
		
		var dcmdv =  dcm.Date[0].Value;
		if(share.isArray(dcmdv)){
		    dcmdv = dcmdv[0];
		}
	    }else{
		var dcmdv =  dcm.Date.Value;
	    }
	}else{
	    var dcmdv = "1970-01-01";
	}
	
	if(dcm.AuxValues &&
	   dcm.AuxValues.Pressure){
	    
	    var dcmap = dcm.AuxValues.Pressure;
	    if(dcmap &&  share.isArray(dcmap)){
		for(var i = 0; i < dcmap.length; ++i){
		    if(dcmap[i].Type == "srg_p_rise"){
			
			if(share.isArray(dcmap[i].Value)){
			    priseArr = dcmap[i].Value;
			    prise = priseArr[priseArr.length -1];
			}else{
			    prise = dcmap[i].Value;
			}
		    }
		}
	    }//pressure
	    var dcmat = dcm.AuxValues.Time;
	    if(dcmat &&  share.isArray(dcmat)){
		for(var i = 0; i < dcmat.length; ++i){
		    if(dcmat[i].Type == "begin_constC"){
			tbegin = dcmat[i].Value
		    }
		    if(dcmat[i].Type == "end_constC"){
			tend = dcmat[i].Value
		    }
		}
	    }//time
	    if(tbegin && tend && prise &&
	       (tbegin.length == tend.length) &&
	       tbegin.length > 0){
		for(var j=0; j< tbegin.length; ++j){
		    var dts = (parseInt(tend[j],10) - parseInt(tbegin[j],10))/1000 
		    emit(dcmdv, dts*prise) 
		} 
	    }
	}
    }
}	
