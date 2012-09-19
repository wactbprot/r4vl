function(doc) {
    
    if(doc.Calibration){
    
	if(doc.Calibration.Standard =="CE3"){
            var ca = doc.Calibration.Analysis,
            cm     = doc.Calibration.Measurement;
	    
            if(ca && cm){
                var cmv = cm.Values,
                cav     = ca.Values,
                cmd     = cm.Date || ca.Date,
                cms     = cm.SequenzControl|| cm.SequenceControl;
            
		if( cmd && cms && cav){
                    var cmvc = cmv.Conductance,
                    cmvp     = cmv.Pressure,
                    cavt     = cav.Temperature,
                    cmsg     = cms.Gas;
                
		    if(cmvc && cmvp && cmsg){
                        var dd =  cmd.Value || cmd[0].Value,
                        ddd    = dd.split(" ")[0],
                        retKey = ddd,
                        retObj = {"Date":ddd,
                                  "Gas": cmsg,
                                  "Conductance":cmvc,
                                  "Pressure": cmvp,
                                  "Temperature":cavt};

                        emit(retKey, retObj);
                    }
                }
            }
        }
    }
}
