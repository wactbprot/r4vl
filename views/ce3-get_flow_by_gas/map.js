function(doc) {
    if(doc.Calibration){
        if(doc.Calibration.Standard =="CE3"){
            var ca = doc.Calibration.Analysis,
            cm     = doc.Calibration.Measurement;
            if(ca && cm){
                var cmv = cm.Values,
                cav     = ca.Values,
                cmd     = cm.Date || ca.Date,
		cmdv    = cmd ? (cmd.Value || cmd[0].Value): "2011-01-01",
                cms     = cm.SequenzControl|| cm.SequenceControl;
                if(cms && cav){
                    var cmvc =cmv.Conductance;
                    var cmvp =cmv.Pressure;
		    var cavt = cav.Temperature;
                    var cmsg = cms.Gas;
                    if(cmvc && cmvp && cmsg){
                        var retKey = cmsg;
			var retObj = {"Date": cmdv,
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
