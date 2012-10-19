function(doc) {
    if(doc.Calibration){
        if(doc.Calibration.Standard =="CE3"){
            var ca =doc.Calibration.Analysis;
            var cm =doc.Calibration.Measurement;
            if(ca && cm){
                var cmv = cm.Values;
                var cav = ca.Values;
                var cmd = cm.Date || ca.Date;
                var cms = cm.SequenzControl|| cm.SequenceControl;
                if( cmd && cms && cav){
                    var cmvc =cmv.Conductance;
                    var cmvp =cmv.Pressure;
		    var cavt = cav.Temperature;
                    var cmsg = cms.Gas;
                    if(cmvc && cmvp && cmsg){
                        var retKey = cmsg;
			var retObj = {"Date": cmd.Value || cmd[0].Value,
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
