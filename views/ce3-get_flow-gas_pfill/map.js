function(doc) {
    if(doc.Calibration){
        if(doc.Calibration.Standard =="CE3"){
            var ca = doc.Calibration.Analysis,
            cm     = doc.Calibration.Measurement;

            if(ca && cm){

                var cav  = ca.Values,
                cad  = ca.Date,
                cms  = cm.SequenceControl;

                if(cav && cad && cms){

                    var gas  = cms.Gas,
                    cadv = cad.Value,
                    cavc = cav.Conductance,
                    cavp = cav.Pressure,
                    cavt = cav.Temperature;

                    if(gas && cavc && cavp && cavt){

                        var Ncond  = 0,
                        Ntemp  = 0,
                        Npfill = 0;

                        for(var i in cavt){
                            if(cavt[i].Type == "Tfm3"){
                                var temp  = cavt[i].Value;
                                Ntemp     = temp.length;
                            }
                        }
                        for(var i in cavc){
                            if(cavc[i].Type == "cnom"){
                                var cond  = cavc[i].Value;
                                Ncond     = cond.length;
                            }
                        }
                        for(var i in cavp){
                            if(cavp[i].Type == "fill"){
                                var pfill = cavp[i].Value;
                                Npfill    = pfill.length;
                            }
                        }
                        if(Npfill > 0 && Npfill == Ncond && Ntemp == Npfill){
                            for(var j = 0; j < Npfill; j++){
                                emit( [gas, pfill[j]] , // key
				      [cadv, cond[j], pfill[j], temp[j]] // value
				    );
                            }
                        }
                    }
                }
            }
        }
    }
}