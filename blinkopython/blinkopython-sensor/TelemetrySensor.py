# TelemetrySensor.py
# 201905121555
# Author antonio.carrasco.valero
# Copyright (C) 2019, Antonio Carrasco Valero Licensed under European Public License EUPL
#  Licensed under the EUPL, Version 1.1 only (the "Licence");
# 
#  You may not use this work except in compliance with the
#  Licence.
# 
#  You may obtain a copy of the Licence at:
#  https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
# 
#  Unless required by applicable law or agreed to in
#  writing, software distributed under the Licence is
#  distributed on an "AS IS" basis,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
#  express or implied.
# 
#  See the Licence for the specific language governing
#  permissions and limitations under the Licence.

import psutil

def telemetrySensorRead( theInterval):
    anInterval = theInterval
    if anInterval < 1:
        anInterval=1
        
    aReading = psutil.cpu_percent(interval=anInterval, percpu=True)    
    return aReading
    
    