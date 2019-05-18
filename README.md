# blinko :  Distributed CPU usage monitoring and LED display written in Erlang and Python2 BiblioPixel
**Distributed CPU usage monitoring and APA102 LED display written in Erlang and Python2 BiblioPixel.**
 
Created @author Antonio Carrasco Valero 20190512

Copyright 2019 Antonio Carrasco Valero

Licensed under the EUPL, Version 1.1 only (the "Licence");

You may not use this work except in compliance with the
Licence.

You may obtain a copy of the Licence at:
https://joinup.ec.europa.eu/software/page/eupl/licence-eupl

Unless required by applicable law or agreed to in
writing, software distributed under the Licence is
distributed on an "AS IS" basis,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
express or implied.

See the Licence for the specific language governing
permissions and limitations under the Licence.

# Dependencies

## user access to the serial

The user running the blinko-show python code must be allowed to write-access serial hardware interface.
I.e. by adding the user to the 'dialout' users group.

    sudo usermod -a -G dialout Ausername


## BiblioPixel

Written in Python to drive LED strips and pannels. 

This blinko implementation includes a snapshot of an earlier and simpler version of BilioPixel. 
This blinko implementation does not utilize BiblioPixel's animation capabilities, 
just the mapping of colors and sending to the LED controller device.

BiblioPixel is now way more advanced in their later versions.
https://github.com/ManiacalLabs/PixelWeb/wiki


## erlport - to call python from erlang



## sysbench - to waste CPU cycles to display on LED strip

Uses the sysbench utility to waste some CPU cycles to be shown on LED strip. Install i.e. by (debian/ubuntu):

    sudo apt-get install sysbench
