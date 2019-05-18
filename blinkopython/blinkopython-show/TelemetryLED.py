# TelemetryLED.py
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

from datetime import datetime

from time import sleep

from threading import Thread

#Load driver for the AllPixel
from bibliopixel.drivers.serial_driver import *

#load the LEDStrip class
from bibliopixel.led import *

LOGEACH = False

LEDSEGMENT_ALLOTHERS = '*'


NUMLEDS_MIN = 1
NUMLEDS_MAX = 144
NUMLEDS_DEFAULT = 144

LEDSCOMMSPEED_MIN = 1
LEDSCOMMSPEED_MAX = 23
LEDSCOMMSPEED_DEFAULT = 10

LEDSMASTERBRIGHTNESS_MIN = 16
LEDSMASTERBRIGHTNESS_MAX = 255
LEDSMASTERBRIGHTNESS_DEFAULT = 64

TELEMETRYTOBRIGHTNESSFACTOR_MIN = 0.05
TELEMETRYTOBRIGHTNESSFACTOR_MAX = 1.0
TELEMETRYTOBRIGHTNESSFACTOR_DEFAULT = 0.25 

SORTTELEMETRY_DEFAULT = True
TRANSITIONS_DEFAULT = True


EIGHTBITSRANGE    = 256
MAXLEDVALUE       = EIGHTBITSRANGE - 1
MAXLEDVALUE_RED   = EIGHTBITSRANGE - 1
MAXLEDVALUE_GREEN = MAXLEDVALUE_RED + EIGHTBITSRANGE 
MAXLEDVALUE_BLUE  = MAXLEDVALUE_GREEN + EIGHTBITSRANGE 
MAXLEDVALUE_ACUM  = MAXLEDVALUE_BLUE




_v_ConfigurationChecked = None
_v_NumLEDs = None
_v_LEDsCommSpeed = None
_v_LEDsMasterBrigtness = None
_v_TelemetryToBrigtnessFactor= None
_v_SortTelemetry = None
_v_Transitions = None


_v_LEDdriver = None
_v_LEDStrip  = None
  
_v_LastTelemetry = None
_v_LastMicros = None
_v_Counters = -1
_v_LapsedAcum = 0
_v_SentCounters = 0
_v_SendingAcum = 0

_v_LEDsSegments          = [ ]
_v_LEDsSegmentsByName    = { }
_v_LastTelemetriesByName = { }



def configureLEDshow( 
    theNumLEDs, 
    theLEDsCommSpeed, 
    theLEDsMasterBrigtness, 
    theTelemetryToBrigtnessFactor,
    theSortTelemetry,
    theTransitions):
    
    global _v_ConfigurationChecked
    global _v_NumLEDs
    global _v_LEDsCommSpeed
    global _v_LEDsMasterBrigtness
    global _v_TelemetryToBrigtnessFactor
    global _v_SortTelemetry
    global _v_Transitions 
        
    _v_ConfigurationChecked = False
    
    _v_NumLEDs = theNumLEDs
    _v_LEDsCommSpeed = theLEDsCommSpeed
    _v_LEDsMasterBrigtness = theLEDsMasterBrigtness
    _v_TelemetryToBrigtnessFactor = theTelemetryToBrigtnessFactor
    _v_SortTelemetry = theSortTelemetry 
    _v_Transitions = theTransitions 
    
    print "configureLEDshow before check"
    printConfigurationLEDshow()
    print
    checkConfigurationLEDshow()
    
    
    

def checkConfigurationLEDshow():
    global _v_ConfigurationChecked
    global _v_NumLEDs
    global _v_LEDsCommSpeed
    global _v_LEDsMasterBrigtness
    global _v_TelemetryToBrigtnessFactor
    global _v_SortTelemetry
    global _v_Transitions 
    
    if _v_ConfigurationChecked:        
        return
        
    if _v_NumLEDs < NUMLEDS_MIN or _v_NumLEDs > NUMLEDS_MAX:
        _v_NumLEDs = NUMLEDS_DEFAULT
        
    if _v_LEDsCommSpeed < LEDSCOMMSPEED_MIN or _v_LEDsCommSpeed > LEDSCOMMSPEED_MAX:
        _v_LEDsCommSpeed = LEDSCOMMSPEED_DEFAULT    
                    
    if _v_LEDsMasterBrigtness < LEDSMASTERBRIGHTNESS_MIN or _v_LEDsMasterBrigtness > LEDSMASTERBRIGHTNESS_MAX:
        _v_LEDsMasterBrigtness = LEDSMASTERBRIGHTNESS_DEFAULT    

    if _v_TelemetryToBrigtnessFactor < TELEMETRYTOBRIGHTNESSFACTOR_MIN or _v_TelemetryToBrigtnessFactor > TELEMETRYTOBRIGHTNESSFACTOR_MAX:
        _v_TelemetryToBrigtnessFactor = TELEMETRYTOBRIGHTNESSFACTOR_DEFAULT    
                                 
    if not ( _v_SortTelemetry == True) and not ( _v_SortTelemetry == False):        
        _v_SortTelemetry = SORTTELEMETRY_DEFAULT
                             
    if not ( _v_Transitions == True) and not ( _v_Transitions == False):        
        _v_Transitions = TRANSITIONS_DEFAULT

    _v_ConfigurationChecked = True
    
    printConfigurationLEDshow()
            
    

def printConfigurationLEDshow():    
    global _v_ConfigurationChecked
    global _v_NumLEDs
    global _v_LEDsCommSpeed
    global _v_LEDsMasterBrigtness
    global _v_TelemetryToBrigtnessFactor
    global _v_SortTelemetry
    global _v_Transitions 
 
    print "_v_NumLEDs=", _v_NumLEDs
    print "_v_LEDsCommSpeed=", _v_LEDsCommSpeed
    print "_v_LEDsMasterBrigtness=", _v_LEDsMasterBrigtness
    print "_v_TelemetryToBrigtnessFactor=", _v_TelemetryToBrigtnessFactor
    print "_v_SortTelemetry=", _v_SortTelemetry
    print "_v_Transitions=", _v_Transitions
    print "_v_ConfigurationChecked=", _v_ConfigurationChecked




def addLEDsSegment( theLEDsSegmentName, theNumLEDs): 
    if not theLEDsSegmentName:
        return None
    
    if not theNumLEDs:
        return None        
    
    print "addLEDsSegment() theLEDsSegmentName=", theLEDsSegmentName, ", theNumLEDs=", theNumLEDs
    
    global _v_NumLEDs
    global _v_LEDsSegments
    global _v_LEDsSegmentsByName
    
    checkConfigurationLEDshow();
    
    printLEDsSegments()
    
    aNumLEDsForSegment = theNumLEDs
    if aNumLEDsForSegment < NUMLEDS_MIN or aNumLEDsForSegment > NUMLEDS_MAX:
        aNumLEDsForSegment = NUMLEDS_DEFAULT    
    
    if not _v_LEDsSegments:
        if aNumLEDsForSegment > _v_NumLEDs:
            return None
        aLEDsLoneSegment = { 
            "segmentName": theLEDsSegmentName, 
            "numLEDs": aNumLEDsForSegment, 
            "firstLEDindex": 0, 
            "lastLEDindex": aNumLEDsForSegment - 1
        }
        _v_LEDsSegments.append( aLEDsLoneSegment)
        _v_LEDsSegmentsByName[ theLEDsSegmentName] = aLEDsLoneSegment
        
        printLEDsSegments()
        
        return aLEDsLoneSegment
    

    aSumLEDsSegmentsNumLeds = sum( [ otherLEDsSegment[ "numLEDs"] for otherLEDsSegment in _v_LEDsSegments])
    aRemainingNumLEDs = _v_NumLEDs - aSumLEDsSegmentsNumLeds
    aNumLEDsToReserve = min( aNumLEDsForSegment, aRemainingNumLEDs)
    if aNumLEDsToReserve < 1:
        return False
    
    aLEDsAfterSegment = { 
        "segmentName": theLEDsSegmentName, 
        "numLEDs": aNumLEDsToReserve, 
        "firstLEDindex": aSumLEDsSegmentsNumLeds, 
        "lastLEDindex": aSumLEDsSegmentsNumLeds + aNumLEDsToReserve - 1
    }
    _v_LEDsSegments.append( aLEDsAfterSegment);
    _v_LEDsSegmentsByName[ theLEDsSegmentName] = aLEDsAfterSegment
    
    printLEDsSegments()
    
    return aLEDsAfterSegment    
        



def printLEDsSegments():   
    global _v_LEDsSegments

    print
    for aLEDsSegment in _v_LEDsSegments:        
        print "segmentName ", aLEDsSegment[ "segmentName"]
        print "numLEDs ", aLEDsSegment[ "numLEDs"]
        print "firstLEDindex ", aLEDsSegment[ "firstLEDindex"]
        print "lastLEDindex ", aLEDsSegment[ "lastLEDindex"]
        print
        
        

                    
def telemetryToLEDshowSegment( theSegmentName, theTelemetry):
    
    global _v_LEDsSegmentsByName
    global _v_LastTelemetriesByName
    
    if not theSegmentName:
        return        
    
    if not theTelemetry:
        return        
    
    print "telemetryToLEDshowSegment() theSegmentName=", theSegmentName, ", theTelemetry=", theTelemetry
    
    aSegmentName = theSegmentName
    aLEDsSegment = _v_LEDsSegmentsByName.get( aSegmentName)
    if not aLEDsSegment:
        aSegmentName = LEDSEGMENT_ALLOTHERS
        aLEDsSegment = _v_LEDsSegmentsByName.get( aSegmentName)
        if not aLEDsSegment:        
            return
    
    aNumLEDs = aLEDsSegment[ "numLEDs"]
    aFirstLEDindex = aLEDsSegment[ "firstLEDindex"]
    aLastLEDindex = aLEDsSegment[ "lastLEDindex"]
    
    aLastTelemetry = _v_LastTelemetriesByName.get( aSegmentName)
    telemetryToLEDshowWithin( theTelemetry, aSegmentName, aNumLEDs, aFirstLEDindex, aLastLEDindex, aLastTelemetry)
    _v_LastTelemetriesByName[ aSegmentName] = theTelemetry
               
               
                    
      
def telemetryToLEDshowWithin( theTelemetry, theSegmentName, theNumLEDs, theFirstLEDindex, theLastLEDindex, theLastTelemetry):                    
    print "telemetryToLEDshowSegment() theSegmentName=", theSegmentName, ", theNumLEDs=", theNumLEDs, ", theFirstLEDindex=", theFirstLEDindex, ", theLastLEDindex=", theLastLEDindex, ", theTelemetry=", theTelemetry, ", theLastTelemetry=", theLastTelemetry

    global _v_LEDStrip
    global _v_LEDdriver
    global _v_LastMicros
    global _v_Counters
    global _v_LapsedAcum
    global _v_SentCounters
    global _v_LastTelemetry
    
    if not theTelemetry:
        return
    
    checkConfigurationLEDshow()

    aNumCPUs = len( theTelemetry)
    if not aNumCPUs:
        return

    getOrCreateLEDStrip()
    if not _v_LEDStrip:
        return
    
    aTelemetry = theTelemetry
    if _v_SortTelemetry:
        aTelemetry = sorted( aTelemetry, reverse=True)
        
    if not _v_Transitions:
        telemetryToLEDshowWithin_Inner( aTelemetry, None)
        return
        
    _v_Counters += 1
    #print "_v_Counters=", _v_Counters

    aNowMicros = time.time()  
    if not _v_LastMicros:
        _v_LastMicros = aNowMicros
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshowWithin_Inner( aTelemetry, None, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        return
    
    aLapsed = aNowMicros - _v_LastMicros
    _v_LapsedAcum += aLapsed
    anInteval = _v_LapsedAcum / _v_Counters
    if LOGEACH:
        print "anInteval=", anInteval
    #print "anInteval=", anInteval
    _v_LastMicros = aNowMicros

    if not _v_LastTelemetry:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshowWithin_Inner( aTelemetry, aNowMicros, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        return    
 
    if not _v_SendingAcum:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshowWithin_Inner( aTelemetry, aNowMicros, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        return   
 
    if not ( aNumCPUs == len( _v_LastTelemetry)):
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshowWithin_Inner( aTelemetry, aNowMicros, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        return           
 
    aTimeToSend = _v_SendingAcum / _v_SentCounters
    #print "aTimeToSend=", aTimeToSend
    
    if anInteval > 1.0:
        anInteval = 1.0
        
    aNumSteps = int( anInteval / aTimeToSend)
    
    if aNumSteps < 2:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshowWithin_Inner( aTelemetry, aNowMicros, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        return
    
    if aNumSteps > 30:
        aNumSteps = 30
    #print "aNumSteps=",aNumSteps
    
    aNumStepsFloat = 1.0 * aNumSteps
    

    
    # print "from"
    # print _v_LastTelemetry
    # print "to"
    # print theTelemetry

    
    aToWait = (anInteval - ( aTimeToSend * aNumStepsFloat))  / aNumStepsFloat
    if aToWait < 0.001:
        aToWait = 0
    
    # print "aToWait=", aToWait
    
    for aStepIdx in range( aNumSteps):
        
        aFraction = ( 1.0 * aStepIdx + 1.0) / aNumStepsFloat
        
        aStepTelemetry = _v_LastTelemetry[:]
        
        for aCPUidx in range( aNumCPUs):
        
            aCPUtelemetry = aTelemetry[ aCPUidx]
            if aCPUtelemetry == None:
                continue
            
            if aCPUtelemetry > 100:
                aCPUtelemetry = 100
                
            aLastCPUtelemetry = _v_LastTelemetry[ aCPUidx]
            aDiff = int( aFraction * ( aCPUtelemetry - aLastCPUtelemetry))
            aStepTelemetry[ aCPUidx] += aDiff
 
        telemetryToLEDshowWithin_Inner( aStepTelemetry, None, theNumLEDs, theFirstLEDindex, theLastLEDindex)
        if aToWait:            
            sleep( aToWait)
        
    _v_LastTelemetry = aTelemetry

                       
                       

        
def telemetryToLEDshowWithin_Inner( theTelemetry, theBeginMicros, theNumLEDs, theFirstLEDindex, theLastLEDindex):   
    global _v_LEDStrip
    global _v_SentCounters
    global _v_SendingAcum
    
    if not theTelemetry:
        return
    
    if not _v_LEDStrip:
        return
    
    aNumCPUs = len( theTelemetry)
    if not aNumCPUs:
        return
    
    aLEDsPerCPU = theNumLEDs / aNumCPUs
    if not aLEDsPerCPU:
        return     
    
    # print theTelemetry
    
    _v_SentCounters += 1
    # print "_v_SentCounters=", _v_SentCounters
    
    aBeginMicros = theBeginMicros
    if _v_Transitions:
        if not aBeginMicros:
            aBeginMicros = time.time() 
    
    for aCPUidx in range(aNumCPUs):
        
        aCPUtelemetry = theTelemetry[ aCPUidx]
        if aCPUtelemetry == None:
            continue
        if aCPUtelemetry > 100:
            aCPUtelemetry = 100
            
        aCPUvalue = int( round( aCPUtelemetry * MAXLEDVALUE_ACUM / 100, 0))
        
        aRed   = 0
        aGreen = 0
        aBlue  = 0
        
        if aCPUvalue <= MAXLEDVALUE_RED:
            aRed = aCPUvalue
        else:
            aRed = MAXLEDVALUE
            
            if aCPUvalue <= MAXLEDVALUE_GREEN:
                aGreen = aCPUvalue - EIGHTBITSRANGE
            else:
                aGreen = MAXLEDVALUE           
                
                if aCPUvalue <= MAXLEDVALUE_BLUE:
                    aBlue = aCPUvalue - EIGHTBITSRANGE - EIGHTBITSRANGE
                else:
                    aBlue = MAXLEDVALUE                    
        
        if _v_TelemetryToBrigtnessFactor == 1.0:
            aColor = ( aRed, aGreen, aBlue,)
        else:
            aColor = ( int( round( aRed * _v_TelemetryToBrigtnessFactor,0)), int( round( aGreen * _v_TelemetryToBrigtnessFactor, 0)), int( round( aBlue * _v_TelemetryToBrigtnessFactor, 0)),)
                    
        aFrom = theFirstLEDindex + aCPUidx * aLEDsPerCPU
        aTo =   min( theLastLEDindex, ( aCPUidx + 1) * aLEDsPerCPU - 1)
        _v_LEDStrip.fill( aColor, aFrom, aTo )

    _v_LEDStrip.update()

    if _v_Transitions:
        anEndMicros = time.time() 
        aLapsedSeding = anEndMicros - aBeginMicros
        _v_SendingAcum += aLapsedSeding
                            
         
         
                    
def telemetryToLEDshow( theTelemetry):   
    global _v_LEDStrip
    global _v_LEDdriver
    global _v_LastMicros
    global _v_Counters
    global _v_LapsedAcum
    global _v_SentCounters
    global _v_LastTelemetry
    
    if not theTelemetry:
        return
    
    checkConfigurationLEDshow()

    aNumCPUs = len( theTelemetry)
    if not aNumCPUs:
        return

    getOrCreateLEDStrip()
    if not _v_LEDStrip:
        return
    
    aTelemetry = theTelemetry
    if _v_SortTelemetry:
        aTelemetry = sorted( aTelemetry, reverse=True)
        
    if not _v_Transitions:
        telemetryToLEDshow_Inner( aTelemetry, None)
        return
        
    _v_Counters += 1
    #print "_v_Counters=", _v_Counters

    aNowMicros = time.time()  
    if not _v_LastMicros:
        _v_LastMicros = aNowMicros
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshow_Inner( aTelemetry, None)
        return
    
    aLapsed = aNowMicros - _v_LastMicros
    _v_LapsedAcum += aLapsed
    anInteval = _v_LapsedAcum / _v_Counters
    if LOGEACH:
        print "anInteval=", anInteval
    #print "anInteval=", anInteval
    _v_LastMicros = aNowMicros

    if not _v_LastTelemetry:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshow_Inner( aTelemetry, aNowMicros)
        return    
 
    if not _v_SendingAcum:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshow_Inner( aTelemetry, aNowMicros)
        return   
 
    if not ( aNumCPUs == len( _v_LastTelemetry)):
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshow_Inner( aTelemetry, aNowMicros)
        return           
 
    aTimeToSend = _v_SendingAcum / _v_SentCounters
    #print "aTimeToSend=", aTimeToSend
    
    if anInteval > 1.0:
        anInteval = 1.0
        
    aNumSteps = int( anInteval / aTimeToSend)
    
    if aNumSteps < 2:
        _v_LastTelemetry = aTelemetry
        telemetryToLEDshow_Inner( aTelemetry, aNowMicros)
        return
    
    if aNumSteps > 30:
        aNumSteps = 30
    #print "aNumSteps=",aNumSteps
    
    aNumStepsFloat = 1.0 * aNumSteps
    

    
    # print "from"
    # print _v_LastTelemetry
    # print "to"
    # print theTelemetry

    
    aToWait = (anInteval - ( aTimeToSend * aNumStepsFloat))  / aNumStepsFloat
    if aToWait < 0.001:
        aToWait = 0
    
    # print "aToWait=", aToWait
    
    for aStepIdx in range( aNumSteps):
        
        aFraction = ( 1.0 * aStepIdx + 1.0) / aNumStepsFloat
        
        aStepTelemetry = _v_LastTelemetry[:]
        
        for aCPUidx in range( aNumCPUs):
        
            aCPUtelemetry = aTelemetry[ aCPUidx]
            if aCPUtelemetry == None:
                continue
            
            if aCPUtelemetry > 100:
                aCPUtelemetry = 100
                
            aLastCPUtelemetry = _v_LastTelemetry[ aCPUidx]
            aDiff = int( aFraction * ( aCPUtelemetry - aLastCPUtelemetry))
            aStepTelemetry[ aCPUidx] += aDiff
 
        telemetryToLEDshow_Inner( aStepTelemetry, None)
        if aToWait:            
            sleep( aToWait)
        
    _v_LastTelemetry = aTelemetry





        
def telemetryToLEDshow_Inner( theTelemetry, theBeginMicros):
   
    global _v_LEDStrip
    global _v_SentCounters
    global _v_SendingAcum
    
    if not theTelemetry:
        return
    
    if not _v_LEDStrip:
        return
    
    aNumCPUs = len( theTelemetry)
    if not aNumCPUs:
        return
    
    aLEDsPerCPU = _v_NumLEDs / aNumCPUs
    if not aLEDsPerCPU:
        return     
    
    # print theTelemetry
    
    _v_SentCounters += 1
    # print "_v_SentCounters=", _v_SentCounters
    
    aBeginMicros = theBeginMicros
    if _v_Transitions:
        if not aBeginMicros:
            aBeginMicros = time.time() 
    
    for aCPUidx in range(aNumCPUs):
        
        aCPUtelemetry = theTelemetry[ aCPUidx]
        if aCPUtelemetry == None:
            continue
        if aCPUtelemetry > 100:
            aCPUtelemetry = 100
            
        aCPUvalue = int( round( aCPUtelemetry * MAXLEDVALUE_ACUM / 100, 0))
        
        aRed   = 0
        aGreen = 0
        aBlue  = 0
        
        if aCPUvalue <= MAXLEDVALUE_RED:
            aRed = aCPUvalue
        else:
            aRed = MAXLEDVALUE
            
            if aCPUvalue <= MAXLEDVALUE_GREEN:
                aGreen = aCPUvalue - EIGHTBITSRANGE
            else:
                aGreen = MAXLEDVALUE           
                
                if aCPUvalue <= MAXLEDVALUE_BLUE:
                    aBlue = aCPUvalue - EIGHTBITSRANGE - EIGHTBITSRANGE
                else:
                    aBlue = MAXLEDVALUE                    
        
        if _v_TelemetryToBrigtnessFactor == 1.0:
            aColor = ( aRed, aGreen, aBlue,)
        else:
            aColor = ( int( round( aRed * _v_TelemetryToBrigtnessFactor,0)), int( round( aGreen * _v_TelemetryToBrigtnessFactor, 0)), int( round( aBlue * _v_TelemetryToBrigtnessFactor, 0)),)
                    
        aFrom = aCPUidx * aLEDsPerCPU
        aTo =   ( aCPUidx + 1) * aLEDsPerCPU - 1
        _v_LEDStrip.fill( aColor, aFrom, aTo )

    _v_LEDStrip.update()

    if _v_Transitions:
        anEndMicros = time.time() 
        aLapsedSeding = anEndMicros - aBeginMicros
        _v_SendingAcum += aLapsedSeding
                
   
def getOrCreateLEDStrip( ):
   
    global _v_LEDStrip
    global _v_LEDdriver
    
    if _v_LEDStrip:
        return _v_LEDStrip
    
    getOrCreateLEDdriver()
    if not _v_LEDdriver:
        return None
   
    _v_LEDStrip = LEDStrip(_v_LEDdriver, masterBrightness=_v_LEDsMasterBrigtness, threadedUpdate = False)
                
    return _v_LEDStrip

          

      
def getOrCreateLEDdriver( ):
   
    global _v_LEDStrip
    global _v_LEDdriver
    
    if _v_LEDdriver:
        return _v_LEDdriver
   
    _v_LEDdriver = DriverSerial( num = _v_NumLEDs, type = LEDTYPE.APA102, SPISpeed=_v_LEDsCommSpeed, c_order = ChannelOrder.BGR, gamma=None)
            
    return _v_LEDdriver      
           
           
def doSomeLights():    
    #telemetryToLEDshow([0])
    #sleep( 0.3)
    #telemetryToLEDshow([100])
    #sleep( 0.3)
    #telemetryToLEDshow([0])
    #sleep( 0.3)
    #telemetryToLEDshow([100])
    #sleep( 0.3)
    #telemetryToLEDshow([0])
    #sleep( 0.3)
                    
    for anInteration in range( 0,200):    
        telemetryToLEDshow([10,20,30,40,50,60,70,80,90,100])
        sleep( 1)
        telemetryToLEDshow([100,90,80,70,60,50,40,30,20,10])
        sleep( 1)
        # telemetryToLEDshow([100,30])
    
# doSomeLights()

def addSomeLEDsSegments():
    
    addLEDsSegment( "S1", 1)
    addLEDsSegment( "S2", 2)
    addLEDsSegment( "S3", 144)
        
# addSomeLEDsSegments()