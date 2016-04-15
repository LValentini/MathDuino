#include <Arduino.h>
#define DEBUG
/*
 * WiFlyHQ Example httpserver.ino
 *
 * This sketch implements a simple Web server that waits for requests
 * and serves up a small form asking for a username, then when the
 * client posts that form the server sends a greeting page with the
 * user's name and an analog reading.
 *
 * This sketch is released to the public domain.
 *
 */

 /* Notes:
  * Uses chunked message bodies to work around a problem where
  * the WiFly will not handle the close() of a client initiated
  * TCP connection. It fails to send the FIN to the client.
  * (WiFly RN-XV Firmware version 2.32).
  */

/* Work around a bug with PROGMEM and PSTR where the compiler always
 * generates warnings.
 */
#undef PROGMEM 
#define PROGMEM __attribute__(( section(".progmem.data") )) 
#undef PSTR 
#define PSTR(s) (__extension__({static prog_char __c[] PROGMEM = (s); &__c[0];})) 

#include <WiFlyHQ.h>

#include <SoftwareSerial.h>

#include <math.h>

#define ThermistorPIN 0                 // Analog Pin 0
#define PhotoresistorPIN 1

//const char mySSID[] = "Kobol";
//const char myPassword[] = "F1b0,N4Cc1";

//const char mySSID[] = "wicesarebackup";
//const char myPassword[] = "asdlkjasdlkj1";

const char mySSID[] = "Gemenon";
const char myPassword[] = "arduino336";

//const char mySSID[] = "wicesare";
//const char myPassword[] = "asdlkjasdlkj1";

SoftwareSerial wifiSerial(2,3);
WiFly wifly;

char macaddr[20];

void sendPong();
void sendSensorsDataXML();
void send404();

char buf[150];
char wbuf[100];
char convbuf[10];
char scanbuf[400];


float vcc = 5.02;                       // only used for display purposes, if used
                                        // set to the measured Vcc.
float pad = 9310;                       // balance/pad resistor value, set this to
                                        // the measured resistance of your pad resistor
float thermr = 2000;                   // thermistor nominal resistance

float Photoresistor(int RawADC) {
   
   int adc_value = RawADC;

   float Vo = (5.0/1024) * ((double)adc_value);
   float illuminance = ((2500 / Vo) - 500 ) / 9.4;

   return illuminance;
}

float Thermistor(int RawADC) {
  long Resistance;  
  float Temp;  // Dual-Purpose variable to save space.

  float a = -0.0008150782317;
  float b = 0.0006688923709;
  float c = -0.000002039768121;
  
  Resistance=((1024 * pad / RawADC) - pad);
  Temp = log(Resistance); // Saving the Log(resistance) so not to calculate  it 4 times later
  
  Temp = 1 / (a + (b * Temp) + (c * Temp * Temp * Temp));
  
  Temp = Temp - 273.15;  // Convert Kelvin to Celsius                      

  return Temp;                                      // Return the Temperature
}

void setup()
{

    Serial.begin(115200);

#ifdef DEBUG
    Serial.println(F("Starting"));
    Serial.print(F("Free memory: "));
    Serial.println(wifly.getFreeMemory(),DEC);
#endif

    wifiSerial.begin(9600);
    if (!wifly.begin(&wifiSerial, &Serial)) {
#ifdef DEBUG
        Serial.println(F("Failed to start wifly"));
#endif
	wifly.terminal();
    }

	wifly.leave();

	delay(1000);

	wifly.scan(scanbuf, sizeof(scanbuf));


#ifdef DEBUG
	Serial.println(scanbuf);
#endif
	
	snprintf_P(scanbuf, sizeof(scanbuf), PSTR("%s b 13 -71 c 00:1c:58:10:1c:90 | a b 10 -93 c 00:12:43:8a:ea:22 | a b 11 -92 c d8:30:62:5f:f2:b1"), "a");

    /* Join wifi network if not already associated */
    if (!wifly.isAssociated()) {
	/* Setup the WiFly to connect to a wifi network */
#ifdef DEBUG
		Serial.println(F("Joining network"));
#endif
		wifly.setSSID(mySSID);
		wifly.setPassphrase(myPassword);
		wifly.enableDHCP();
		wifly.setDHCP(1);
		wifly.save();

		if (wifly.join()) {
#ifdef DEBUG
		    Serial.println(F("Joined wifi network"));
#endif
		} else {
#ifdef DEBUG
		    Serial.println(F("Failed to join wifi network"));
#endif
		    wifly.terminal();
		}
    } else {
#ifdef DEBUG
        Serial.println(F("Already joined network"));
#endif
    }

    wifly.setBroadcastInterval(0);	// Turn off UPD broadcast

#ifdef DEBUG
    Serial.print(F("MAC: "));
    Serial.println(wifly.getMAC(macaddr, sizeof(macaddr)));
    Serial.print(F("IP: "));
    Serial.println(wifly.getIP(buf, sizeof(buf)));
#endif

    wifly.setDeviceID("Wifly-WebServer");

    if (wifly.isConnected()) {
#ifdef DEBUG
        Serial.println(F("Old connection active. Closing"));
#endif
	    wifly.close();
    }

    wifly.setProtocol(WIFLY_PROTOCOL_TCP | WIFLY_PROTOCOL_UDP);
    if (wifly.getPort() != 80) {
        wifly.setPort(80);
	    wifly.save();
#ifdef DEBUG
	    Serial.println(F("Set port to 80, rebooting to make it work"));
#endif
	    wifly.reboot();
	    delay(3000);
    }
    Serial.println(F("Ready"));

}

void loop()
{

    if (wifly.available() > 0) {

    	/* See if there is a request */
		if (wifly.gets(buf, sizeof(buf))) {
		    if (strncmp_P(buf, PSTR("GET /ping"), 9) == 0) {

				/* GET request */
#ifdef DEBUG
				Serial.println(F("PONG XML requested"));
#endif
				while (wifly.gets(buf, sizeof(buf)) > 0) {
				    //Skip rest of request
				}

				sendPong();

	   	 	} else if (strncmp_P(buf, PSTR("GET /data"), 9) == 0) {

	        	/* POST request */
#ifdef DEBUG
	        	Serial.println(F("DATACOLLECTOR XML: sendind sensors data"));
#endif

				while (wifly.gets(buf, sizeof(buf)) > 0) {
				    //Skip rest of request
				}

                // discard rest of input
		    	// wifly.flushRx();		
				sendSensorsDataXML();

	    	} else {

	       		// Unexpected request
#ifdef DEBUG
				Serial.print(F("Unexpected: "));
				Serial.println(buf);
				Serial.println(F("Sending 404"));
#endif
				while (wifly.gets(buf, sizeof(buf)) > 0) {
				    //Skip rest of request
				}

                // discard rest of input
				wifly.flushRx();		
				send404();

	    	}
		}
    }
}

/** Send an index HTML page with an input box for a username */
void sendPong()
{
    /* Send the header direclty with print */
    wifly.println(F("HTTP/1.1 200 OK"));
    wifly.println(F("Content-Type: text/xml"));
    wifly.println(F("Transfer-Encoding: chunked"));
    wifly.println();

    /* Send the body using the chunked protocol so the client knows when
     * the message is finished.
     * Note: we're not simply doing a close() because in version 2.32
     * firmware the close() does not work for client TCP streams.
     */
    wifly.sendChunkln(F("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
	snprintf_P(wbuf, sizeof(wbuf), PSTR("<pong>%s</pong>"), macaddr);
    wifly.sendChunkln(wbuf);

    wifly.sendChunkln();
}

void sendSensorsDataXML() {
    /* Send the header directly with print */
    wifly.println(F("HTTP/1.1 200 OK"));
    wifly.println(F("Content-Type: text/xml"));
    wifly.println(F("Transfer-Encoding: chunked"));
    wifly.println();

    /* Send the body using the chunked protocol so the client knows when
     * the message is finished.
     */
    wifly.sendChunkln(F("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
    wifly.sendChunkln(F("<datacollector>"));

	snprintf_P(wbuf, sizeof(wbuf), PSTR("<dcid>%s</dcid>"), macaddr);
    wifly.sendChunkln(wbuf);

    wifly.sendChunkln(F("<geoloc>"));
	int ind;

	char scanbufcpy[500];
	strncpy(scanbufcpy, scanbuf, 500);


	char *ap, *oap;
	char *apdesctok, *apdesc;
	ap = strtok_r(scanbufcpy, "|", &oap);
	while(ap != NULL) {

    	wifly.sendChunkln(F("<ap>"));
		
		ind = 0;
		apdesctok = strtok_r(ap, " ", &apdesc);	
		while(apdesctok != NULL) {
			ind++;
			switch(ind) {
				case 3:
					snprintf_P(wbuf, sizeof(wbuf), PSTR("<channel>%s</channel>"), apdesctok);
    				wifly.sendChunkln(wbuf);
					break;
				case 4:
					snprintf_P(wbuf, sizeof(wbuf), PSTR("<rssi>%s</rssi>"), apdesctok);
    				wifly.sendChunkln(wbuf);
					break;
				case 6:
					snprintf_P(wbuf, sizeof(wbuf), PSTR("<mac>%s</mac>"), apdesctok);
    				wifly.sendChunkln(wbuf);
					break;
				default:
					;
			}
			apdesctok = strtok_r(NULL, " ", &apdesc);	
		}

    	wifly.sendChunkln(F("</ap>"));
		
		//Serial.println();
		ap = strtok_r(NULL, "|", &oap);
	}
    wifly.sendChunkln(F("</geoloc>"));

    wifly.sendChunkln(F("<sensors>"));

    /* Include readings from Analog pins */
	dtostrf(Thermistor(analogRead(ThermistorPIN)), 5,2, convbuf);
    snprintf_P(wbuf, sizeof(wbuf), PSTR("<sensor><type>temp</type><value>%s</value></sensor>"), convbuf);
#ifdef DEBUG
    Serial.println(wbuf);
#endif
    wifly.sendChunkln(wbuf);

	dtostrf(Photoresistor(analogRead(PhotoresistorPIN)), 5,2, convbuf);
    snprintf_P(wbuf, sizeof(wbuf), PSTR("<sensor><type>light</type><value>%s</value></sensor>"), convbuf);
#ifdef DEBUG
    Serial.println(wbuf);
#endif
    wifly.sendChunkln(wbuf);
    
	sprintf(convbuf,"NA");
	snprintf_P(wbuf, sizeof(wbuf), PSTR("<sensor><type>humidity</type><value>%s</value></sensor>"), convbuf);
#ifdef DEBUG
    Serial.println(wbuf);
#endif
    wifly.sendChunkln(wbuf);
	
	sprintf(convbuf,"NA");
	snprintf_P(wbuf, sizeof(wbuf), PSTR("<sensor><type>pressure</type><value>%s</value></sensor>"), convbuf);
#ifdef DEBUG
    Serial.println(wbuf);
#endif
    wifly.sendChunkln(wbuf);

    wifly.sendChunkln(F("</sensors>"));
    wifly.sendChunkln(F("</datacollector>"));
    wifly.sendChunkln();

}

/** Send a 404 error */
void send404()
{
    wifly.println(F("HTTP/1.1 404 Not Found"));
    wifly.println(F("Content-Type: text/html"));
    wifly.println(F("Transfer-Encoding: chunked"));
    wifly.println();
    wifly.sendChunkln(F("<html><head>"));
    wifly.sendChunkln(F("<title>404 Not Found</title>"));
    wifly.sendChunkln(F("</head><body>"));
    wifly.sendChunkln(F("<h1>Not Found</h1>"));
    wifly.sendChunkln(F("<hr>"));
    wifly.sendChunkln(F("</body></html>"));
    wifly.sendChunkln();
}
