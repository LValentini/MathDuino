BOARD_TAG    = uno
ARDUINO_PORT = /dev/ttyACM0
ARDUINO_LIBS = WiFlyHQ SoftwareSerial PString

debug: nodebug
	touch mathduinosensors.ino
	sed -i '1s/^/#define DEBUG\n/' mathduinosensors.ino

nodebug:
	touch mathduinosensors.ino
	sed -i '/#define DEBUG/d' mathduinosensors.ino 


include $(ARDMAKE_DIR)/Arduino.mk

