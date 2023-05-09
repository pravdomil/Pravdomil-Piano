#include <Arduino.h>
#include <BLEMidi.h>

#define PLAY(a, b)                                                             \
  if ((status[i] & a) != (temp[i] & a)) {                                      \
    if (temp[i] & a)                                                           \
      BLEMidiServer.noteOn(0, 64 - b, 127);                                    \
    else                                                                       \
      BLEMidiServer.noteOff(0, 64 - b, 127);                                   \
  }

#define clockPin 2
#define latchPin 4
#define dataPin 5
#define powerPin 15
#define chips 8

uint8_t status[chips];
uint8_t temp[chips];

void setup() {
  pinMode(clockPin, OUTPUT);
  pinMode(latchPin, OUTPUT);
  pinMode(powerPin, OUTPUT);

  digitalWrite(powerPin, 1);

  BLEMidiServer.begin("Pravdomil Piano");
}

void loop() {
  if (BLEMidiServer.isConnected()) {
    digitalWrite(latchPin, 0);
    digitalWrite(latchPin, 1);

    for (uint8_t i = 0; i < chips; i++)
      temp[i] = shiftIn();

    for (uint8_t i = 0; i < chips; i++) {
      PLAY(8, 0 + ((chips - 1 - i) * 8));
      PLAY(1, 1 + ((chips - 1 - i) * 8));
      PLAY(4, 2 + ((chips - 1 - i) * 8));
      PLAY(2, 3 + ((chips - 1 - i) * 8));
      PLAY(64, 4 + ((chips - 1 - i) * 8));
      PLAY(32, 5 + ((chips - 1 - i) * 8));
      PLAY(128, 6 + ((chips - 1 - i) * 8));
      PLAY(16, 7 + ((chips - 1 - i) * 8));
    }

    for (uint8_t i = 0; i < chips; i++)
      status[i] = temp[i];
  }

  delay(10);
}

uint8_t shiftIn() {
  uint8_t value = 0;

  for (uint8_t i = 0; i < 8; i++) {
    value |= digitalRead(dataPin) << i;

    digitalWrite(clockPin, LOW);
    digitalWrite(clockPin, HIGH);
  }

  return value;
}
