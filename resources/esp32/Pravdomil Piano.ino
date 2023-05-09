#include <Arduino.h>
#include <BLEMidi.h>

#define PLAY(a, b)                                                             \
  if ((status[i] & a) != (value & a)) {                                        \
    if (value & a)                                                             \
      BLEMidiServer.noteOn(0, 64 - b, 127);                                    \
    else                                                                       \
      BLEMidiServer.noteOff(0, 64 - b, 127);                                   \
  }

auto clockPin = 2;
auto latchPin = 4;
auto dataPin = 5;
auto powerPin = 15;
auto chips = 8;

uint8_t status[chips];

void setup() {
  pinMode(clockPin, OUTPUT);
  pinMode(latchPin, OUTPUT);
  pinMode(powerPin, OUTPUT);

  digitalWrite(powerPin, 1);

  BLEMidiServer.begin("Pravdomil Piano");
}

void loop() {
  digitalWrite(latchPin, 0);
  digitalWrite(latchPin, 1);

  for (uint8_t i = 0; i < chips; i++) {
    auto value = shiftIn2(dataPin, clockPin);

    if (BLEMidiServer.isConnected()) {
      PLAY(8, 0 + ((chips - 1 - i) * 8));
      PLAY(1, 1 + ((chips - 1 - i) * 8));
      PLAY(4, 2 + ((chips - 1 - i) * 8));
      PLAY(2, 3 + ((chips - 1 - i) * 8));
      PLAY(64, 4 + ((chips - 1 - i) * 8));
      PLAY(32, 5 + ((chips - 1 - i) * 8));
      PLAY(128, 6 + ((chips - 1 - i) * 8));
      PLAY(16, 7 + ((chips - 1 - i) * 8));
    }

    status[i] = value;
  }

  delay(10);
}

uint8_t shiftIn2(uint8_t dataPin, uint8_t clockPin) {
  uint8_t value = 0;

  for (uint8_t i = 0; i < 8; i++) {
    value |= digitalRead(dataPin) << i;

    digitalWrite(clockPin, LOW);
    digitalWrite(clockPin, HIGH);
  }

  return value;
}
