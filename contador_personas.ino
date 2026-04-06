// ============================================================
//  Contador de Personas — Proyecto Integrador
//  Juan Carlos Perez Hernandez
//  UABC — Análisis de Tecnologías Emergentes
//
//  Sensores: 2x HC-SR04
//  Salida:   Serial (CSV) → RStudio
//  LEDs:     Verde (entrada) | Rojo (salida)
// ============================================================

// --- Pines Sensor A (izquierdo) ---
#define TRIG_A 2
#define ECHO_A 3

// --- Pines Sensor B (derecho) ---
#define TRIG_B 4
#define ECHO_B 5

// --- Pines LEDs ---
#define LED_VERDE 8
#define LED_ROJO  9

// --- Configuración ---
#define UMBRAL_CM       40    // distancia en cm para detectar presencia
#define TIEMPO_ESPERA   1500  // ms entre lecturas (evita doble conteo)

// Variables de estado
bool sensor_a_activado = false;
bool sensor_b_activado = false;
unsigned long tiempo_a  = 0;
unsigned long tiempo_b  = 0;
int id_lectura          = 0;

// ============================================================
void setup() {
  Serial.begin(9600);

  pinMode(TRIG_A, OUTPUT);
  pinMode(ECHO_A, INPUT);
  pinMode(TRIG_B, OUTPUT);
  pinMode(ECHO_B, INPUT);
  pinMode(LED_VERDE, OUTPUT);
  pinMode(LED_ROJO,  OUTPUT);

  // Encender ambos LEDs 1 segundo al arrancar (prueba visual)
  digitalWrite(LED_VERDE, HIGH);
  digitalWrite(LED_ROJO,  HIGH);
  delay(1000);
  digitalWrite(LED_VERDE, LOW);
  digitalWrite(LED_ROJO,  LOW);

  // Encabezado CSV para RStudio
  Serial.println("id,dist_a,dist_b,tiempo_a,tiempo_b,delta_ms,primero,etiqueta");
}

// ============================================================
// Mide distancia en cm con un HC-SR04
float medirDistancia(int trig, int echo) {
  digitalWrite(trig, LOW);
  delayMicroseconds(2);
  digitalWrite(trig, HIGH);
  delayMicroseconds(10);
  digitalWrite(trig, LOW);

  long duracion = pulseIn(echo, HIGH, 30000); // timeout 30ms
  if (duracion == 0) return 999; // sin eco = sin objeto
  return duracion * 0.0343 / 2.0;
}

// ============================================================
void loop() {
  float dist_a = medirDistancia(TRIG_A, ECHO_A);
  float dist_b = medirDistancia(TRIG_B, ECHO_B);
  unsigned long ahora = millis();

  // Detectar activación de Sensor A
  if (dist_a < UMBRAL_CM && !sensor_a_activado) {
    sensor_a_activado = true;
    tiempo_a = ahora;
  }

  // Detectar activación de Sensor B
  if (dist_b < UMBRAL_CM && !sensor_b_activado) {
    sensor_b_activado = true;
    tiempo_b = ahora;
  }

  // Cuando ambos se activaron → determinar dirección
  if (sensor_a_activado && sensor_b_activado) {
    long delta = (long)tiempo_b - (long)tiempo_a;
    String primero   = (tiempo_a <= tiempo_b) ? "A" : "B";
    String etiqueta  = (tiempo_a <= tiempo_b) ? "salida" : "entrada";

    // LED visual
    if (etiqueta == "entrada") {
      digitalWrite(LED_VERDE, HIGH);
      digitalWrite(LED_ROJO,  LOW);
    } else {
      digitalWrite(LED_VERDE, LOW);
      digitalWrite(LED_ROJO,  HIGH);
    }

    // Enviar fila CSV al serial
    id_lectura++;
    Serial.print(id_lectura);   Serial.print(",");
    Serial.print(dist_a);       Serial.print(",");
    Serial.print(dist_b);       Serial.print(",");
    Serial.print(tiempo_a);     Serial.print(",");
    Serial.print(tiempo_b);     Serial.print(",");
    Serial.print(delta);        Serial.print(",");
    Serial.print(primero);      Serial.print(",");
    Serial.println(etiqueta);

    // Reset para la siguiente persona
    delay(TIEMPO_ESPERA);
    sensor_a_activado = false;
    sensor_b_activado = false;
    digitalWrite(LED_VERDE, LOW);
    digitalWrite(LED_ROJO,  LOW);
  }

  delay(50);
}
