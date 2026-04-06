// Proyecto Integrador - Contador de Personas
// Juan Carlos Perez Hernandez
// UABC - Analisis de Tecnologias Emergentes
// Sensores: 2x HC-SR04 | Salida: Serial CSV | LEDs: verde/rojo

// Pines sensor A (izquierdo)
#define TRIG_A 2
#define ECHO_A 3

// Pines sensor B (derecho)
#define TRIG_B 4
#define ECHO_B 5

// Pines LEDs
#define LED_VERDE 8
#define LED_ROJO  9

// Distancia maxima en cm para considerar que hay un objeto
#define UMBRAL_CM     40
// Tiempo de espera entre eventos para evitar doble conteo
#define TIEMPO_ESPERA 1500

bool sensor_a_activado = false;
bool sensor_b_activado = false;
unsigned long tiempo_a = 0;
unsigned long tiempo_b = 0;
int id_lectura         = 0;

void setup() {
  Serial.begin(9600);

  pinMode(TRIG_A, OUTPUT);
  pinMode(ECHO_A, INPUT);
  pinMode(TRIG_B, OUTPUT);
  pinMode(ECHO_B, INPUT);
  pinMode(LED_VERDE, OUTPUT);
  pinMode(LED_ROJO,  OUTPUT);

  // Parpadeo inicial para confirmar que el sistema arranco
  digitalWrite(LED_VERDE, HIGH);
  digitalWrite(LED_ROJO,  HIGH);
  delay(1000);
  digitalWrite(LED_VERDE, LOW);
  digitalWrite(LED_ROJO,  LOW);

  // Encabezado del CSV que se enviara por serial
  Serial.println("id,dist_a,dist_b,tiempo_a,tiempo_b,delta_ms,primero,etiqueta");
}

// Envia un pulso al sensor y calcula la distancia en cm.
// pulseIn mide cuanto dura el pulso de retorno en el pin ECHO.
// Si no hay respuesta en 30ms devuelve 999 (sin objeto detectado).
float medirDistancia(int trig, int echo) {
  digitalWrite(trig, LOW);
  delayMicroseconds(2);
  digitalWrite(trig, HIGH);
  delayMicroseconds(10);
  digitalWrite(trig, LOW);

  long duracion = pulseIn(echo, HIGH, 30000);
  if (duracion == 0) return 999;
  return duracion * 0.0343 / 2.0;
}

void loop() {
  float dist_a = medirDistancia(TRIG_A, ECHO_A);
  float dist_b = medirDistancia(TRIG_B, ECHO_B);
  unsigned long ahora = millis();

  // Guardar el momento exacto en que se activa cada sensor
  if (dist_a < UMBRAL_CM && !sensor_a_activado) {
    sensor_a_activado = true;
    tiempo_a = ahora;
  }

  if (dist_b < UMBRAL_CM && !sensor_b_activado) {
    sensor_b_activado = true;
    tiempo_b = ahora;
  }

  // Cuando ambos sensores se activaron, determinar la direccion.
  // El sensor que se activo primero indica de que lado viene la persona.
  if (sensor_a_activado && sensor_b_activado) {
    long delta = (long)tiempo_b - (long)tiempo_a;
    String primero  = (tiempo_a <= tiempo_b) ? "A" : "B";
    String etiqueta = (tiempo_a <= tiempo_b) ? "salida" : "entrada";

    if (etiqueta == "entrada") {
      digitalWrite(LED_VERDE, HIGH);
      digitalWrite(LED_ROJO,  LOW);
    } else {
      digitalWrite(LED_VERDE, LOW);
      digitalWrite(LED_ROJO,  HIGH);
    }

    // Enviar los datos como fila CSV por el puerto serial
    id_lectura++;
    Serial.print(id_lectura); Serial.print(",");
    Serial.print(dist_a);     Serial.print(",");
    Serial.print(dist_b);     Serial.print(",");
    Serial.print(tiempo_a);   Serial.print(",");
    Serial.print(tiempo_b);   Serial.print(",");
    Serial.print(delta);      Serial.print(",");
    Serial.print(primero);    Serial.print(",");
    Serial.println(etiqueta);

    // Resetear estado y apagar LEDs antes del siguiente evento
    delay(TIEMPO_ESPERA);
    sensor_a_activado = false;
    sensor_b_activado = false;
    digitalWrite(LED_VERDE, LOW);
    digitalWrite(LED_ROJO,  LOW);
  }

  delay(50);
}
