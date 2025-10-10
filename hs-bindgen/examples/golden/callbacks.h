// Non-typedef callbacks (direct function pointers)
int readFileWithProcessor(void (*processLine)(int lineNumber), int fileId);
void watchTemperature(void (*onTempChange)(int degrees), int sensorId);

// Simple callback types (using typedef)
typedef void (*FileOpenedNotification)(void);
typedef void (*ProgressUpdate)(int percentComplete);
typedef int (*DataValidator)(int value);

void onFileOpened(FileOpenedNotification notify);
void onProgressChanged(ProgressUpdate update);
int validateInput(DataValidator validator, int rawValue);

// Callbacks with struct
struct Measurement {
  double value, timestamp;
};

typedef void (*MeasurementReceived)(struct Measurement *data);
void onNewMeasurement(MeasurementReceived handler);

typedef void (*MeasurementReceived2)(struct Measurement data);
void onNewMeasurement2(MeasurementReceived2 handler);

typedef void (*SampleBufferFull)(int samples[10]);
void onBufferReady(SampleBufferFull handler);

// Function pointers that take other function pointers as arguments
void transformMeasurement(struct Measurement *data, void (*transformer)(struct Measurement *m, double (*scale)(double, int), int factor));

// Multiple nested callbacks with custom types
void processWithCallbacks(void (*handler)(struct Measurement *m, FileOpenedNotification notify, int priority));
