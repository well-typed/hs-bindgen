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

// Callbacks with fixed-length arrays not supported as well as passing structs
// by value
//
// typedef void (*SampleBufferFull)(int samples[10]);
// void onBufferReady(SampleBufferFull handler);

