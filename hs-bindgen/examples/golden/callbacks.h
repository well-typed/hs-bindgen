// ----------------------------------------------------------------------------
// Basic Callbacks - Direct function pointers (no typedef)
// ----------------------------------------------------------------------------
int readFileWithProcessor(void (*processLine)(int lineNumber), int fileId);
void watchTemperature(void (*onTempChange)(int degrees), int sensorId);

// ----------------------------------------------------------------------------
// Simple Callback Types - Using typedef for clarity
// ----------------------------------------------------------------------------
typedef void (*FileOpenedNotification)(void);
typedef void (*ProgressUpdate)(int percentComplete);
typedef int (*DataValidator)(int value);

void onFileOpened(FileOpenedNotification notify);
void onProgressChanged(ProgressUpdate update);
int validateInput(DataValidator validator, int rawValue);

// ----------------------------------------------------------------------------
// Callbacks with Structs - Processing structured data
// ----------------------------------------------------------------------------
struct Measurement {
  double value;
  double timestamp;
};

typedef void (*MeasurementReceived)(struct Measurement *data);
void onNewMeasurement(MeasurementReceived handler);

typedef void (*MeasurementReceived2)(struct Measurement data);
void onNewMeasurement2(MeasurementReceived2 handler);

typedef void (*SampleBufferFull)(int samples[10]);
void onBufferReady(SampleBufferFull handler);

// ----------------------------------------------------------------------------
// Higher-Order Callbacks - Functions taking function pointers as arguments
// ----------------------------------------------------------------------------
void transformMeasurement(
    struct Measurement *data,
    void (*transformer)(struct Measurement *m, double (*scale)(double, int), int factor)
);

void processWithCallbacks(
    void (*handler)(struct Measurement *m, FileOpenedNotification notify, int priority)
);

// ----------------------------------------------------------------------------
// Structs with Function Pointers - Handler registration pattern
// ----------------------------------------------------------------------------
struct MeasurementHandler {
  void (*onReceived)(struct Measurement *data);
  int (*validate)(struct Measurement *data);
  void (*onError)(int errorCode);
};

void registerHandler(struct MeasurementHandler *handler);

struct DataPipeline {
  void (*preProcess)(struct Measurement *m, DataValidator validator);
  void (*process)(struct Measurement *m);
  void (*postProcess)(struct Measurement *m, ProgressUpdate update);
};

void executePipeline(struct Measurement *data, struct DataPipeline *pipeline);

// ----------------------------------------------------------------------------
// Union with Function Pointers - Mode-based callback selection
// ----------------------------------------------------------------------------
union ProcessorCallback {
  void (*simple)(struct Measurement *data);
  void (*withValidator)(struct Measurement *data, DataValidator validator);
  void (*withProgress)(struct Measurement *data, ProgressUpdate progress);
};

struct Processor {
  enum { MODE_SIMPLE, MODE_VALIDATED, MODE_PROGRESS } mode;
  union ProcessorCallback callback;
};

void runProcessor(struct Measurement *data, struct Processor *processor);

// ----------------------------------------------------------------------------
// Third-Order Function - Deeply nested callback chain
// ----------------------------------------------------------------------------
void processMeasurementWithValidation(
    struct Measurement *data,
    void (*processor)(
        struct Measurement *m,
        void (*transformer)(struct Measurement *m, DataValidator validator, int threshold),
        DataValidator validator
    )
);

typedef int foo;
void f(void (*callback)(foo x));
