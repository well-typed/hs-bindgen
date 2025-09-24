struct Measurement {
  double value, timestamp;
};

typedef void (*MeasurementReceived)(struct Measurement data);
void onNewMeasurement(MeasurementReceived handler);

typedef void (*SampleBufferFull)(int samples[10]);
void onBufferReady(SampleBufferFull handler);
