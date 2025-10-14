#include <stdio.h>
#include <stdlib.h>
#include "callbacks.h"

int readFileWithProcessor(void (*processLine)(int lineNumber), int fileId) {
    printf("Opening file %d...\n", fileId);

    if (processLine == NULL) {
        return -1;
    }

    for (int line = 1; line <= 5; line++) {
        processLine(line);
    }

    return 0;
}

void watchTemperature(void (*onTempChange)(int degrees), int sensorId) {
    if (onTempChange == NULL) {
        printf("Error: No temperature callback provided\n");
        return;
    }

    // Simulate temperature readings from sensor
    int temps[] = {20, 22, 25, 23, 21};
    printf("Monitoring sensor %d...\n", sensorId);

    for (int i = 0; i < 5; i++) {
        onTempChange(temps[i]);  // Notify about temperature change
    }
}

void onFileOpened(FileOpenedNotification notify) {
    // Simulate file opening
    printf("File operation starting...\n");
    if (notify != NULL) {
        notify();  // Trigger the notification
    }
}

void onProgressChanged(ProgressUpdate update) {
    // Simulate progress updates
    if (update != NULL) {
        for (int percent = 0; percent <= 100; percent += 25) {
            update(percent);
        }
    }
}

int validateInput(DataValidator validator, int rawValue) {
    if (validator == NULL) {
        return rawValue;  // No validation, return as-is
    }

    // Apply the validation function
    int validated = validator(rawValue);
    printf("Input %d validated to %d\n", rawValue, validated);
    return validated;
}

void onNewMeasurement(MeasurementReceived handler) {
    struct Measurement sample = {3.14, 1234567890.0};

    if (handler != NULL) {
        handler(&sample);  // Send the measurement
    }
}

void transformMeasurement(struct Measurement *data, void (*transformer)(struct Measurement *m, double (*scale)(double, int), int factor)) {
    if (data == NULL || transformer == NULL) {
        printf("Error: NULL pointer in transformMeasurement\n");
        return;
    }

    printf("Transforming measurement with value: %.2f\n", data->value);

    // Create a simple scaling function to pass to the transformer
    // Note: In a real implementation, this would be more dynamic
    transformer(data, NULL, 2);
}

void processWithCallbacks(void (*handler)(struct Measurement *m, FileOpenedNotification notify, int priority)) {
    if (handler == NULL) {
        printf("Error: NULL handler in processWithCallbacks\n");
        return;
    }

    struct Measurement sample = {99.9, 1111111111.0};

    // Create a simple notification callback
    printf("Processing with callbacks...\n");
    handler(&sample, NULL, 5);
}
