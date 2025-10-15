#include <stdio.h>
#include <stdlib.h>
#include "callbacks.h"

// ============================================================================
// Basic Callbacks
// ============================================================================

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

    int temps[] = {20, 22, 25, 23, 21};
    printf("Monitoring sensor %d...\n", sensorId);

    for (int i = 0; i < 5; i++) {
        onTempChange(temps[i]);
    }
}

// ============================================================================
// Simple Callback Types
// ============================================================================

void onFileOpened(FileOpenedNotification notify) {
    printf("File operation starting...\n");
    if (notify != NULL) {
        notify();
    }
}

void onProgressChanged(ProgressUpdate update) {
    if (update != NULL) {
        for (int percent = 0; percent <= 100; percent += 25) {
            update(percent);
        }
    }
}

int validateInput(DataValidator validator, int rawValue) {
    if (validator == NULL) {
        return rawValue;
    }

    int validated = validator(rawValue);
    printf("Input %d validated to %d\n", rawValue, validated);
    return validated;
}

// ============================================================================
// Callbacks with Structs
// ============================================================================

void onNewMeasurement(MeasurementReceived handler) {
    struct Measurement sample = {3.14, 1234567890.0};

    if (handler != NULL) {
        handler(&sample);
    }
}

// ============================================================================
// Higher-Order Callbacks
// ============================================================================

void transformMeasurement(struct Measurement *data, void (*transformer)(struct Measurement *m, double (*scale)(double, int), int factor)) {
    if (data == NULL || transformer == NULL) {
        printf("Error: NULL pointer in transformMeasurement\n");
        return;
    }

    printf("Transforming measurement with value: %.2f\n", data->value);
    transformer(data, NULL, 2);
}

void processWithCallbacks(void (*handler)(struct Measurement *m, FileOpenedNotification notify, int priority)) {
    if (handler == NULL) {
        printf("Error: NULL handler in processWithCallbacks\n");
        return;
    }

    struct Measurement sample = {99.9, 1111111111.0};
    printf("Processing with callbacks...\n");
    handler(&sample, NULL, 5);
}

// ============================================================================
// Structs with Function Pointers
// ============================================================================

void registerHandler(struct MeasurementHandler *handler) {
    if (handler == NULL) {
        printf("Error: NULL handler\n");
        return;
    }

    printf("Registering measurement handler...\n");

    // Simulate receiving a measurement
    struct Measurement sample = {42.0, 1234567890.0};

    if (handler->onReceived != NULL) {
        handler->onReceived(&sample);
    }

    if (handler->validate != NULL) {
        int isValid = handler->validate(&sample);
        printf("Validation result: %d\n", isValid);
    }

    if (handler->onError != NULL && sample.value < 0) {
        handler->onError(-1);
    }
}

void executePipeline(struct Measurement *data, struct DataPipeline *pipeline) {
    if (data == NULL || pipeline == NULL) {
        printf("Error: NULL pointer in executePipeline\n");
        return;
    }

    printf("Executing data pipeline for measurement: %.2f\n", data->value);

    if (pipeline->preProcess != NULL) {
        pipeline->preProcess(data, NULL);
    }

    if (pipeline->process != NULL) {
        pipeline->process(data);
    }

    if (pipeline->postProcess != NULL) {
        pipeline->postProcess(data, NULL);
    }

    printf("Pipeline execution complete\n");
}

// ============================================================================
// Union with Function Pointers
// ============================================================================

void runProcessor(struct Measurement *data, struct Processor *processor) {
    if (data == NULL || processor == NULL) {
        printf("Error: NULL pointer in runProcessor\n");
        return;
    }

    printf("Running processor in mode %d\n", processor->mode);

    switch (processor->mode) {
        case MODE_SIMPLE:
            if (processor->callback.simple != NULL) {
                processor->callback.simple(data);
            }
            break;

        case MODE_VALIDATED:
            if (processor->callback.withValidator != NULL) {
                processor->callback.withValidator(data, NULL);
            }
            break;

        case MODE_PROGRESS:
            if (processor->callback.withProgress != NULL) {
                processor->callback.withProgress(data, NULL);
            }
            break;

        default:
            printf("Unknown processor mode\n");
    }
}

// ============================================================================
// Third-Order Function
// ============================================================================

void processMeasurementWithValidation(
    struct Measurement *data,
    void (*processor)(
        struct Measurement *m,
        void (*transformer)(struct Measurement *m, DataValidator validator, int threshold),
        DataValidator validator
    )
) {
    if (data == NULL || processor == NULL) {
        printf("Error: NULL pointer in processMeasurementWithValidation\n");
        return;
    }

    printf("Processing measurement %.2f with validation chain\n", data->value);
    processor(data, NULL, NULL);
}
