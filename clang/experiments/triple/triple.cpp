/**
 * Parse triple
 *
 * See https://llvm.org/doxygen/classllvm_1_1Triple.html
 */

#include <stdio.h>

#include "llvm/TargetParser/Triple.h"
#include "llvm/ADT/Twine.h"

int main(int argc, char* argv[]) {
    if(argc != 2) {
        printf("Usage: %s <triple>\n", argv[0]);
        return -1;
    }

    llvm::Twine triple_str(argv[1]);
    llvm::Triple triple(triple_str);

    // Note: the use of .data() here is not really correct (the strings may not
    // be NULL terminated).

    printf("getArch: %d\n", triple.getArch());
    printf("  getArchTypeName: %s\n", llvm::Triple::getArchTypeName(triple.getArch()).data());
    printf("  getArchTypePrefix: %s\n", llvm::Triple::getArchTypePrefix(triple.getArch()).data());
    printf("getSubArch: %d\n", triple.getSubArch());
    printf("  getArchName: %s\n", llvm::Triple::getArchName(triple.getArch(), triple.getSubArch()).data());
    printf("getVendor: %d\n", triple.getVendor());
    printf("  getVendorTypeName: %s\n", llvm::Triple::getVendorTypeName(triple.getVendor()).data());
    printf("getOS: %d\n", triple.getOS());
    printf("  getOSTypeName: %s\n", llvm::Triple::getOSTypeName(triple.getOS()).data());
    printf("  getOSVersion: %s\n", triple.getOSVersion().getAsString().c_str());

    printf("hasEnvironment: %d\n", triple.hasEnvironment());
    if(triple.hasEnvironment()) {
        printf("  getEnvironment: %d\n", triple.getEnvironment());
        printf("  getEnvironmentTypeName: %s\n", llvm::Triple::getEnvironmentTypeName(triple.getEnvironment()).data());
        printf("  getEnvironmentVersion: %s\n", triple.getEnvironmentVersion().getAsString().c_str());
    }

    printf("getObjectFormat: %d\n", triple.getObjectFormat());
    printf("  getObjectFormatTypeName: %s\n", llvm::Triple::getObjectFormatTypeName(triple.getObjectFormat()).data());

    return 0;
}