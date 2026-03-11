#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# CEF version to download (pinned for reproducibility)
CEF_VERSION="145.0.28+g51162e8+chromium-145.0.7632.160"
CEF_PLATFORM="linux64_minimal"
# The CEF binary distribution uses '+' in the version, which is URL-encoded as
# '%2B' on the Spotify CDN.
CEF_DIR_NAME="cef_binary_${CEF_VERSION}_${CEF_PLATFORM}"
CEF_ARCHIVE="${CEF_DIR_NAME}.tar.bz2"
CEF_URL="https://cef-builds.spotifycdn.com/${CEF_ARCHIVE//+/%2B}"

CEF_ROOT="$SCRIPT_DIR/$CEF_DIR_NAME"
BINDING_SPEC_DIR="$SCRIPT_DIR/binding-specs"
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src/"

# Create directories
mkdir -p "$BINDING_SPEC_DIR"
mkdir -p "$HS_OUTPUT_DIR"

echo "# "
echo "# Downloading CEF binary distribution"
echo "# "

if [ ! -d "$CEF_ROOT" ]; then
    if [ ! -f "$SCRIPT_DIR/$CEF_ARCHIVE" ]; then
        echo "Downloading $CEF_ARCHIVE (~287MB)..."
        curl -L -o "$SCRIPT_DIR/$CEF_ARCHIVE" "$CEF_URL"
    else
        echo "Archive already downloaded."
    fi

    echo "Extracting..."
    tar -xjf "$SCRIPT_DIR/$CEF_ARCHIVE" -C "$SCRIPT_DIR"
    rm -f "$SCRIPT_DIR/$CEF_ARCHIVE"
    echo "Extracted to $CEF_ROOT"
else
    echo "CEF distribution already present at $CEF_ROOT"
fi

# CEF headers use #include "include/capi/cef_base_capi.h" (relative to CEF
# root), so -I must point to the CEF distribution root directory.
INCLUDE_DIR="$CEF_ROOT"
LIB_DIR="$CEF_ROOT/Release"

echo "# "
echo "# Generating Haskell bindings in dependency order"
echo "# "

cd "$PROJECT_ROOT"

# Helper function to generate bindings with optional external binding specs
# Usage: generate_bindings HEADER MODULE_NAME [external specs...]
#
# The dependency order below was determined using:
#   cabal run hs-bindgen-cli -- info include-graph -I "$CEF_ROOT" "include/capi/cef_app_capi.h"
# combined with a topological sort of the resulting graph.
generate_bindings() {
    local HEADER="$1"
    local MODULE_NAME="$2"
    shift 2

    local BINDING_SPEC_FILE="$BINDING_SPEC_DIR/${HEADER%.h}.yaml"
    local EXTERNAL_SPECS=("$@")

    echo "Generating bindings for $HEADER -> $MODULE_NAME"

    local CMD=(
        cabal run hs-bindgen-cli -- preprocess
        -I "$INCLUDE_DIR"
        -D CEF_API_VERSION=14500
        --hs-output-dir "$HS_OUTPUT_DIR"
        --create-output-dirs
        --overwrite-files
        --module "$MODULE_NAME"
        --parse-all
        --select-from-main-headers
        --enable-program-slicing
        --gen-binding-spec "$BINDING_SPEC_FILE"
      )

    # Add external binding specs if any
    for spec in "${EXTERNAL_SPECS[@]}"; do
        if [ -f "$spec" ]; then
            CMD+=(--external-binding-spec "$spec")
        else
            echo "WARNING: Missing binding spec: $spec (prior generation failed?)"
        fi
    done

    CMD+=("include/capi/$HEADER")

    if ! "${CMD[@]}"; then
        echo "WARNING: Failed to generate bindings for $HEADER (skipping)"
        SKIPPED_HEADERS+=("$HEADER")
    fi
}

SKIPPED_HEADERS=()

# Generate bindings for non-capi headers (these are under include/, not
# include/capi/, and declare CEF's version/hash introspection functions).
# They must be generated first since they have no dependencies on capi headers.

generate_non_capi_binding() {
    local HEADER="$1"
    local MODULE_NAME="$2"
    shift 2

    local HEADER_BASENAME
    HEADER_BASENAME="$(basename "${HEADER%.h}")"
    local BINDING_SPEC_FILE="$BINDING_SPEC_DIR/${HEADER_BASENAME}.yaml"
    local EXTERNAL_SPECS=("$@")

    echo "Generating bindings for $HEADER -> $MODULE_NAME"

    local CMD=(
        cabal run hs-bindgen-cli -- preprocess
        -I "$INCLUDE_DIR"
        -D CEF_API_VERSION=14500
        --hs-output-dir "$HS_OUTPUT_DIR"
        --create-output-dirs
        --overwrite-files
        --module "$MODULE_NAME"
        --parse-all
        --select-from-main-headers
        --enable-program-slicing
        --gen-binding-spec "$BINDING_SPEC_FILE"
      )

    for spec in "${EXTERNAL_SPECS[@]}"; do
        if [ -f "$spec" ]; then
            CMD+=(--external-binding-spec "$spec")
        else
            echo "WARNING: Missing binding spec: $spec (prior generation failed?)"
        fi
    done

    CMD+=("$HEADER")

    if ! "${CMD[@]}"; then
        echo "WARNING: Failed to generate bindings for $HEADER (skipping)"
        SKIPPED_HEADERS+=("$HEADER")
    fi
}

# cef_api_hash.h: declares cef_api_hash() and cef_api_version()
generate_non_capi_binding "include/cef_api_hash.h" "CEF.ApiHash"

# cef_version_info.h: declares cef_version_info() and cef_version_info_all()
generate_non_capi_binding "include/cef_version_info.h" "CEF.VersionInfo" \
    "$BINDING_SPEC_DIR/cef_api_hash.yaml"

# Generate bindings in dependency order (79 capi headers total)
# Dependency order determined by topological sort of #include relationships.

# --- Level 0: no dependencies ---

# 1. cef_base_capi.h (no dependencies)
generate_bindings "cef_base_capi.h" "CEF.Base"

# --- Level 1: depend only on cef_base ---

# 2. cef_auth_callback_capi.h
generate_bindings "cef_auth_callback_capi.h" "CEF.AuthCallback" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 3. cef_callback_capi.h
generate_bindings "cef_callback_capi.h" "CEF.Callback" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 4. cef_command_line_capi.h
generate_bindings "cef_command_line_capi.h" "CEF.CommandLine" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 5. cef_crash_util_capi.h
generate_bindings "cef_crash_util_capi.h" "CEF.CrashUtil" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 6. cef_devtools_message_observer_capi.h
generate_bindings "cef_devtools_message_observer_capi.h" "CEF.DevtoolsMessageObserver" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 7. cef_dom_capi.h
generate_bindings "cef_dom_capi.h" "CEF.Dom" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 8. cef_download_item_capi.h
generate_bindings "cef_download_item_capi.h" "CEF.DownloadItem" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 9. cef_file_util_capi.h
generate_bindings "cef_file_util_capi.h" "CEF.FileUtil" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 10. cef_i18n_util_capi.h
generate_bindings "cef_i18n_util_capi.h" "CEF.I18nUtil" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 11. cef_menu_model_delegate_capi.h
generate_bindings "cef_menu_model_delegate_capi.h" "CEF.MenuModelDelegate" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 12. cef_origin_whitelist_capi.h
generate_bindings "cef_origin_whitelist_capi.h" "CEF.OriginWhitelist" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 13. cef_parser_capi.h
generate_bindings "cef_parser_capi.h" "CEF.Parser" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 14. cef_path_util_capi.h
generate_bindings "cef_path_util_capi.h" "CEF.PathUtil" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 15. cef_print_settings_capi.h
generate_bindings "cef_print_settings_capi.h" "CEF.PrintSettings" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 16. cef_process_util_capi.h
#
# NOTE: CEF translator bug — this auto-generated header uses
# `struct _cef_command_line_t` without #include-ing
# cef_command_line_capi.h (the C++ counterpart cef_process_util.h
# does include it). As a result, cef_launch_process is skipped with
# a warning about an out-of-scope declaration. The generated module
# will be empty.
#
# See https://github.com/chromiumembedded/cef/issues/4123
generate_bindings "cef_process_util_capi.h" "CEF.ProcessUtil" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 17. cef_registration_capi.h
generate_bindings "cef_registration_capi.h" "CEF.Registration" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 18. cef_request_capi.h
generate_bindings "cef_request_capi.h" "CEF.Request" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 19. cef_resource_bundle_handler_capi.h
generate_bindings "cef_resource_bundle_handler_capi.h" "CEF.ResourceBundleHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 20. cef_response_capi.h
generate_bindings "cef_response_capi.h" "CEF.Response" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 21. cef_response_filter_capi.h
generate_bindings "cef_response_filter_capi.h" "CEF.ResponseFilter" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 22. cef_shared_memory_region_capi.h
generate_bindings "cef_shared_memory_region_capi.h" "CEF.SharedMemoryRegion" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 23. cef_stream_capi.h
generate_bindings "cef_stream_capi.h" "CEF.Stream" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 24. cef_string_visitor_capi.h
generate_bindings "cef_string_visitor_capi.h" "CEF.StringVisitor" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 25. cef_task_capi.h
generate_bindings "cef_task_capi.h" "CEF.Task" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 26. cef_task_manager_capi.h
generate_bindings "cef_task_manager_capi.h" "CEF.TaskManager" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 27. cef_unresponsive_process_callback_capi.h
generate_bindings "cef_unresponsive_process_callback_capi.h" "CEF.UnresponsiveProcessCallback" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 28. cef_values_capi.h
generate_bindings "cef_values_capi.h" "CEF.Values" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# 29. cef_waitable_event_capi.h
generate_bindings "cef_waitable_event_capi.h" "CEF.WaitableEvent" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml"

# --- Level 2: depend on levels 0-1 ---

# 30. cef_cookie_capi.h
generate_bindings "cef_cookie_capi.h" "CEF.Cookie" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml"

# 31. cef_trace_capi.h
generate_bindings "cef_trace_capi.h" "CEF.Trace" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml"

# 32. cef_menu_model_capi.h
generate_bindings "cef_menu_model_capi.h" "CEF.MenuModel" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_menu_model_delegate_capi.yaml"

# 33. cef_media_router_capi.h
generate_bindings "cef_media_router_capi.h" "CEF.MediaRouter" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_registration_capi.yaml"

# 34. cef_xml_reader_capi.h
generate_bindings "cef_xml_reader_capi.h" "CEF.XmlReader" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_stream_capi.yaml"

# 35. cef_zip_reader_capi.h
generate_bindings "cef_zip_reader_capi.h" "CEF.ZipReader" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_stream_capi.yaml"

# 36. cef_server_capi.h
generate_bindings "cef_server_capi.h" "CEF.Server" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_task_capi.yaml"

# 37. cef_thread_capi.h
generate_bindings "cef_thread_capi.h" "CEF.Thread" \
    "$BINDING_SPEC_DIR/cef_task_capi.yaml"

# 38. cef_accessibility_handler_capi.h
generate_bindings "cef_accessibility_handler_capi.h" "CEF.AccessibilityHandler" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 39. cef_image_capi.h
generate_bindings "cef_image_capi.h" "CEF.Image" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 40. cef_preference_capi.h
generate_bindings "cef_preference_capi.h" "CEF.Preference" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_registration_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 41. cef_process_message_capi.h
generate_bindings "cef_process_message_capi.h" "CEF.ProcessMessage" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_shared_memory_region_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 42. cef_resource_bundle_capi.h
generate_bindings "cef_resource_bundle_capi.h" "CEF.ResourceBundle" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 43. cef_x509_certificate_capi.h
generate_bindings "cef_x509_certificate_capi.h" "CEF.X509Certificate" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# --- Level 3 ---

# 44. cef_drag_data_capi.h
generate_bindings "cef_drag_data_capi.h" "CEF.DragData" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_image_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_stream_capi.yaml"

# 45. cef_request_context_capi.h
generate_bindings "cef_request_context_capi.h" "CEF.RequestContext" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_cookie_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_media_router_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_preference_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_registration_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 46. cef_frame_capi.h
generate_bindings "cef_frame_capi.h" "CEF.Frame" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_dom_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_process_message_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_stream_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_string_visitor_capi.yaml"

# 47. cef_shared_process_message_builder_capi.h
generate_bindings "cef_shared_process_message_builder_capi.h" "CEF.SharedProcessMessageBuilder" \
    "$BINDING_SPEC_DIR/cef_process_message_capi.yaml"

# 48. cef_ssl_info_capi.h
generate_bindings "cef_ssl_info_capi.h" "CEF.SslInfo" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_x509_certificate_capi.yaml"

# 49. cef_ssl_status_capi.h
generate_bindings "cef_ssl_status_capi.h" "CEF.SslStatus" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_x509_certificate_capi.yaml"

# --- Level 4 ---

# 50. cef_urlrequest_capi.h
generate_bindings "cef_urlrequest_capi.h" "CEF.Urlrequest" \
    "$BINDING_SPEC_DIR/cef_auth_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_context_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_response_capi.yaml"

# 51. cef_navigation_entry_capi.h
generate_bindings "cef_navigation_entry_capi.h" "CEF.NavigationEntry" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_ssl_status_capi.yaml"

# --- Level 5 ---

# 52. cef_browser_capi.h
generate_bindings "cef_browser_capi.h" "CEF.Browser" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_devtools_message_observer_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_drag_data_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_image_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_navigation_entry_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_registration_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_context_capi.yaml"

# --- Level 6: depend on browser ---

# 53. cef_audio_handler_capi.h
generate_bindings "cef_audio_handler_capi.h" "CEF.AudioHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 54. cef_command_handler_capi.h
generate_bindings "cef_command_handler_capi.h" "CEF.CommandHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 55. cef_context_menu_handler_capi.h
generate_bindings "cef_context_menu_handler_capi.h" "CEF.ContextMenuHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_menu_model_capi.yaml"

# 56. cef_dialog_handler_capi.h
generate_bindings "cef_dialog_handler_capi.h" "CEF.DialogHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 57. cef_display_handler_capi.h
generate_bindings "cef_display_handler_capi.h" "CEF.DisplayHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml"

# 58. cef_download_handler_capi.h
generate_bindings "cef_download_handler_capi.h" "CEF.DownloadHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_download_item_capi.yaml"

# 59. cef_drag_handler_capi.h
generate_bindings "cef_drag_handler_capi.h" "CEF.DragHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_drag_data_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml"

# 60. cef_find_handler_capi.h
generate_bindings "cef_find_handler_capi.h" "CEF.FindHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 61. cef_focus_handler_capi.h
generate_bindings "cef_focus_handler_capi.h" "CEF.FocusHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_dom_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml"

# 62. cef_frame_handler_capi.h
generate_bindings "cef_frame_handler_capi.h" "CEF.FrameHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml"

# 63. cef_jsdialog_handler_capi.h
generate_bindings "cef_jsdialog_handler_capi.h" "CEF.JsdialogHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 64. cef_keyboard_handler_capi.h
generate_bindings "cef_keyboard_handler_capi.h" "CEF.KeyboardHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 65. cef_life_span_handler_capi.h
generate_bindings "cef_life_span_handler_capi.h" "CEF.LifeSpanHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 66. cef_load_handler_capi.h
generate_bindings "cef_load_handler_capi.h" "CEF.LoadHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml"

# 67. cef_permission_handler_capi.h
generate_bindings "cef_permission_handler_capi.h" "CEF.PermissionHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml"

# 68. cef_print_handler_capi.h
generate_bindings "cef_print_handler_capi.h" "CEF.PrintHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_print_settings_capi.yaml"

# 69. cef_render_handler_capi.h
generate_bindings "cef_render_handler_capi.h" "CEF.RenderHandler" \
    "$BINDING_SPEC_DIR/cef_accessibility_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_drag_data_capi.yaml"

# 70. cef_resource_handler_capi.h
generate_bindings "cef_resource_handler_capi.h" "CEF.ResourceHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_cookie_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_response_capi.yaml"

# 71. cef_v8_capi.h
generate_bindings "cef_v8_capi.h" "CEF.V8" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_task_capi.yaml"

# --- Level 7 ---

# 72. cef_resource_request_handler_capi.h
generate_bindings "cef_resource_request_handler_capi.h" "CEF.ResourceRequestHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_resource_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_response_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_response_filter_capi.yaml"

# 73. cef_scheme_capi.h
generate_bindings "cef_scheme_capi.h" "CEF.Scheme" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_resource_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_response_capi.yaml"

# --- Level 8 ---

# 74. cef_render_process_handler_capi.h
generate_bindings "cef_render_process_handler_capi.h" "CEF.RenderProcessHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_dom_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_load_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_process_message_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_v8_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# 75. cef_request_context_handler_capi.h
generate_bindings "cef_request_context_handler_capi.h" "CEF.RequestContextHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_preference_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_resource_request_handler_capi.yaml"

# 76. cef_request_handler_capi.h
generate_bindings "cef_request_handler_capi.h" "CEF.RequestHandler" \
    "$BINDING_SPEC_DIR/cef_auth_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_resource_request_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_ssl_info_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_unresponsive_process_callback_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_x509_certificate_capi.yaml"

# --- Level 9 ---

# 77. cef_client_capi.h
generate_bindings "cef_client_capi.h" "CEF.Client" \
    "$BINDING_SPEC_DIR/cef_audio_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_command_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_context_menu_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_dialog_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_display_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_download_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_drag_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_find_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_focus_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_frame_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_jsdialog_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_keyboard_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_life_span_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_load_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_permission_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_print_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_process_message_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_render_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_handler_capi.yaml"

# --- Level 10 ---

# 78. cef_browser_process_handler_capi.h
generate_bindings "cef_browser_process_handler_capi.h" "CEF.BrowserProcessHandler" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_client_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_command_line_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_preference_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_request_context_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_values_capi.yaml"

# --- Level 11 (top-level) ---

# 79. cef_app_capi.h
generate_bindings "cef_app_capi.h" "CEF.App" \
    "$BINDING_SPEC_DIR/cef_base_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_browser_process_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_command_line_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_render_process_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_resource_bundle_handler_capi.yaml" \
    "$BINDING_SPEC_DIR/cef_scheme_capi.yaml"

if [ ${#SKIPPED_HEADERS[@]} -gt 0 ]; then
    echo "# "
    echo "# WARNING: ${#SKIPPED_HEADERS[@]} header(s) failed to generate bindings:"
    for h in "${SKIPPED_HEADERS[@]}"; do
        echo "#   - $h"
    done
    echo "# "
fi

echo "# "
echo "# Updating cabal.project.local"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<-EOF
package cef
    extra-include-dirs:
        $INCLUDE_DIR
    extra-lib-dirs:
        $LIB_DIR
EOF
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd "$SCRIPT_DIR/hs-project"
LD_LIBRARY_PATH="$LIB_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cabal build cef-bin

# CEF uses dladdr() to find libcef.so's directory and looks for resource files
# (icudtl.dat, pak files, locales/) there. The binary distribution puts these
# in Resources/ while libcef.so is in Release/, so we symlink them across.
for f in "$CEF_ROOT/Resources/"*; do
    ln -sf "$f" "$CEF_ROOT/Release/"
done

cabal run cef-bin
