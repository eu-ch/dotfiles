#!/bin/sh

# User-configurable
PROJ_NAME=Kvlt
BUILD_CONFIG=DebugGame
ENGINE_PATH=~/Dev/unreal-engine-4/src/unreal-engine-4

PROJ_NAME_EDITOR="${PROJ_NAME}Editor"

# Files and directories
PROJ_DIR=$(dirname $(realpath "$0"))
UPROJ_PATH="${PROJ_DIR}/${PROJ_NAME}.uproject"

# Executables
BUILD_BAT="${ENGINE_PATH}/Engine/Build/BatchFiles/Linux/Build.sh"
MONO_EXE="${ENGINE_PATH}/Engine/Build/BatchFiles/Linux/RunMono.sh"
UBT_EXE="${MONO_EXE} ${ENGINE_PATH}/Engine/Binaries/DotNET/UnrealBuildTool.exe"
UE_EXE="${ENGINE_PATH}/Engine/Binaries/Linux/UE4Editor-Linux-${BUILD_CONFIG}"

# Validations
PASSED_VALIDATION=1
if [ ! -d "${ENGINE_PATH}" ]; then
    PASSED_VALIDATION=0
    echo Error: Unreal Engine directory \"${ENGINE_PATH}\" does not exist - please set ENGINE_PATH variable correctly
fi
if [ ! -f "${UPROJ_PATH}" ]; then
    PASSED_VALIDATION=0
    echo Error: Project file \"${UPROJ_PATH}\" does not exist - please set PROJ_NAME variable correctly
fi
if [ ${PASSED_VALIDATION} == 0 ]; then
    exit 1
fi

# ---------------------------------------------------

print_usage() {
    echo "Usage: uecli.sh (BuildEditor|Clean|GenerateClangDB|GenerateDefaultProject|GenerateVSCodeProject|RunEditor)"
}

build_editor() {
    ${BUILD_BAT} ${PROJ_NAME_EDITOR} Linux ${BUILD_CONFIG} -project=${UPROJ_PATH}
}

clean_project() {
    ${UBT_EXE} ${PROJ_NAME_EDITOR} -Clean -project=${UPROJ_PATH} -game ${BUILD_CONFIG} Linux #-Verbose
}

generate_clang_db() {
    echo Generating Unreal headers
    ${BUILD_BAT} ${PROJ_NAME_EDITOR} -project=${UPROJ_PATH} -game -engine -SkipBuild Linux ${BUILD_CONFIG}
    echo Generating compile_commands.json
    ${UBT_EXE} ${PROJ_NAME_EDITOR} -mode=GenerateClangDatabase -project=${UPROJ_PATH} -game ${BUILD_CONFIG} Linux #-Verbose 
    echo Copying compile_commands.json into ${PROJ_DIR}
    cp -f ${ENGINE_PATH}/compile_commands.json ${PROJ_DIR}/
}

generate_default_project() {
    ${UBT_EXE} ${UPROJ_NAME_EDITOR} -Project ${UPROJ_PATH} -Game -Engine -ProjectFiles -VSCode 
}

generate_vscode_project() {
    echo Error: GenerateVSCodeProject command is not implemented
    exit 1
}

run_editor() {
    ${UE_EXE} ${UPROJ_PATH} 
}

# Lower casing $1 for convenience
case "${1,,}" in
    "buildeditor")
        build_editor;;
    "clean")
        clean_project;;
    "generateclangdb")
        generate_clang_db;;
    "generatedefaultproject")
        generate_default_project;;
    "generatevscodeproject")
        generate_vscode_project;;
    "runeditor")
        run_editor;;
    "" | "-h" | "--help")
        print_usage;;
    *)
        echo "Error: nnknown argument '${1}'"
        print_usage
esac
