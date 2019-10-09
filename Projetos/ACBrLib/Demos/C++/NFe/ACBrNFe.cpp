#include <cstdint>
#include <string>
#include "ACBrNFe.h"
#if defined(ISWINDOWS)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

ACBrNFe::ACBrNFe(std::string eArqConfig, std::string eChaveCrypt){
    #if defined(ISWINDOWS)
        #if defined(ENVIRONMENT32)
            nHandler = LoadLibrary(L"ACBrNFe32.dll");
        #else
            nHandler = LoadLibrary(L"ACBrNFe64.dll");
        #endif        
    #else 
        #if defined(ENVIRONMENT32)
            nHandler = dlopen("libacbrnfe32.so");
        #else
            nHandler = dlopen("libacbrnfe64.so");
        #endif     
    #endif

    NFE_Inicializar method;

    #if defined(ISWINDOWS)
        method = (NFE_Inicializar)GetProcAddress(nHandler, "NFE_Inicializar");
    #else
        method = (NFE_Inicializar)dlsym(nHandler, "NFE_Inicializar");
    #endif

    int ret = method(eArqConfig.c_str(), eChaveCrypt.c_str());
    CheckResult(ret);
}

ACBrNFe::~ACBrNFe(){
    NFE_Finalizar method;

    #if defined(ISWINDOWS)
        method = (NFE_Finalizar)GetProcAddress(nHandler, "NFE_Finalizar");
    #else
        method = (NFE_Finalizar)dlsym(nHandler, "NFE_Finalizar");
    #endif

    int ret = method();
    CheckResult(ret);

    #if defined(ISWINDOWS)
        FreeLibrary(nHandler);
    #else
        dlclose(nHandler);
    #endif 
}

std::string ACBrNFe::Nome(){
    NFE_Nome method;
    
    #if defined(ISWINDOWS)
        method = (NFE_Nome)GetProcAddress(nHandler, "NFE_Nome");
    #else
        method = (NFE_Nome)dlsym(nHandler, "NFE_Nome");
    #endif    

    std::string buffer(BUFFER_LEN, ' ');
    int bufferLen = BUFFER_LEN;

    method(buffer.c_str(), &bufferLen);
    return ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Versao(){
    NFE_Versao method;
    
    #if defined(ISWINDOWS)
        method = (NFE_Versao)GetProcAddress(nHandler, "NFE_Versao");
    #else
        method = (NFE_Versao)dlsym(nHandler, "NFE_Versao");
    #endif    

    std::string buffer(BUFFER_LEN, ' ');
    int bufferLen = BUFFER_LEN;

    method(buffer.c_str(), &bufferLen);
    return ProcessResult(buffer, bufferLen);
}

void ACBrNFe::CheckResult(int ret){
    if (ret >= 0) return;

    std::string buffer(BUFFER_LEN, ' ');
    int bufferLen = BUFFER_LEN;
    
    NFE_UltimoRetorno method;

    #if defined(ISWINDOWS)
        method = (NFE_UltimoRetorno)GetProcAddress(nHandler, "NFE_UltimoRetorno");
    #else
        method = (NFE_UltimoRetorno)dlsym(nHandler, "NFE_method");
    #endif

    method(buffer.c_str(), &bufferLen);

    if (bufferLen <= BUFFER_LEN) throw(std::exception(buffer.c_str()));
    
    buffer.clear();
    buffer.resize(bufferLen, ' ');
    method(buffer.c_str(), &bufferLen);
    throw(std::exception(buffer.c_str()));
}

std::string ACBrNFe::ProcessResult(std::string buffer, int bufferLen){
    if (bufferLen > BUFFER_LEN) {
        NFE_UltimoRetorno method;
        
        #if defined(ISWINDOWS)
            method = (NFE_UltimoRetorno)GetProcAddress(nHandler, "NFE_UltimoRetorno");
        #else
            method = (NFE_UltimoRetorno)dlsym(nHandler, "NFE_UltimoRetorno");
        #endif

        buffer.clear();
        buffer.resize(bufferLen, ' ');
        method(buffer.c_str(), &bufferLen);        
    }

    return buffer;
}