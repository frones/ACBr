#include <cstdint>
#include <string>
#include "ACBrNFeDinamico.h"

// Check windows
#if _WIN32 || _WIN64
#if _WIN64
#define ENVIRONMENT64
#else
#define ENVIRONMENT32
#endif
#endif

#if _WIN32 || _WIN64
#define ISWINDOWS
#elif __GNUC__
#define ISUNIX
#endif

// Check GCC
#if __GNUC__
#if __x86_64__ || __ppc64__
#define ENVIRONMENT64
#else
#define ENVIRONMENT32
#endif
#endif

#if defined(ISWINDOWS)
#include <windows.h>
#endif

#define BUFFER_LEN 256

class ACBrNFe {
    private:
    #if defined(ISWINDOWS)
        HMODULE nHandler;
    #else
        void* nHandler;
    #endif      

      void CheckResult(int ret);
      std::string ProcessResult(std::string buffer, int bufferLen);

    public:
        ACBrNFe(std::string eArqConfig, std::string eChaveCrypt);
        ACBrNFe() : ACBrNFe("", ""){}
        ~ACBrNFe();

        std::string Nome();
        std::string Versao();

};