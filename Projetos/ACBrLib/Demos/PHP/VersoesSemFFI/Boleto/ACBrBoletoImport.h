// ACBrBoletoImport.h - Contém as declarações das funções da ACBrLibBoleto em modo MT para carregamento dinâmico.

#include <cstdint>

// function Boleto_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
typedef int (*Boleto_Inicializar)(uintptr_t* handle, const char* eArqConfig, const char* eChaveCrypt);

// function Boleto_Finalizar(): longint;
typedef int (*Boleto_Finalizar)(uintptr_t handle);

// function Boleto_Nome(const sNome: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_Nome)(uintptr_t handle, const char* sNome, int* esTamanho);

// function Boleto_Versao(const sVersao: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_Versao)(uintptr_t handle, const char* sVersao, int* esTamanho);

// function Boleto_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_UltimoRetorno)(uintptr_t handle, const char* sMensagem, int* esTamanho);

// function Boleto_ConfigLer(const eArqConfig: PChar): longint;
typedef int (*Boleto_ConfigLer)(uintptr_t handle, const char* eArqConfig);

// function Boleto_ConfigGravar(const eArqConfig: PChar): longint;
typedef int (*Boleto_ConfigGravar)(uintptr_t handle, const char* eArqConfig);

// function Boleto_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_ConfigLerValor)(uintptr_t handle, const char* eSessao, const char* eChave, const char* sValor, int* esTamanho);

// function Boleto_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
typedef int (*Boleto_ConfigGravarValor)(uintptr_t handle, const char* eSessao, const char* eChave, const char* sValor);

// function Boleto_ConfigImportar(const eArqConfig: PChar): longint;
typedef int (*Boleto_ConfigImportar)(uintptr_t handle, const char* eArqConfig);

// function Boleto_ConfigExportar(const libHandle: PLibHandle; const sValor: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_ConfigExportar)(uintptr_t handle, const char* sValor, int* esTamanho);

//function Boleto_ConfigurarDados(const libHandle: PLibHandle; eArquivoIni: PChar): longint; 
typedef int (*Boleto_ConfigurarDados)(uintptr_t handle, const char* eArquivoIni);

//function Boleto_IncluirTitulos(const libHandle: PLibHandle; eArquivoIni, eTpSaida: PChar): longint;
typedef int (*Boleto_IncluirTitulos)(uintptr_t handle, const char* eArquivoIni, const char* eTpSaida);

//function Boleto_LimparLista(const libHandle: PLibHandle): longint;
typedef int (*Boleto_LimparLista)(uintptr_t handle);

//function Boleto_TotalTitulosLista(const libHandle: PLibHandle): longint;
typedef int (*Boleto_TotalTitulosLista)(uintptr_t handle);

//function Boleto_Imprimir(const libHandle: PLibHandle; eNomeImpressora: PChar): longint; 
typedef int (*Boleto_Imprimir)(uintptr_t handle, const char* eNomeImpressora);

//function Boleto_ImprimirBoleto(const libHandle: PLibHandle; eIndice: longint; eNomeImpressora: PChar): longint;
typedef int (*Boleto_ImprimirBoleto)(uintptr_t handle, int eIndice, const char* eNomeImpressora);

//function Boleto_GerarPDF(const libHandle: PLibHandle): longint;
typedef int (*Boleto_GerarPDF)(uintptr_t handle);

//function Boleto_GerarPDFBoleto(const libHandle: PLibHandle; eIndice: longint): longint;
typedef int (*Boleto_GerarPDFBoleto)(uintptr_t handle, int eIndice);

//function Boleto_GerarHTML(const libHandle: PLibHandle): longint;
typedef int (*Boleto_GerarHTML)(uintptr_t handle);

//function Boleto_GerarRemessa(const libHandle: PLibHandle; eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint; 
typedef int (*Boleto_GerarRemessa)(uintptr_t handle, const char* eDir, int eNumArquivo, const char* eNomeArq);

//function Boleto_LerRetorno(const libHandle: PLibHandle; eDir, eNomeArq: PChar): longint; 
typedef int (*Boleto_LerRetorno)(uintptr_t handle, const char* eDir, const char* eNomeArq);

//function Boleto_ObterRetorno(const libHandle: PLibHandle; eDir, eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint; 
typedef int (*Boleto_ObterRetorno)(uintptr_t handle, const char* eDir, const char* eNomeArq, const char* sResposta, int* esTamanho);

//function Boleto_EnviarEmail(const libHandle: PLibHandle; ePara, eAssunto, eMensagem, eCC: PChar): longint;
typedef int (*Boleto_EnviarEmail)(uintptr_t handle, const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);

//function Boleto_EnviarEmailBoleto(const libHandle: PLibHandle; eIndice: longint; ePara, eAssunto, eMensagem, eCC: PChar): longint;
typedef int (*Boleto_EnviarEmailBoleto)(uintptr_t handle, int eIndice, const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);

//function Boleto_SetDiretorioArquivo(const libHandle: PLibHandle; eDir, eArq: PChar): longint;
typedef int (*Boleto_SetDiretorioArquivo)(uintptr_t handle, const char* eDir, const char* eArq);

//function Boleto_ListaBancos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_ListaBancos)(uintptr_t handle,  const char* sResposta, int* esTamanho);

//function Boleto_ListaCaractTitulo(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint; 
typedef int (*Boleto_ListaCaractTitulo)(uintptr_t handle,  const char* sResposta, int* esTamanho);

//function Boleto_ListaOcorrencias(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint; 
typedef int (*Boleto_ListaOcorrencias)(uintptr_t handle,  const char* sResposta, int* esTamanho);

//function Boleto_ListaOcorrenciasEX(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint; 
typedef int (*Boleto_ListaOcorrenciasEX)(uintptr_t handle,  const char* sResposta, int* esTamanho);

//function Boleto_TamNossoNumero(const libHandle: PLibHandle; eCarteira, enossoNumero, eConvenio: PChar): longint; 
typedef int (*Boleto_TamNossoNumero)(uintptr_t handle,  const char* eCarteira,  const char* enossoNumero,  const char* eConvenio);

//function Boleto_CodigosMoraAceitos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint; 
typedef int (*Boleto_CodigosMoraAceitos)(uintptr_t handle,  const char* sResposta, int* esTamanho);

//function Boleto_SelecionaBanco(const libHandle: PLibHandle; eCodBanco: PChar): longint;
typedef int (*Boleto_SelecionaBanco)(uintptr_t handle,  const char* eCodBanco);

//function Boleto_MontarNossoNumero(const libHandle: PLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_MontarNossoNumero)(uintptr_t handle, int eIndice, const char* sResposta, int* esTamanho);

//function Boleto_RetornaLinhaDigitavel(const libHandle: PLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_RetornaLinhaDigitavel)(uintptr_t handle, int eIndice, const char* sResposta, int* esTamanho);

//function Boleto_RetornaCodigoBarras(const libHandle: PLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_RetornaCodigoBarras)(uintptr_t handle, int eIndice, const char* sResposta, int* esTamanho);

//function Boleto_EnviarBoleto(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*Boleto_EnviarBoleto)(uintptr_t handle, const char* sResposta, int* esTamanho);