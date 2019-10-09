// ACBrNFeImport.h - Contém as declarações das funções da ACBrLibNFe para carregamento dinâmico.

#include <cstdint>

// function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
typedef int (*NFE_Inicializar)(const char* eArqConfig, const char* eChaveCrypt);

// function NFE_Finalizar(): longint;
typedef int (*NFE_Finalizar)();

// function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Nome)(const char* sNome, int* esTamanho);

// function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Versao)(const char* sVersao, int* esTamanho);

// function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
typedef int (*NFE_UltimoRetorno)(const char* sMensagem, int* esTamanho);

// function NFE_ConfigLer(const eArqConfig: PChar): longint;
typedef int (*NFE_ConfigLer)(const char* eArqConfig);

// function NFE_ConfigGravar(const eArqConfig: PChar): longint;
typedef int (*NFE_ConfigGravar)(const char* eArqConfig);

// function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConfigLerValor)(const char* eSessao, const char* eChave, char* sValor, int* esTamanho);

// function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
typedef int (*NFE_ConfigGravarValor)(const char* eSessao, const char* eChave, const char* sValor);

// function NFE_CarregarXML(const eArquivoOuXML: PChar): longint;
typedef int (*NFE_CarregarXML)(const char* eArquivoOuXML);

// function NFE_CarregarINI(const eArquivoOuINI: PChar): longint;
typedef int (*NFE_CarregarINI)(const char* eArquivoOuINI);

// function NFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
typedef int (*NFE_CarregarEventoXML)(const char* eArquivoOuXML);

// function NFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
typedef int (*NFE_CarregarEventoINI)(const char* eArquivoOuINI);

// function NFE_LimparLista: longint;
typedef int (*NFE_LimparLista)();

// function NFE_LimparListaEventos: longint;
typedef int (*NFE_LimparListaEventos)();

// function NFE_Assinar: longint;
typedef int (*NFE_Assinar)();

// function NFE_Validar: longint;
typedef int (*NFE_Validar)();

// function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ValidarRegrasdeNegocios)(const char* sResposta, int* esTamanho);

// function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_VerificarAssinatura)(const char* sResposta, int* esTamanho);

// function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_StatusServico)(const char* sResposta, int* esTamanho);

// function NFE_Consultar(const eChaveOuNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Consultar)(const char* eChaveOuNFe, const char* sResposta, int* esTamanho);

// function NFE_Inutilizar(const ACNPJ, AJustificativa: PChar; Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
//  const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Inutilizar)(const char* ACNPJ, const char* AJustificativa, int Ano, int Modelo, int Serie, 
                             int NumeroInicial, int NumeroFinal, const char* sResposta, int* esTamanho);

// function NFE_Enviar(ALote: Integer; Imprimir, Sincrono, Zipado: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Enviar)(int ALote, bool Imprimir, bool Sincrono, bool Zipado, const char* sResposta, int* esTamanho);

// function NFE_ConsultarRecibo(ARecibo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConsultarRecibo)(const char* ARecibo, const char* sResposta, int* esTamanho);

// function NFE_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Cancelar)(const char* eChave, const char* eJustificativa, const char* eCNPJ, int ALote, const char* sResposta, int* esTamanho);

// function NFE_EnviarEvento(idLote: Integer; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_EnviarEvento)(int idLote, const char* sResposta, int* esTamanho);

// function NFE_ConsultaCadastro(cUF, nDocumento: PChar; nIE: boolean; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConsultaCadastro)(const char* cUF, const char* nDocumento, bool NIE, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorUltNSU)(int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorNSU)(int AcUFAutor, const char* eCNPJCPF, const char* eNSU, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorChave)(int AcUFAutor, const char* eCNPJCPF, const char* echNFe, const char* sResposta, int* esTamanho);

// function NFE_EnviarEmail(const ePara, eChaveNFe: PChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
typedef int (*NFE_EnviarEmail)(const char* ePara, const char* eChaveNFe, bool AEnviaPDF, const char* eAssunto, const char* eCC, 
                               const char* eAnexos, const char* eMensagem);

// function NFE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFe: PChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
typedef int (*NFE_EnviarEmailEvento)(const char* ePara, const char* eChaveEvento, const char* eChaveNFe, bool AEnviaPDF, const char* eAssunto,
                                     const char* eCC, const char* eAnexos, const char* eMensagem);

// function NFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado: PChar): longint;
typedef int (*NFE_Imprimir)(const char* cImpressora, int nNumCopias, const char* cProtocolo, const char* bMostrarPreview, const char* cMarcaDagua, 
                            const char* bViaConsumidor, const char* bSimplificado);

// function NFE_ImprimirPDF(): longint;
typedef int (*NFE_ImprimirPDF)();

// function NFE_ImprimirEvento(const eChaveNFe, eChaveEvento: PChar): longint;
typedef int (*NFE_ImprimirEvento)(const char* eChaveNFe, const char* eChaveEvento);

// function NFE_ImprimirEventoPDF(const eChaveNFe, eChaveEvento: PChar): longint;
typedef int (*NFE_ImprimirEventoPDF)(const char* eChaveNFe, const char* eChaveEvento);

// function NFE_ImprimirInutilizacao(const eChave: PChar): longint;
typedef int (*NFE_ImprimirInutilizacao)(const char* eChave);

// function NFE_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
typedef int (*NFE_ImprimirInutilizacaoPDF)(const char* eChave);