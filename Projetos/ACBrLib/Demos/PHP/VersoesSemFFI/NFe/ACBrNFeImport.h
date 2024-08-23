// ACBrNFeImport.h - Contém as declarações das funções da ACBrLibNFe para carregamento dinâmico.

#include <cstdint>

// function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
typedef int (*NFE_Inicializar)(uintptr_t* handle, const char* eArqConfig, const char* eChaveCrypt);

// function NFE_Finalizar(): longint;
typedef int (*NFE_Finalizar)(uintptr_t handle);

// function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Nome)(uintptr_t handle, const char* sNome, int* esTamanho);

// function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Versao)(uintptr_t handle, const char* sVersao, int* esTamanho);

// function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
typedef int (*NFE_UltimoRetorno)(uintptr_t handle, const char* sMensagem, int* esTamanho);

// function NFE_ConfigLer(const eArqConfig: PChar): longint;
typedef int (*NFE_ConfigLer)(uintptr_t handle, const char* eArqConfig);

// function NFE_ConfigGravar(const eArqConfig: PChar): longint;
typedef int (*NFE_ConfigGravar)(uintptr_t handle, const char* eArqConfig);

// function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConfigLerValor)(uintptr_t handle, const char* eSessao, const char* eChave, const char* sValor, int* esTamanho);

// function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
typedef int (*NFE_ConfigGravarValor)(uintptr_t handle, const char* eSessao, const char* eChave, const char* sValor);

// function NFE_ConfigImportar(const eArqConfig: PChar): longint;
typedef int (*NFE_ConfigImportar)(uintptr_t handle, const char* eArqConfig);

// function NFE_ConfigExportar(const libHandle: PLibHandle; const sValor: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConfigExportar)(uintptr_t handle, const char* sValor, int* esTamanho);

// function NFE_CarregarXML(const eArquivoOuXML: PChar): longint;
typedef int (*NFE_CarregarXML)(uintptr_t handle, const char* eArquivoOuXML);

// function NFE_CarregarINI(const eArquivoOuINI: PChar): longint;
typedef int (*NFE_CarregarINI)(uintptr_t handle, const char* eArquivoOuINI);

// function NFE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ObterXml)(uintptr_t handle, int AIndex, const char* sMensagem, int* esTamanho);

// function NFE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
typedef int (*NFE_GravarXml)(uintptr_t handle, int AIndex, const char* eNomeArquivo, const char* ePathArquivo);

// function NFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
typedef int (*NFE_CarregarEventoXML)(uintptr_t handle, const char* eArquivoOuXML);

// function NFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
typedef int (*NFE_CarregarEventoINI)(uintptr_t handle, const char* eArquivoOuINI);

// function NFE_LimparLista: longint;
typedef int (*NFE_LimparLista)(uintptr_t handle);

// function NFE_LimparListaEventos: longint;
typedef int (*NFE_LimparListaEventos)(uintptr_t handle);

// function NFE_Assinar: longint;
typedef int (*NFE_Assinar)(uintptr_t handle);

// function NFE_Validar: longint;
typedef int (*NFE_Validar)(uintptr_t handle);

// function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ValidarRegrasdeNegocios)(uintptr_t handle, const char* sResposta, int* esTamanho);

// function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_VerificarAssinatura)(uintptr_t handle, const char* sResposta, int* esTamanho);

// function NFE_GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: longint;
// AEmissao, ACNPJCPF: PChar; const sResposta : PChar; var esTamanho : longint): longint;
typedef int (*NFE_GerarChave)(uintptr_t handle, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie,
	int ANumero, int ATpEmi, const char* AEmissao, const char* ACNPJCPF, const char* sResposta, int* esTamanho);

//function NFE_ObterCertificados(const libHandle : PLibHandle; const sResposta : PChar; var esTamanho : longint) : longint;
typedef int (*NFE_ObterCertificados)(uintptr_t handle, const char* sResposta, int* esTamanho);

//function NFE_GetPath(const libHandle : PLibHandle; ATipo: longint; const sResposta : PChar; var esTamanho : longint) : longint;
typedef int (*NFE_GetPath)(uintptr_t handle, int ATipo, const char* sResposta, int* esTamanho);

//function NFE_GetPathEvento(const libHandle : PLibHandle; ACodEvento: PChar; const sResposta : PChar; var esTamnho : longint) : longint;
typedef int (*NFE_GetPathEvento)(uintptr_t handle, const char* ACodEvento, const char* sResposta, int* esTamanho);

// function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_StatusServico)(uintptr_t handle, const char* sResposta, int* esTamanho);

// function NFE_Consultar(const eChaveOuNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Consultar)(uintptr_t handle, const char* eChaveOuNFe, const char* sResposta, int* esTamanho);

// function NFE_Inutilizar(const ACNPJ, AJustificativa: PChar; Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
//  const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Inutilizar)(uintptr_t handle, const char* ACNPJ, const char* AJustificativa, int Ano, int Modelo, int Serie,
	int NumeroInicial, int NumeroFinal, const char* sResposta, int* esTamanho);

// function NFE_Enviar(ALote: Integer; Imprimir, Sincrono, Zipado: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Enviar)(uintptr_t handle, int ALote, bool Imprimir, bool Sincrono, bool Zipado, const char* sResposta, int* esTamanho);

// function NFE_ConsultarRecibo(ARecibo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConsultarRecibo)(uintptr_t handle, const char* ARecibo, const char* sResposta, int* esTamanho);

// function NFE_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_Cancelar)(uintptr_t handle, const char* eChave, const char* eJustificativa, const char* eCNPJ, int ALote, const char* sResposta, int* esTamanho);

// function NFE_EnviarEvento(idLote: Integer; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_EnviarEvento)(uintptr_t handle, int idLote, const char* sResposta, int* esTamanho);

// function NFE_ConsultaCadastro(cUF, nDocumento: PChar; nIE: boolean; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_ConsultaCadastro)(uintptr_t handle, const char* cUF, const char* nDocumento, bool IsIE, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorUltNSU)(uintptr_t handle, int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorNSU)(uintptr_t handle, int AcUFAutor, const char* eCNPJCPF, const char* eNSU, const char* sResposta, int* esTamanho);

// function NFE_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
typedef int (*NFE_DistribuicaoDFePorChave)(uintptr_t handle, int AcUFAutor, const char* eCNPJCPF, const char* echNFe, const char* sResposta, int* esTamanho);

// function NFE_EnviarEmail(const ePara, eChaveNFe: PChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
typedef int (*NFE_EnviarEmail)(uintptr_t handle, const char* ePara, const char* eXmlNFe, bool AEnviaPDF, const char* eAssunto, const char* eCC,
	const char* eAnexos, const char* eMensagem);

// function NFE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFe: PChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
typedef int (*NFE_EnviarEmailEvento)(uintptr_t handle, const char* ePara, const char* eXmlEvento, const char* eXmlNFe, bool AEnviaPDF, const char* eAssunto,
	const char* eCC, const char* eAnexos, const char* eMensagem);

// function NFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado: PChar): longint;
typedef int (*NFE_Imprimir)(uintptr_t handle, const char* cImpressora, int nNumCopias, const char* cProtocolo, const char* bMostrarPreview, const char* cMarcaDagua,
	const char* bViaConsumidor, const char* bSimplificado);

// function NFE_ImprimirPDF(): longint;
typedef int (*NFE_ImprimirPDF)(uintptr_t handle);

// function NFE_ImprimirEvento(const eChaveNFe, eChaveEvento: PChar): longint;
typedef int (*NFE_ImprimirEvento)(uintptr_t handle, const char* eXmlNFe, const char* eXmlEvento);

// function NFE_ImprimirEventoPDF(const eChaveNFe, eChaveEvento: PChar): longint;
typedef int (*NFE_ImprimirEventoPDF)(uintptr_t handle, const char* eXmlNFe, const char* eXmlEvento);

// function NFE_ImprimirInutilizacao(const eChave: PChar): longint;
typedef int (*NFE_ImprimirInutilizacao)(uintptr_t handle, const char* eXmlInutilizacao);

// function NFE_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
typedef int (*NFE_ImprimirInutilizacaoPDF)(uintptr_t handle, const char* eXmlInutilizacao);