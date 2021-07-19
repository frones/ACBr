#include <cstdint>
#include <string>
#include <optional>
#include <vector>
#include "ACBrNFeImport.h"

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
	uintptr_t libHandler;

	std::string Trim(std::string& buffer) const;
	static std::vector<std::string> Split(std::string str, std::string token);
	void CheckResult(std::int32_t ret) const;
	std::string ProcessResult(std::string buffer, std::int32_t buffer_len) const;

public:
	ACBrNFe(std::string eArqConfig, std::string eChaveCrypt);
	ACBrNFe() : ACBrNFe("", "") {};
	~ACBrNFe();

	std::string Nome() const;
	std::string Versao() const;
	void ConfigLer(std::string eArqConfig) const;
	void ConfigGravar(std::string eArqConfig) const;
	void ConfigGravarValor(std::string eSessao, std::string eChave, std::string sValor) const;
	std::string ConfigLerValor(std::string eSessao, std::string eChave) const;
	void ConfigImportar(std::string eArqConfig) const;
	std::string ConfigExportar() const;
	void CarregarXML(std::string eArquivoOuXML) const;
	void CarregarINI(std::string eArquivoOuINI) const;
	std::string ObterXml(std::int32_t AIndex) const;
	void GravarXml(std::int32_t AIndex, std::string eNomeArquivo, std::string ePathArquivo) const;
	void CarregarEventoXML(std::string eArquivoOuXML) const;
	void CarregarEventoINI(std::string eArquivoOuINI) const;
	void LimparLista() const;
	void LimparListaEventos() const;
	void Assinar() const;
	void Validar() const;
	std::string ValidarRegrasdeNegocios() const;
	std::string VerificarAssinatura() const;
	std::string GerarChave(std::int32_t ACodigoUF, std::int32_t ACodigoNumerico, std::int32_t AModelo,
		std::int32_t ASerie, std::int32_t ANumero, std::int32_t ATpEmi,
		std::string AEmissao, std::string ACNPJCPF) const;
	std::vector<std::string> ObterCertificados() const;
	std::string GetPath(std::int32_t ATipo) const;
	std::string GetPathEvento(std::string ACodEvento) const;
	std::string StatusServico() const;
	std::string Consultar(std::string eChaveOuNFe) const;
	std::string Inutilizar(std::string ACNPJ, std::string AJustificativa, std::int32_t Ano, std::int32_t Modelo,
		std::int32_t Serie, std::int32_t NumeroInicial, std::int32_t NumeroFinal) const;
	std::string Enviar(std::int32_t ALote, bool Imprimir, bool Sincrono, bool Zipado) const;
	std::string ConsultarRecibo(std::string ARecibo) const;
	std::string Cancelar(std::string eChave, std::string eJustificativa, std::string eCNPJ, std::int32_t ALote) const;
	std::string EnviarEvento(std::int32_t idLote) const;
	std::string ConsultaCadastro(std::string cUF, std::string nDocumento, bool IsIE) const;
	std::string DistribuicaoDFePorUltNSU(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string eultNSU) const;
	std::string DistribuicaoDFePorNSU(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string eNSU) const;
	std::string DistribuicaoDFePorChave(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string echNFe) const;
	void EnviarEmail(std::string ePara, std::string eXmlNFe, bool AEnviaPDF, std::string eAssunto, std::string eCC,
		std::string eAnexos, std::string eMensagem) const;
	void EnviarEmailEvento(std::string ePara, std::string eXmlEvento, std::string eXmlNFe, bool AEnviaPDF, std::string eAssunto,
		std::string eCC, std::string eAnexos, std::string eMensagem) const;
	void Imprimir(std::string cImpressora, int nNumCopias, std::string cProtocolo, std::optional<bool> bMostrarPreview,
		std::optional<bool> cMarcaDagua, std::optional<bool> bViaConsumidor, std::optional<bool> bSimplificado) const;
	void ImprimirPDF() const;
	void ImprimirEvento(std::string eXmlNFe, std::string eXmlEvento) const;
	void ImprimirEventoPDF(std::string eXmlNFe, std::string eXmlEvento) const;
	void ImprimirInutilizacao(std::string eXmlInutilizacao) const;
	void ImprimirInutilizacaoPDF(std::string eXmlInutilizacao) const;
};