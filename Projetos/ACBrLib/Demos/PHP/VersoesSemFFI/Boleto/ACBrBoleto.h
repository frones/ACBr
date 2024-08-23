#include <cstdint>
#include <string>
#include <optional>
#include <vector>

#include "ACBrBoletoImport.h"

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

enum TpSaida{
	PDF,
	Impressora,
	EMail
};

class ACBrBoleto {
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
	ACBrBoleto(std::string eArqConfig, std::string eChaveCrypt);
	ACBrBoleto() : ACBrBoleto("", "") {};
	~ACBrBoleto();

	std::string Nome() const;
	std::string Versao() const;
	void ConfigLer(std::string eArqConfig) const;
	void ConfigGravar(std::string eArqConfig) const;
	void ConfigGravarValor(std::string eSessao, std::string eChave, std::string sValor) const;
	std::string ConfigLerValor(std::string eSessao, std::string eChave) const;
	void ConfigImportar(std::string eArqConfig) const;
	std::string ConfigExportar() const;

	void ConfigurarDados(std::string eArquivoIni) const;
	void IncluirTitulos(std::string eArquivoIni, std::optional<TpSaida> eTpSaida) const;
	void LimparLista() const;
	std::int32_t TotalTitulosLista() const;
	void Imprimir() const;
	void Imprimir(std::string eNomeImpressora) const;
	void ImprimirBoleto(std::int32_t idx) const;
	void ImprimirBoleto(std::int32_t idx, std::string eNomeImpressora) const;
	void GerarPDF() const;
	void GerarPDFBoleto(std::int32_t idx) const;
	void GerarHTML() const;
	void GerarRemessa(std::string eDir, std::int32_t eNumArquivo, std::string eNomeArq) const;
	void LerRetorno(std::string eDir, std::string eNomeArq) const;
	std::string ObterRetorno(std::string eDir, std::string eNomeArq) const;
	void EnviarEmail(std::string ePara, std::string eNomeArq, std::string eMensagem, std::vector<std::string> eCC) const;
	void EnviarEmailBoleto(std::int32_t idx, std::string ePara, std::string eNomeArq, std::string eMensagem, std::vector<std::string> eCC) const;
	void SetDiretorioArquivo(std::string eDir, std::string eNomeArq) const;
	std::vector<std::string> ListaBancos() const;
	std::vector<std::string> ListaCaractTitulo() const;
	std::vector<std::string> ListaOcorrencias() const;
	std::vector<std::string> ListaOcorrenciasEX() const;
	std::int32_t TamNossoNumero(std::string eCarteira, std::string enossoNumero, std::string eConvenio) const;
	std::string CodigosMoraAceitos() const;
	void SelecionaBanco(std::string eCodBanco) const;
	std::string MontarNossoNumero(std::int32_t idx) const;
	std::string RetornaLinhaDigitavel(std::int32_t idx) const;
	std::string RetornaCodigoBarras(std::int32_t idx) const;
	std::string EnviarBoleto() const;
};