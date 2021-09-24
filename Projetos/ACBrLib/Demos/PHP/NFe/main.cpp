#include <iostream>
#include <dlfcn.h>
#include <stdio.h>
#include <memory>
#include <phpcpp.h>
#include "ACBrNFe.h"

class AcbrLibNFe : public Php::Base
{
 private:
   std::shared_ptr<ACBrNFe> nfe;

 public:
	AcbrLibNFe() = default;
	virtual ~AcbrLibNFe() = default;

	void __construct(Php::Parameters &params){
        Php::Value self(this);
        std::string eChaveCrypt = params.size() >= 1 ? params[0] : "";

        try
        {
            nfe = std::make_shared<ACBrNFe>("[Memory]", eChaveCrypt);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }  
    }

	Php::Value __get(const Php::Value &name){
        Php::Value self(this);
        
        try
        {
            // check if the property name is supported
            if (name == "Nome") return nfe->Nome();
            if (name == "Versao") return nfe->Versao();
                   
            // property not supported, fall back on default
            return Php::Base::__get(name);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void ConfigGravarValor(Php::Parameters &params){
        Php::Value self(this);
        
        if(params.size() != 3) throw Php::Exception("Número errado de parâmetros.");

        std::string eSessao = params[0];
        std::string eChave = params[1];
        std::string sValor = params[2];
        
        try
        {
            nfe->ConfigGravarValor(eSessao, eChave, sValor);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value ConfigLerValor(Php::Parameters &params){
        Php::Value self(this);
        
        if(params.size() != 2) throw Php::Exception("Número errado de parâmetros.");
        std::string eSessao = params[0];
        std::string eChave = params[1];

        try
        {
            return nfe->ConfigLerValor(eSessao, eChave);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void ConfigImportar(Php::Parameters &params){
        Php::Value self(this);
        
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");
        std::string eArqConfig = params[0];

        try
        {
            nfe->ConfigImportar(eArqConfig);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ConfigExportar(){
        Php::Value self(this);

        try
        {
            return nfe->ConfigExportar();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	void CarregarINI(Php::Parameters &params){
        Php::Value self(this);
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eArquivo = params[0];

        try
        {
            return nfe->CarregarINI(eArquivo);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	void CarregarXML(Php::Parameters &params){  
        Php::Value self(this);      
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eArquivo = params[0];

        try
        {
            return nfe->CarregarXML(eArquivo);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }       
    }
    
	Php::Value ObterXml(Php::Parameters &params){
        Php::Value self(this);
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t idx = params[0];

        try
        {
            return nfe->ObterXml(idx);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	void CarregarEventoINI(Php::Parameters &params){
        Php::Value self(this);
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eArquivo = params[0];

        try
        {
            nfe->CarregarEventoINI(eArquivo);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	void CarregarEventoXML(Php::Parameters &params){
        Php::Value self(this);
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eArquivo = params[0];

        try
        {
            nfe->CarregarEventoXML(eArquivo);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }       
    }

	void LimparLista(){
        Php::Value self(this);

        try
        {
            nfe->LimparLista();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	void LimparListaEventos(){
        Php::Value self(this);

        try
        {
            nfe->LimparListaEventos();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }         
    }

	void Assinar(){
        Php::Value self(this);

        try
        {
            nfe->Assinar();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void Validar(){
        Php::Value self(this);

        try
        {
            nfe->Validar();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value ValidarRegrasdeNegocios(){
        Php::Value self(this);

        try
        {
            return nfe->ValidarRegrasdeNegocios();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value VerificarAssinatura(){
        Php::Value self(this);

        try
        {
            return nfe->VerificarAssinatura();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value GerarChave(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 8) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t ACodigoUF = params[0];
        std::int32_t ACodigoNumerico = params[1]; 
	    std::int32_t AModelo = params[2]; 
	    std::int32_t ASerie = params[3]; 
	    std::int32_t ANumero = params[4]; 
	    std::int32_t ATpEmi = params[5]; 
	    std::string AEmissao = params[6]; 
	    std::string ACNPJCPF = params[7];

        try
        {
            return nfe->GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value StatusServico(){
        Php::Value self(this);

        try
        {
            return nfe->StatusServico();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value Consultar(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eChaveOuNFe = params[0]; 

        try
        {
            return nfe->Consultar(eChaveOuNFe);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value Inutilizar(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 7) throw Php::Exception("Número errado de parâmetros.");

        std::string ACNPJ = params[0]; 
	    std::string AJustificativa = params[1]; 
	    std::int32_t Ano = params[2]; 
	    std::int32_t Modelo = params[3]; 
	    std::int32_t Serie = params[4]; 
	    std::int32_t NumeroInicial = params[5]; 
	    std::int32_t NumeroFinal = params[6];

        try
        {
            return nfe->Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, NumeroInicial, NumeroFinal);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value Enviar(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t ALote = params[0];
	    bool Sincrono = params.size() >= 2 ? params[2].boolValue() : false;
        bool Zipado = params.size() >= 3 ? params[3].boolValue() : false;
        
        try
        {
            return nfe->Enviar(ALote, false, Sincrono, Zipado);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value ConsultarRecibo(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string ARecibo = params[0];

        try
        {
            return nfe->ConsultarRecibo(ARecibo);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value Cancelar(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 4) throw Php::Exception("Número errado de parâmetros.");

        std::string eChave = params[0];
	    std::string eJustificativa = params[1];
	    std::string eCNPJCPF = params[2];
	    std::int32_t ALote = params[3];

        try
        {
            return nfe->Cancelar(eChave, eJustificativa, eCNPJCPF, ALote);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

	Php::Value EnviarEvento(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t ALote = params[0];

        try
        {
            return nfe->EnviarEvento(ALote);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value ConsultaCadastro(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() < 2) throw Php::Exception("Número errado de parâmetros.");

        std::string cUF = params[0];
        std::string nDocumento = params[1];
        bool IsIE = params.size() >= 3 ? params[2].boolValue() : false;

        try
        {
            return nfe->ConsultaCadastro(cUF, nDocumento, IsIE);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value DistribuicaoDFePorUltNSU(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 3) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t AcUFAutor = params[0];
	    std::string eCNPJCPF = params[1];
	    std::string eultNSU = params[2];

        try
        {
            return nfe->DistribuicaoDFePorUltNSU(AcUFAutor, eCNPJCPF, eultNSU);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value DistribuicaoDFePorNSU(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 3) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t AcUFAutor = params[0];
	    std::string eCNPJCPF = params[1];
	    std::string eNSU = params[2];

        try
        {
            return nfe->DistribuicaoDFePorNSU(AcUFAutor, eCNPJCPF, eNSU);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	Php::Value DistribuicaoDFePorChave(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 3) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t AcUFAutor = params[0];
	    std::string eCNPJCPF = params[1];
	    std::string echNFe = params[2];

        try
        {
            return nfe->DistribuicaoDFePorChave(AcUFAutor, eCNPJCPF, echNFe);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void EnviarEmail(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 6) throw Php::Exception("Número errado de parâmetros.");

        std::string ePara = params[0];
	    std::string eXmlNFe = params[1];
	    bool AEnviaPDF = params[2];
	    std::string eAssunto = params[3];
	    std::string eCC = params[4];
	    std::string eAnexos = params[4];
	    std::string eMensagem = params[5];

        try
        {
            return nfe->EnviarEmail(ePara, eXmlNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void EnviarEmailEvento(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 8) throw Php::Exception("Número errado de parâmetros.");

        std::string ePara = params[0];
	    std::string eXmlEvento = params[1];
	    std::string eXmlNFe = params[2];
	    bool AEnviaPDF = params[3];
	    std::string eAssunto = params[4];
	    std::string eCC = params[5];
	    std::string eAnexos = params[6];
	    std::string eMensagem = params[7];

        try
        {
            return nfe->EnviarEmailEvento(ePara, eXmlEvento, eXmlNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void ImprimirPDF(){
        Php::Value self(this);

        try
        {
            return nfe->ImprimirPDF();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void ImprimirEventoPDF(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 2) throw Php::Exception("Número errado de parâmetros.");

        std::string eXmlNFe = params[0];
        std::string eXmlEvento = params[1];

        try
        {
            nfe->ImprimirEventoPDF(eXmlNFe, eXmlEvento);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }

	void ImprimirInutilizacaoPDF(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eXmlInutilizacao = params[0];

        try
        {
            nfe->ImprimirInutilizacaoPDF(eXmlInutilizacao);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }
    }
};


/**
 *  tell the compiler that the get_module is a pure C function
 */
extern "C" {
    
    /**
     *  Function that is called by PHP right after the PHP process
     *  has started, and that returns an address of an internal PHP
     *  strucure with all the details and features of your extension
     *
     *  @return void*   a pointer to an address that is understood by PHP
     */
    PHPCPP_EXPORT void *get_module() 
    {
        //putenv((char *)"DISPLAY=:99");
        
        // static(!) Php::Extension object that should stay in memory
        // for the entire duration of the process (that's why it's static)
        static Php::Extension extension("ACBrLibNFe", "1.0");

        // Instanciando uma instancia fixa para não ter problemas de impressão, por causa do erro da LCL.
        static std::shared_ptr<ACBrNFe> nfe = std::make_shared<ACBrNFe>("[Memory]", "");
        
        // description of the class so that PHP knows which methods are accessible
        Php::Class<AcbrLibNFe> NFe("ACBrNFe");

        // alternative way to make an object unclonable
        NFe.method("__clone", Php::Private);

        // register the methods
        NFe.method<&AcbrLibNFe::__construct>("__construct", {
            Php::ByVal("eChaveCrypt", Php::Type::String, false)
        });

        NFe.method<&AcbrLibNFe::ConfigGravarValor>("ConfigGravarValor", {
            Php::ByVal("eSessao", Php::Type::String),
            Php::ByVal("eChave", Php::Type::String),
            Php::ByVal("sValor", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ConfigLerValor>("ConfigLerValor", {
            Php::ByVal("eSessao", Php::Type::String),
            Php::ByVal("eChave", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ConfigImportar>("ConfigImportar", {
            Php::ByVal("eArqConfig", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ConfigExportar>("ConfigExportar");

        NFe.method<&AcbrLibNFe::CarregarINI>("CarregarINI", {
            Php::ByVal("eArquivoOuINI", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::CarregarXML>("CarregarXML", {
            Php::ByVal("eArquivoOuXml", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ObterXml>("ObterXml", {
            Php::ByVal("AIndex", Php::Type::Numeric)
        });

        NFe.method<&AcbrLibNFe::CarregarEventoINI>("CarregarEventoINI", {
            Php::ByVal("eArquivoOuINI", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::CarregarEventoXML>("CarregarEventoXML", {
            Php::ByVal("eArquivoOuXML", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::LimparLista>("LimparLista");
        NFe.method<&AcbrLibNFe::LimparListaEventos>("LimparListaEvento");
        NFe.method<&AcbrLibNFe::Assinar>("Assinar");
        NFe.method<&AcbrLibNFe::Validar>("Validar");
        NFe.method<&AcbrLibNFe::ValidarRegrasdeNegocios>("ValidarRegrasdeNegocios");
        NFe.method<&AcbrLibNFe::VerificarAssinatura>("VerificarAssinatura");
        
        NFe.method<&AcbrLibNFe::GerarChave>("GerarChave", {
            Php::ByVal("ACodigoUF", Php::Type::Numeric),
            Php::ByVal("ACodigoNumerico", Php::Type::Numeric),
            Php::ByVal("AModelo", Php::Type::Numeric),
            Php::ByVal("ASerie", Php::Type::Numeric),
            Php::ByVal("ANumero", Php::Type::Numeric),
            Php::ByVal("ATpEmi", Php::Type::Numeric),
            Php::ByVal("AEmissao", Php::Type::String),
            Php::ByVal("ACNPJCPF", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::StatusServico>("StatusServico");
        NFe.method<&AcbrLibNFe::Consultar>("Consultar", {
            Php::ByVal("eChaveOuNFe", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::Inutilizar>("Inutilizar", {
            Php::ByVal("ACNPJ", Php::Type::String),
            Php::ByVal("AJustificativa", Php::Type::String),
            Php::ByVal("Ano", Php::Type::Numeric),
            Php::ByVal("Modelo", Php::Type::Numeric),
            Php::ByVal("Serie", Php::Type::Numeric),
            Php::ByVal("NumeroInicial", Php::Type::Numeric),
            Php::ByVal("NumeroFinal", Php::Type::Numeric)           
        });

        NFe.method<&AcbrLibNFe::Enviar>("Enviar", {
            Php::ByVal("ALote", Php::Type::Numeric),
            Php::ByVal("Sincrono", Php::Type::Bool, false),
            Php::ByVal("Zipado", Php::Type::Bool, false),
        });

        NFe.method<&AcbrLibNFe::ConsultarRecibo>("ConsultarRecibo", {
            Php::ByVal("ARecibo", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::Cancelar>("Cancelar", {
            Php::ByVal("eChave", Php::Type::String),
            Php::ByVal("eJustificativa", Php::Type::String),
            Php::ByVal("eCNPJCPF", Php::Type::String),
            Php::ByVal("ALote", Php::Type::Numeric)
        });

        NFe.method<&AcbrLibNFe::EnviarEvento>("EnviarEvento", {
            Php::ByVal("ALote", Php::Type::Numeric)
        });

        NFe.method<&AcbrLibNFe::ConsultaCadastro>("ConsultaCadastro", {
            Php::ByVal("cUF", Php::Type::String),
            Php::ByVal("nDocumento", Php::Type::String),
            Php::ByVal("IsIE", Php::Type::Bool, false)
        });

        NFe.method<&AcbrLibNFe::DistribuicaoDFePorUltNSU>("DistribuicaoDFePorUltNSU", {
            Php::ByVal("AcUFAutor", Php::Type::Numeric),
            Php::ByVal("eCNPJCPF", Php::Type::String),
            Php::ByVal("eultNSU", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::DistribuicaoDFePorNSU>("DistribuicaoDFePorNSU", {
            Php::ByVal("AcUFAutor", Php::Type::Numeric),
            Php::ByVal("eCNPJCPF", Php::Type::String),
            Php::ByVal("eNSU", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::DistribuicaoDFePorChave>("DistribuicaoDFePorChave", {
            Php::ByVal("AcUFAutor", Php::Type::Numeric),
            Php::ByVal("eCNPJCPF", Php::Type::String),
            Php::ByVal("echNFe", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::EnviarEmail>("EnviarEmail", {
            Php::ByVal("ePara", Php::Type::String),
            Php::ByVal("eXmlNFe", Php::Type::String),
            Php::ByVal("AEnviaPDF", Php::Type::Bool),
            Php::ByVal("eAssunto", Php::Type::String),
            Php::ByVal("eCC", Php::Type::String),
            Php::ByVal("eAnexos", Php::Type::String),
            Php::ByVal("eMensagem", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::EnviarEmailEvento>("EnviarEmailEvento", {
            Php::ByVal("ePara", Php::Type::String),
            Php::ByVal("eXmlEvento", Php::Type::String),
            Php::ByVal("eXmlNFe", Php::Type::String),
            Php::ByVal("AEnviaPDF", Php::Type::Bool),
            Php::ByVal("eAssunto", Php::Type::String),
            Php::ByVal("eCC", Php::Type::String),
            Php::ByVal("eAnexos", Php::Type::String),
            Php::ByVal("eMensagem", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ImprimirPDF>("ImprimirPDF");

        NFe.method<&AcbrLibNFe::ImprimirEventoPDF>("ImprimirEventoPDF", {
            Php::ByVal("eXmlNFe", Php::Type::String),
            Php::ByVal("eXmlEvento", Php::Type::String)
        });

        NFe.method<&AcbrLibNFe::ImprimirInutilizacaoPDF>("ImprimirInutilizacaoPDF", {
            Php::ByVal("eXmlInutilizacao", Php::Type::String)
        });

        // add the class to the extension
        extension.add(std::move(NFe));

        return extension;
    }
}
