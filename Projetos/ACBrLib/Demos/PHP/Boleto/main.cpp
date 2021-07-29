#include <iostream>
#include <dlfcn.h>
#include <stdio.h>
#include <memory>
#include <cstdint>
#include <phpcpp.h>
#include "ACBrBoleto.h"

class AcbrLibBoleto : public Php::Base
{
 private:
   std::shared_ptr<ACBrBoleto> bol;

 public:
	AcbrLibBoleto() = default;
	virtual ~AcbrLibBoleto() = default;

	void __construct(Php::Parameters &params){
        Php::Value self(this);
        std::string eChaveCrypt = params.size() >= 1 ? params[0] : "";

        try
        {
            bol = std::make_shared<ACBrBoleto>("[Memory]", eChaveCrypt);
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
            if (name == "Nome") return bol->Nome();
            if (name == "Versao") return bol->Versao();
                   
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
            bol->ConfigGravarValor(eSessao, eChave, sValor);
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
            return bol->ConfigLerValor(eSessao, eChave);
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
            bol->ConfigImportar(eArqConfig);
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
            return bol->ConfigExportar();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void ConfigurarDados(Php::Parameters &params){
        Php::Value self(this);
        
        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");
        std::string eArquivoIni = params[0];

        try
        {
            bol->ConfigurarDados(eArquivoIni);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void IncluirTitulos(Php::Parameters &params){
        Php::Value self(this);
        
        if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");
        
        std::string eArquivoIni = params[0];

        std::optional<TpSaida> eTpSaida = std::nullopt;
        if(params.size() == 2)
            eTpSaida = (TpSaida)params[1].numericValue();                  

        try
        {
            bol->IncluirTitulos(eArquivoIni, eTpSaida);
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
            bol->LimparLista();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value TotalTitulosLista(){
        Php::Value self(this);                

        try
        {
            return bol->TotalTitulosLista();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void GerarPDF(){
        Php::Value self(this);                

        try
        {
            bol->GerarPDF();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void GerarPDFBoleto(Php::Parameters &params){
        Php::Value self(this);    

        if(params.size() != 1) throw Php::Exception("Número errado de parâmetros.");
        std::int32_t idx = params[0];            

        try
        {
            bol->GerarPDFBoleto(idx);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void GerarHTML(){
        Php::Value self(this);                

        try
        {
            bol->GerarHTML();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void GerarRemessa(Php::Parameters &params){
        Php::Value self(this);    

        std::string eDir = params.size() >= 1 ? (std::string)params[0] : ""; 
        std::int32_t eNumArquivo = params.size() >= 2 ? (std::int32_t)params[1] : 0; 
        std::string eNomeArq = params.size() == 3 ? (std::string)params[2] : "";           

        try
        {
            bol->GerarRemessa(eDir, eNumArquivo, eNomeArq);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void LerRetorno(Php::Parameters &params){
        Php::Value self(this);    

        std::string eDir = params.size() >= 1 ? (std::string)params[0] : ""; 
        std::string eNomeArq = params.size() == 2 ? (std::string)params[1] : "";           

        try
        {
            bol->LerRetorno(eDir, eNomeArq);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ObterRetorno(Php::Parameters &params){
        Php::Value self(this);    

        std::string eDir = params.size() >= 1 ? (std::string)params[0] : ""; 
        std::string eNomeArq = params.size() == 2 ? (std::string)params[1] : "";           

        try
        {
            return bol->ObterRetorno(eDir, eNomeArq);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void EnviarEmail(Php::Parameters &params){
        Php::Value self(this);    

        if(params.size() < 3) throw Php::Exception("Número errado de parâmetros.");

        std::string ePara = params[0];         
        std::string eNomeArq = params[1];         
        std::string eMensagem = params[2];         
        std::vector<std::string> eCC = params.size() == 4 ? (std::vector<std::string>)params[3] : std::vector<std::string>(0);          

        try
        {
            bol->EnviarEmail(ePara, eNomeArq, eMensagem, eCC);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void EnviarEmailBoleto(Php::Parameters &params){
        Php::Value self(this);    

        if(params.size() < 4) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t idx = params[0];         
        std::string ePara = params[1];         
        std::string eNomeArq = params[2];         
        std::string eMensagem = params[3];         
        std::vector<std::string> eCC = params.size() == 5 ? (std::vector<std::string>)params[4] : std::vector<std::string>(0);          

        try
        {
            bol->EnviarEmailBoleto(idx, ePara, eNomeArq, eMensagem, eCC);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void SetDiretorioArquivo(Php::Parameters &params){
        Php::Value self(this);    

        if(params.size() < 2) throw Php::Exception("Número errado de parâmetros.");

        std::string eDir = params[0]; 
        std::string eNomeArq = params[1];           

        try
        {
            bol->SetDiretorioArquivo(eDir, eNomeArq);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ListaBancos(){
        Php::Value self(this);    

        try
        {
            return bol->ListaBancos();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ListaCaractTitulo(){
        Php::Value self(this);    

        try
        {
            return bol->ListaCaractTitulo();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ListaOcorrencias(){
        Php::Value self(this);    

        try
        {
            return bol->ListaOcorrencias();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value ListaOcorrenciasEX(){
        Php::Value self(this);    

        try
        {
            return bol->ListaOcorrenciasEX();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value TamNossoNumero(Php::Parameters &params){
        Php::Value self(this);

        if(params.size() < 3) throw Php::Exception("Número errado de parâmetros.");

        std::string eCarteira = params[0];         
        std::string enossoNumero = params[1];         
        std::string eConvenio = params[2];  

        try
        {
            return bol->TamNossoNumero(eCarteira, enossoNumero, eConvenio);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value CodigosMoraAceitos(){
        Php::Value self(this);

        try
        {
            return bol->CodigosMoraAceitos();
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    void SelecionaBanco(Php::Parameters &params){
        Php::Value self(this);    

        if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");

        std::string eCodBanco = params[0];         

        try
        {
            bol->SelecionaBanco(eCodBanco);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value MontarNossoNumero(Php::Parameters &params){
        Php::Value self(this);

         if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t idx = params[0];

        try
        {
            return bol->MontarNossoNumero(idx);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value RetornaLinhaDigitavel(Php::Parameters &params){
        Php::Value self(this);

         if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t idx = params[0];

        try
        {
            return bol->RetornaLinhaDigitavel(idx);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value RetornaCodigoBarras(Php::Parameters &params){
        Php::Value self(this);

         if(params.size() < 1) throw Php::Exception("Número errado de parâmetros.");

        std::int32_t idx = params[0];

        try
        {
            return bol->RetornaCodigoBarras(idx);
        }
        catch(const std::exception& e)
        {
            throw Php::Exception(e.what());
        }        
    }

    Php::Value EnviarBoleto(){
        Php::Value self(this);

        try
        {
            return bol->EnviarBoleto();
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
        putenv((char *)"DISPLAY=:99");
        
        // static(!) Php::Extension object that should stay in memory
        // for the entire duration of the process (that's why it's static)
        static Php::Extension extension("ACBrLibBoleto", "1.0");

        // Instanciando uma instancia fixa para não ter problemas de impressão, por causa do erro da LCL.
        static std::shared_ptr<ACBrBoleto> bol = std::make_shared<ACBrBoleto>("[Memory]", "");
        
        // description of the class so that PHP knows which methods are accessible
        Php::Class<AcbrLibBoleto> Boleto("ACBrBoleto");

        // alternative way to make an object unclonable
        Boleto.method("__clone", Php::Private);

        // register the methods
        Boleto.method<&AcbrLibBoleto::__construct>("__construct", {
            Php::ByVal("eChaveCrypt", Php::Type::String, false)
        });

        Boleto.method<&AcbrLibBoleto::ConfigGravarValor>("ConfigGravarValor", {
            Php::ByVal("eSessao", Php::Type::String),
            Php::ByVal("eChave", Php::Type::String),
            Php::ByVal("sValor", Php::Type::String)
        });

        Boleto.method<&AcbrLibBoleto::ConfigLerValor>("ConfigLerValor", {
            Php::ByVal("eSessao", Php::Type::String),
            Php::ByVal("eChave", Php::Type::String)
        });

        Boleto.method<&AcbrLibBoleto::ConfigImportar>("ConfigImportar", {
            Php::ByVal("eArqConfig", Php::Type::String)
        });

        Boleto.method<&AcbrLibBoleto::ConfigExportar>("ConfigExportar");

        Boleto.method<&AcbrLibBoleto::ConfigurarDados>("ConfigurarDados", {
            Php::ByVal("eArquivoIni", Php::Type::String)
        });

        Boleto.method<&AcbrLibBoleto::IncluirTitulos>("IncluirTitulos", {
            Php::ByVal("eArquivoIni", Php::Type::String),
            Php::ByVal("TpSaida", Php::Type::Numeric, false)
        });

        Boleto.method<&AcbrLibBoleto::LimparLista>("LimparLista");

        Boleto.method<&AcbrLibBoleto::TotalTitulosLista>("TotalTitulosLista");

        Boleto.method<&AcbrLibBoleto::GerarPDF>("GerarPDF");

        Boleto.method<&AcbrLibBoleto::GerarPDFBoleto>("GerarPDFBoleto", {
            Php::ByVal("idx", Php::Type::Numeric)
        });
        
        Boleto.method<&AcbrLibBoleto::GerarHTML>("GerarHTML");

        Boleto.method<&AcbrLibBoleto::GerarRemessa>("GerarRemessa", {
            Php::ByVal("eDir", Php::Type::String, false),
            Php::ByVal("eNumArquivo", Php::Type::Numeric, false),
            Php::ByVal("eNomeArq", Php::Type::String, false),
        });

        Boleto.method<&AcbrLibBoleto::LerRetorno>("LerRetorno", {
            Php::ByVal("eDir", Php::Type::String, false),
            Php::ByVal("eNomeArq", Php::Type::String, false),
        });

        Boleto.method<&AcbrLibBoleto::ObterRetorno>("ObterRetorno", {
            Php::ByVal("eDir", Php::Type::String, false),
            Php::ByVal("eNomeArq", Php::Type::String, false),
        });

        Boleto.method<&AcbrLibBoleto::EnviarEmail>("EnviarEmail", {
            Php::ByVal("ePara", Php::Type::String),
            Php::ByVal("eNomeArq", Php::Type::String),
            Php::ByVal("eMensagem", Php::Type::String),
            Php::ByVal("eCC", Php::Type::Array, false),
        });

        Boleto.method<&AcbrLibBoleto::EnviarEmailBoleto>("EnviarEmailBoleto", {
            Php::ByVal("idx", Php::Type::Numeric),
            Php::ByVal("ePara", Php::Type::String),
            Php::ByVal("eNomeArq", Php::Type::String),
            Php::ByVal("eMensagem", Php::Type::String),
            Php::ByVal("eCC", Php::Type::Array, false),
        });

        Boleto.method<&AcbrLibBoleto::SetDiretorioArquivo>("SetDiretorioArquivo", {
            Php::ByVal("eDir", Php::Type::String),
            Php::ByVal("eNomeArq", Php::Type::String),
        });

        Boleto.method<&AcbrLibBoleto::ListaBancos>("ListaBancos");

        Boleto.method<&AcbrLibBoleto::ListaCaractTitulo>("ListaCaractTitulo");

        Boleto.method<&AcbrLibBoleto::ListaOcorrencias>("ListaOcorrencias");

        Boleto.method<&AcbrLibBoleto::ListaOcorrenciasEX>("ListaOcorrenciasEX");

        Boleto.method<&AcbrLibBoleto::TamNossoNumero>("TamNossoNumero", {
            Php::ByVal("eCarteira", Php::Type::String),
            Php::ByVal("enossoNumero", Php::Type::String),
            Php::ByVal("eConvenio", Php::Type::String),
        });

        Boleto.method<&AcbrLibBoleto::CodigosMoraAceitos>("CodigosMoraAceitos");

        Boleto.method<&AcbrLibBoleto::SelecionaBanco>("SelecionaBanco", {
            Php::ByVal("eCodBanco", Php::Type::String)
        });

        Boleto.method<&AcbrLibBoleto::MontarNossoNumero>("MontarNossoNumero", {
            Php::ByVal("idx", Php::Type::Numeric)
        });

        Boleto.method<&AcbrLibBoleto::EnviarBoleto>("EnviarBoleto");

        // add the class to the extension
        extension.add(std::move(Boleto));

        return extension;
    }
}
