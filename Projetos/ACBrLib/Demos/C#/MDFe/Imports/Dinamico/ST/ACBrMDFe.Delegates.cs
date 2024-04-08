using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using static ACBrLib.MDFe.ACBrMDFe;

namespace ACBrLib.MDFe
{
    public sealed partial class ACBrMDFe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterXml(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GravarXml(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterIni(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GravarIni(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarEventoXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarEventoINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_LimparLista();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_LimparListaEventos();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Assinar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Validar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
            int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterCertificados(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GetPath(int tipo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GetPathEvento(string aCodEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_StatusServico(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Consultar(string eChaveOuCTe, bool aExtrairEventos, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Enviar(int aLote, bool imprimir, bool sincrono, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EncerrarMDFe(string eChaveOuMDFe, string eDtEnc, string cMunicipioDescarga, string nCNPJ, string nProtocolo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConsultaMDFeNaoEnc(string nCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEmail(string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Imprimir(string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirPDF();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento);

        protected override void InitializeMethods()
        {
            AddMethod<MDFE_Inicializar>("MDFE_Inicializar");
            AddMethod<MDFE_Finalizar>("MDFE_Finalizar");
            AddMethod<MDFE_Nome>("MDFE_Nome");
            AddMethod<MDFE_Versao>("MDFE_Versao");
            AddMethod<MDFE_OpenSSLInfo>("MDFE_OpenSSLInfo");
            AddMethod<MDFE_ConfigImportar>("MDFE_ConfigImportar");
            AddMethod<MDFE_ConfigExportar>("MDFE_ConfigExportarz");
            AddMethod<MDFE_UltimoRetorno>("MDFE_UltimoRetorno");
            AddMethod<MDFE_ConfigLer>("MDFE_ConfigLer");
            AddMethod<MDFE_ConfigGravar>("MDFE_ConfigGravar");
            AddMethod<MDFE_ConfigLerValor>("MDFE_ConfigLerValor");
            AddMethod<MDFE_ConfigGravarValor>("MDFE_ConfigGravarValor");
            AddMethod<MDFE_CarregarXML>("MDFE_CarregarXML");
            AddMethod<MDFE_CarregarINI>("MDFE_CarregarINI");
            AddMethod<MDFE_ObterXml>("MDFE_ObterXml");
            AddMethod<MDFE_GravarXml>("MDFE_GravarXml");
            AddMethod<MDFE_ObterIni>("MDFE_ObterIni");
            AddMethod<MDFE_GravarIni>("MDFE_GravarIni");
            AddMethod<MDFE_CarregarEventoXML>("MDFE_CarregarEventoXML");
            AddMethod<MDFE_CarregarEventoINI>("MDFE_CarregarEventoINI");
            AddMethod<MDFE_LimparLista>("MDFE_LimparLista");
            AddMethod<MDFE_LimparListaEventos>("MDFE_LimparListaEventos");
            AddMethod<MDFE_Assinar>("MDFE_Assinar");
            AddMethod<MDFE_Validar>("MDFE_Validar");
            AddMethod<MDFE_ValidarRegrasdeNegocios>("MDFE_ValidarRegrasdeNegocios");
            AddMethod<MDFE_VerificarAssinatura>("MDFE_VerificarAssinatura");
            AddMethod<MDFE_GerarChave>("MDFE_GerarChave");
            AddMethod<MDFE_ObterCertificados>("MDFE_ObterCertificados");
            AddMethod<MDFE_GetPath>("MDFE_GetPath");
            AddMethod<MDFE_GetPathEvento>("MDFE_GetPathEvento");
            AddMethod<MDFE_StatusServico>("MDFE_StatusServico");
            AddMethod<MDFE_Consultar>("MDFE_Consultar");
            AddMethod<MDFE_Enviar>("MDFE_Enviar");
            AddMethod<MDFE_ConsultarRecibo>("MDFE_ConsultarRecibo");
            AddMethod<MDFE_Cancelar>("MDFE_Cancelar");
            AddMethod<MDFE_EnviarEvento>("MDFE_EnviarEvento");
            AddMethod<MDFE_EncerrarMDFe>("MDFE_EncerrarMDFe");
            AddMethod<MDFE_ConsultaMDFeNaoEnc>("MDFE_ConsultaMDFeNaoEnc");
            AddMethod<MDFE_DistribuicaoDFePorUltNSU>("MDFE_DistribuicaoDFePorUltNSU");
            AddMethod<MDFE_DistribuicaoDFePorNSU>("MDFE_DistribuicaoDFePorNSU");
            AddMethod<MDFE_DistribuicaoDFePorChave>("MDFE_DistribuicaoDFePorChave");
            AddMethod<MDFE_EnviarEmail>("MDFE_EnviarEmail");
            AddMethod<MDFE_EnviarEmailEvento>("MDFE_EnviarEmailEvento");
            AddMethod<MDFE_Imprimir>("MDFE_Imprimir");
            AddMethod<MDFE_ImprimirPDF>("MDFE_ImprimirPDF");
            AddMethod<MDFE_ImprimirEvento>("MDFE_ImprimirEvento");
            AddMethod<MDFE_ImprimirEventoPDF>("MDFE_ImprimirEventoPDF");
        }
    }
}