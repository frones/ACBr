using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.MDFe
{
    public sealed partial class ACBrMDFe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_OpenSSLInfo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterXml(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GravarXml(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterIni(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GravarIni(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarEventoXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_CarregarEventoINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_LimparLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_LimparListaEventos(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Assinar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Validar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ValidarRegrasdeNegocios(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_VerificarAssinatura(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GerarChave(IntPtr handle, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
            int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GetPath(IntPtr handle, int tipo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_GetPathEvento(IntPtr handle, string aCodEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_StatusServico(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Consultar(IntPtr handle, string eChaveOuCTe, bool aExtrairEventos, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Enviar(IntPtr handle, int aLote, bool imprimir, bool sincrono, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConsultarRecibo(IntPtr handle, string aRecibo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Cancelar(IntPtr handle, string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEvento(IntPtr handle, int alote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EncerrarMDFe(IntPtr handle, string eChaveOuMDFe, string eDtEnc, string cMunicipioDescarga, string nCNPJ, string nProtocolo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ConsultaMDFeNaoEnc(IntPtr handle, string nCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorUltNSU(IntPtr handle, int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorNSU(IntPtr handle, int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_DistribuicaoDFePorChave(IntPtr handle, int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEmail(IntPtr handle, string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_EnviarEmailEvento(IntPtr handle, string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_Imprimir(IntPtr handle, string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirPDF(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirEvento(IntPtr handle, string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MDFE_ImprimirEventoPDF(IntPtr handle, string eArquivoXmlNFe, string eArquivoXmlEvento);

        protected override void InitializeMethods()
        {
            AddMethod<MDFE_Inicializar>("MDFE_Inicializar");
            AddMethod<MDFE_Finalizar>("MDFE_Finalizar");
            AddMethod<MDFE_Nome>("MDFE_Nome");
            AddMethod<MDFE_Versao>("MDFE_Versao");
            AddMethod<MDFE_OpenSSLInfo>("MDFE_OpenSSLInfo");
            AddMethod<MDFE_UltimoRetorno>("MDFE_UltimoRetorno");
            AddMethod<MDFE_ConfigImportar>("MDFE_ConfigImportar");
            AddMethod<MDFE_ConfigExportar>("MDFE_ConfigExportar");
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