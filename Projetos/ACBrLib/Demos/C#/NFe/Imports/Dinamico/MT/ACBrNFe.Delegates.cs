using System;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.NFe
{
    public sealed partial class ACBrNFe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterXml(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GravarXml(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterIni(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GravarIni(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarEventoXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarEventoINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_LimparLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_LimparListaEventos(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Assinar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Validar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ValidarRegrasdeNegocios(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_VerificarAssinatura(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GerarChave(IntPtr handle, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
            int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GetPath(IntPtr handle, int tipo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GetPathEvento(IntPtr handle, string aCodEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_StatusServico(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Consultar(IntPtr handle, string eChaveOuNFe, bool AExtrairEventos, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConsultaCadastro(IntPtr handle, string cUF, string nDocumento, bool nIE, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Inutilizar(IntPtr handle, string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Enviar(IntPtr handle, int aLote, bool imprimir, bool sincrono, bool zipado, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConsultarRecibo(IntPtr handle, string aRecibo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Cancelar(IntPtr handle, string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEvento(IntPtr handle, int alote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorUltNSU(IntPtr handle, int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorNSU(IntPtr handle, int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorChave(IntPtr handle, int acUFAutor, string eCnpjcpf, string echNFe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEmail(IntPtr handle, string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEmailEvento(IntPtr handle, string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Imprimir(IntPtr handle, string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview, string cMarcaDagua, string bViaConsumidor, string bSimplificado);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirPDF(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirEvento(IntPtr handle, string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirEventoPDF(IntPtr handle, string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirInutilizacao(IntPtr handle, string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirInutilizacaoPDF(IntPtr handle, string eArquivoXml);
    }
}