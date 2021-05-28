using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.GNRe
{
    public sealed partial class ACBrGNRe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_CarregarXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_CarregarINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ObterXml(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_GravarXml(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_CarregarGuiaRetorno(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_LimparLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_LimparListaGuiaRetorno(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Assinar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Validar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_VerificarAssinatura(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Enviar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Consultar(IntPtr handle, string eUF, int AReceita, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_EnviarEmail(IntPtr handle, string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_Imprimir(IntPtr handle, string eNomeImpressora, string eMostrarPreview);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GNRE_ImprimirPDF(IntPtr handle);

        protected override void InitializeMethods()
        {
            AddMethod<GNRE_Inicializar>("GNRE_Inicializar");
            AddMethod<GNRE_Finalizar>("GNRE_Finalizar");
            AddMethod<GNRE_Nome>("GNRE_Nome");
            AddMethod<GNRE_Versao>("GNRE_Versao");
            AddMethod<GNRE_UltimoRetorno>("GNRE_UltimoRetorno");
            AddMethod<GNRE_ConfigImportar>("GNRE_ConfigImportar");
            AddMethod<GNRE_ConfigExportar>("GNRE_ConfigExportar");
            AddMethod<GNRE_ConfigLer>("GNRE_ConfigLer");
            AddMethod<GNRE_ConfigGravar>("GNRE_ConfigGravar");
            AddMethod<GNRE_ConfigLerValor>("GNRE_ConfigLerValor");
            AddMethod<GNRE_ConfigGravarValor>("GNRE_ConfigGravarValor");
            AddMethod<GNRE_CarregarXML>("GNRE_CarregarXML");
            AddMethod<GNRE_CarregarINI>("GNRE_CarregarINI");
            AddMethod<GNRE_ObterXml>("GNRE_ObterXml");
            AddMethod<GNRE_GravarXml>("GNRE_GravarXml");
            AddMethod<GNRE_CarregarGuiaRetorno>("GNRE_CarregarGuiaRetorno");
            AddMethod<GNRE_LimparLista>("GNRE_LimparLista");
            AddMethod<GNRE_LimparListaGuiaRetorno>("GNRE_LimparListaGuiaRetorno");
            AddMethod<GNRE_Assinar>("GNRE_Assinar");
            AddMethod<GNRE_Validar>("GNRE_Validar");
            AddMethod<GNRE_VerificarAssinatura>("GNRE_VerificarAssinatura");
            AddMethod<GNRE_ObterCertificados>("GNRE_ObterCertificados");
            AddMethod<GNRE_Enviar>("GNRE_Enviar");
            AddMethod<GNRE_Consultar>("GNRE_Consultar");
            AddMethod<GNRE_EnviarEmail>("GNRE_EnviarEmail");
            AddMethod<GNRE_Imprimir>("GNRE_Imprimir");
            AddMethod<GNRE_ImprimirPDF>("GNRE_ImprimirPDF");
        }
    }
}