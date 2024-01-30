using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Reinf
{
    public sealed partial class ACBrReinf
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CriarEventoReinf(IntPtr handle, string eArqIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_EnviarReinf(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConsultarReinf(IntPtr handle, string eProtocolo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConsultarReciboReinf(IntPtr handle, string ePerApur, int aTipoEvento, string eNrInscEstab,
            string eCnpjPrestador, string eNrInscTomador, string eDtApur, string eCpfCnpjBenef,
            string eCnpjFonte, StringBuilder buffer, ref int bufferSize);
        
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CriarEnviarReinf(IntPtr handle, string eArqIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_LimparReinf(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CarregarXMLEventoReinf(IntPtr handle, string eArquivoOuXML);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetIDContribuinte(IntPtr handle, string aIdContribuinte);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetIDTransmissor(IntPtr handle, string aIdTransmissor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetTipoContribuinte(IntPtr handle, int aTipoContribuinte);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetVersaoDF(IntPtr handle, string sVersao);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<Reinf_Inicializar>("Reinf_Inicializar");
            AddMethod<Reinf_Finalizar>("Reinf_Finalizar");
            AddMethod<Reinf_Nome>("Reinf_Nome");
            AddMethod<Reinf_Versao>("Reinf_Versao");
            AddMethod<Reinf_UltimoRetorno>("Reinf_UltimoRetorno");
            AddMethod<Reinf_ConfigImportar>("Reinf_ConfigImportar");
            AddMethod<Reinf_ConfigExportar>("Reinf_ConfigExportar");
            AddMethod<Reinf_ConfigLer>("Reinf_ConfigLer");
            AddMethod<Reinf_ConfigGravar>("Reinf_ConfigGravar");
            AddMethod<Reinf_ConfigLerValor>("Reinf_ConfigLerValor");
            AddMethod<Reinf_ConfigGravarValor>("Reinf_ConfigGravarValor");
            AddMethod<Reinf_CriarEventoReinf>("Reinf_CriarEventoReinf");
            AddMethod<Reinf_EnviarReinf>("Reinf_EnviarReinf");
            AddMethod<Reinf_ConsultarReinf>("Reinf_ConsultarReinf");
            AddMethod<Reinf_ConsultarReciboReinf>("Reinf_ConsultarReciboReinf");
            AddMethod<Reinf_CriarEnviarReinf>("Reinf_CriarEnviarReinf");
            AddMethod<Reinf_LimparReinf>("Reinf_LimparReinf");
            AddMethod<Reinf_CarregarXMLEventoReinf>("Reinf_CarregarXMLEventoReinf");
            AddMethod<Reinf_SetIDContribuinte>("Reinf_SetIDContribuinte");
            AddMethod<Reinf_SetIDTransmissor>("Reinf_SetIDTransmissor");
            AddMethod<Reinf_SetTipoContribuinte>("Reinf_SetTipoContribuinte");
            AddMethod<Reinf_SetVersaoDF>("Reinf_SetVersaoDF");
            AddMethod<Reinf_ObterCertificados>("Reinf_ObterCertificados");
        }
    }
}