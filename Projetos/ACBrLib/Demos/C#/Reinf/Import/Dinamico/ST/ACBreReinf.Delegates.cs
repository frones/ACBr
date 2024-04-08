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
        public delegate int Reinf_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CriarEventoReinf(string eArqIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_EnviarReinf(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConsultarReinf(string eProtocolo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ConsultarReciboReinf(string ePerApur, int aTipoEvento, string eNrInscEstab,
            string eCnpjPrestador, string eNrInscTomador, string eDtApur, string eCpfCnpjBenef,
            string eCnpjFonte, StringBuilder buffer, ref int bufferSize);
        
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CriarEnviarReinf(string eArqIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_LimparReinf();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_CarregarXMLEventoReinf(string eArquivoOuXML);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetIDContribuinte(string aIdContribuinte);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetIDTransmissor(string aIdTransmissor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetTipoContribuinte(int aTipoContribuinte);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_SetVersaoDF(string sVersao);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Reinf_ObterCertificados(StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<Reinf_Inicializar>("Reinf_Inicializar");
            AddMethod<Reinf_Finalizar>("Reinf_Finalizar");
            AddMethod<Reinf_Nome>("Reinf_Nome");
            AddMethod<Reinf_Versao>("Reinf_Versao");
            AddMethod<Reinf_OpenSSLInfo>("Reinf_OpenSSLInfo");
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