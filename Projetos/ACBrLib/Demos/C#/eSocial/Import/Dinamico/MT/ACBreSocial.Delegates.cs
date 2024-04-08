using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.eSocial
{
    public sealed partial class ACBreSocial
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_OpenSSLInfo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_CriarEventoeSocial(IntPtr handle, string eArqIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_EnviareSocial(IntPtr handle, int aGrupo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConsultareSocial(IntPtr handle, string eProtocolo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_CriarEnviareSocial(IntPtr handle, string eArqIni, int aGrupo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_LimpareSocial(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_CarregarXMLEventoeSocial(IntPtr handle, string eArquivoOuXML);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_SetIDEmpregador(IntPtr handle, string aIdEmpregador);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_SetIDTransmissor(IntPtr handle, string aIdTransmissor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_SetTipoEmpregador(IntPtr handle, int aTipoEmpregador);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_SetVersaoDF(IntPtr handle, string sVersao);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConsultaIdentificadoresEventosEmpregador(IntPtr handle, string aIdEmpregador, int aTipoEvento, DateTime aPeriodoApuracao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConsultaIdentificadoresEventosTabela(IntPtr handle, string aIdEmpregador, int aTipoEvento, string aChave, DateTime aDataInicial, DateTime aDataFinal, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ConsultaIdentificadoresEventosTrabalhador(IntPtr handle, string aIdEmpregador, string aCPFTrabalhador, DateTime aDataInicial, DateTime aDataFinal, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_DownloadEventos(IntPtr handle, string aIdEmpregador, string aCPFTrabalhador, DateTime aDataInicial, DateTime aDataFinal, StringBuilder buffer, ref int bufferSize);
        
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int eSocial_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<eSocial_Inicializar>("eSocial_Inicializar");
            AddMethod<eSocial_Finalizar>("eSocial_Finalizar");
            AddMethod<eSocial_Nome>("eSocial_Nome");
            AddMethod<eSocial_Versao>("eSocial_Versao");
            AddMethod<eSocial_OpenSSLInfo>("eSocial_OpenSSLInfo");
            AddMethod<eSocial_UltimoRetorno>("eSocial_UltimoRetorno");
            AddMethod<eSocial_ConfigImportar>("eSocial_ConfigImportar");
            AddMethod<eSocial_ConfigExportar>("eSocial_ConfigExportar");
            AddMethod<eSocial_ConfigLer>("eSocial_ConfigLer");
            AddMethod<eSocial_ConfigGravar>("eSocial_ConfigGravar");
            AddMethod<eSocial_ConfigLerValor>("eSocial_ConfigLerValor");
            AddMethod<eSocial_ConfigGravarValor>("eSocial_ConfigGravarValor");
            AddMethod<eSocial_CriarEventoeSocial>("eSocial_CriarEventoeSocial");
            AddMethod<eSocial_EnviareSocial>("eSocial_EnviareSocial");
            AddMethod<eSocial_ConsultareSocial>("eSocial_ConsultareSocial");
            AddMethod<eSocial_CriarEnviareSocial>("eSocial_CriarEnviareSocial");
            AddMethod<eSocial_LimpareSocial>("eSocial_LimpareSocial");
            AddMethod<eSocial_CarregarXMLEventoeSocial>("eSocial_CarregarXMLEventoeSocial");
            AddMethod<eSocial_SetIDEmpregador>("eSocial_SetIDEmpregador");
            AddMethod<eSocial_SetIDTransmissor>("eSocial_SetIDTransmissor");
            AddMethod<eSocial_SetTipoEmpregador>("eSocial_SetTipoEmpregador");
            AddMethod<eSocial_SetVersaoDF>("eSocial_SetVersaoDF");
            AddMethod<eSocial_ConsultaIdentificadoresEventosEmpregador>("eSocial_ConsultaIdentificadoresEventosEmpregador");
            AddMethod<eSocial_ConsultaIdentificadoresEventosTabela>("eSocial_ConsultaIdentificadoresEventosTabela");
            AddMethod<eSocial_ConsultaIdentificadoresEventosTrabalhador>("eSocial_ConsultaIdentificadoresEventosTrabalhador");
            AddMethod<eSocial_DownloadEventos>("eSocial_DownloadEventos");
            AddMethod<eSocial_ObterCertificados>("eSocial_ObterCertificados");

        }
    }
}