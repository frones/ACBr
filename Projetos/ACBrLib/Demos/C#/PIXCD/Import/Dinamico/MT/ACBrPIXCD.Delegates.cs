using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.PIXCD
{
    public sealed partial class ACBrPIXCD
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_GerarQRCodeEstatico(IntPtr handle, double AValor, string AinfoAdicional, string ATxID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarPix(IntPtr handle, string Ae2eid, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarPixRecebidos(IntPtr handle, DateTime ADataInicio, DateTime ADataFim, string ATxId, string ACpfCnpj, int PagAtual, int ItensPorPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_SolicitarDevolucaoPix(IntPtr handle, string AInfDevolucao, string Ae2eid, string AidDevolucao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarDevolucaoPix(IntPtr handle, string Ae2eid, string AidDevolucao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CriarCobrancaImediata(IntPtr handle, string AInfCobSolicitada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobrancaImediata(IntPtr handle, string ATxId, int ARevisao, StringBuilder buffer, ref int bufferSize);
        
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobrancasCob(IntPtr handle, DateTime ADataInicio, DateTime ADataFim, string ACpfCnpj, Boolean ALocationPresente, int AStatus, int PagAtual, int ItensPorPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_RevisarCobrancaImediata(IntPtr handle, string AInfCobRevisada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CancelarCobrancaImediata(IntPtr handle, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CriarCobranca(IntPtr handle, string AInfCobVSolicitada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobranca(IntPtr handle, string ATxId, int ARevisao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobrancasCobV(IntPtr handle, DateTime ADataInicio, DateTime ADataFim, string ACpfCnpj, Boolean ALocationPresente, int AStatus, int PagAtual, int ItensPorPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_RevisarCobranca(IntPtr handle, string AInfCobVRevisada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CancelarCobranca(IntPtr handle, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_OpenSSLInfo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<PIXCD_Inicializar>("PIXCD_Inicializar");
            AddMethod<PIXCD_Finalizar>("PIXCD_Finalizar");
            AddMethod<PIXCD_Nome>("PIXCD_Nome");
            AddMethod<PIXCD_Versao>("PIXCD_Versao");
            AddMethod<PIXCD_UltimoRetorno>("PIXCD_UltimoRetorno");
            AddMethod<PIXCD_ConfigImportar>("PIXCD_ConfigImportar");
            AddMethod<PIXCD_ConfigExportar>("PIXCD_ConfigExportar");
            AddMethod<PIXCD_ConfigLer>("PIXCD_ConfigLer");
            AddMethod<PIXCD_ConfigGravar>("PIXCD_ConfigGravar");
            AddMethod<PIXCD_ConfigLerValor>("PIXCD_ConfigLerValor");
            AddMethod<PIXCD_ConfigGravarValor>("PIXCD_ConfigGravarValor");
            AddMethod<PIXCD_GerarQRCodeEstatico>("PIXCD_GerarQRCodeEstatico");
            AddMethod<PIXCD_ConsultarPix>("PIXCD_ConsultarPix");
            AddMethod<PIXCD_ConsultarPixRecebidos>("PIXCD_ConsultarPixRecebidos");
            AddMethod<PIXCD_SolicitarDevolucaoPix>("PIXCD_SolicitarDevolucaoPix");
            AddMethod<PIXCD_ConsultarDevolucaoPix>("PIXCD_ConsultarDevolucaoPix");
            AddMethod<PIXCD_CriarCobrancaImediata>("PIXCD_CriarCobrancaImediata");
            AddMethod<PIXCD_ConsultarCobrancaImediata>("PIXCD_ConsultarCobrancaImediata");
            AddMethod<PIXCD_ConsultarCobrancasCob>("PIXCD_ConsultarCobrancasCob");
            AddMethod<PIXCD_RevisarCobrancaImediata>("PIXCD_RevisarCobrancaImediata");
            AddMethod<PIXCD_CancelarCobrancaImediata>("PIXCD_CancelarCobrancaImediata");
            AddMethod<PIXCD_CriarCobranca>("PIXCD_CriarCobranca");
            AddMethod<PIXCD_ConsultarCobranca > ("PIXCD_ConsultarCobranca");
            AddMethod<PIXCD_ConsultarCobrancasCobV>("PIXCD_ConsultarCobrancasCobV");
            AddMethod<PIXCD_RevisarCobranca>("PIXCD_RevisarCobranca");
            AddMethod<PIXCD_CancelarCobranca>("PIXCD_CancelarCobranca");
            AddMethod<PIXCD_OpenSSLInfo>("PIXCD_OpenSSLInfo");
        }
    }
}