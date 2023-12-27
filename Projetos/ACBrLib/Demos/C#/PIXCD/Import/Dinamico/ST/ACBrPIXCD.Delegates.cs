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
        public delegate int PIXCD_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_GerarQRCodeEstatico(double AValor, string AinfoAdicional, string ATxID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarPix(string Ae2eid, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarPixRecebidos(DateTime ADataInicio, DateTime ADataFim, string ATxId, string ACpfCnpj, int PagAtual, int ItensPorPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_SolicitarDevolucaoPix(string AInfDevolucao, string Ae2eid, string AidDevolucao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarDevolucaoPix(string Ae2eid, string AidDevolucao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CriarCobrancaImediata(string AInfCobSolicitada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobrancaImediata(string ATxId, int ARevisao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_RevisarCobrancaImediata(string AInfCobRevisada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CancelarCobrancaImediata(string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CriarCobranca(string AInfCobVSolicitada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConsultarCobranca(string ATxId, int ARevisao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_RevisarCobranca(string AInfCobVRevisada, string ATxId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_CancelarCobranca(string ATxId, StringBuilder buffer, ref int bufferSize); 

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
            AddMethod<PIXCD_RevisarCobrancaImediata>("PIXCD_RevisarCobrancaImediata");
            AddMethod<PIXCD_CancelarCobrancaImediata>("PIXCD_CancelarCobrancaImediata");
            AddMethod<PIXCD_CriarCobranca>("PIXCD_CriarCobranca");
            AddMethod<PIXCD_ConsultarCobranca > ("PIXCD_ConsultarCobranca");
            AddMethod<PIXCD_RevisarCobranca>("PIXCD_RevisarCobranca");
            AddMethod<PIXCD_CancelarCobranca>("PIXCD_CancelarCobranca");
        }
    }
}